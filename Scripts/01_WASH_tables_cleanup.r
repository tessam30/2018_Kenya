# Read, clean and rename drinking water indicators

library(tidyverse)
library(readxl)
library(purrr)
#library(readxl) ### read more here https://readxl.tidyverse.org

# Previously set datapath in 00_SetupData.R file
dir(datapath)


# Read in the data and the data labels ------------------------------------
read_path <- file.path(datapath, "KEN_WASH_2018_tables.xlsx")
excel_sheets(read_path)


# To read them all at once and break apart into separate data frames
# May not need this as the function below will do the reformatting
# read_path %>%
#   excel_sheets() %>%
#   
#   # Use basename with the file_path_sans_ext to strip extra info; Not NEEDED
#   # set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
#   set_names() %>%
#   map(~read_excel(path = read_path, sheet = .x), .id = "sheet") %>%
#   
#   # Use the list2env command to convert the list of dataframes to separate dfs using 
#   # the name provided by the set_names(command). Necessary b/c not all the sheets are the same size
#   list2env(., envir = .GlobalEnv)


df_wash <- excel_sheets(read_path) %>%
  set_names() %>%
  map(read_excel, path = read_path)
str(df_wash)

# Clean up tables ---------------------------------------------------------
# Write a function to do the following across data frames
# 1. Fix the geography names by stripping extra characters
# 2. create binaries for level of geography
# 3. convert all numeric variables to percentages

# Create a separate dataframe with a cross walk in it
geo_cw <- map_df(df_wash[6], `[`)
# can also use pluck
# test <- df_wash %>% pluck(6)

geo_clean <- function(df) {
  df %>% 
    # Create a new column with admin names only
    mutate(admin_unit = gsub("[^[:alnum:][:space:]]", "", Geography),
           admin_type = case_when(
             admin_unit == "National" ~ "National",
             admin_unit == "Rural"    ~ "Rural",
             admin_unit == "Urban"    ~ "Urban",
             TRUE                     ~ "County" 
           )) %>% 
    # Now convert all numeric (doubles) fields to percentages (see https://dplyr.tidyverse.org/reference/summarise_all.html)
    mutate_if(is.double, funs(. / 100)) %>% 
    left_join(., geo_cw, by = c("admin_unit" = "County"))
}

# Apply the function to the first 5 datasets loaded in the df_wash list
# Convert each element to a new dataframe
df_wash_clean <- map(df_wash[1:5], ~geo_clean(.))

# Now convert each element of list to a separate data.frame
list2env(df_wash_clean, envir=.GlobalEnv)

# Check each df for structure
map(list(improved_sanitation, unimproved_sanitation), names)

# Reshape function to make indicators long --------------------------------

# Write a function to execute the reshape for each dataframe
df_long <- function(df, x, y) {
  x_quo <- enquo(x)
  y_quo <- enquo(y)

  df %>% 
    gather(., !!x_quo:!!y_quo, 
           value = "percent", 
           key = "metric") %>% 
    rename(county = admin_unit) %>% 
    group_by(metric) %>% 
    mutate(rank = rank(percent))
  }



# Applying function to different data frames ------------------------------

impr_sanit <- df_long(improved_sanitation, 
        `Flush to Piped Sewer System`,
        `Composting Toilet`) 

impr_h20 <- df_long(improved_water, 
                    `pipd_into_dwelling`, 
                    `Bottled water`)

unimp_sanit <- unimproved_sanitation %>% 
  mutate(`Number of households` = `Number of households ('000)` * 1e5) %>% 
  df_long(`Flush to Some where else`,
          `Not Stated`)

unimpr_h20 <- unimproved_water %>% 
  mutate(`Number of Households` = `Number of Households ('000)` * 1e5) %>% 
  df_long(`Unprotected well`, `Not Stated`)

waste <- waste_disposal %>% 
  mutate(`Number of Households` = `Number of Households ('000)` * 1e5) %>% 
  df_long(`Collected by County Government`, `Not Stated`)


# Seems that Tableau does not need the data long & the water and sanitation tables can be combined
map(list(improved_sanitation, unimproved_sanitation), names)

sanit_wide <- improved_sanitation %>% 
  select(`Flush to Piped Sewer System`:`admin_unit`) %>% 
  left_join(., unimproved_sanitation, by = c("admin_unit")) %>% 
  mutate(`Number of households` = `Number of households ('000)` * 1e5)


map(list(improved_water, unimproved_water), names)
water_wide <- improved_water %>% 
  select(`pipd_into_dwelling`:`admin_unit`) %>% 
  left_join(., unimproved_water, by = c("admin_unit")) %>% 
  mutate(`Number of Households` = `Number of Households ('000)` * 1e5)
names(water_wide)


# write data to csvs ------------------------------------------------------

# Combine dataframe names into a list for purrring into a set of .csvs
datalist <- list(impr_sanit  = impr_sanit, 
               unimp_sanit   = unimp_sanit,
               impr_h20      = impr_h20,
               unimpr_h20    = unimpr_h20, 
               waste         = waste) 

datalist_wide <- list(sanit_wide = sanit_wide,
                      water_wide = water_wide, 
                      waste_disposal = waste_disposal)

datalist_wide %>%  
  names() %>% 
  map(., ~ write_csv(datalist_wide[[.]], file.path(washpath, str_c(., ".csv"))))

dir(washpath)





