# Read, clean and rename drinking water indicators

#library(tidyverse)
library(tidyverse)
library(readxl)
#library(readxl) ### read more here https://readxl.tidyverse.org

# Previously set datapath in 00_SetupData.R file
dir(datapath)

# Set file_name (b/c it's so long) and check sheets in file so we know what to call in the read_excel call
file_name <- c("Table 3.9 Percentage Distribution of Households by Main Source of Drinking Water and ResidenceCounty.xlsx")
excel_sheets(file.path(datapath, file_name))


# Read in the data and the data labels ------------------------------------

  impr_water <- read_excel(file.path(datapath, file_name), 
                           skip = 12,
                           n_max = 51, # 51 because 47 Counties + rural + urban + National + County name
                           sheet = "Table 1")
  
  impr_water_names <- read_excel(file.path(datapath, 
                                 file_name), 
                                 skip = 10,
                                 n_max = 2,
                                 sheet = "Table 1")

  impr_water_ideal <- read_excel(file.path(datapath, file_name),
                                sheet = "ideal_table")

  

# Clean up tables ---------------------------------------------------------

# Ideal Table first  
# Start with the low hanging fruit and clean up the ideal table. Need to do three things
# 1. Fix the geography names
# 2. Create binary variables for county, rural/urban, and national (see case_when video: https://www.youtube.com/watch?v=amiW9H-oOS4)
# 3. Convert all values to be percentages
    
ideal_df <- 
    impr_water_ideal %>%
    # Create a new column with admin names only
    mutate(admin_unit = gsub("[^[:alnum:][:space:]]", "", Geography),
           admin_type = case_when(
             admin_unit == "National" ~ "National",
             admin_unit == "Rural"    ~ "Rural",
             admin_unit == "Urban"    ~ "Urban",
             TRUE                     ~ "County" 
           )) %>% 
    # Now convert all numeric fields to percentages (see https://dplyr.tidyverse.org/reference/summarise_all.html)
    mutate_at(vars(pipd_into_dwelling:`Bottled water`), funs(. / 100))
  
  # TODO: Check the County names and see if we need to fix any such as Taita  Taveta, harakaNithi
  ideal_df %>% group_by(admin_unit) %>% tally() %>% print(n = Inf)
  
# Reshape the data to be long so we can filter in Tableau
  ideal_df_long_county <- 
    ideal_df %>% 
    filter(Geography != "County") %>% 
    filter(admin_type == "County") %>% 
    gather(., pipd_into_dwelling:`Bottled water`, value = "percent", key = "water_type") %>% 
    rename(county = admin_unit)
  
  
# Now for the messy data
  # 1. Tackle the names to get them in a form we can wrangle
  # 2. Drop all the columns that have missing information
  # 3. Re-run same process we used on the ideal data
  
# Create a function to detect when all the values of a vector are true  
not_all_na <- function(x) {!all(is.na(x))}

impr_water %>% 
  select_if(not_all_na) %>% names()
names(ideal_df)

impr_water_clean <- 
  impr_water %>%
  rename(Geography = X__1,
         pipd_into_dwelling = `Into dwelling`,
         piped_into_yard = `Into plot/ yard`,
         `piped_into_public tap_stand_pipe` = `public tap/ stand pipe`,
         `Tubewell/ borehole with pump` = X__11,
         `Protected well` = X__13,
         `Protected spring` = X__14,
         `Rain water collection` = X__15,
         `Bottled water` = X__16) %>% 
  select_if(not_all_na) %>% 
  mutate(admin_unit = gsub("[^[:alnum:][:space:]]", "", Geography),
         admin_type = case_when(
           admin_unit == "National" ~ "National",
           admin_unit == "Rural"    ~ "Rural",
           admin_unit == "Urban"    ~ "Urban",
           TRUE                     ~ "County" 
         )) %>% 
  # Now convert all numeric fields to percentages (see https://dplyr.tidyverse.org/reference/summarise_all.html)
  mutate_at(vars(pipd_into_dwelling:`Bottled water`), funs(. / 100))
summary(ideal_df)  
  
  
  
  