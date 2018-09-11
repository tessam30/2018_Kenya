# Purpose: Process Poverty data from KIHBS
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_09
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr")


# Set path and folders where data live ------------------------------------

read_path <- "KEN_IHBS_2016.xlsx"
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"


# Start the load routine --------------------------------------------------

# List of the names of the excel sheets
read_path %>%
  excel_sheets()

# To read them all at once and break apart into separate data frames
read_path %>%
  excel_sheets() %>%
  
  # Use basename with the file_path_sans_ext to strip extra info; Not NEEDED
  # set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  set_names() %>%
  map(~read_excel(path = read_path, sheet = .x), .id = "sheet") %>%
  
  # Use the list2env command to convert the list of dataframes to separate dfs using the name provided by the set_names(command). Necessary b/c not all the sheets are the same size
  list2env(., envir = .GlobalEnv)


# Write the files to the data folder for later use if needed
# Create your list first, then in second chunk write to folder
df <- excel_sheets(read_path) %>%
  set_names() %>%
  map(read_excel, path = read_path)

df %>%
  names() %>%
  map(., ~write_csv(df[[.]], paste0(datapath, "/", ., ".csv")))


# Combine like variables and merge in crosswalk ---------------------------

food_pov <- 
  Food_poverty %>% 
  gather(., 
         Headcount_rate_food_poverty:Severity_of_poverty,
         value = "percent", 
         key = "food_poverty_type") %>% 
  left_join(., geo_cw, by = c("geo_id" = "County"))

poverty <- 
  Overall_poverty %>% 
  gather(., 
         Headcount_rate:Severity_of_poverity, 
         value = "percent", 
         key = "poverty_type") %>% 
  left_join(., geo_cw, by = c("geo_id" = "County"))

child_pov_pop <- 
  Proportion_poor_children %>% 
  select(contains("population"), geo_id) %>%
  gather(., 
         population:population_0_17,
         value = "poopulation",
         key = "age_range") %>% 
  # Use case_when to get at unique combo of age_range + geography for merge
  mutate(age = case_when(
    age_range == "population"      ~ "All",
    age_range == "population_0_17" ~ "0_17",
    age_range == "population_0_5"  ~ "0_5",
    age_range == "population_6_13" ~ "6_13",
    TRUE ~ "14_17"
  ))

child_pov_pct <- 
  Proportion_poor_children %>% 
  select(-contains("population")) %>% 
  gather(., 
         Poverty_hc_rate:Poverty_hc_rate_0_17, 
         value = "poverty",
         key = "age_range") %>% 
  mutate(age = case_when(
    age_range == "Poverty_hc_rate"      ~ "All",
    age_range == "Poverty_hc_rate_0_17" ~ "0_17",
    age_range == "Poverty_hc_rate_0_5"  ~ "0_5",
    age_range == "Poverty_hc_rate_6_13" ~ "6_13",
    TRUE ~ "14_17"
  ))

# Now combine the two to get a useable population + rate data frame
# TODO: FIX the merge fields to get all the districts (should match with GADM names)
pov_child <- 
  child_pov_pct %>% 
  left_join(child_pov_pop, by = c("geo_id", "age")) %>% 
  select(-age_range.y) %>% 
  rename(pov_hc_range = age_range.x) %>% 
  left_join(., geo_cw, by = c("geo_id" = "County"))



# Mapping magic -----------------------------------------------------------

# Might as well go ahead and join it with a shapefile and create couple sample maps
  gis_admin1 <- read_sf(file.path(gispath, "gadm36_KEN_1.shp"))

 
  gis_admin1 %>%
  left_join(., pov_child, by = c("GID_1")) %>%
  ggplot(.) +
  geom_sf(
    lwd = 0.1, col = "white",
    aes(fill = poverty)
  ) +
  facet_wrap(~ age, nrow = 1) +
  scale_fill_viridis_c() +
  theme(legend.position = "none")

