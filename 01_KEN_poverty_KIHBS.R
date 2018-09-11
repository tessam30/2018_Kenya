# Purpose: Process Poverty data from KIHBS
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_09
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr")

read_path <- "KEN_IHBS_2016.xlsx"

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
  select(contains("population"), geo_id, population) %>%
  gather(., 
         population_0_5:population_0_17,
         value = "poopulation",
         key = "age_range")

child_pov_pct <- 
  Proportion_poor_children %>% 
  select(-contains("population")) %>% 
  gather(., 
         Poverty_hc_rate:Poverty_hc_rate_0_17, 
         value = "poverty",
         key = "poverty age range")

