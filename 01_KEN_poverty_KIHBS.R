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

# To read them all at once and store in a list
df <- read_path %>% 
  excel_sheets() %>% 
  
  # Use basename with the file_path_sans_ext to strip extra info; Not NEEDED
  #set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% 
  set_names() %>% 
  map(~ read_excel(path = read_path, sheet = .x), .id = "sheet")





