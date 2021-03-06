# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey", "data.table", "lemon", "widyr", "RColorBrewer", "readxl")

# Create folders for project (if they do not exist)
# Data cannot be shared on Github so it has to be manually copied over due to IT restrictions
dir_list <- list("Data", "Tableau", "Scripts", "Logos", "Images", "Documents", "Data", "Budget")
dir_sublist <- list("Budget", "Elections", "Finance", "GIS", "Health", "IPC", "KIHBS", "MSME", "malnutrition", "Poverty", "Wash", "Youth")

map(dir_list, ~dir.create(.))
map(dir_sublist, ~dir.create(file.path("Data", .)))

# Setting shortcuts so you can use the file.path command throughout to easily write/read to directories
datapath <- "Data"
kihbspath <- "Data/KIHBS"
dataout <- "Data/KIHBS/Dataout"
povpath <- "Data/Poverty"
elecpath <- "Data/Elections"
msmepath <- "Data/MSME"
healthpath <- "Data/Health"
gispath <- "Data/GIS"
budgetpath <- "Data/Budget"
washpath <- "Data/Wash"
imagepath <- "Images"
rpath <- "Scripts"
ipcpath <- "Data/IPC"
fcspath <- "Data/Household Food Consumption score"

#Source helper functions to be used throughout analysis
file_list <- list("strip_geom.R", 
                  "KEN_helper_functions.R",
                  "county_list.R")

file_list %>% 
  map(~source(file.path(rpath, .)))
rm(file_list)


# Create merge base file that contains geography for household roster
# The hh roster is needed to recover hh level info for merging different datasets
hh_info <- read_dta(file.path(kihbspath, "HH_information.dta"))
vtable(hh_info)

hh_base <- hh_info %>% select(county, strat, resid, eatype, clid, hhid, hhsize, iday, weight) %>% 
  mutate(county_id = labelled::to_factor(county, levels = "values")) %>% 
  mutate(county_id = as.numeric(as.character(county_id)))

sjlabelled::get_labels(hh_base)
Hmisc::describe(hh_base)
sample_size = dim(hh_base)[1]


# Create a county crosswalk with county id and name
county_labels <- enframe(get_labels(hh_base$county)) %>% 
  mutate(county_id = row_number()) %>% 
  rename(county_name = value)
county_labels %>% print(n = 47)

# Read in ASAL county identifiers
# ASAL geo is used throughout to create maps on the fly and b/c they are recongized by Mission
asal_geo <- st_read(file.path(datapath, "GIS/ASAL_Counties_2018/ASAL_Counties_2018.shp"))
asal <- strip_geom(asal_geo, Counties, CID, Category) %>% 
  mutate(Category_num = case_when(
    Category == "Arid - 85-100% Aridity"         ~ 1,
    Category == "Semi - Arid - 10-29% Aridity"   ~ 3,
    Category == "Semi - Arid - 30-84% Aridity"   ~ 2,
    TRUE ~ 4
  ))
asal %>% group_by(Category) %>% 
  summarise(counties = paste(Counties, collapse = ", "),
            sorder = length(Counties)) %>% 
  arrange(desc(sorder)) 


# Read in proper shapefile
# counties_geo is the GoK recognized shapefile
counties_geo <- st_read(file.path(datapath, "GIS/CountyBoundary2013/CountyBoundary2013.shp"))
str(counties_geo)

# Merge in poverty data for use with derived datasets
# Poverty data is used to create basic poverty maps and for tableau
pov <- read_csv(file.path(povpath, "Overall_poverty.csv")) %>% 
  mutate(national_tag = ifelse(CC_1 == 0, 1, 0)) %>% 
  mutate(CID = CC_1) %>% 
  mutate_at(vars(Headcount_rate:Severity_of_poverity), funs(./100)) %>% 
  mutate_at(vars(Population, Number_poor), funs(. * 1000)) 

pov_natl <- 
  pov %>% 
  filter(national_tag == 1) %>% 
  rename_all(~stringr::str_c(., "_nat")) 

# Combining national average (constants with poverty data for merging w/ budget etc).
pov_all <- 
  cbind(pov, pov_natl) 