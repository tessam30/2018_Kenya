# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey", "data.table", "lemon")

# Create folders for project (if they do not exist)
dir.create("Data")
dir.create("Data/Wash")


datapath <- "Data"
kihbspath <- "Data/KIHBS"
dataout <- "Data/KIHBS/Dataout"
povpath <- "Data/Poverty"
elecpath <- "Data/Elections"
msmepath <- "Data/MSME"
gispath <- "Data/GIS"
budgetpath <- "Data/Budget"
washpath <- "Data/Wash"
imagepath <- "Images"
rpath <- "Scripts"

#Source helper functions
file_list <- list("strip_geom.R", 
                  "KEN_helper_functions.R",
                  "county_list.R")

# Source custom scripts and data needed for project
file_list %>% 
  map(~source(file.path(rpath, .)))
rm(file_list)


# Create merge base file that contains geography for household roster
hh_info <- read_dta(file.path(kihbspath, "HH_information.dta"))
vtable(hh_info)

hh_base <- hh_info %>% select(county, strat, resid, eatype, clid, hhid, hhsize, iday, weight) %>% 
  mutate(county_id = labelled::to_factor(county, levels = "values")) %>% 
  mutate(county_id = as.numeric(as.character(county_id)))

sjlabelled::get_labels(hh_base)
Hmisc::describe(hh_base)
sample_size = dim(hh_base)[1]


# Create a county crosswalk with county id and name
county_labels <- as_data_frame(get_labels(hh_base$county)) %>% 
  mutate(county_id = row_number()) %>% 
  rename(county_name = value)
county_labels %>% print(n = 47)

# Read in ASAL county identifiers
asal_geo <- st_read(file.path(datapath, "GIS/ASAL_Counties_2018/ASAL_Counties_2018.shp"))
asal <- strip_geom(asal_geo, Counties, CID, Category)
asal %>% group_by(Category) %>% 
  summarise(counties = paste(Counties, collapse = ", "),
            sorder = length(Counties)) %>% 
  arrange(desc(sorder)) 

# Read in proper shapefile
counties_geo <- st_read(file.path(datapath, "GIS/CountyBoundary2013/CountyBoundary2013.shp"))
str(counties_geo)

# Merge in poverty data for use with derived datasets
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

