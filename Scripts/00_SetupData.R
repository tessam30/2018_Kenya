# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey")

# Create folders for project (if they do not exist)
dir.create("Data")
dir.create("Data/Wash")


datapath <- "Data"
kihbspath <- "Data/KIHBS"
dataout <- "Data/KIHBS/Dataout"
gispath <- "Data/gadm36_KEN_shp"
budgetpath <- "Data/Budget"
washpath <- "Data/Wash"
imagepath <- "Images"
rpath <- "Scripts"

#Source helper functions
source(file.path(rpath, "strip_geom.R"))
source(file.path(rpath, "KEN_helper_functions.R"))


# Create merge base file that contains geography for household roster
hh_info <- read_dta(file.path(kihbspath, "HH_information.dta"))
vtable(hh_info)

hh_base <- hh_info %>% select(county, strat, resid, eatype, clid, hhid, hhsize, iday, weight) %>% 
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values")))
sjlabelled::get_labels(hh_base)
Hmisc::describe(hh_base)
sample_size = dim(hh_base)[1]
remove(hh_info)

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


