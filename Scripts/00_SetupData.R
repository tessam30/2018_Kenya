# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable")

# Create folders for project (if they do not exist)
dir.create("Data")
dir.create("Data/Wash")


datapath <- "Data"
kihbspath <- "Data/KIHBS"
gispath <- "Data/gadm36_KEN_shp"
washpath <- "Data/Wash"
rpath <- "Scripts"

#Source helper functions
source(file.path(rpath, "strip_geom.R"))
source(file.path(rpath, "KEN_helper_functions.R"))


# Create merge base file that contains geography for household roster
hh_info <- read_dta(file.path(kihbspath, "HH_information.dta"))
vtable(hh_info)

hh_base <- hh_info %>% select(county, strat, resid, eatype, clid, hhid, hhsize, iday, weight)
sjlabelled::get_labels(hh_base)
Hmisc::describe(hh_base)
sample_size = dim(hh_base)[1]
remove(hh_base)



