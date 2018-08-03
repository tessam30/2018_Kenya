# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements")

dir.create("Data")
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"

# Read in constituency data
gis_admin2 <- read_sf()