# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "llamar")

dir.create("Data")
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"

#Source helper functions
source("strip_geom.R")
source("KEN_helper_functions.R")
