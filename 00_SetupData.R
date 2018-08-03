# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools")

dir.create("Data")
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"

# Read in constituency data
# https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_KEN_shp.zip
gis_admin2 <- read_sf(file.path(gispath, "gadm36_KEN_2.shp"))

# Download election results from Kenya Election Comission
download.file("https://www.iebc.or.ke/uploads/resources/m3f8arLNjp.pdf", 
              file.path(datapath, "KEN_Election_2017.pdf"))

elec <- pdf_text(file.path(datapath, "KEN_Election_2017.pdf"))


