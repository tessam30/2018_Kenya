# Kenya - Markets Location & Prices
# Author: Baboyma Kagniniwa | USAID/BFS
# Date: 2018_09_11
# Audience: Kenya Mission

## Libraries
library(tidyverse)
library(sf)
library(rgdal)
library(rgeos)
library(readxl)


## Data Paths & Files
gispath <- "Data/gadm36_KEN_shp"
marketspath <- "Data/FewsNet"

dir.create("Data")
dir.create(gispath)
dir.create(marketspath)

adm0 <- "gadm36_KEN_0.shp"
adm1 <- "gadm36_KEN_1.shp"

# Get the latest datasets from FewsNEt DropBox 
# and remove the dates from the file name
m.locs <- "Market-coordinates.xlsx"
m.prices <- "FEWS NET Data Warehouse Price Data.xlsx"

## KEN Admins

# Admin 0: Country boundaries
ken.adm0 <- read_sf(paste0(gispath, "/", adm0))

# Admin 1: County boundaries
ken.adm1 <- read_sf(paste0(gispath, "/", adm1))

# Get adm0 current coords systems
#ken.crs <- st_crs(ken.adm0) 
#ken.crs$epsg #4326 
#ken.crs$proj4string #"+proj=longlat +datum=WGS84 +no_defs"

# plot KEN Admin0 Boundaries
ken.adm0 %>%
  ggplot() +
  geom_sf() +
  theme_bw()

# reproject adm0 from 4326 to 3857
ken.adm0p <- st_transform(ken.adm0, 3857)

# create a 50km buffer of projected Admin0
ken.bRadius <- 50 * 1000 #50km
# 50km buffer (out)
ken.adm0bOut <- st_buffer(ken.adm0p, ken.bRadius)
# 50km buffer (in)
ken.adm0bIn <- st_buffer(ken.adm0p, -ken.bRadius)

# extract Buffered Zone from Adm0
ken.adm0bzOut <- st_difference(ken.adm0bOut, ken.adm0p)
ken.adm0bzIn <- st_difference(ken.adm0p, ken.adm0bIn)

# extract Buffered Zone only
ken.adm0bz <- st_difference(ken.adm0bOut, ken.adm0bIn)

# plot adm0 and its buffer
ken.adm0bzOut %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=ken.adm0p, fill="white") +
  theme_bw()

# ken.adm0bzOut %>%
#   ggplot() +
#   geom_sf() +
#   geom_sf(data=ken.adm0p, fill="white") +
#   geom_sf(data=ken.adm0bIn, fill="steelblue") +
#   theme_bw()



## Fews Markets Locations
fews.markets <- read_excel(paste0(marketspath, "/", m.locs), sheet=1)

str(fews.markets)
head(fews.markets)

# Countries being monitored
sort(unique(fews.markets$country))


# clean and reformat markets dataset 
fews.markets <- fews.markets %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
  st_transform(3857)

# Extract KEN Markets Only
ken.markets <- fews.markets %>%
  st_intersection(ken.adm0bOut)
  
# ken.markets %>%
#   ggplot() +
#   geom_sf() +
#   theme_bw()

ken.markets %>%
  ggplot() +
  #geom_sf(data = ken.adm0bzOut) + # only the 50km zone
  geom_sf(data = ken.adm0bOut) +
  geom_sf(data = ken.adm0p, fill = 'white') +
  geom_sf() +
  ggtitle("KENYA - Commodities Markets") +
  theme_bw()







