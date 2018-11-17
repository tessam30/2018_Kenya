# Kenya helper functions for election data and poverty mapping
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_11
# Audience: Kenya Mission

## Libraries
library(tidyverse)
library(sf)
library(rgdal)
library(readxl)


## Data Paths & Files
gispath <- "Data/gadm36_KEN_shp"
marketspath <- "Data/FewsNet"

dir.create(gispath)
dir.create(marketspath)

adm0 <- "gadm36_KEN_0.shp"
adm1 <- "gadm36_KEN_1.shp"

# Get the latest datasets from FewsNEt DropBox and remove the dates from the file name
m.locs <- "Market-coordinates.xlsx"
m.prices <- "FEWS NET Data Warehouse Price Data.xlsx"

## KEN Admins

# Admin 0: Country boundaries
ken.adm0 <- read_sf(paste0(gispath, "/", adm0))

ken.crs <- CRS(ken.adm0)


ken.adm0 %>%
  ggplot() +
  geom_sf() +
  theme_bw()

# Admin 1: County boundaries
ken.adm1 <- read_sf(paste0(gispath, "/", adm1))

ken.adm1 %>%
  ggplot() +
  geom_sf() +
  theme_bw()


## Fews Markets Locations
fews.markets <- read_excel(paste0(marketspath, "/", m.locs), sheet=1)

str(fews.markets)
head(fews.markets)

unique(fews.markets$country)

ken.markets <- fews.markets %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(country == 'KE') %>% #View()
  select(-country, -aliases, -urban_rural)

ken.markets2 <- fews.markets %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326)

ken.markets %>%
  ggplot() +
  geom_sf(data=ken.adm0) +
  geom_point(aes(x=latitude, y=longitude)) +
  coord_sf() +
  theme_bw()



