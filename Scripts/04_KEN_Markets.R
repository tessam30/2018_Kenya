# Kenya - Markets Location & Prices
# Author: Baboyma Kagniniwa | USAID/BFS
# Date: 2018_09_11
# Audience: Kenya Mission

## Libraries
library(tidyverse)
library(sf)
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
  ggtitle("KENYA - FewsNet Markets") +
  theme_bw()

## Fews Markets Commodities Prices
fews.prices <- read_excel(paste0(marketspath, "/", m.prices), sheet=1)

str(fews.prices)
head(fews.prices)
glimpse(fews.prices)

# filter markets in Kenya
ken.prices <- fews.prices %>%
  filter(market_id %in% ken.markets$id)

# look up markets in both datasets
sort(unique(ken.prices$market))
sort(unique(ken.markets$market))

# Compare #FALSE => prices dataset has less markets
# Be-careful here. You might want to lat/long in prices dataset
#length(unique(ken.prices$market)) == length(unique(ken.markets$market))

## let's repeat selection by location with prices datasets
str(fews.prices)
colnames(fews.prices)
# [1] "geographic_group"               "fewsnet_region"                
# [3] "fewsnet_region_code"            "country"                       
# [5] "market"                         "cpcv2"                         
# [7] "cpcv2_description"              "product"                       
# [9] "source_organization"            "source_document"               
# [11] "collection_status"              "period_date"                   
# [13] "price_type"                     "product_source"                
# [15] "unit"                           "unit_type"                     
# [17] "unit_name"                      "currency"                      
# [19] "status"                         "value"                         
# [21] "common_unit"                    "common_currency"               
# [23] "exchange_rate"                  "common_unit_price"             
# [25] "common_currency_price"          "value_one_month_ago"           
# [27] "value_one_year_ago"             "value_two_years_ago"           
# [29] "value_three_years_ago"          "value_four_years_ago"          
# [31] "value_five_years_ago"           "value_one_year_ahead"          
# [33] "two_year_average"               "five_year_average"             
# [35] "pct_change_from_one_month_ago"  "start_date"                    
# [37] "id"                             "datacollectionperiod"          
# [39] "datacollection"                 "datasourcedocument"            
# [41] "datasourceorganization"         "market_id"                     
# [43] "dataseries"                     "dataseries_name"               
# [45] "dataseries_specialization_type" "specialization_type"           
# [47] "created"                        "modified"                      
# [49] "status_changed"                 "collection_status_changed"     
# [51] "collection_schedule"            "longitude"                     
# [53] "latitude"                      

fews.prices <- fews.prices %>%
  select(id, market_id, market, country, product, product_source, 
         unit, unit_type, unit_name, currency, exchange_rate, 
         value, latitude, longitude) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
  st_transform(3857)

ken.prices <- fews.prices %>%
  st_intersection(ken.adm0bOut)

ken.prices %>%
  ggplot() +
  geom_sf(data = ken.adm0bOut) +
  geom_sf(data = ken.adm0p, fill='white') +
  geom_sf() +
  theme_bw()

# what are the common products?
sort(unique(ken.prices$product))
