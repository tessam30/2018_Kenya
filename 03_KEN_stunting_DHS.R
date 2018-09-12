# Purpose: Extract DHS Stunting data for last two DHSs
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_09
# Audience: Kenya Mission

# Purpose: Query DHS API and pull select indicators for PEPFAR South Africa Training
# Date: 2018_09_07


# install.packages("devtools")
# devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(tidyverse)
library(llamar)
library(purrr)
library(sf)
library(styler)


# Better yet, return all indicators with stunting in the definition
fetch_indicators() %>%
  filter(str_detect(definition, "stunt")) %>%
  select(tag_ids, indicator_id, label)

# Check availability by years
fetch_data(countries = c("KE"), tag = 14, years = 2008:2017)

# set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value"))
# set_return_fields()
# Returns a list of values
dhs_api <-
  fetch_data(
    countries = c("KE"),
    indicators = c("CN_NUTS_C_HA2"),
    years = 2008:2014,
    breakdown_level = "subnational"
  )

stunting <-
  map_dfr(dhs_api, ~as.data.frame(.)) %>%
  filter(!is.na(data_id))

# Split into two dataframes based on year
df_list <-
  split(stunting, stunting$survey_year)

names(df_list) <- c("dhs_2008", "dhs_2014")
list2env(df_list, envir = .GlobalEnv)

# The 2014 data has the geographic regions nested, the ... indicates admin2
dhs_2014_admin2 <-
  dhs_2014 %>%
  filter(str_detect(characteristic_label, "\\..") | characteristic_label == "Nairobi") %>%
  mutate(county = str_remove(characteristic_label, "\\.."))

dhs_2014_admin1 <-
  dhs_2014 %>%
  filter(!str_detect(characteristic_label, "\\.."))

# Combinig the twa years, creating lead/lag and eventually difference
dhs_combined_admin1 <-
  rbind(dhs_2014_admin1, dhs_2008) %>% 
  arrange(characteristic_label, survey_year) %>% 
  mutate(
    sortvar = fct_reorder(characteristic_label, value),
    
    # Move Nyanza down 1 notch in the factor listing
    sortvar = fct_relevel(sortvar, "Nyanza", after = 1),
    stunting = value / 100
  ) %>% 
  group_by(characteristic_label) %>% 
  mutate(stunting_lag = lag(stunting),
         stunt_diff = stunting_lag - stunting) %>% 
  ungroup()
# TODO: fill in missing values within groups


  



# Download shapefile that can be used to join data to from DHS
# https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=KE


dhs_path_2014 <- "Data/KEN_DHS_2014_County/shps"
shp2 <- "sdr_subnational_boundaries2.shp"
shp1 <- "sdr_subnational_boundaries.shp"

geo_df_admin2 <- read_sf(file.path(dhs_path_2014, shp2))
geo_df_admin1 <- read_sf(file.path(dhs_path_2014, shp1))

list(geo_df_admin2, dhs_2014) %>%
  map(., names)

# Should be able to join on REG_ID from shapefile and region_id from API data
geo_df_admin2 %>%
  left_join(., dhs_2014_admin2, by = c("REG_ID" = "region_id")) %>%
  ggplot() +
  geom_sf(
    lwd = 0.1,
    col = "white",
    aes(fill = value)
  ) +
  scale_fill_viridis_c(option = "B", alpha = 0.75, direction = -1)



map <- geo_df_admin1 %>%
  left_join(., dhs_combined_admin1, by = c("DHSREGEN" = "characteristic_label")) %>%
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  ) %>%
  ggplot() +
  geom_sf(
    lwd = 0.1,
    col = "white",
    aes(fill = value)
  ) +
  scale_fill_gradientn(colours = llamar::RdPu[2:9]) +
  facet_wrap(~survey_year) +
  geom_text(aes(
    label = DHSREGEN,
    x = lon,
    y = lat
  ),
  color = "#ffffff",
  size = 3
  )

# Plot slope graphs across the two DHS
p1 <- ggplot(dhs_combined_admin1,
       aes(x = stunting,
           y = sortvar,
           fill = stunting)) + 
  geom_point(size = 6, shape = 21, colour = grey90K) +
  scale_fill_gradientn(colours = llamar::RdPu[2:9]) +
  scale_x_continuous(labels = scales::percent, limits = c(0, .5)) +
  llamar::theme_xygrid()
  
  
  
  







# Expor data --------------------------------------------------------------


export_list <- list(dhs_2008, dhs_2014_admin1, dhs_2014_admin2, dhs_combined_admin1)
names(export_list) <- c("dhs_2008", "dhs_2014_admin1", "dhs_2014_admin2", "dhs_combined_admin1")

export_list %>%
  names() %>%
  map(., ~write_csv(export_list[[.]], paste0(datapath, "/", ., ".csv")))