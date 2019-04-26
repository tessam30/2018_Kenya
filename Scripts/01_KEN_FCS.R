# Purpose: Read in County-level NVDI data to provide input to J2SR index
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_04_24
# Audience: Kenya Mission

# Check what's pre-loaded for you
library(lubridate)
ls(package:lubridate)

fcs_raw <- read_excel(file.path(fcspath, "VCI3M Updated.xlsx"))

fcs <- 
  fcs_raw %>% 
  mutate(date = str_c(YEAR, "-", MONTH, "-", 1) %>% ymd) %>% 
  group_by(YEAR, COUNTY) %>% 
  mutate(fcs_yearly = mean(VCI3M, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(MONTH, COUNTY) %>% 
  mutate(fcs_monthly = mean(VCI3M, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(COUNTY) %>% 
  mutate(fcs_overall = mean(VCI3M, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(COUNTY = fct_reorder(COUNTY, fcs_overall),
         fcs_dev_monthly = VCI3M - fcs_monthly)

fcs %>% 
  ggplot(aes(x = date, y = fcs_dev_monthly)) +
  geom_line() +
  facet_wrap(~COUNTY) 
