# Purpose: Read in ACLED data to produce time-series plots
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_04_26
# Audience: Kenya Mission

#acled <- read.csv(file.path(datapath, "2009-03-01-2019-03-16-Kenya.csv"))
acled_sum <- read_excel(file.path(datapath, "2009-03-01-2019-03-16-Kenya_coded.xlsx"))
acled <- read_excel(file.path(datapath, "2009-03-01-2019-03-16-Kenya_coded.xlsx"), 
                        sheet = "2009-03-01-2019-03-16-Kenya")
acled_agg <- read_excel(file.path(datapath, "KE_ACLED_J2SR.xlsx"))

# Add in proposed crossalk.
acled_cw <- tibble::tribble(
  ~event_type_code,                       ~cateogry,
                 1,                   "Armed clash",
                 2, "Remote explosive/landmine/IED",
                 3,              "Peaceful protest",
                 4,         "Violent demonstration",
                 5,                         "Other",
                 6,                        "Attack"
        )


acled_long <- 
  acled %>% 
  filter(!is.na(event)) %>% 
  mutate(month = format(event_date, "%m")) %>% 
  select(data_id, iso, month, year, everything()) %>% 
  count(month, year, event_type_code, admin1, CID) %>% 
  group_by(admin1, year) %>% 
  mutate(yearly_count = sum(n)) %>% 
  ungroup() %>% 
  group_by(admin1) %>% 
  mutate(tot_count = sum(n)) %>% 
  ungroup() %>% 
  group_by(admin1, event_type_code) %>% 
  mutate(event_count = sum(n)) %>% 
  ungroup() %>% 
  left_join(acled_cw, by = c("event_type_code")) 
  mutate(county = fct_reorder(admin1, tot_count))

acled_geo <- 
  asal_geo %>% 
  left_join(acled_long, by = c("CID"))
  
# What does a time series plot of everything look like?
 acled_geo %>% 
   select(CID, year, county, yearly_count) %>% 
   group_by(CID, year, county) %>% 
   summarise(count = mean(yearly_count)) %>% 
   ggplot() +
   geom_sf(aes(fill = count), colour = "white", size = 0.25) +
   scale_fill_viridis_c(alpha = 0.65, option = "B") +
   facet_wrap(~year)
   


