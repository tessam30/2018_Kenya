# Purpose: Read in ACLED data to produce time-series plots
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_04_26
# Audience: Kenya Mission

#acled <- read.csv(file.path(datapath, "2009-03-01-2019-03-16-Kenya.csv"))
acled_sum <- read_excel(file.path(datapath, "2009-03-01-2019-03-16-Kenya_coded.xlsx"))
acled <- read_excel(file.path(datapath, "2009-03-01-2019-03-16-Kenya_coded.xlsx"), 
                        sheet = "2009-03-01-2019-03-16-Kenya")
acled_agg <- read_excel(file.path(datapath, "KE_ACLED_J2SR.xlsx"))

ACLED_caption = c("Source: Armed Conflict Location & Event Data Project (ACLED) 2009-2019")



# Add in proposed crossalk.
acled_cw <- tibble::tribble(
  ~event_type_code,                       ~category,
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
  group_by(month, year, event_type_code, admin1, CID) %>% 
  summarise(n = sum(event, na.rm = TRUE),
            deaths = sum(fatalities, na.rm = TRUE)) %>% 
  group_by(admin1, year) %>% 
  mutate(yearly_count = sum(n),
         yearly_deaths = sum(deaths)) %>% 
  ungroup() %>% 
  group_by(admin1) %>% 
  mutate(tot_count = sum(n), 
         tot_deaths = sum(deaths)) %>% 
  ungroup() %>% 
  group_by(admin1, event_type_code) %>% 
  mutate(event_count = sum(n)) %>% 
  ungroup() %>% 
  left_join(acled_cw, by = c("event_type_code")) %>% 
  mutate(county = fct_reorder(admin1, tot_count, .desc = TRUE),
         date = lubridate::dmy(str_c("1-", month, "-", year)),
         county_death = fct_reorder(admin1, tot_deaths, .desc = TRUE)) 


# Double check numbers for maps
acled_long %>% 
  group_by(county, year, CID) %>% 
  summarise(count = sum(n, na.rm = TRUE),
            deaths = sum(deaths)) %>% 
  group_by(county) %>% 
  mutate(total = sum(count)) %>% 
  filter(county == "Garissa")


  
# What do fatalities look like across the 
acled %>% 
  filter(!is.na(event)) %>% 
  group_by(admin1, event_type) %>% 
  summarise(deaths = sum(fatalities, na.rm = TRUE)) %>% 
  arrange(desc(deaths)) %>% 
  spread(event_type, deaths)
  

acled_geo <- 
  asal_geo %>% 
  left_join(acled_long, by = c("CID"))

  
# What does a time series plot of everything look like?
 acled_geo %>% 
   filter(CID != 47 ) %>% 
   select(CID, year, county, yearly_count) %>% 
   group_by(CID, year, county) %>% 
   summarise(count = mean(yearly_count)) %>% 
   ggplot() +
   geom_sf(aes(fill = count), colour = "white", size = 0.25) +
   scale_fill_viridis_c(alpha = 0.65, option = "B") +
   facet_wrap(~year)

# So, we want to create a plot where the scales are from 0 to 150 for Nairobi and
# from 0 - 60 for the rest of the counties, so create fake ymin ymax vars to 
# correspond to this
 
# Total conflict events 
 acled_long %>% 
   mutate(ymax = ifelse(CID != 47, 69, 150),
          ymin = 0) %>% 
   select(year, county, yearly_count, yearly_deaths, ymin, ymax, county_death) %>% 
   group_by(year, county, ymin, ymax, county_death) %>% 
   summarise(count = mean(yearly_count),
             death_count = mean(yearly_deaths)) %>% 
   ggplot(aes(x = year, y = count, fill = count)) + geom_col() +
   facet_wrap(~county, scales = "free_y") +
   scale_fill_viridis_c(option = "A", direction = -1) +
   theme_minimal() +
   scale_x_continuous(breaks = seq(2009, 2019, 2)) +
   geom_blank(aes(y = ymin)) +
   geom_blank(aes(y = ymax)) +
   labs(caption = ACLED_caption,
        title = "Nairobi had the most major conflict events from 2009 - 2019.",
        y = "",
        x = "") +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.ticks.x.bottom = element_line(size = 0.5, colour = grey30K),
         legend.position = "none")
 
 ggsave(file.path(imagepath, "KEN_ACLED_events_summary.pdf"),
        plot = last_plot(),
        height = 17, width = 16, units = c("in"), dpi = "retina",
        useDingbats = FALSE)
   
 
g# Deaths from events
 acled_long %>% 
   mutate(ymax = ifelse(CID %in% c(9, 7, 47, 23, 5, 30, 4, 10), 175, 50),
          ymin = 0) %>% 
   select(year, yearly_deaths, county_death, ymin, ymax) %>% 
   group_by(year, county_death, ymin, ymax) %>% 
   summarise(death_count = mean(yearly_deaths)) %>% 
   ggplot(aes(x = year, y = death_count, fill = death_count)) + geom_col() +
   facet_wrap(~county_death, scales = "free_y") +
   scale_fill_viridis_c(option = "A", direction = -1) +
   theme_minimal() +
   scale_x_continuous(breaks = seq(2009, 2019, 2)) +
   geom_blank(aes(y = ymax)) +
   geom_blank(aes(y = ymin)) +
   labs(caption = ACLED_caption,
        title = "Mandera and Garissa had the most fatalities due to conflict events from 2009 - 2019.",
        y = "", 
        x = "") +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.ticks.x.bottom = element_line(size = 0.5, colour = grey30K),
         legend.position = "none")
 

   ggsave(file.path(imagepath, "KEN_ACLED_fatalities_summary.pdf"),
          plot = last_plot(),
          height = 17, width = 16, units = c("in"), dpi = "retina",
          useDingbats = FALSE)

acled_long %>% 
  mutate(event_color = ifelse(category %in% c("Attack", "Armed clash"), 1, 0)) %>% 
  ggplot(aes(x = year, y = n, fill = factor(event_color))) +
  geom_bar(stat = "identity") +
  facet_wrap(~county) +
  theme_minimal()
  
  

