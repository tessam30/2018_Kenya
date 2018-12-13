# Purpose: Analyze Election data from 2013 & 2018
# Author: Tim Essam, Ph.D., Kevin Horn, Ph.D.
# Date: 2018_12_07


# load data ---------------------------------------------------------------

elec <- read_csv(file.path(elecpath, "20181127_2013_2017_Elections_by_const.csv")) %>% 
  mutate(County_name = str_to_title(COUNTY),
         turnout_flag = ifelse(CONST_CODE %in% c(242, 124) & YEAR == 2017, 1, 0),
         Odinga_won = ifelse(VOTES_ODINGA > VOTES_KENYATTA, 1, 0),
         Kenyatta_won = ifelse(VOTES_ODINGA < VOTES_KENYATTA, 1, 0)) %>% 
  # What is the difference in turnout rates over time?
  group_by(CONST_CODE) %>% 
  mutate(turnout_lag = lag(TURNOUT, n = 1, order_by = YEAR),
         turnout_delta = TURNOUT - turnout_lag,
         constit_changed = ifelse(sum(Odinga_won) == 1, "swing", "no change"),
         swing_party = case_when(
           VOTES_ODINGA > VOTES_KENYATTA & constit_changed == "swing" & YEAR == 2017 ~ "swing Odinga",
           VOTES_ODINGA < VOTES_KENYATTA & constit_changed == "swing" & YEAR == 2017 ~ "swing Kenyatta",
           TRUE ~ "no change"
         )) %>%  
  ungroup() %>% 
  mutate(County_name = ifelse(County_name == "Muranga","Murang'a", County_name)) %>% 
  # One county seems to be missing = Murang'a not Muranga
  left_join(., asal, by = c("County_name" = "Counties"))

# Check the merge to see that it is doing what you think it is
elec %>% group_by(COUNTY, County_name) %>% count() %>% print(n = Inf)

# Bring in the constituencies to check for mergeability and completeness
constit_geo <- read_sf(file.path(gispath, "Constituency/Constituency.shp"))
str(constit_geo)
#lot(constit_geo)[1]

# Note
elec_geo <- 
  constit_geo %>% 
  right_join(., elec, by = c("CONSTI_COD" = "CONST_CODE"))

strip_geom(elec_geo, CONST_NAME, CONSTITUEN) %>% 
  group_by(CONST_NAME, CONSTITUEN) %>% 
  count() %>% print(n = Inf)

# Plot the swing by constituency
swing_max_od = unlist(elec %>% summarise(shift_share_max = max(abs(share_shift_Odinga), na.rm = TRUE)))
swing_max_ky = unlist(elec %>% summarise(shift_share_max = max(abs(share_shift_Kenyatta), na.rm = TRUE)))
  
# What did the swings look like? For Odinga? for Kenyatta
elec_geo %>% 
  filter(YEAR == 2017) %>%
  ggplot(.) +
  geom_sf(aes(fill = share_shift_Kenyatta), colour = "white", size = 0.25) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'PiYG'),
                       limits = c(-1 * swing_max_ky, swing_max_ky), 
                       labels = scales::percent) +
  theme(legend.position = "top")
  ggsave(file.path(imagepath, "Kenyatta swing 2013 to 2017.pdf"),
         plot = last_plot(), 
         device = "pdf",
         width = 8.5, height = 11,
         dpi = "retina")

# What did turnout look like in each county?
  #Filter out outliers in NYANDO (242) and TURKANA WEST (124) in 2017
  
  turnout_dev = unlist(elec %>% 
                         filter(turnout_flag == 0) %>% 
                                summarise(RELATIVE_TURNOUT_CONST = max(abs(RELATIVE_TURNOUT_CONST), na.rm = TRUE)))
  
  
  elec_geo %>% 
    ggplot(.) +
    geom_sf(aes(fill = RELATIVE_TURNOUT_CONST), colour = "white", size = 0.25) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'PiYG'),
                         limits = c(-1 * turnout_dev, turnout_dev), 
                         labels = scales::percent) +
    facet_wrap(~YEAR) +
    theme(legend.position = "top")
  ggsave(file.path(imagepath, "Constituency_turnout_Deviation.pdf"),
         plot = last_plot(), 
         device = "pdf",
         width = 11, height = 8.5,
         dpi = "retina")

# Turnout rates across years - where did things decrease the most?
 turnout_delta_max = unlist(elec %>% 
                           filter(YEAR == 2017 & turnout_flag == 0) %>% 
                           summarise(turnout_delta_max = max(abs(turnout_delta), na.rm = TRUE)))
  
   elec_geo %>% 
    filter(turnout_flag == 0) %>% 
    ggplot(.) +
    geom_sf(aes(fill = TURNOUT_COUNTY), colour = "white", size = 0) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu'),
                         limits = c(.5, 1), 
                         labels = scales::percent) +
    theme(legend.position = "top")+
     facet_wrap(~YEAR)
   
   ggsave(file.path(imagepath, "Constituency_turnout.pdf"),
          plot = last_plot(), 
          device = "pdf",
          width = 11, height = 8.5,
          dpi = "retina")
   
   
# Show who won where by year
   elec_geo %>% 
     filter(turnout_flag == 0) %>% 
     ggplot(.) +
     geom_sf(aes(fill = percent_share_ODINGA), colour = "white", size = 0.25) +
     scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'Spectral'),
                          limits = c(0, 1), 
                          labels = scales::percent) +
     facet_grid(YEAR ~ Odinga_won) +
     theme(legend.position = "top") 
   
   ggsave(file.path(imagepath, "KEN_election_results.pdf"),
          plot = last_plot(), 
          device = "pdf",
          height = 11, width = 8.5,
          dpi = "retina")
   
   
   elec_geo %>% 
     filter(YEAR == 2017) %>% 
     ggplot() +
     geom_sf(aes(fill = factor(swing_party)), colour = "white", size = 0.25, alpha = 0.75) +
     scale_fill_manual(values = c("no change" = "#E0E0E0",
                                  "swing Kenyatta" ="#d53e4f",
                                  "swing Odinga" = "#3288bd")) +
     theme(legend.position = "top")
   
   ggsave(file.path(imagepath, "KEN_swing_constituencies.pdf"),
          plot = last_plot(), 
          device = "pdf",
          height = 11, width = 8.5,
          dpi = "retina")

# TODO: Additional questions to look into 
# What constituencies had the largest growth in registered voters (levels and percentages)
# At the county level, any major swings that stand out? Any constituency packing
   