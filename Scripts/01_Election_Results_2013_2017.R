# Purpose: Analyze Election data from 2013 & 2018
# Author: Tim Essam, Ph.D., Kevin Horn, Ph.D.
# Date: 2018_12_07


# load data ---------------------------------------------------------------

elec <- read_csv(file.path(elecpath, "20181127_2013_2017_Elections_by_const.csv")) %>% 
  mutate(County_name = str_to_title(COUNTY)) %>% 
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
  turnout_dev = unlist(elec %>% summarise(RELATIVE_TURNOUT_CONST = max(abs(RELATIVE_TURNOUT_CONST), na.rm = TRUE)))
  
  
  elec_geo %>% 
    ggplot(.) +
    geom_sf(aes(fill = RELATIVE_TURNOUT_CONST), colour = "white", size = 0.25) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, ''),
                         labels = scales::percent) +
    theme(legend.position = "top")
