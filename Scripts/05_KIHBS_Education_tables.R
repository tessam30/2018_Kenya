# Purpose: Load and process Kenya education data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_02_27
# Audience: Kenya Mission

ed_path <- "KEN_KIHBS_Educ_stats.xlsx"

df_ed <- excel_sheets(file.path(kihbspath, ed_path)) %>% 
  map(~read_excel(file.path(kihbspath, ed_path), sheet = .)) 

  
df_ed[[1]] %>% 
  gather(key = "level", value = "percent", -c("CID", "County_ed")) %>% 
  ggplot(aes(x = level, y = percent, fill = level)) +
  geom_bar(stat = "identity") +
    coord_flip() + facet_wrap(~County_ed)

  
# Show where illiterate males/females are
  df_ed[[4]] %>% gather(type, value, -c(CID, County_ed)) %>% 
    left_join(asal_geo, by = c("CID")) %>% 
    filter(!type %like% "illiterate") %>% 
    ggplot() +
    geom_sf(aes(fill = value, geometry = geometry), colour = "white", alpha = 0.85) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu')) +
    facet_wrap(~ type) +
    labs(title = "Ability to read 15-24 year olds")
  
df_ed[[3]] %>% 
  #select(-c("..5", "Number_individuals")) %>% 
  gather(type, value, -c(CID, County_ed)) %>%   
  left_join(asal_geo, by = c("CID")) %>% 
  ggplot() +
  geom_sf(aes(fill = value, geometry = geometry), colour = "white", alpha = 0.85) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu')) +
  facet_wrap(~ type) +
  labs(title = "Education")
  
  
  
  