# Purpose: Process data on youth statistics
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_05_14
# Audience: Kenya Mission

path <- (file.path(datapath, "Youth", "KE_youth.xlsx"))
youth <- 
  path %>% 
  excel_sheets() %>% 
  map(read_excel, path = path)
str(youth)

cst_shape <- st_read(file.path(gispath, "Constituency", "Constituency.shp")) %>% 
  rename(ConsID = CONSTI_COD)

youth_cst <- 
  youth[[3]] %>%
    select_at(vars(contains("Cons"), starts_with("Age"), contains("Pop"))) %>% 
    select(-AgeDepdRat) %>% 
    gather(age_cat, pop, Age0_5:Age65Plus) %>% 
    mutate(percent = pop / TotalPop, 
           const = 1) 
youth_cst <- cst_shape %>% right_join(., youth_cst, by = c("ConsID"))
  
  youth_cst %>% 
    mutate(opacity = ifelse(percent > 0.25, 1, .99)) %>% 
    filter(age_cat %in% c("Age10_18")) %>% 
  ggplot() +
  geom_sf(aes(fill = (percent), alpha = opacity), colour = "white", size = 0.05) +
    scale_fill_viridis_c(alpha = 0.75, option = "B", direction = -1) 
  
  youth_cst %>% 
    filter(age_cat %in% c("Age10_18")) %>%
    mutate(Constituency = str_remove(Constituency, " Constituency")) %>% 
    filter(percent > 0.25) %>% 
    mutate(const_sort = fct_reorder(Constituency, percent, .desc = FALSE)) %>% 
    ggplot(aes(y = percent, x = const_sort)) + geom_col(aes(fill = percent)) +
    coord_flip() +
    scale_fill_viridis_c(alpha = 0.75, option = "B", direction = -1) 
  
  

# Kenya FinAccess Results -------------------------------------------------
library(CGPfunctions)
fin_geo <- st_read(file.path(gispath, "KE_Regions", "KE_Regions.shp")) %>% 
    select(Region = Reg_Name)
fin <- read_excel(file.path(datapath, "Finance", "KEN_FinAccess2019_summary.xlsx"))

fin_df <- 
  fin %>% 
  left_join(fin_geo, by = c("Region")) %>% 
  mutate(Year_chr = as.character(Year))

 fin_df %>% 
  ggplot() +
  geom_sf(aes(fill = Inclusion), colour = "white", size = 0.25, alpha = 0.75) +
  scale_fill_gradientn(colours = llamar::RdPu)  +
  facet_wrap(~ Year)

fin_df %>% 
  ggplot(aes(x = Year, y = Inclusion, group = Region, fill = Inclusion)) +
  geom_line(size = 1, colour = grey20K) +
  geom_point(size = 6, shape = 21, colour = grey90K) +
  scale_fill_gradientn(colours = llamar::RdPu) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() 


newggslopegraph(fin_df, Year_chr, Inclusion, Region)

