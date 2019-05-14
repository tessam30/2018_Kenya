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
           const = 1) %>%
    left_join(., cst_shape, by = c("ConsID"))
  
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
  

