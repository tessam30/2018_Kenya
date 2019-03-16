# Mapping out county GDP
library(gridExtra)
library(ggpubr)

gcp <- read_excel(file.path(datapath, "GCP report 2019.xlsx"), skip = 1)

gcp_long <- 
  gcp %>% 
  gather(key = "sector", value = "gcp", 
         `Agriculture, forestry and fishing`:`Other service activities`) %>% 
  mutate(share = gcp / Total) 


gcp_long_geo <- 
  gcp_long %>% 
  filter(`County Name` != "Total") %>% 
  group_by(sector) %>% 
  mutate(sector_sort = fct_reorder(`County Name`, gcp, .desc = TRUE)) %>% 
  ungroup() %>% 
  left_join(., asal_geo, by =c("County Code" = "CID")) %>%
  # Assign unique id codes to the sectors to minimize writing
  transform(., sector_id = match(sector, unique(sector)))





# Function to create map + bar graph --------------------------------------
gdp_bar <- function(df, x, y, sect_id) {
  yvar <- enquo(y)
  xvar <- enquo(x)
  
  ptitle = gcp_long_geo$sector[gcp_long_geo$sector_id == sect_id]
  
  p1 <- 
    df %>% 
    filter(sector_id == sect_id) %>% 
    mutate(sortvar = fct_reorder(!!xvar, !!yvar)) %>% 
    ggplot(aes(x = sortvar, y = !!yvar, fill = !!yvar)) + geom_col() +
    coord_flip() + theme_minimal() +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    theme(legend.position = "none")
    
  map1 <- 
    df %>% 
    filter(sector_id == sect_id) %>% 
    ggplot() +
    geom_sf(aes(fill = !!yvar), colour = "white", size = 0.25) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    theme_minimal() +
    theme(legend.position = "none") 
    
  (grid.arrange(map1, p1))
}






  
  

gcp_long_geo %>% 
  filter(sector_id == 1) %>% 





# GDP scraped -------------------------------------------------------------

gdp <- read_excel(file.path(datapath, "KEN_GDP_county.xlsx"))

gdp_long <- 
  gdp %>% 
  gather(key = "GDP", value = "gdp", share_2013:share_2017) %>% 
  separate(GDP, into = c("GDP", "year")) %>% 
  mutate(County = fct_reorder(CountyName, share_ave, .desc = TRUE))

gdp_long %>% 
  filter(County != "Nairobi") %>% 
  ggplot(aes(year, County, group = CID)) +
  geom_tile(aes(fill = gdp), colour = "white")+
  scale_fill_viridis_c()

gdp_long %>% 
  ggplot(aes(year, gdp, group = CID)) + geom_line() +
  facet_wrap(~County, scales = "free") + theme_minimal() +
  labs(title = "")
