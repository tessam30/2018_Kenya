# Mapping out county GDP
library(gridExtra)
library(ggpubr)
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/


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
  left_join(., asal_geo, by = c("County Code" = "CID")) %>%
  # Assign unique id codes to the sectors to minimize writing
  transform(., sector_id = match(sector, unique(sector))) %>% 
  mutate(sector = case_when(
    sector_id == 5 ~ "Water supply and waste collection",
    sector_id == 7 ~ "Wholesale and retail trade", 
    sector_id == 10 ~ "Information and communication",
    sector_id == 14 ~ "Public administration and defence",
    TRUE ~ sector)
    )





# Function to create map + bar graph --------------------------------------
gcp_plot <- function(df, x, y, sect_id) {
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
    theme(legend.position = "none") +
    labs(caption = "GeoCenter calcuations based on Kenya GCP Report 2019",
         y = "", x = "")
    
  map1 <- 
    df %>% 
    filter(sector_id == sect_id) %>% 
    ggplot() +
    geom_sf(aes(fill = !!yvar), colour = "white", size = 0.25) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    theme_minimal() +
    theme(legend.position = "none") 
    
  ggarrange(map1, p1) %>% 
    annotate_figure(., fig.lab = str_c(ptitle, " share of gross county product")) 
  
  ggsave(file.path(imagepath, str_c(ptitle, " gcp plot.pdf")),
                     plot = last_plot(),
           device = "pdf",
                     height = 8.5, width = 11, dpi = 300, 
           useDingbats = FALSE)
}

gcp_plot(gcp_long_geo, Counties, share, 1)
sector_list  <- (seq(1, 17, by = 1))

map(sector_list, ~ gcp_plot(gcp_long_geo, Counties, share, .))

gcp_long_geo %>% 
  group_by(sector) %>% 
  mutate(sect_tot = sum(Total)) %>% 
  ungroup() %>% 
  mutate(facet_sort = fct_reorder(sector, Total)) %>% glimpse()%>% 
  ggplot() +
  geom_sf(aes(fill = share), colour = "white", size = 0.25) +
  theme_minimal() +
  facet_wrap(~facet_sort) +
  scale_fill_viridis_c(direction = -1, alpha = 0.90, option = "A", label = percent_format(accuracy = 2)) +
  labs(fill = "share of gcp") +
  theme(legend.position = "top") 
  







gcp_long <- gcp_long_geo %>% select(-geometry)
write_csv(gcp_long, file.path(dataout, "KEN_GCP_long_2019.csv"))  
  





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
