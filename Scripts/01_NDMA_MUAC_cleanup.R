# Purpose: Scrape out Malnutrition data
# Author: Tim Essam
# Date: 2018_12_19
# MUAC == Mid-Upper Arm Circumference; Malnutrition metric

# Time series tutorials to look into
# https://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html
# https://cran.rstudio.com/web/packages/sweep/vignettes/SW01_Forecasting_Time_Series_Groups.html
# https://www.rstudio.com/resources/videos/the-future-of-time-series-and-financial-analysis-in-the-tidyverse/
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ00-introduction-to-tidyquant.html

library(gghighlight)

# Determine universe of files to use --------------------------------------

muac_files <- list.files(file.path(datapath, "malnutrition"), pattern = "MUAC")
muac_path <- "Data/malnutrition"

muac_raw <- tibble(filename = muac_files) %>% 
  mutate(file_contents = map(filename, 
         ~read_excel(file.path(muac_path, .), range = "B1:N72", sheet = "Data"))) %>% 
  mutate(filename = str_remove(filename, " Prices and MUAC.xlsx")) %>% 
  unnest() 

# Process the raw data into a usable format
muac <- 
  muac_raw %>% 
  rename(varinfo = "...1",
         Jan = "...2",
         Feb = "...3",
         Mar = "...4",
         Apr = "...5",
         May = "...6",
         Jun = "...7",
         Jul = "...8",
         Aug = "...9",
         Sep = "...10",
         Oct = "...11",
         Nov = "...12",
         Dec = "...13", 
         county = filename) %>% 
  filter(!is.na(varinfo)) %>% 
  
  # Flag variable names so we know what data go w/ what variable
  # Also create flags to allow for filling down based on commodity
  mutate(ave_flag = ifelse(str_detect(varinfo, regex("average", ignore_case = TRUE)), 1, 0),
         commodity = ifelse(varinfo  %like% "Price|price|Maize|risk", varinfo, NA_character_),
         commodity_flag = ifelse(varinfo  %like% "Price|price|Maize|risk", 1, 0)) %>% 
  select(county, varinfo, commodity, commodity_flag, ave_flag, everything()) %>% 
  filter(ave_flag == 0, varinfo != "Year") %>% 
  fill(commodity) %>% 
  
  #Clean up county names & Commodity categories
  mutate(county = case_when(
    county == "Embu (Mbeere) " ~ "Embu",
    county == "Nyeri (Kieni)" ~ "Nyeri",
    county == "Tharaka Nithi (Tharaka)" ~ "Tharaka Nithi",
    county == "Meru North" ~ "Meru",
    TRUE ~ county),
    
    commodity = case_when(
      commodity == "Beans Prices" ~ "Beans",
      commodity == "Goat Price" ~ "Goat",
      commodity == "Sheep Price" ~ "Sheep",
      commodity %in% c("Maize Flour", "Maize Price", "Maize Price (1 Kg)", "Maize Price/90 Kg bags") ~ "Maize",
      commodity == "Terms of Trade (Kilograms of maize exchanging for a goat - Goat price/Maize price)" ~ "Goat/Maize ToT",
      commodity == "Terms of Trade (Kilograms of maize exchanging for a Sheep - Sheep price/Maize price)" ~ "Sheep/Maize Tot",
      commodity == "Percent of children U-5yrs at risk of malnutrition (MUAC<135 mm)" ~ "Malnutrition Risk",
      TRUE ~ commodity)
    ) %>% 
  left_join(., county_list, by = c("county" = "Counties")) %>% 
  filter(commodity_flag != 1) %>% 
  select(-contains("flag")) %>% 
  select(county:commodity, CID:Category, everything())

muac_long <- 
  muac %>% 
  gather(., key = "month", value = "value", Jan:Dec) %>% 
  mutate(year = as.numeric(varinfo),
         value = as.numeric(value),
         date = as.Date(str_c(1, month, year, sep = "/"), format = "%d/%b/%Y"),
         value = ifelse(commodity == "Malnutrition Risk", value/100, value),
         recent_flag = ifelse(between(lubridate::year(date), 2013, 2017), 1, 0)) %>% 
  arrange(county, commodity, date) %>% 
  
  # Create monthly / county averages
  group_by(county, commodity, month, recent_flag) %>% 
  mutate(monthly_ave = mean(value, na.rm = TRUE), 
         monthly_dev = value - monthly_ave) %>% 
  ungroup()
  
  
  

# See if data make sense
muac_long %>% 
  filter(commodity == "Malnutrition Risk") %>% 
  ggplot(aes(x = date, y = value, group = county)) +
  geom_line() + 
  gghighlight(value > .25)

muac_malnut <- muac_long %>% filter(commodity == "Malnutrition Risk") %>% rename(malnutrition = commodity)
prices <- muac_long %>% filter(commodity != "Malnutrition Risk")

# Set span for fitting loess
span = 0.33
muac_malnut %>% 
  group_by(county) %>% 
  mutate(mean = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(county_sort = fct_reorder(county, mean, .desc = TRUE)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = 0.15, ymax = 0.4), 
              data = , fill = "#d6604d", alpha = 0.25) +
  geom_ribbon(aes(ymin = 0, ymax = 0.15), 
              data = , fill = "#4393c3" , alpha = 0.25) +
  geom_smooth(colour = "#d6604d", span = span, alpha = 0.5, size = 0.25) +
  geom_line(colour = grey70K) +
  facet_wrap(~county_sort) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       caption = "Source: GeoCenter Caculations based on National Drought Management Authority MUAC data 2008-2018") +
  ggtitle("DRAFT: Acute malnutrition trends for select counties. Areas in red represent data points above 15% threshold.",
          subtitle = "Percent of under-5 children at risk of malnutrition (MUAC < 135 mm)") + 
  ggsave(file.path(imagepath, "KEN_acute_malnutrition_trends_2008_2018.pdf"), 
         height = 8.5, width = 11.5, units = "in")

# Run same chunk but just for select counties of Turkana, Marsabit, Isiolo and Samburu.
# NOTE: https://stackoverflow.com/questions/17521438/geom-rect-and-alpha-does-this-work-with-hard-coded-values; 
# TLDR: geom_rect draws a rectangle for each row in the data frame, limit how many rectangles are drawn.
muac_malnut %>% 
  filter(county %in% c("Turkana", "Marsabit", "Isiolo", "Samburu")) %>% 
  group_by(county) %>% 
  mutate(mean = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(county_sort = fct_reorder(county, mean, .desc = TRUE)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = 0.15, ymax = 0.4), 
              data = , fill = "#d6604d", alpha = 0.20) +
  geom_ribbon(aes(ymin = 0, ymax = 0.15), 
              data = , fill = "#4393c3" , alpha = 0.20) +
  geom_rect(data = muac_malnut[1, ], ymin = 0, ymax = .4,
            xmin = as.Date("2009-01-01"), xmax = as.Date("2010-01-01"),
            fill = "#fdfbec", alpha = 0.33) +
  geom_rect(data = muac_malnut[1, ], ymin = 0, ymax = .4,
            xmin = as.Date("2011-01-01"), xmax = as.Date("2012-01-01"),
            fill = "#fdfbec", alpha = 0.33) +
  geom_rect(data = muac_malnut[1, ], ymin = 0, ymax = .4,
            xmin = as.Date("2017-01-01"), xmax = as.Date("2018-01-01"),
            fill = "#", alpha = 0.33) +
  geom_smooth(colour = "#", span = span, alpha = 0.5, size = 0.25) +
  geom_line(colour = grey70K) +
  facet_wrap(~county_sort) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       caption = "Source: GeoCenter Caculations based on National Drought Management Authority MUAC data 2008-2018") +
  ggtitle("DRAFT: Acute malnutrition trends for select counties. Areas in red represent months above 15% threshold.",
          subtitle = "Percent of under-5 children at risk of malnutrition (MUAC < 135 mm)")+ 
  ggsave(file.path(imagepath, "KEN_acute_malnutrition_trends_PR_Counties_2008_2018.pdf"), 
         height = 8.5, width = 11.5, units = "in")


muac_malnut %>% 
  group_by(county, year, CID) %>% 
  summarise(ave = mean(value, na.rm = TRUE)) %>% 
  left_join(., asal_geo, by = c("CID")) %>% 
  ggplot() +
  geom_sf(data = asal_geo, fill = "#f0f0f0", colour = "#969696", size = 0.25) +
  geom_sf(aes(fill = ave), colour = "white", size = 0.5) +
  facet_wrap(~year, nrow = 2) + theme_minimal() +
  scale_fill_viridis_c(option = "B", direction = -1,
                       labels = scales::percent_format(accuracy = 1)) +
  labs(fill = "DRAFT: Average acute malnutrition rate",
       title = "Annual acute malnutrition rate averages by county.",
       subtitle = "County averages based on monthly averages -- need to be recalculated with raw numbers",
       caption = "Source: GeoCenter Caculations based on National Drought Management Authority MUAC data 2008-2018") +
  theme(legend.position = "top",
        strip.text = element_text(hjust = 0)) +
  ggsave(file.path(imagepath, "KEN_acute_malnutrition_trends_map_2008_2018.pdf"), 
         height = 8.5, width = 11.5, units = "in")




prices %>% 
  filter(commodity == "Maize") %>% 
  ggplot(aes(x = date, y = value, group = county)) +
  geom_smooth(aes(colour = "#D3D3D3"), span = span, alpha = 0.5) +
  geom_line() +
  facet_wrap(~county) +
  ggtitle("Goat prices by county -- real or nominal?") +
  theme(legend.position =  "none")


map(list(muac_malnut, prices), ~str(.))

write_csv(muac_malnut, file.path(muac_path, "KEN_acute_malnutrition_2008_18.csv"))
# TODO: Deflate prices ----------------------------------------------------

# See if you can animate MUAC data; Nothing great from this given the time 
tmp <- muac_malnut %>% 
  left_join(., asal_geo, by = c("CID" = "CID"))

library(gganimate)
library(transformr)

  tmp %>% 
  ggplot(.) +
  geom_sf(aes(fill = value), colour = "white") +
  scale_fill_viridis_c(option = "B", alpha = 0.75, label = percent_format(accuracy = 2),
                       direction = -1) +
    labs(fill = "MUAC rate") +
  theme_void() +
  labs(title = 'Date: {current_frame}') +
  transition_manual(date) 

  tmp %>% 
    ggplot(aes(x = date, y = value)) +
             geom_line(color = grey80K, size = 1) +
    transition_reveal(date)

