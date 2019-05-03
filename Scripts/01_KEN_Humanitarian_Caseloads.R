# Purpose: Load and consolidate humanitarian caseloads across two datasets
# Everything to be aggregated up to the County level
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

library(gridExtra)
library(ggpubr)

Data_caption = c("Source: GeoCenter calculations based on Kenya National Drought Management Authority")
# Load two spreadsheets with figures --------------------------------------
# What am I looking at?
# Food Assistance are the raw numbers used to "derive" the humanitarian caseloads I guess?
# Humanitarian caseloads should be the number of caseloads by each county. The catch is that
# the way data was tracked changed in 14 and things need to be combined into the county level

food_assist_4_14 <- read_excel(file.path(datapath, "Beneficiary numbers_2004 - 2014_by division.xlsx"),
                            skip = 1, sheet = "Raw Data")%>% 
  mutate_at(vars(`Phase VI Mar 07 - Sep 07`:`Phase X Mar 09 to Aug 09`), as.numeric)

hum_case_4_14 <- read_excel(file.path(datapath, "Beneficiary numbers_2004 - 2014_by division.xlsx"),
                            sheet = "CaseLoad")

hum_case_14_19 <- read_excel(file.path(datapath, "Beneficiary numbers_2014 - 2019 by sub county.xlsx"))

# Looking for characters instead of numbers; need to fix these
list(food_assist_4_14, hum_case_4_14, hum_case_14_19) %>% 
  map(., str)


# Reshaping, date-ifying, temp plotting -----------------------------------


# Step 1, need to reshape the data and get the column names into dates. 
# Then we need to collapse everything down to the county level

hc1 <- 
  hum_case_4_14 %>% 
  # Fix county names so they merge
  mutate(County = ifelse(County == "kwale", "Kwale", County)) %>% 
  gather(phase, caseloads, `Sep 04_Feb 05`:`Mar 14-Aug 14`) %>% 
  
  rename(Pop_est = `2009 Census Pop`) %>% 
  
  # Split up the phase into parsable dates
  mutate(phase = str_replace(phase, c("_"), "-")) %>% 
  separate(phase, sep = "-", into = c("start_date", "end_date")) %>% 
  
  # Compress down to County level 
  group_by(CID, County, start_date, end_date) %>% 
  summarise(caseloads = sum(caseloads, na.rm = TRUE),
            Pop_est = sum(Pop_est)) %>% 
  ungroup() %>% 
  mutate(start_date = lubridate::myd(str_c(start_date, " ", 1)),
         end_date = lubridate::myd(str_c(end_date, " ", 1))) %>% 
  group_by(start_date) %>% 
  mutate(phase = group_indices()) %>% 
  ungroup()



# Now for the second half of the data
hc2 <- 
  hum_case_14_19 %>% 
  mutate(County = ifelse(County == "Tharaka", "Tharaka Nithi", County)) %>% 
  gather(phase, caseloads, `Sep 14-Feb 15`:`Sep 18-Feb 19`) %>% 
  rename(Pop_est = `2009 Census Pop`) %>% 
  separate(phase, sep = "-", into = c("start_date", "end_date")) %>% 
  # Compress down to County level 
  group_by(CID, County, start_date, end_date) %>% 
  summarise(caseloads = sum(caseloads, na.rm = TRUE),
            Pop_est = sum(Pop_est)) %>% 
  ungroup() %>% 
  mutate(start_date = lubridate::myd(str_c(start_date, " ", 1)),
         end_date = lubridate::myd(str_c(end_date, " ", 1))) %>% 
  arrange(CID, start_date) %>% 
  group_by(start_date) %>% 
  mutate(phase = group_indices() + 20) %>% 
  ungroup() 


map(list(hc1, hc2), names)
map(list(hc1$start_date, hc2$start_date), table)
map(list(hc1$County, hc2$County), table)

human_caseloads <- rbind(hc1, hc2) %>% 
  group_by(CID) %>% 
  mutate(total_caseloads = sum(caseloads, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(county_sort = fct_reorder(County, total_caseloads, .desc = TRUE)) 



# Create real dates and phases to go w/ them
droughts <- c("2011-01-01", "2017-01-01") %>% as.Date()
bar <- human_caseloads %>% 
  ggplot(aes(x = start_date, y = (caseloads), fill = caseloads)) +
  geom_rect(ymin = 0, ymax = Inf,
            xmin = as.Date("2006-01-01"), xmax = as.Date("2007-01-01"),
            fill = "#fdfbec", alpha = 0.75) +
  geom_rect(ymin = 0, ymax = Inf,
            xmin = as.Date("2004-01-01"), xmax = as.Date("2005-01-01"),
            fill = "#fdfbec", alpha = 0.75) +
  geom_rect(ymin = 0, ymax = Inf,
            xmin = as.Date("2009-01-01"), xmax = as.Date("2010-01-01"),
            fill = "#fdfbec", alpha = 0.75) +
  geom_rect(ymin = 0, ymax = Inf,
            xmin = as.Date("2011-01-01"), xmax = as.Date("2012-01-01"),
            fill = "#fdfbec", alpha = 0.75) +
  geom_rect(ymin = 0, ymax = Inf,
            xmin = as.Date("2017-01-01"), xmax = as.Date("2018-01-01"),
            fill = "#fdfbec", alpha = 0.75) +
  geom_col() + 
  facet_wrap(~ county_sort) +
  scale_fill_viridis_c(option = "A", alpha = 0.85, direction = -1,
                       labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "",
       caption = Data_caption,
       title = "Turkana and Kitui had the most humanitarian caseloads from 2004-2018",
       subtitle = "Major drought events shown in light yellow") +
  theme_minimal() +
  scale_x_date() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x.bottom = element_line(size = 0.5, colour = grey30K),
        legend.position = "none")

hc_map <- human_caseloads %>% 
  group_by(CID) %>% 
  summarise(total = sum(caseloads)) %>% 
  right_join(asal_geo, by = c("CID")) %>%
  mutate(total = ifelse(is.na(total), 0, total)) %>% 
  ggplot() +
  geom_sf(aes(fill = total), colour = "white", size = 0.25) +
  scale_fill_viridis_c(option = "A", alpha = 0.85, direction = -1,
                       labels = scales::comma) +
  #facet_wrap(~start_date, nrow = 3) +
  theme_minimal()

# ggarrange(hc_map, bar, ncol = 2) %>% 
#   annotate_figure(., fig.lab = "Turkana and Kitui had the most humanitarian caseloads from 2004 - 2018")

  ggsave(file.path(imagepath, "KEN_caseloads.pdf"),
         plot = last_plot(),
         device = "pdf",
         height = 17, width = 16, dpi = 300, 
         useDingbats = FALSE)
  

# Data export for .AI'ing -------------------------------------------------

hc_totals <- human_caseloads %>% 
    group_by(CID, County) %>% 
    summarise(total = sum(caseloads)) %>% 
    ungroup() %>% 
    right_join(asal_geo, by = c("CID")) %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
st_write(hc_totals, file.path(gispath, "KEN_humanitarian_caseloads_totals.shp"), delete_layer = TRUE)
write_csv(hc_totals, file.path(datapath, "KEN_humanitarian_caseloads.totals.csv"))
