# Purpose: Create Kenya visualizations for the Health Team
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_09_03
# Audience: Kenya Mission

# Load the different datasets

# Early Infant Diagnosis Positivity Rates ---------------------------------

eid <- read_csv(file.path(healthpath, "EID Positivity 2015-2019  - MTCT Rate.csv"))

eid <- eid %>% 
  mutate(positivity = Positives / (Negatives + Positives)) %>% 
  group_by(Year) %>% 
  mutate(tot_pos = sum(Positives),
         tot_neg = sum(Negatives),
         tot_positivity = tot_pos / (tot_neg + tot_pos),
         ave_positivity = mean(Positives)) %>% 
  ungroup() %>% 
  mutate(dev_positivity = positivity - tot_positivity) %>% 
  group_by(Name) %>% 
  mutate(tot_pos_county = sum(Positives) / sum(Negatives + Positives)) %>% 
  ungroup() %>% 
  mutate(csort_pct = fct_reorder(Name, tot_pos_county, .desc = TRUE),
         csort_num = fct_reorder(Name, Positives, .desc = TRUE)) %>% 
  left_join(asal, by = c(`County ID` = "CID")) %>% 
  group_by(Year, Category_num) %>% 
  mutate(asal_ave_positivity = mean(Positives)) %>% 
  ungroup()

# Percentage of EIDS
theme_panel <-   theme(legend.position = "none",
                       strip.text = element_text(hjust = 0, size = 9),
                       panel.grid.minor = element_blank(),
                       panel.spacing = unit(1.25, "lines"))


# Generic plotting function for line over average plots

hiv_line <- function(df, x, y, yave, fill, wrap) {
  df %>% 
    ggplot() +
    geom_area(aes(x = {{ x }}, y = {{ yave }}), 
              fill = grey30K, alpha = 0.50) +
    geom_line(aes(x = {{ x }}, y = {{ y }})) +
    geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }}),
               size = 3.5, shape = 21, colour = "white", stroke = 1) +
    facet_wrap(vars({{ wrap }})) +
    theme_minimal() 
}


# Sources
PEPFAR <- c("Source: USAID Kenya PEPFAR")

# EID rates

hiv_line(eid, x = Year, y = positivity, yave = tot_positivity, fill = positivity, wrap = csort_pct) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_c(direction = -1, option = "A") +
  labs(x = "", y = "",
       title = "Samburu and Turkana had the highest early infant diagnosis (EID) positivity rates",
       subtitle = "Grey area represents country average for each year",
       caption = PEPFAR) +
  theme_panel + 
  ggsave(file.path(imagepath, "KEN_EID_percent.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)

#Raw numbers of EIDS
hiv_line(eid, x = Year, y = Positives, yave = ave_positivity, 
         fill = Positives, wrap = csort_num) +
  scale_fill_viridis_c(direction = -1, option = "A") +
  labs(x = "", y = "", 
       title = "Nairobi and Homa Bay have the higest early infant diagnosis positivity counts", 
       subtitle = "Grey area represents country average for each year",
       caption = PEPFAR) +
  theme_panel +
  ggsave(file.path(imagepath, "KEN_EID_counts.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)


# HIV Prevalence ----------------------------------------------------------

tx_curr <- read_csv(file.path(healthpath, 
                              "TX CURR FY15-FY19 - Number of adults and Children receiving ART.csv"))
  
tx_curr <- 
  tx_curr %>% 
  group_by(Year) %>% 
  mutate(ave_ART = mean(`TX Curr`)) %>% 
  ungroup() %>% 
  mutate(dev_ART = `TX Curr` - ave_ART, 
         csort = fct_reorder(County, `TX Curr`, .desc = TRUE))


# Tx Curr Plot
hiv_line(tx_curr, x = Year, y = `TX Curr`, yave = ave_ART, fill = `TX Curr`, wrap = csort) +
  scale_fill_viridis_c(direction = -1, option = "A") +
  #scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  labs(x = "", y = "",
       title = "Nairobi and Homa Bay have the largest populations receiving antiretroviral treatment (ART)",
       subtitle = "Grey area represents country average for each year",
       caption = PEPFAR) +
  theme_panel +
  ggsave(file.path(imagepath, "KEN_ART_counts.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)


# HIV Indicators -- all worksheets ----------------------------------------
excel_path <- file.path(healthpath, "HIV Indicators.xlsx")
sheet_names <- excel_sheets(excel_path)

sheet_names %>% 
  map(function(sheet) {
    assign(x = sheet,
           value = readxl::read_xlsx(path = excel_path,sheet = sheet),
           envir = .GlobalEnv)
  })

# TODO - figure out optimal way to load excel sheets with varying names; moving on for now

VL_sup <- 
  `VL Suppression` %>% 
  gather("indicator", "percent",
         `VL Suppression 2016`:`VL Suppression 2019`) %>% 
  separate(indicator, c("type1", "type2", "Year")) %>% 
  unite("indicator", c("type1", "type2"), sep = " ") %>% 
  group_by(Name) %>% 
  mutate(ave_vl_sup = mean(percent)) %>% 
  ungroup() %>% 
  mutate(csort = fct_reorder(Name, ave_vl_sup, .desc = FALSE),
         Year = as.numeric(Year))

  
# No meaningful average to compare these figures to, so only showing trends
hiv_line(VL_sup, x = Year, y = percent, yave = percent, fill = percent, wrap = csort) +
  scale_fill_viridis_c(direction = -1, option = "D") +
  theme_panel +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = c(0, .25, .5, .75, 1)) +
  labs(x = "", y = "", 
       title = "Mandera and Lamu have the lowest viral load suppression percentages",
       caption = PEPFAR) +
  ggsave(file.path(imagepath, "KEN_VL_suppression_percent.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)



# TB Graphics -------------------------------------------------------------
tb_excel_path <- file.path(healthpath, "TB County data 2014_2018 - DSTB and DTRB Case Notified.xlsx")
tb_sheet_names <- excel_sheets(file.path(healthpath, "TB County data 2014_2018 - DSTB and DTRB Case Notified.xlsx"))

tb_sheet_names %>% 
  map(function(sheet) {
    assign(x = sheet,
           value = readxl::read_xlsx(path = tb_excel_path, sheet = sheet),
           envir = .GlobalEnv)
  })


tb_not <- 
  `TB Cases Notified` %>% 
  filter(!is.na(CID)) %>% 
  gather(tmp, value, -c(CID, County)) %>% 
  separate(tmp, c("indicator", "Year"), sep = 4) %>% 
  spread(indicator, value) %>% 
  group_by(Year) %>% 
  mutate_at(c("DRTB", "DSTB"), .funs = funs(ave = mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Year = as.numeric(Year), 
         csort_drtb = fct_reorder(County, DRTB, .desc = TRUE),
         csort_dstb = fct_reorder(County, DSTB, .desc = TRUE))


hiv_line(tb_not, x = Year, y = DRTB, yave = DRTB_ave, fill = DRTB, wrap = csort_drtb) +
  scale_fill_viridis_c(direction = -1, option = "B") +
  theme_panel +
  labs(x = "", y = "", 
       title = "Nairobi and Meru have the most cases notified drug resistance tuberculosis",
       subtitle = "Grey area represents country average for each year",
       caption = "Source: USAID Kenya Health Team") +
  ggsave(file.path(imagepath, "KEN_TB_resistance.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)
  

hiv_line(tb_not, x = Year, y = DSTB, yave = DSTB_ave, fill = DSTB, wrap = csort_dstb) +
  scale_fill_viridis_c(direction = -1, option = "B") +
  theme_panel +
  labs(x = "", y = "", 
       title = "Nairobi and Kiambu have the most cases for drug sensitive tuberculosis",
       subtitle = "Grey area represents country average for each year",
       caption = "Source: USAID Kenya Health Team") +
  ggsave(file.path(imagepath, "KEN_TB_sensitive.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)


# TB treatment success for DSTB
tb_success <- 
  `Treatment Success for DSTB` %>% 
  gather(Year, percent, Y2014:Y2017) %>% 
  mutate(Year = str_extract_all(Year, "\\d+") %>% as.numeric(),
         csort = fct_reorder(County, percent, .desc = TRUE)) 

# No meaningful average, so using fill
hiv_line(tb_success, x = Year, y = percent, yave = percent, fill = percent, wrap = csort) +
  scale_fill_viridis_c(direction = -1, option = "D") +
  theme_panel +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = c(0, .25, .5, .75, 1)) +
  labs(x = "", y = "", 
       title = "Mandera and Marsabit have the hightest treatment success rates for drug sensitive tuberculosis",
       caption = "Source: USAID Kenya Health Team") +
  ggsave(file.path(imagepath, "KEN_VL_suppression_percent.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)


  
  







budget_summary %>% 
  filter(AHADI %in% c(0, 1)) %>% 
  #filter(ASAL %in% c("Arid - 85-100% Aridity")) %>% 
  mutate(csort = fct_reorder(County, CID_absorption_dev, .desc = TRUE)) %>% 
  #mutate(csort = County) %>% 
  ggplot(aes(x = budget_year, y = CID_absorption_dev)) +
  geom_area(fill = grey10K, alpha = 0.50) +
  geom_line(colour = grey50K) +
  geom_point(aes(fill = CID_absorption_dev), 
             size = 3.5, shape = 21, colour = "white", stroke = 2) + 
  facet_wrap(ASAL_CODE ~ csort, labeller = label_wrap_gen(multi_line = FALSE)) +
  theme_minimal() +
  scale_fill_viridis_c(direction = -1, option = "D") +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 9),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.25, "lines")) +
  labs(caption = GC_caption, x = "", y = "",
       title = "Development Absorption Rates By County 2014/15 - 2017/18") +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1.03), # Be careful in case absorption rate is > 1
                     breaks = c(0, 0.25, 0.50, 0.75, 1))
ggsave(file.path(imagepath, "KEN_Dev_absorption_rate_timeseries.pdf"),
       plot = last_plot(), dpi = "retina", 
       height = 17, width = 16)

