#######################################################%####
#                                                          #
####     Kenya Timeline Project                         ####
#                                                          #
##%######################################################%##

# Purpose: Gather, reshape and plot basic time events and stats
# Author: Tim Essam (code), Brent McCusker (data)
# Date: 2019_05_22
# Audience: Kenya Mission

library(tidyverse)
library(rlang)
library(purrr)
library(readxl)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(ggpubr)
library(ggrepel)
library(lubridate)
library(llamar)
library(extrafont) # run font_import() if fist time installing 


# Theme timeline for plots ------------------------------------------------
# Sets the ggplot background elements for the timeline.
theme_timeline <- function(base_size = 10, base_family = "Lato Light") {
  ret <- theme_minimal(base_size, base_family) %+replace%
    theme(axis.ticks.x = element_blank(), 
          axis.line.x = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())
  ret
}


# Load Data ---------------------------------------------------------------
excel_sheets(file.path(datapath, "Kenya TImelines.xlsx"))

# datapath is the Data directory for the project, modify this as needed;
df_bar <- read_excel(file.path(datapath, "Kenya TImelines.xlsx"), sheet = "Bar Data")
df_line <- read_excel(file.path(datapath, "Kenya TImelines.xlsx"), sheet = "Line Data")

# Checking consistency of data and variable names
map(list(df_bar, df_line), str)

# Turn off scientific notation for now
options(scipen = 999)


# Basic function to do grouped summaries
group_count <- function(df, ...) {
  grouping <- enquos(...)
  
  df %>% group_by(!!!grouping) %>% 
    count() %>% 
    print(n = Inf)
}


# Mutate and plot ---------------------------------------------------------
# Transforming the wide data into a long dataset so it can be plotted and facete
df_line_long <- 
  df_line %>% 
  select(date, KE_pop = k_pop, everything()) %>% 
  mutate(KE_pop = KE_pop/1e6) %>% 
  gather(key = "indicator", value = "value", KE_GDP_growth:UG_ag_pct_GDP) %>% 
  
  # Use the 'extra' option within separate to keep indicator name together
  separate(indicator, into = c("Country", "Indicator"), extra = "merge") %>%
  mutate(Country = case_when(
    Country == "KE" ~ "Kenya", 
    Country == "TZ" ~ "Tanzania",
    Country == "UG" ~ "Uganda",
    TRUE ~ NA_character_),
    flag = ifelse(Country == "Kenya", 1, 0))

# Checking that ouput and reshaped data is consistent
df_line_long %>% group_by(Country, Indicator) %>%  count() %>% arrange(Indicator)

# Setting the range of the x-axis in the time-series plots; Adjust if you update/add data beyond 2020
dat_min <- c("1960-01-01")
dat_max <- c("2020-01-01")

# Set break vector and limits so plots can be aligned
dat_seq <- seq(as.POSIXct(dat_min),
               as.POSIXct(dat_max), "10 years")
lims <- as.POSIXct(strptime(c(dat_min, dat_max), 
                            format = "%Y-%m-%d"))

# Check w/ a Gantt chart any potential overlap in events
bar_long <- df_bar %>%
  select(Start, End, everything()) %>% 
  mutate(Start = ymd(Start),
         End = ymd(End)) %>%
  gather(date_node, event_date, -c(Sector:Description2)) %>%
  arrange(date_node, event_date) %>%
  mutate(Event_abbr = fct_reorder(Event_abbr, event_date))

#Where to put dotted lines
bar_long %>% 
  filter(Sector == "AG POLICY") %>% # Ag POLICY has numerous overlapping events
  ggplot(aes(x = Event_abbr, y = event_date, colour = Sector)) + 
  geom_line(size = 6) + 
  guides(colour = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  scale_y_date(date_breaks = "10 years", labels = date_format("%b ‘%y")) +
  facet_wrap(~Sector, scales = "free") 


# General line plot function ----------------------------------------------
# General function to make line plots of individual indicators in case they are needed

line_plot <- function(df, ...) {
  F <- quos(...)
  df %>% filter(!!!F) %>% 
    mutate(ylim = min(value)) %>% 
    ggplot(aes(x = date, y = value)) +
    geom_line(colour = grey50K) +
    scale_x_datetime(limits = lims,
                     labels = date_format("%Y")) +
    theme_minimal()
}

# Create plots of each indicator to see how they will apear overlaid
df_line_long %>% split(.$Indicator) %>% 
  map(., ~line_plot(.) + facet_wrap(~ Country, nrow = 3))


# Fix the indicator names so they are in plain english on plots
indicator_names <- c(
  ag_pct_GDP = 'Agriculture GDP',
  FDI_pct_GDP = 'Foreign Direct Investment, net inflows (% of GDP)',
  GDP_growth = 'GDP Growth Rate',
  pop_growth_rate = 'Population Growth Rate'
)


# Produces the line plot for the timeline ---------------------------------


line_p <- df_line_long %>% 
  mutate(Country = fct_rev(Country),
         label = if_else(date == max(date), as.character(Country), NA_character_)) %>% 
  ggplot(., aes(x = date, y = value, group = Country)) +
  geom_line(aes(colour = Country)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("1960-01-01"), as.POSIXct("2020-01-01"), "10 years"),
    limits = lims,
    labels = date_format("%Y")
  ) +
  scale_color_manual(values = c(grey30K, grey30K, grey80K)) +
  facet_wrap(~Indicator, nrow = 4, scales = "free",
             labeller = labeller(Indicator = indicator_names)) + #fixed strip text names
  theme_minimal(base_family = "Lato Light") +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(hjust = 0, size = 14)) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE, 
                   label.size = NA,
                   fill = NA,
                   family = "Lato Light",
                   segment.color = 'transparent') +
  labs(caption = "Source: USAID GeoCenter gathered facts and figures")
line_p

# Bar plot of the events stacked - This will be combined w/ line graphs to form basis for graphic
bar_p <- df_bar %>% 
  ggplot(.) + 
  geom_rect(aes(xmin = Start, 
                xmax = End, 
                ymin = ymin, 
                ymax = ymax, 
                fill = Sector_full), 
            colour = "white") + 
  facet_wrap(~Sector_full, nrow = 6) +
  geom_text_repel(aes(x = Start + (End - Start)/2, 
                      y = ymin + (ymax - ymin)/2, 
                      label = Event_abbr), 
                  size = 3,
                  point.padding = NA,
                  family = "Lato Light") +
  theme_timeline() +
  theme(strip.text.x = element_text(hjust = 0, size = 14)) +
  scale_fill_viridis_d(alpha = 0.45) +
  scale_x_datetime(
    breaks = seq(as.POSIXct(dat_min),
                 as.POSIXct(dat_max), "10 years"),
    labels = date_format("%Y"),
    expand = c(0.05, 0.05),
    limits = lims) 
bar_p



# Combine plots, align to a common scale and export with embedded  --------

ken_tl <- ggarrange(bar_p, line_p, nrow = 2,
                    align = "v") %>% 
  annotate_figure(., top = text_grob("Kenya: Historical Events Summarized"))  

ggsave(file.path(imagepath, "KEN_timeline_2019_05_22.pdf"), plot = ken_tl,
       dpi = 300, width = 18, height = 12, units = "in",
       device = cairo_pdf, scale = 2)

write_csv(df_bar, file.path(datapath, "KEN_bar_graphdata.csv"))
write_csv(df_line_long, file.path(datapath, "KEN_line_graphdata.csv"))

