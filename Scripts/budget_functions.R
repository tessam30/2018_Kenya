# Purpose: Budget plots and functions for processing budget data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_26
# Audience: Kenya Mission



# Captions & Flipped Bar Graphs -------------------------------------------

# Caption of data, for later use in graphics
GC_caption = c("Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2014/15, 2015/16, 2016/17, 2017/18, 2018/19")

bar_formatr <- list(
  geom_col(),
  theme_minimal(), 
  coord_flip()
)


# County Look -------------------------------------------------------------
# Use this function to create a summary plot of which counties spent the most


# Who planned the most, expended the most? Generalize into a function
county_look <- function(df, x, barcolor = grey70K) {
  xvar <- enquo(x)
  
  df %>% 
    group_by(County) %>% 
    mutate(tmp = sum(!!xvar, na.rm = TRUE)) %>% 
    ungroup() %>% 
    #mutate(c_sort = County) %>% 
    mutate(c_sort = fct_reorder(County, tmp, .desc = TRUE)) %>% 
    ggplot(aes(x = budget_year, y = !!xvar)) +
    geom_col(fill = barcolor) +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(hjust = 0, size = 12)) +
    facet_wrap(~ASAL_CODE + c_sort, 
               labeller = label_wrap_gen(multi_line = FALSE)) +
    labs(x = "", y = "") #+
  #scale_fill_manual(values = c("#cc4c02", 
  #                             "#fe9929",
  #                             "#fed98e",
  #                             "#ffffd4"))
}


# Budget maps -------------------------------------------------------------

# Function to create small multiple maps based on budget_totals_pdf data
budg_map <- function(df, xvar, leg_text = "") {
  xvar <- enquo(xvar) # grab the input text to know what to map
  
  df %>%
    left_join(asal_geo, by = c("CID" = "CID")) %>%
    ggplot(.) +
    geom_sf(aes(fill = !!xvar, geometry = geometry), colour = "white", size = 0.5) +
    facet_wrap(~budget_year, nrow = 1) +
    scale_fill_viridis_c(option = "D", direction = -1, alpha = 0.75) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.key.width = unit(1.5, "cm")
    ) + # adjust the width of the legend
    labs(
      fill = leg_text,
      caption = GC_caption
    )
}


# Budget scatter plot -----------------------------------------------------
# Used to check for outliers and see how absorption rates compare across time

# Scatter plot function to check outliers
budg_scatter <- function(df, xvar, yvar) {
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  
  df %>% 
    ggplot(aes(!!xvar, !!yvar)) + geom_point() +
    theme_minimal()
}



# High quality budget maps ------------------------------------------------


# ---- Use this if you need a print produciton quality map    
# Generic mapping function
budget_map <- function(df, xvar)  {
  xvar = enquo(xvar)
  
  df %>% 
    left_join(., asal_geo, by = c("CID" = "CID")) %>% 
    ggplot() +
    geom_sf(aes(fill = !!xvar, geometry = geometry), colour = "white", size = 0.5) +
    facet_wrap(~budget_year, nrow = 1) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.key.width = unit(1.25, "cm"),
          text = element_text(family = "Lato"),
          panel.grid = element_blank(),
          strip.text = element_text(hjust = 0, size = 12))
}
