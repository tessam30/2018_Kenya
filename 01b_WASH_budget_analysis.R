##%######################################################%##
#                                                          #
####          Budget plots and share analysis           ####
#                                                          #
##%######################################################%##
# Date: 2019_02_09
# Author: Tim Essam, Ph.D.
# Audience: Kenya Mission

# Pulls data from 01_KEN_Budget_cleanup.R

# Use the 01_KEN_Budget_cleanup file to get data prepped
# WASH Plots and Maps -----------------------------------------------------
health_color = "#ef6548"
water_color = "#1d91c0"



 # "#8dd3c7" #green --> Health - 7
 # "#ffffb3" #yellow --> Governor - 2
 # "#bebada" #purple --> Treasury - 3
 # "#fb8072" #red --> Econ growth - 5
 # "#80b1d3" #blue --> Water and Natural Resources - 11
 # "#fdb462" #orange --> Education - 6
 # "#b3de69" #light green --> Agriculture - 9
 # "#fccde5" #pink --> County Assembly - 1
 # "#d9d9d9" #grey --> Governor or County Executive - 2
 # "#bc80bd" #dark purple --> Infrastructure - 4
 # "#ccebc5" #mint green --> Public Service Boards - 12
 # "#ffed6f" #bright yellow --> land housing 8

# Budget colors based on color brewer
 bud_clrs <- c("#fccde5", "#ffffb3", "#bebada", "#bc80bd", 
              "#fb8072", "#fdb462", "#8dd3c7", "#ffed6f",
              "#b3de69", "#cab2d6", "#80b1d3", "#ccebc5")

    
# First, we'll show the total development spending on Water as a total share       
bs_plot <- function(df, x) {
  group_size = 12 # This can be adjusted to be calculated automoatically depending on category sizes
  ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x] # Grab category label for title/file name
  #color = colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(group_size)[x] # Grab distinct color for each fill
  color = bud_clrs[x]
  
  df %>% 
    filter(`Category Code` == x) %>% 
    mutate(county_sort = fct_reorder(County, `Exp Dev`, .desc = TRUE),
           text_lab = percent(exp_dev_share, 2)) %>% 
    select(county_sort, `Exp Dev`, budget_year, total_exp_dev, exp_dev_share, text_lab) %>%
    ggplot(aes(label = exp_dev_share)) +
    geom_col(aes(x = budget_year, y = total_exp_dev), fill = "#f0f0f0") +
    geom_col(aes(x = budget_year, y = `Exp Dev`), fill = color, alpha = 1) +
    geom_text(aes(x = budget_year, y = `Exp Dev`, 
                  label = str_c("  ", text_lab)), size = 2.5, colour = grey70K,
              hjust = 0) +
    facet_wrap(~county_sort) + 
    coord_flip() + 
    theme_minimal() +
    ggtitle(str_c(ptitle, " Spending Compared to Overall Development Expenditures"),
            subtitle = "Percentage indicates share of development spending relative to total development expenditures") +
    labs(y = "Development expenditures in Kshs. Million", x = "",
         caption = "Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2015/16, 2016/17, 2017/18") +
    theme(strip.text = element_text(hjust = 0),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 8, hjust = 0),
          text = element_text(family = "Lato"),
          plot.caption = element_text(size = 7),
          panel.grid = element_line(size = 0.25)) +
    ggsave(file.path(imagepath, str_c("KEN Development ", ptitle, " Budget Share Summary.pdf")), 
           plot = last_plot(),
           height = 8.5, width = 11, dpi = 300, useDingbats = FALSE)
}

# Need to figure out how to purrr this or nest it to iterate across all budget categories
  # Create a vector of budget category numbers to iterate over to save all budget share
  b_list <- (seq(1, 12, by = 1))
  map(b_list, ~bs_plot(budget, .))



# Heat map of budget categories across time
budget %>% select(County, exp_dev_share, budget_year, total_exp_dev, Budget_title) %>%
  mutate(county_sort = fct_reorder(County, total_exp_dev, .desc = TRUE)) %>% 
  ggplot(aes(x = county_sort, y = Budget_title, fill = exp_dev_share)) +
  geom_tile(color = "white") + scale_fill_viridis_c(direction = -1, alpha = 0.75) +
  theme_minimal() +  coord_fixed(ratio = 1.5) +
  facet_wrap(~ budget_year, nrow = 3) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,  hjust = 1)) +
  ggtitle("")

# Stacked bar graph of budget totals over time -- Not that useful
  budget %>% 
    select(County, exp_dev_share, budget_year, total_exp_dev, Budget_title) %>%
    mutate(county_sort = fct_reorder(County, total_exp_dev, .desc = TRUE)) %>% 
    ggplot(aes(x = budget_year, y = total_exp_dev)) +
    geom_col(aes(fill = Budget_title)) +
    coord_flip() +
    facet_wrap(~ county_sort)

  # function to create plot based on a category

# Absorption rate function summary ----------------------------------------

  b_plot <- function(df, x, y = exp_dev_share) {
    # df - data to read
    # x = category code, should be numeric
    # y = fill variable to use 
    ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x]
    yvar <- enquo(y)
    
    df %>% 
      filter(`Category Code` == x) %>% 
      select(budget_year,  Absorption_dev, County, `Category Code`, Budget_title, exp_dev_share) %>% 
      ggplot(aes(x = budget_year, y = !!yvar)) +
      geom_line(colour = "grey") +
      geom_point(aes(colour = !!yvar)) +
      scale_colour_viridis_c(direction = -1) +
      facet_wrap(~ County, nrow = 8) +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 8),
            legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.25)) +
      scale_x_continuous(breaks = seq(2015, 2018, by = 1)) +
      ggtitle(str_c(ptitle, "Absorption Rate")) +
      labs(y = "", x = "",
           caption = "Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2015/16, 2016/17, 2017/18") +
        ggsave(file.path(imagepath, str_c(ptitle, " Absorption Rate Summary.pdf")), 
               plot = last_plot(),
               height = 8.5, width = 11, dpi = 300, useDingbats = FALSE)
  }
  
  b_plot(budget, x = 6)
  map(b_list, ~b_plot(budget, .))
  