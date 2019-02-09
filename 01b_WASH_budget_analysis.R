
# Use the 01_KEN_Budget_cleanup file to get data prepped
# WASH Plots and Maps -----------------------------------------------------
health_color = "#ef6548"
water_color = "#1d91c0"
    
# First, we'll show the total development spending on Water as a total share       
bs_plot <- function(df, x) {
  group_size = 12 # This can be adjusted to be calculated automoatically depending on category sizes
  ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x] # Grab category label for title/file name
  color = colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(group_size)[x] # Grab distinct color for each fill
  
  df %>% 
    filter(`Category Code` == x) %>% 
    mutate(county_sort = fct_reorder(County, `Exp Dev`, .desc = TRUE),
           text_lab = percent(exp_dev_share, 2)) %>% 
    select(county_sort, `Exp Dev`, budget_year, total_exp_dev, exp_dev_share, text_lab) %>%
    ggplot(aes(label = exp_dev_share)) +
    geom_col(aes(x = budget_year, y = total_exp_dev), fill = "#dddddd") +
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
           plot = last_plot(), device = 
           height = 8.5, width = 11, dpi = 300, useDingbats = FALSE)
}

# Need to figure out how to purrr this or nest it to iterate across all budget categories
  # Create a vector of budget category numbers to iterate over to save all budget share

b_list <- (seq(1, 12, by = 1))
map(b_list, ~bs_plot(budget, .))


bs_plot(budget, 3) 


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

  

