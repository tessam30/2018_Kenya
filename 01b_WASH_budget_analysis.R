
# Use the 01_KEN_Budget_cleanup file to get data prepped
# WASH Plots and Maps -----------------------------------------------------
health_color = "#ef6548"
water_color = "#1d91c0"
    
# First, we'll show the total development spending on Water as a total share       
bs_plot <- function(df, x, color = "#1d91c0") {
  ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x]
  
  df %>% 
    filter(`Category Code` == x) %>% 
    mutate(county_sort = fct_reorder(County, `Exp Dev`, .desc = TRUE),
           text_lab = percent(exp_dev_share, 2)) %>% 
    select(county_sort, `Exp Dev`, budget_year, total_exp_dev, exp_dev_share, text_lab) %>%
    ggplot(aes(label = exp_dev_share)) +
    geom_col(aes(x = budget_year, y = total_exp_dev), fill = "#dddddd") +
    geom_col(aes(x = budget_year, y = `Exp Dev`), fill = color) +
    geom_text(aes(x = budget_year, y = `Exp Dev`, 
                  label = str_c("  ", text_lab)), size = 2.5, colour = grey70K,
              hjust = 0) +
    facet_wrap(~county_sort) + 
    coord_flip() + 
    theme_minimal() +
    ggtitle(str_c("Development ", ptitle, " Spending Compared to Overall Development Spending"),
            subtitle = "Percentage indicates share of spending relative to total") +
    labs(y = "Development spending in Kshs. Million", x = "", size = 8) +
    theme(strip.text = element_text(hjust = 0))
}

# Need to figure out how to purrr this or nest it to iterate across all budget categories







health_plot <- bs_plot(budget, 7, color = health_color) 
water_plot <- bs_plot(budget, 11)
ggsave(plot = water_plot, file.path(imagepath, "KEN_Water_Dev_budget.pdf"), height = 8, width = 11.5)
ggsave(plot = health_plot, file.path(imagepath, "KEN_Health_Dev_budget.pdf"), device = "pdf",
       height = 8, width = 11.5)

  

