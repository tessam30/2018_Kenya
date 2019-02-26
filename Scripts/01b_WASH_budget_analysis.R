## %######################################################%##
#                                                          #
####          Budget plots and share analysis           ####
#                                                          #
## %######################################################%##
# Date: 2019_02_09
# Author: Tim Essam, Ph.D.
# Audience: Kenya Mission

# Pulls data from 01_KEN_Budget_cleanup.R

# Use the 01_KEN_Budget_cleanup file to get data prepped
# WASH Plots and Maps -----------------------------------------------------
  health_color <- "#ef6548"
  water_color <- "#1d91c0"



# "#fccde5" #pink --> County Assembly - 1
# "#d9d9d9" #grey --> Governor or County Executive - 2
# "#bebada" #purple --> Treasury - 3 --> #0F8554 dark green
# "#bc80bd" #dark purple --> Infrastructure - 4
# "#fb8072" #red --> Econ growth - 5
# "#fdb462" #orange --> Education - 6
# "#8dd3c7",#green --> Health - 7 OR #38A6A5
# "#ffffb3" #yellow --> Governor - 2
# "#ffed6f" #bright yellow --> land housing 8
# "#b3de69" #light green --> Agriculture - 9 #73AF48
# "#cab2d6" # ---> Youth 10
# "#80b1d3" #blue --> Water and Natural Resources - 11
# "#ccebc5" #mint green --> Public Service Boards - 12


# Budget colors based on color brewer
  bud_clrs <- c(
    "#fccde5", "#ffffb3", "#bebada", "#bc80bd",
    "#fb8072", "#fdb462", "#8dd3c7", "#ffed6f",
    "#b3de69", "#cab2d6", "#80b1d3", "#ccebc5"
  )
# 5F4690,#1D6996, 3, 4, 5, #EDAD08,#E17C05,#CC503E,#94346E,#6F4070,#994E95,#666666

# Another option for colors from CartoDB -- #855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#9C9C5E,#A06177,#8C785D,#467378,#7C7C7C ; Prism colors
  cat_colrs <- c(
    "#5F4690", "#1D6996", "#38A6A5",
    "#0F8554", "#73AF48", "#EDAD08",
    "#E17C05", "#CC503E", "#94346E",
    "#6F4070", "#994E95", "#666666"
  )


# Plot basics


# First, we'll show the total development spending on Water as a total share
bs_plot <- function(df, x) {
  # group_size = 12 # This can be adjusted to be calculated automoatically depending on category sizes
  ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x] # Grab category label for title/file name
  
  # color = colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(group_size)[x] # Grab distinct color for each fill
  color <- bud_clrs[x]

  df %>%
    filter(`Category Code` == x) %>%
    mutate(
      county_sort = fct_reorder(County, `Exp Dev`, .desc = TRUE),
      text_lab = percent(exp_dev_share, 2)
    ) %>%
    select(county_sort, `Exp Dev`, budget_year, total_exp_dev, exp_dev_share, text_lab) %>%
    ggplot(aes(label = exp_dev_share)) +
    geom_col(aes(x = budget_year, y = total_exp_dev), fill = "#f0f0f0") +
    geom_col(aes(x = budget_year, y = `Exp Dev`), fill = color, alpha = 1) +
    geom_text(aes(
      x = budget_year, y = `Exp Dev`,
      label = str_c("  ", text_lab)
    ),
    size = 2.5, colour = grey70K,
    hjust = 0
    ) +
    facet_wrap(~county_sort) +
    coord_flip() +
    theme_minimal() +
    ggtitle(str_c(ptitle, " Development Expenditures Compared to Overall Development Expenditures"),
      subtitle = "Percentage indicates share of development spending relative to total development expenditures"
    ) +
    labs(
      y = "Development expenditures in Kshs. Million", x = "",
      caption = GC_caption
    ) +
    theme(
      strip.text = element_text(hjust = 0),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8, hjust = 0),
      text = element_text(family = "Lato"),
      plot.caption = element_text(size = 7),
      panel.grid = element_line(size = 0.25),
      panel.grid.minor = element_blank()
    ) +
    ggsave(file.path(imagepath, str_c(ptitle, " Budget Share Summary.pdf")),
      plot = last_plot(),
      height = 8.5, width = 11, dpi = 300, useDingbats = FALSE
    )
}

# Need to figure out how to purrr this or nest it to iterate across all budget categories
# Create a vector of budget category numbers to iterate over to save all budget share
  b_list <- (seq(1, 12, by = 1))
  map(b_list, ~ bs_plot(budget, .))
  
  bs_plot(budget, 5)


# Heatmap of all budget categories ----------------------------------------


budget %>%
  filter(`Category Code` != 0) %>%
  select(County, exp_dev_share, budget_year, total_exp_dev, Budget_title) %>%
  mutate(
    county_sort = fct_reorder(County, total_exp_dev, .desc = TRUE),
    budget_sort = fct_reorder(Budget_title, exp_dev_share, .desc = TRUE)
  ) %>%
  ggplot(aes(x = County, y = budget_year)) +
  geom_tile(aes(fill = exp_dev_share), color = grey80K) +
  scale_fill_viridis_c(
    direction = -1, alpha = 0.90,
    option = "A", label = percent_format(accuracy = 2)
  ) + # format labels in legend
  theme_minimal() +
  coord_fixed(ratio = 1.5) + # Fix the size of the squares
  facet_wrap(~budget_sort, ncol = 2) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.05),
    axis.title = element_text(size = 8, hjust = 0), # making the x-axis title smaller
    panel.spacing = unit(1.5, "lines"),
    panel.grid.minor.x = element_blank()
  ) + # adding more spaced between panels
  labs(
    fill = "Share of Development Expenditures",
    y = "", x = "",
    caption = GC_caption
  )  +
  ggtitle("Development expenditures on Transport and Infrastructure formed the largest budget share") +
  ggsave(file.path(imagepath, "KEN_budget_shares_heatmap.pdf"),
    height = 17, width = 16, dpi = "retina"
  )




# Stacked bar graph of budget totals over time -- Not that useful
budget %>%
  select(County, exp_dev_share, budget_year, total_exp_dev, Budget_title) %>%
  mutate(county_sort = fct_reorder(County, total_exp_dev, .desc = TRUE)) %>%
  ggplot(aes(x = budget_year, y = total_exp_dev)) +
  geom_col(aes(fill = Budget_title)) +
  coord_flip() +
  facet_wrap(~county_sort)



# TODO: Create a County profile map / product that summarises the budget; Markdown?
# Take the bs_plot and flip it to produce a map of every county, sorted by greatest budget shares within a county

county_plot <- function(df, x) {
  # group_size = 12 # This can be adjusted to be calculated automoatically depending on category sizes
  ptitle <- county_list$Counties[county_list$CID == x] # Grab category label for title/file name
  
  # color = colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(group_size)[x] # Grab distinct color for each fill
  color <- c(
    "#fccde5", "#ffffb3", "#bebada", "#bc80bd",
    "#fb8072", "#fdb462", "#8dd3c7", "#ffed6f",
    "#b3de69", "#cab2d6", "#80b1d3", "#ccebc5"
  )
  
  df %>%
    filter(`CID` == x & `Category Code` != 0) %>%
    group_by(Budget_title) %>% 
    mutate(tmp_sort = sum(`Exp Dev`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(
      budg_sort = fct_reorder(Budget_title, tmp_sort, .desc = TRUE),
      text_lab = percent(exp_dev_share, 2)
    ) %>%
    select(budg_sort, `Exp Dev`, budget_year, total_exp_dev, exp_dev_share, text_lab, `Category Code`) %>%
    ggplot(aes(label = exp_dev_share)) +
    geom_col(aes(x = budget_year, y = total_exp_dev), fill = "#f0f0f0") +
    geom_col(aes(x = budget_year, y = `Exp Dev`, fill = factor(`Category Code`)), alpha = 1) +
    geom_text(aes(
      x = budget_year, y = `Exp Dev`,
      label = str_c("  ", text_lab)
    ),
    size = 2.5, colour = grey70K,
    hjust = 0
    ) +
    facet_wrap(~budg_sort, ncol = 4) +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = color) +
    ggtitle(str_c(ptitle, " Development Expenditures Compared to Overall Development Expenditures"),
            subtitle = "Percentage indicates share of development spending relative to total development expenditures"
    ) +
    labs(
      y = "Development expenditures in Kshs. Million", x = "",
      caption = GC_caption
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(hjust = 0),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8, hjust = 0),
      text = element_text(family = "Lato"),
      plot.caption = element_text(size = 7),
      panel.grid = element_line(size = 0.25),
      panel.grid.minor = element_blank()
    ) +
     ggsave(file.path(imagepath, str_c(ptitle, " Budget Share Summary.pdf")),
            plot = last_plot(),
            height = 8.5, width = 11, dpi = 300, useDingbats = FALSE
    )
}

county_plot(budget, 47) # Test function

c_list <- (seq(1, 47, by = 1))
map(c_list, ~ county_plot(budget, .)) # Writing all 47 products to pdf

# Absorption Rates over time ----------------------------------------------


b_plot <- function(df, x, y = exp_dev_share) {
  # df - data to read
  # x = category code, should be numeric
  # y = fill variable to use
  ptitle <- budget_cw$Budget_title[budget_cw$`Category Code` == x]
  yvar <- enquo(y)

  df %>%
    filter(`Category Code` == x) %>%
    select(budget_year, Absorption_dev, County, `Category Code`, Budget_title, exp_dev_share) %>%
    ggplot(aes(x = budget_year, y = !!yvar)) +
    geom_area(fill = grey10K, alpha = 0.50) +
    geom_line(colour = "grey") +
    geom_point(aes(fill = !!yvar),
               size = 3.5, shape = 21, colour = "white", stroke = 2) +
    scale_fill_viridis_c(direction = -1, option = "D") +
    facet_wrap(~County, nrow = 8) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 8),
      legend.position = "none",
      panel.spacing = unit(1.5, "lines"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
      # breaks = seq(0, 1, by = 0.25)
    ) +
    scale_x_continuous(breaks = seq(2015, 2018, by = 1)) +
    ggtitle(str_c(ptitle, " Absorption Rate")) +
    labs(
      y = "absorption rate", x = "",
      caption = "Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2015/16, 2016/17, 2017/18"
     ) +
     ggsave(file.path(imagepath, str_c(ptitle, " Absorption Rate Summary.pdf")),
       plot = last_plot(),
       height = 8.5, width = 11, dpi = 300, useDingbats = FALSE
    )
}

b_plot(budget, x = 4, Absorption_dev)
map(b_list, ~ b_plot(budget, ., Absorption_dev))


# Create one final function to make County-organized plots of abso --------

c_plot <- function(df, x, y = Absorption_dev) {
  # df - data to read
  # x = category code, should be numeric
  # y = fill variable to use
  ptitle <- county_list$Counties[county_list$CID == x]
  yvar <- enquo(y)
  
  df %>%
    filter(CID == x & `Category Code` != 0) %>%
    group_by(Budget_title, CID) %>% 
    mutate(tmp1 = sum(`Exp Dev`, na.rm = TRUE),
           tmp2 = sum(`ASBADev`, na.rm = TRUE),
           sort_var = tmp1/tmp2) %>% 
    ungroup() %>% 
    mutate(Budget_title = fct_reorder(Budget_title, sort_var, .desc = TRUE)) %>% 
    select(budget_year, Absorption_dev, County, `Category Code`, Budget_title, exp_dev_share) %>%
    ggplot(aes(x = budget_year, y = !!yvar)) +
    geom_area(fill = grey10K, alpha = 0.50) +
    geom_line(colour = "grey") +
    geom_point(aes(fill = !!yvar), size = 3.5, shape = 21, colour = "white", stroke = 2) +
    scale_fill_viridis_c(direction = -1, option = "D") +
    facet_wrap(~Budget_title, ncol = 3) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 8),
      legend.position = "none",
      panel.spacing = unit(1.5, "lines"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(hjust = 0)
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
      # breaks = seq(0, 1, by = 0.25)
    ) +
    scale_x_continuous(breaks = seq(2015, 2018, by = 1)) +
    labs(title = str_c(ptitle, " development absorption rate summary by budget category"),
      y = "absorption rate", x = "",
      subtitle = "Development absorption rate is the share of actual development expenditure out of the budgeted expenditure.",
      caption = "Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2015/16, 2016/17, 2017/18"
     ) +
    ggsave(file.path(imagepath, str_c(ptitle, " Absorption Rate Summary.pdf")),
            plot = last_plot(),
            height = 8.5, width = 11, dpi = 300, useDingbats = FALSE
    )
}

map(c_list, ~ c_plot(budget, .))
c_plot(budget, 42)




# Exporting budget data ---------------------------------------------------

write_csv(budget, path = file.path(budgetpath, "KEN_budget_draft_2019_02_10.csv"))



# Export budget data for ESRI map production w/ shape files ---------------






