# Purpose: Load and process Kenya budget data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_26
# Audience: Kenya Mission


# load data and crosswalks ---------------------------------------------------------------
source(file.path(rpath, "budget_cw.R"))

# Reading these in separately to check insheeting carefully. Data have been scraped from PDFs
excel_sheets(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"))

budget_2014_in <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"),
                             sheet = "Budget Nos 14-15")

budget_2015_in <- read_excel(file.path(budgetpath, 
                                       "County Budget Database_TE_Edits.xlsx"), 
                             sheet = "Budget Nos 15-16")
budget_2016_in <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"), 
                              sheet = "Budget Nos 16-17")

budget_2017_in <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"),
                             sheet = "Budget Nos 17-18") 

budget_list <- list(budget_2014_in, budget_2015_in, budget_2016_in, budget_2017_in)
map(budget_list, ~str(.))

# Need to fix a few columns that are read-in as characters rather than numbers
# Acutally fixed these upstream so that Mission can have original working raw data
budget_2014 <- budget_2014_in %>% 
  mutate(budget_year = 2014) %>% 
  select(-County)

budget_2015 <- budget_2015_in %>% 
  mutate(budget_year = 2015)

budget_2016 <- budget_2016_in %>% 
  mutate(budget_year = 2016)

budget_2017 <- budget_2017_in %>% 
  mutate(budget_year = 2017)

# Use $ at the end to ensure that we only captures objects that end in "_in"
rm(list = ls(pattern = "*_in$"))

# Caption of data, for later use in graphics
GC_caption <- c("Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2014/15, 2015/16, 2016/17, 2017/18")

# Create budget database with proper roll-ups and budget shares -----------

budget_raw <- 
  bind_rows(budget_2015, budget_2016, budget_2017, budget_2014) %>%
  
  # add in ASAL categories
  left_join(., asal, by = c("CID" ="CID")) %>% 
  
  # Add in budget category crosswalk
  left_join(., budget_cw, by = c("Category Code" = "Category Code")) %>% 
  
  # Clean up names and rearrange for ease of viewing
  select(CID, 
         County = Counties, 
         budget_year, 
         `Category Code`,
         Budget_title, 
         budget_type = `Category.x`, 
         `ASBA Rec`:`Abs Rate Dev`,
         ASAL = `Category.y`,
         everything()) %>% 
  arrange(CID, budget_type, budget_year) %>% 
  
  # Before looking at whether or not categories change, we need count the budget_title
  # occurences per year to see where categories are repeating.
  # Anything greater than 1 indicates a repeated budget category
  group_by(CID, budget_year, `Category Code`) %>% 
  mutate(budget_category_count = n()) %>% 
  ungroup() %>% 
  select(budget_category_count, everything()) %>% 
  arrange(CID, `Category Code`, budget_year)

# checking structure to ensure data types are as expected. Get some wonkiness from pdfs
str(budget_raw)
Hmisc::describe(budget_raw)

# Subset the raw totals - These will be used later to compare with my own calculations
  budget_totals_pdf <-  
    budget_raw %>% 
    filter(`Category Code` == 13) %>% 
    mutate(budget_type = "Total") %>% 
    mutate(Expend_excheq_rec = bs_calc(`Exp Rec`, `Exq Rec`),
           Expend_excheq_dev = bs_calc(`Exp Dev`, `Exq Dev`),
           Absorption_recur = bs_calc(`Exp Rec`, `ASBA Rec`),
           Absorption_dev = bs_calc(`Exp Dev`, ASBADev))
  
  # Who planned the most, expended the most? Generalize into a function
  county_look <- function(df, x) {
    xvar <- enquo(x)
    
    df %>% 
      group_by(County) %>% 
      mutate(tmp = sum(!!xvar, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(c_sort = fct_reorder(County, tmp, .desc = TRUE)) %>% 
      ggplot(aes(x = budget_year, y = !!xvar))+
      geom_col(fill = grey70K) +
      coord_flip() +
      theme_minimal() +
      facet_wrap(~c_sort) +
      labs(x = "", y = "")
  }
  
  county_look(budget_totals_pdf, `ASBADev`)
  county_look(budget_totals_pdf, `Exp Dev`)
  

  # Collapsing everything down to standardized budget categories to create budget shares
  # Based on yearly totals
  # Need to recreate the following variables that are based on two variables
  # The bs_calc takes care of the calculation, just feed in x and y 
  # Exp_exq_rec = Exp Rec / Exq Rec 
  # Exp_exq_dev = Exp Dev / Exq Dev 
  # Absorption_recurring =  Exp Rec / ASBA Rec 
  # Absorption_development = Exp Dev / ASBADev 
  
  budget <- 
    budget_raw %>% 
      filter(`Category Code` != 13) %>% 
      group_by(CID, `Category Code`, budget_year, Budget_title, County, ASAL) %>% 
      summarise_at(vars(`ASBA Rec`:`Exp Dev`), funs(sum(., na.rm = TRUE))) %>% 
    
    # using bs_calc custom function to calculate relative shares
      mutate(Expend_excheq_rec = bs_calc(`Exp Rec`, `Exq Rec`),
             Expend_excheq_dev = bs_calc(`Exp Dev`, `Exq Dev`),
             Absorption_recur = bs_calc(`Exp Rec`, `ASBA Rec`),
             Absorption_dev = bs_calc(`Exp Dev`, ASBADev)) %>% 
    # Create a count for each county / year to see how many categories are in data
    group_by(CID, budget_year) %>% 
    mutate(category_count = n()) %>% 
    ungroup() %>% 
    select(CID, category_count, `Category Code`, budget_year, Budget_title, everything()) %>% 
    
    # Created lagged budget share categories for arrows in tableau and to look at changes
    group_by(CID, `Category Code`) %>% 
    mutate_at(vars(`Exp Dev`, Absorption_dev), .funs = funs(lag = lag(. , n = 1, by = budget_year))) %>% 
    mutate(exp_higher_2016 = `Exp Dev` > `Exp Dev_lag`,
           exp_compare = case_when(
             `Exp Dev` > `Exp Dev_lag` ~ "higher than previous year",
             `Exp Dev` <= `Exp Dev_lag` ~ "lower or same than previous year",
             TRUE ~ NA_character_
           )) %>% 
    ungroup() %>% 
    
    # Create development expenditure shares as share of total development expenditures
    # This is needed to do share analysis and show relative importance of each category 
    group_by(CID, budget_year) %>% 
    mutate(total_exp_dev = sum(`Exp Dev`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exp_dev_share = bs_calc(`Exp Dev`, total_exp_dev))

  
  # Check how many categories are "complete" for each code
  # Need to have a sense of what categories we can trust for comparison over time
  budget %>% group_by(`Category Code`, budget_year) %>% count() %>% spread(budget_year, n)
    
  budget %>% 
    group_by(Budget_title) %>% 
    nest() %>% 
    mutate(plot = map(data, ~ggplot(., aes(budget_year, y = Absorption_dev))+
                        geom_line(colour = "grey") +
                        geom_point(aes(colour = Absorption_dev)) +
                        scale_colour_viridis_c(direction = -1) +
                        facet_wrap(~ County) +
                        theme_minimal()
    ),
    filename = paste0(Budget_title, ".pdf")
    ) %>% 
    select(filename, plot) %>% 
    pwalk(., ggsave, path = imagepath)
  
# Overall absorption rate from the totals cateogry (# 13)
  budget_totals_pdf %>% 
    left_join(asal_geo, by = c("CID" = "CID")) %>% 
    ggplot(.) +
    geom_sf(aes(fill = Absorption_dev), colour = "white", size = 0.5) +
    facet_wrap(~ budget_year, nrow = 1) +
    scale_fill_viridis_c(option = "A", direction = -1, label = percent_format(accuracy = 2)) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.key.width = unit(2, "cm")) + #adjust the width of the legend
    labs(caption = GC_caption,
         fill = "Overall absorption rate") +
    ggtitle("Garissa and Bomet had the highest average development absorption rates")+
    ggsave(file.path(imagepath, "KEN_develompent_absorption_rates_map.pdf"),
           height = 11.7, width = 16.5)
    
    budget_totals_pdf %>% 
      group_by(budget_year) %>% 
      mutate(dev_exp = sum(`Exp Dev`, na.rm = TRUE),
             ASBADev = sum(ASBADev, na.rm = TRUE),
             natl_absorp = dev_exp / ASBADev) %>% 
      ungroup() %>% 
      group_by(County) %>%
      mutate(ave_absorp = mean(Absorption_dev, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(County_sort = fct_reorder(County, ave_absorp, .desc = TRUE)) %>% 
      ggplot(aes(x = budget_year, y = natl_absorp)) +
      geom_line(color = grey40K) +
      geom_line(aes(y = Absorption_dev)) +
      facet_wrap(~ County_sort) + theme_minimal() +
      labs(x = "", y = "",
           caption = GC_caption,
           title = "Bomet, Isiolo, and West Pokot had the highest development absorption rates",
              subtitle = "Gray line indicates national absorption rate")
    

# Compare GOK development expenditure totals to GC calculated totals
options(scipen = 999)
   b_gc <- budget %>% filter(`Category Code` == 7) %>% select(CID, County, budget_year, total_exp_dev) 
  
# Be careful how you merge the data b/c not all category codes are available for all Counties    
 b_gc_pdf <-  budget_totals_pdf %>% select(CID, County, budget_year, `Exp Dev`) %>% 
   left_join(., b_gc, by = c("CID" = "CID", "budget_year" = "budget_year")) %>% 
   mutate(diff = `Exp Dev` - total_exp_dev,
          diff = round(diff, 2)) %>% 
   arrange(diff)
  
# Incorrect budget totals -------------------------------------------------
 # Kisumu in 2017 should be 70.8 for City of Kisumu Development Expenditures - still off even after this
 # Wajir 2015/16 development expenditures do not add up to pdf total
 # Isiolo 2017/18 developmen expenditures do not add up
 # Tana River in 2017/18 is off due to a special program
 # Kirinyaga development expenditures do not add up in 2017/18 
 # Nyeri totals are 1,140,315,329 in 2017 for development expenditures, but line item budget only sums to 841
 
  
# Plot to make - absorption rate by county by budget category, looped over county, sorted by category
      
    
    
# Show which counties do not have categories across all 12 categories (47 X 12 matrix basically)
    budget %>% 
      filter(`Category Code` != 0) %>% 
      group_by(County, Budget_title) %>% 
      count() %>% 
      mutate(count = as.character(n)) %>% 
      ggplot(aes(x = Budget_title, y = County )) + 
      geom_tile(aes(fill = count), colour = "white") +
      scale_fill_brewer(palette = 13, direction = 1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "top") +
      labs(title = "Health is the most consistent budget category across 2014/15 - 2017/18",
           y = "", x = "", caption = "Source: 2014/15, 2015/16, 2016/17 & 2017/18 Budget Data",
           fill = "Number of times budget category appears") +
      coord_flip() +
      ggsave(file.path(imagepath, "Budget_summary.pdf"), width = 11, height = 5) 
      
    
    
    
    
# Or the purr way - saving all plots as pdfs
budget %>% 
    group_by(Budget_title) %>% 
    nest() %>% 
    mutate(plot = map(data, ~ggplot(., aes(budget_year, y = Absorption_dev))+
                        geom_line(colour = "grey") +
                        geom_point(aes(colour = Absorption_dev)) +
                        scale_colour_viridis_c(direction = -1) +
                        facet_wrap(~ County) +
                        theme_minimal()
                      ),
           filename = paste0(Budget_title, ".pdf")
           ) %>% 
    select(filename, plot) %>% 
  pwalk(., ggsave, path = imagepath)

# Heatmap maybe just as effective for showing development absorption rates across time
  budget %>% 
    filter(!is.na(Absorption_dev) & Budget_title != "Uncategorized") %>% 
    ggplot(aes(y = Budget_title, x = County)) +
    geom_tile(aes(fill = Absorption_dev), colour = grey60K) +
    facet_rep_wrap(~ budget_year, nrow = 4, repeat.tick.labels = TRUE) + 
    coord_equal() + 
    scale_fill_viridis_c(option = "A", alpha = 0.9, direction = -1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
    
# Budget notes to be resolved - updated in github on 2/4/2019
    # Homa Bay 2015 Econ Growth (3) really was 18.34
    # Machakos jumps to nearly 2 in 2016 for Transport and Infrastructure (4)
    # Tana River is very high in 2015 for Econ Growth (5); Tharaki Nithi also worth checking out
    # Kisumu and Nandi are high for Education in 2016, and 2017 respectively
    # Uasin Gishu shoots up in 2016 for Health (7); Should the 19.3 really be 193? ~ Numbers match if yes.
    # Kisumu Agriculture shoots up in 2017 (9) to nearly 4 --> see table Table 3.51 in the 2017/18 report pp141/120
    # Embu in 2016 is odd at over 60; Table Table 3.18 was scraped incorrectly in 2016/17. Need to verify everythinng


  
  
 # Now create GeoCenter totals to compare with PDFs from the Controller's Office Official Reports
  budget_totals_GC <- 
    budget %>% 
    group_by(CID, budget_year, County, ASAL) %>% 
    summarise_at(vars(`ASBA Rec`:`Exp Dev`), funs(sum(., na.rm = TRUE))) %>% 
    mutate(Expend_excheq_rec_tot = bs_calc(`Exp Rec`, `Exq Rec`),
           Expend_excheq_dev_tot = bs_calc(`Exp Dev`, `Exq Dev`),
           Absorption_recur_tot = bs_calc(`Exp Rec`, `ASBA Rec`),
           Absorption_dev_tot = bs_calc(`Exp Dev`, ASBADev)) %>% 
    ungroup() %>% 
    group_by(CID) %>% 
           mutate(absorb_dev_lag = lag(Absorption_dev_tot, n = 1, order_by = budget_year), 
           absorb_dev_delta = Absorption_dev_tot - absorb_dev_lag) %>% 
  ungroup()

  budget_totals_GC %>% 
    mutate(county_sort = fct_reorder(County, `Exp Dev`, .desc = TRUE)) %>% 
    group_by(budget_year) %>% 
    mutate(exp_dev_ave = mean(`Exp Dev`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(.) +
    geom_line(aes(x = budget_year, y = exp_dev_ave), colour = "grey") +
    geom_line(aes(x = budget_year, y = `Exp Dev`)) + 
    geom_point(aes(x = budget_year, y = `Exp Dev`)) +
    facet_wrap(~county_sort) +
    theme_minimal()
  
  

# Read in County Totals from Eric K.’s Github Account ---------------------

County_budget_allocation <- "https://raw.githubusercontent.com/kabuchanga/KE-budgetsDB/master/data_final/KEN_county_budget_totals_15-18.csv"

county_BA <- read_csv(County_budget_allocation) %>% 
  mutate_at(vars(contains("Rate")), funs(. / 100)) %>% 
  left_join(asal, by = c("CID" = "CID")) %>% 
  mutate(year = case_when(
    FYear == "2015-2016" ~ 2015, 
    FYear == "2016-2017" ~ 2016, 
    FYear == "2017-2018" ~ 2017,
    TRUE ~ NA_real_
  )) %>% 
  arrange(CID, year) %>% 
  group_by(CID) %>% 
  mutate(prv_year_absorb = lag(`Overall Absorption Rate`, n = 1, order_by = year),
         prv_2year_absorb = lag(`Overall Absorption Rate`, n = 2, order_by = year), 
         absorb_delta = `Overall Absorption Rate` - prv_year_absorb,
         tot_absorb_delta = `Overall Absorption Rate` - prv_2year_absorb) %>% 
  left_join(., b_gc_pdf, by = c("CID" = "CID", "year" = "budget_year")) %>% 
  select(County, Dev_Expenditure, total_exp_dev, `Exp Dev`, diff, everything()) %>% 
  mutate(final_check = (Dev_Expenditure - total_exp_dev) %>% 
           round(., 2))  %>% 
  select(CID, County, year, final_check, everything()) %>% 
  arrange(final_check)

county_BA %>% 
  left_join(asal_geo, by = c("CID" = "CID")) %>% 
  ggplot(.) +
  geom_sf(aes(fill = `Overall Absorption Rate`), colour = "white", size = 0.5) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "A", direction = -1, label = percent_format(accuracy = 2)) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm")) + #adjust the width of the legend
  labs(caption = GC_caption,
       fill = "Overall absorption rate") +
  ggtitle("Garissa and Bomet had the highest average development absorption rates") +
  ggsave(file.path(imagepath, "KEN_develompent_absorption_rates_map.pdf"),
         height = 11.7, width = 16.5)
  

# Tabular summary of absorption rates by county
county_BA %>% 
  group_by(County) %>% 
  mutate(ave_abs_rate = mean(`Overall Absorption Rate`)) %>% 
  ungroup() %>% 
  select(County, year, ave_abs_rate, `Overall Absorption Rate`) %>% 
  spread(year, `Overall Absorption Rate`) %>% 
  arrange(-ave_abs_rate,) %>% 
  print(n = 47)


yabsorp_max = unlist(county_BA %>% 
                       ungroup() %>% 
                       summarise(max_dev = max(abs(absorb_delta), na.rm = TRUE)))

county_BA %>% 
  left_join(asal_geo, by = c("CID" = "CID")) %>% 
  ggplot(.) +
  geom_sf(aes(fill = absorb_delta), colour = "white", size = 0.05) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'PiYG'),
                       limits = c(-1 * absorp_max, absorp_max), 
                       labels = scales::percent) +
  facet_wrap(~ year)
  
# Show counties over time, sorted by 2018 levels
county_BA %>% 
  group_by(year) %>% 
  mutate(ave_absorp = mean(`Overall Absorption Rate`)) %>% 
  ungroup() %>% 
  ggplot() + # Move the 
  geom_line(aes(x = year, y = ave_absorp), colour = "gray", size = 1.25, alpha = 0.75) +
  geom_point(aes(x = year, y = `Overall Absorption Rate`)) +
  geom_line(aes(x = year, y = `Overall Absorption Rate`)) + 
  facet_wrap(~ County) +
  theme_minimal() +
  scale_x_continuous(breaks=seq(2015, 2017, 1))
  

  
  

# Variable creation for analysis and visualization ------------------------


# Exploratory plots -------------------------------------------------------


# Quick visualization of absorbtion rates across categories
budget_totals_GC %>% 
  left_join(asal_geo, by = c("CID" = "CID")) %>% 
  gather(key = "Absorption_type", value = "percent", Absorption_recur_tot:Absorption_dev_tot) %>% 
  ggplot() +
  geom_sf(aes(fill = percent), colour = "white", size = 0.5) +
  scale_fill_viridis_c(alpha = 0.66, direction = -1, option = "A") +
  facet_wrap(Absorption_type ~ budget_year)

# Checking out absorbtion rates by categories
budget %>% 
  left_join(asal_geo, by = c("CID" = "CID")) %>% 
  filter(budget_year == 2016) %>% 
  ggplot() +
  geom_sf(aes(fill = `Exp Dev`), colour = "white", size = 0.5) +
  scale_fill_viridis_c(alpha = 0.66, direction = -1, option = "A") +
  facet_wrap(~Budget_title)

# Export data for Plotting in Tableau
  budget <- 
    budget %>% 
    left_join(., pov_all, by = c("CID")) %>% 
    left_join(., asal, by = c("CID")) %>% 
    select(-County.y) %>% 
    rename(County = County.x)

  export_list <- list(KEN_budget_2015_2018 = budget,
                      KEN_budget_totals_2015_2018 = budget_totals_GC,
                      county_BA = county_BA, 
                      KEN_budget_raw_2015_2018 = budget_raw)

  export_list %>%  
    names() %>% 
    map(., ~ write_csv(export_list[[.]], file.path(budgetpath, str_c(., ".csv"))))  




