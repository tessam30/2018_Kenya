# Purpose: Load and process Kenya budget data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_26
# Audience: Kenya Mission


# load data and crosswalks ---------------------------------------------------------------
source(file.path(rpath, "budget_cw.R"))

excel_sheets(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"))
budget_2015_in <- read_excel(file.path(budgetpath, 
                                       "County Budget Database_TE_Edits.xlsx"), 
                             sheet = "Budget Nos 15-16")
budget_2016_in <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"), 
                              sheet = "Budget Nos 16-17")

budget_2017_in <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"),
                             sheet = "Budget Nos 17-18") 

budget_list <- list(budget_2015_in, budget_2016_in, budget_2017_in)
map(budget_list, ~str(.))

# Need to fix a few columns that are read-in as characters rather than numbers
budget_2015 <- budget_2015_in %>% 
  mutate_at(vars(`Exq Dev`, `Exp Rec`, `Exp Dev`), as.numeric) %>% 
  mutate(budget_year = 2015)

budget_2016 <- budget_2016_in %>% 
  mutate_at(vars(`Exp_Exq Rec`), as.numeric) %>% 
  mutate(budget_year = 2016)

budget_2017 <- budget_2017_in %>% 
  mutate(budget_year = 2017)

# Use $ at the end to ensure that we only captures objects that end in "_in"
rm(list = ls(pattern = "*_in$"))

# Create budget database with proper roll-ups and budget shares -----------

budget_raw <- 
  bind_rows(budget_2015, budget_2016, budget_2017) %>%
  
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

str(budget_raw)
Hmisc::describe(budget_raw)

# Subset the raw totals
  budget_totals_pdf <-  
    budget_raw %>% 
    filter(`Category Code` == 13) %>% 
    mutate(budget_type = "Total") %>% 
    mutate(Expend_excheq_rec = bs_calc(`Exp Rec`, `Exq Rec`),
           Expend_excheq_dev = bs_calc(`Exp Dev`, `Exq Dev`),
           Absorbtion_recur = bs_calc(`Exp Rec`, `ASBA Rec`),
           Absorbtion_dev = bs_calc(`Exp Dev`, ASBADev))

  # Collapsing everything down to standardized budget categories to create budget shares
  # Based on yearly totals
  # Need to recreate the following variables that are based on two variables
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
    ungroup() 

  # Check how many categories are "filled" for each code
  budget %>% group_by(`Category Code`, budget_year) %>% count() %>% spread(budget_year, n)
  
# function to create plot based on a category

    budget %>% 
      filter(`Category Code` == 12) %>%
      select(budget_year,  Absorption_dev, County, `Category Code`, Budget_title) %>% 
      ggplot(aes(x = budget_year, y = Absorption_dev)) +
      geom_line(colour = "grey") +
      geom_point(aes(colour = Absorption_dev)) +
      scale_colour_viridis_c(direction = -1) +
      facet_wrap(~ County) +
      theme_minimal() 
      
# Budget notes to be resolved
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

  

# Read in County Totals from Eric K.’s Github Account ---------------------

County_budget_allocation <- "https://raw.githubusercontent.com/kabuchanga/KE-budgetsDB/master/data_final/KEN_county_budget_totals_15-18.csv"

county_BA <- read_csv(County_budget_allocation) %>% 
  mutate_at(vars(contains("Rate")), funs(. / 100)) %>% 
  left_join(asal, by = c("CID" = "CID")) %>% 
  mutate(year = case_when(
    FYear == "2015-2016" ~ 2015, 
    FYear == "2016-2017" ~ 2016, 
    FYear == "2017-2018" ~ 2017,
  )) %>% 
  arrange(CID, year) %>% 
  group_by(CID) %>% 
  mutate(prv_year_absorb = lag(`Overall Absorption Rate`, n = 1, order_by = year),
         prv_2year_absorb = lag(`Overall Absorption Rate`, n = 2, order_by = year), 
         absorb_delta = `Overall Absorption Rate` - prv_year_absorb,
         tot_absorb_delta = `Overall Absorption Rate` - prv_2year_absorb)

county_BA %>% 
  left_join(asal_geo, by = c("CID" = "CID")) %>% 
  ggplot(.) +
  geom_sf(aes(fill = `Overall Absorption Rate`), colour = "white", size = 0.5) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "A", direction = -1)

absorp_max = unlist(county_BA %>% ungroup() %>% summarise(max_dev = max(abs(absorb_delta), na.rm = TRUE)))

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

  export_list <- list(KEN_budget_2015_16 = budget,
                      KEN_budget_totals_2015_16 = budget_totals_GC,
                      county_BA = county_BA, 
                      KEN_budget_raw_2015_16 = budget_raw)

  export_list %>%  
    names() %>% 
    map(., ~ write_csv(export_list[[.]], file.path(budgetpath, str_c(., ".csv"))))  




