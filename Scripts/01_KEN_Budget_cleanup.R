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
budget_2016_in  <- read_excel(file.path(budgetpath, "County Budget Database_TE_Edits.xlsx"), 
                              sheet = "Budget Nos 16-17")

map(list(budget_2015_in, budget_2016_in), ~str(.))

# Need to fix a few columns that are read-in as characters rather than numbers
budget_2015 <- budget_2015_in %>% 
  mutate_at(vars(`Exq Dev`, `Exp Rec`, `Exp Dev`), as.numeric) %>% 
  mutate(budget_year = 2015)

budget_2016 <- budget_2016_in %>% 
  mutate_at(vars(`Exp_Exq Rec`), as.numeric) %>% 
  mutate(budget_year = 2016)


# Create budget database with proper roll-ups and budget shares -----------

budget_raw <- 
  bind_rows(budget_2015, budget_2016) %>%
  
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
             `Exp Dev` > `Exp Dev_lag` & budget_year == 2016 ~ "higher in 2016",
             `Exp Dev` <= `Exp Dev_lag` & budget_year == 2016 ~ "lower or same in 2015",
             TRUE ~ NA_character_
           )) %>% 
    ungroup() 


 # Now create GeoCenter totals to compare with PDFs
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
  # Data are repeated for each year, waiting on Eric to fix
  #
  
County_budget_allocation <- "https://raw.githubusercontent.com/kabuchanga/KE-budgetsDB/master/data_final/County%20Budget%20Allocation%2C%20Expenditure%20and%20Absorption%20Rate.csv"

county_BA <- read_csv(County_budget_allocation) %>% 
  mutate_at(vars(contains("Rate")), funs(. / 100)) %>% 
  left_join(asal, by = c("CID" = "CID")) %>% 
  mutate(year = ifelse(FYear == "2015-2016", 2015, 2016)) %>% 
  arrange(CID, year) %>% 
  group_by(CID) %>% 
  mutate(prv_year_absorb = lag(`Overall Absorption Rate`, n = 1, order_by = year), 
         absorb_delta = `Overall Absorption Rate` - prv_year_absorb)

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
                       labels = scales::percent) 
  
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

# Checking out absorbtion rates by categoryies
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




