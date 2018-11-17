# Purpose: Start processing Kenya Integrated Household Budget Survey
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_19
# Audience: Kenya Mission

# Read in the shock data 

# Load shock data ---------------------------------------------------------

shk_df <- read_dta(file.path(kihbspath, "Recent_Shocks.dta"))
hh_inf <- read_dta(file.path(kihbspath, "HH_Information.dta"))
source(file.path(rpath, "shocks_cw.R"))

theme_set(theme_light())


# --Investigate data to see what we are working with ------------------------
# -- First, we'll clean up varaible names to have a dataset that makes sense
shk_df <- 
    shk_df %>% 
    select(clid, 
           hhid,
           item_code = q01, 
           item_desc = q02,
           s_severity  = q03,
           s_rank      = q04,
           s_loss      = q05,
           asset_effect = q06,
           specific_effect = q07,
           time_year = q08_ye, 
           time_mo   = q08_mo,
           cope1     = q09_1,
           cope2     = q09_2, 
           cope3     = q09_3) %>% 
    arrange(clid, hhid)
  str(shk_df)

# Shock crosswalk mergeed in
# Located here https://github.com/tessam30/2018_Kenya/wiki/Shocks-&-Coping
  shk_df_cw <- left_join(shk_df, shock_cw, by = c("item_code" = "code")) 
  names(shk_df_cw)  

# First, let's check if we are dealing with whole sample or just a subset
  shk_df_cw %>% group_by(s_severity) %>% count() 

# Appears to be only households reporting shocks. So when we merge with the full sample,
# we will have to fill in all NA values with 0s to get our statistics right

  shk_df_cw %>% group_by(s_rank) %>% count() # 8133 shocks are not ranked, but reported.


# Plots of shocks prior to processing ---------------------------------------------------------
# -- Food Prices are the most prominent shock that is reported but not ranked
  shk_df_cw %>% filter(is.na(s_rank)) %>% 
    group_by(item_desc) %>% 
    count() %>% 
    rename(unranked_shocks = n) %>% 
    arrange(desc(unranked_shocks)) %>% 
    print(n = Inf)

  shk_df_cw %>% group_by(shock_alt, shock) %>% 
    mutate(total_shocks = n()) %>% 
    ungroup() %>% 
    group_by(shock_alt, s_rank, total_shocks, shock) %>% 
    summarise(count = n()) %>% 
    spread(s_rank, count) %>% 
    arrange(desc(total_shocks)) %>%
    select(shock, shock_alt, everything()) %>% 
    knitr::kable()

  shk_df_cw %>% group_by(shock, s_rank) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(shock, count)) + 
    facet_wrap(~s_rank) +
    geom_col() +
    coord_flip() 

  shk_df_cw %>% group_by(shock) %>% 
    summarise(count = n()) %>% 
    mutate(shock = fct_reorder(shock, count)) %>% 
    ggplot(aes(shock, count)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = count, hjust = -0.01)) 

# Plot the results w/ a basic graph
  shk_df_cw %>% 
    count(shock_alt,  sort = TRUE) %>% 
    mutate(shock_alt = fct_reorder(shock_alt, n)) %>% 
    ggplot(aes(shock_alt, n)) +
    geom_col()+
    coord_flip() +
    geom_text(aes(label = n, hjust = -0.1)) 

# Coping mechanisms employedd
  shk_df_cw %>% group_by(cope1) %>% 
    summarise(count = n()) %>% 
    mutate(cope1 = fct_reorder(as_factor(cope1), count)) %>% 
    ggplot(aes(cope1, count)) +
    geom_col() +
    coord_flip()



# Cost and logical processing ---------------------------------------------
# Variables needed:
#   1. losses reported are in range
#   2. total shocks reported by households
#   3. total shocks reportes by shocks_alt
#   4. Coping strategies employed (good, bad, neutral)

# -- Question: How to group the shocks? For now, we will pluck out all shocks regardless of rank

# -- Question: How to calculate the total cost associated with each shock?
# For now, adding across all shock shock_alt categories to get totals. 

  shocks_alt <- 
    shk_df_cw %>%
    
    # Flag out of range observations for calculating totals
    mutate(valid_loss = ifelse(s_loss < 9999999999, 1, 0)) %>% 
  
    # total shocks across all categories to see depth of vulnerability
    group_by(clid, hhid) %>% 
    mutate(total = n()) %>% 
    ungroup() %>% 
    
    # Total shocks within alternate classification to see breadth of vulnerability
    group_by(clid, hhid, shock_alt) %>% 
    mutate(total_alt = n(),
           cost_alt = ifelse(valid_loss == 1, sum(s_loss, na.rm = TRUE), 0)) 
  
  
# --Plot alternative shocks with total economic loss associated with each one
# --Key takeaway - Other, Violence, and Financial and Livestock shocks are the most expensive for a household. 
  shocks_alt %>% 
    filter(valid_loss == 1) %>% 
    group_by(shock_alt) %>% 
    summarise(loss = sum((s_loss * 0.0098), na.rm = TRUE)/n(), 
              count = n()) %>% 
    mutate(shock_alt = fct_reorder(shock_alt, loss)) %>% 
    arrange(desc(loss)) %>% 
    ggplot(aes(shock_alt, loss)) +
    geom_col() +
    coord_flip() +
    geom_point(aes(shock_alt, count)) +
    geom_text(aes(label = round(loss, 0), hjust = -0.5)) +
    ggtitle(label = "Average loss (in $USD) of different shocks + frequency of shock")

  

# Reshape from long to wide for merging with hh_base ----------------------

# --Need to filter on shock variables that result in valid shocks (last five years, )

 shocks <-  shocks_alt %>% 
   
   # Collapse down by shock_alt to get rid of households with multiple shocks
   # of the same type and that have NA values for the shocks costs
   group_by(clid, hhid, shock_alt, shock) %>% 
  
  # In the collapse, you are essentially taking the mean of the group b/c it should be constant
   summarise(total = mean(total, na.rm = TRUE), 
             total_alt = mean(total_alt, na.rm = TRUE),
             cost_alt = mean(cost_alt, na.rm = TRUE)) %>% 
   ungroup %>% 
    
  # Isolate only the variables we need as our shock module data
  select(clid, hhid, shock_alt, cost_alt, total_alt) %>% 
  
  # Gather everything into a single variable/value pair so we can then respread it
  gather(category, value, -(clid:shock_alt)) %>% 
  distinct %>% 
  
  # Combines shock_alt + cateogry into a new variable called shock_var which is then reshaped wide
  unite(shock_var, shock_alt, category) %>% 
  spread(shock_var, value, fill = 0) %>% 
  
  # Cleaning up variable names
  rename_at(vars(contains("_total_alt")), ~ gsub("_total_alt", "", .)) %>% 
  rename_at(vars(contains("_alt")), ~ gsub("_alt", "", .))%>% 
  
  # Ordering data for ease of reading
  select(clid, hhid, ag, conflict, crop_price, demographic, hazard, health, financial, food_price, 
         livestock, input_price, other, violence, water_short, everything()) %>% 
  arrange(clid, hhid) %>% 
  ungroup() %>% 
  
  # Total costs in case they are needed
  mutate(total_cost = rowSums(select(., ag_cost:water_short_cost)),
         total_shocks = rowSums(select(., ag:water_short))) %>% 
  
  # Creating binary shock variables for the various categories b/c current shock captures occurences (>1 in some cases)
  mutate_at(vars(ag:water_short), .funs = funs(bin = ifelse(. > 0, 1, 0))) %>% 
  right_join(hh_base, by = c("clid" = "clid", "hhid" = "hhid")) %>% 
  
  mutate_at(vars(ag:water_short_bin), funs(replace(., is.na(.), 0))) %>% 
  
  #mutate_at(vars(county), .funs = funs(county_lab = as.character(as_factor(.))))
  # create major shock categories to ensure consistency with other studies
  mutate(ag_shock = ifelse(ag_bin == 1 | livestock_bin == 1, 1, 0),
         conflict_shock = ifelse(conflict_bin == 1, 1, 0), 
         financial_shock = ifelse(financial_bin ==1, 1, 0), 
         hazard_shock = ifelse(hazard_bin == 1 | water_short_bin, 1, 0), 
         health_shock = ifelse(demographic_bin ==1 | health_bin == 1, 1, 0), 
         price_shock = ifelse(crop_price_bin ==1 | food_price_bin ==1 | input_price_bin == 1, 1, 0),
         other_shock = ifelse(other_bin == 1, 1, 0)) %>% 
  
  mutate(anyshock = ifelse(rowSums(select(., ag_shock:other_shock)) > 0, 1, 0),
         national = "Kenya")



# Filter by only the 1st ranked shocks  -----------------------------------
# -- Repeating the above flow, but filtering on only most severe shocks to test if numbers align to the KIHBS report.

shocks_severe <-  shocks_alt %>% 
  filter(s_rank == 1) %>% 
  
  # Collapse down by shock_alt to get rid of households with multiple shocks
  # of the same type and that have NA values for the shocks costs
  group_by(clid, hhid, shock_alt, shock) %>% 
  
  # In the collapse, you are essentially taking the mean of the group b/c it should be constant
  summarise(total = mean(total, na.rm = TRUE), 
            total_alt = mean(total_alt, na.rm = TRUE),
            cost_alt = mean(cost_alt, na.rm = TRUE)) %>% 
  ungroup %>% 
  # Isolate only the variables we need as our shock module data
  select(clid, hhid, shock_alt, cost_alt, total_alt) %>% 
  
  # Gather everything into a single variable/value pair so we can then respread it
  gather(category, value, -(clid:shock_alt)) %>% 
  distinct %>% 
  
  # Combines shock_alt + cateogry into a new variable called shock_var which is then reshaped wide
  unite(shock_var, shock_alt, category) %>% 
  spread(shock_var, value, fill = 0) %>% 
  
  # Cleaning up variable names
  rename_at(vars(contains("_total_alt")), ~ gsub("_total_alt", "", .)) %>% 
  rename_at(vars(contains("_alt")), ~ gsub("_alt", "", .))%>% 
  
  # Ordering data for ease of reading
  select(clid, hhid, ag, conflict, crop_price, demographic, hazard, health, financial, food_price, 
         livestock, input_price, other, violence, water_short, everything()) %>% 
  arrange(clid, hhid) %>% 
  ungroup() %>% 
  
  # Create a total_cost and total_shocks variable
  mutate(total_cost = rowSums(select(., ag_cost:water_short_cost)),
         total_shocks = rowSums(select(., ag:water_short))) %>% 
  
  # Creating binary shock variables for the various categories
  mutate_at(vars(ag:water_short), .funs = funs(bin = ifelse(. > 0, 1, 0))) %>% 
  right_join(hh_base, by = c("clid" = "clid", "hhid" = "hhid")) %>% 
  mutate_at(vars(ag:water_short_bin), funs(replace(., is.na(.), 0))) %>% 
  
  #mutate_at(vars(county), .funs = funs(county_lab = as.character(as_factor(.))))
  # create major shock categories
  mutate(ag_shock = ifelse(ag_bin == 1 | livestock_bin == 1, 1, 0),
         conflict_shock = ifelse(conflict_bin == 1, 1, 0), 
         financial_shock = ifelse(financial_bin ==1, 1, 0), 
         hazard_shock = ifelse(hazard_bin == 1 | water_short_bin, 1, 0), 
         health_shock = ifelse(demographic_bin ==1 | health_bin == 1, 1, 0), 
         price_shock = ifelse(crop_price_bin ==1 | food_price_bin ==1 | input_price_bin == 1, 1, 0),
         other_shock = ifelse(other_bin == 1, 1, 0)) %>% 
  
  mutate(anyshock = ifelse(rowSums(select(., ag_shock:other_shock)) > 0, 1, 0),
         national = "Kenya")

table(shocks_severe$ag_bin)
table(shocks$ag_bin)



# Plotting results  -------------------------------------------------------
# -- Now we can calculate statistics based on county or shock type
shocks %>% 
  select(county, county_id, ag_bin:water_short_bin)%>%
  gather(shock, value, -(county:county_id)) %>% 
  group_by(shock, county, county_id) %>% 
  summarise(ave = mean(value)) %>% 
  left_join(county_labels, by = c("county_id" = "county_id")) %>%
  mutate(county_name = fct_reorder(county_name, ave, .desc = TRUE)) %>% 
  filter(shock != "health_bin") %>% 
  ggplot(aes(shock, ave, fill = shock)) +
  geom_col() +
  coord_flip() +
  #geom_text(aes(label = round(ave, 2), hjust = -0.1)) +
  facet_wrap(~county_name) +
  scale_fill_brewer(palette = "Set3") +
  ggtitle(label = "Kitui appears to be the most vulnerable county (unweighted shocks)")

shocks %>% 
  select(county, county_id, ag_shock:anyshock) %>% 
  gather(shock, value, -(county:county_id)) %>%
  group_by(shock, county, county_id) %>% 
  summarise(ave = mean(value, na.rm = TRUE)) %>% 
  left_join(county_labels) %>%
  mutate(county_name = fct_reorder(county_name, ave, .desc = TRUE)) %>% 
  ggplot(aes(shock, ave, fill = shock)) +
  geom_col() +
  coord_flip() +
  #geom_text(aes(label = round(ave, 2), hjust = -0.1)) +
  facet_wrap(~county_name) +
  scale_fill_brewer(palette = "Set3")
  
# Clean up all the extra datasets generated
remove(shk_df, shk_df_cw, shocks_alt)
