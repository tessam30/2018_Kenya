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


# Investigate data to see what we are working with ------------------------
# First, we'll clean up varaible names to have a dataset that makes sense
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

# Food Prices are the most prominent shock that is reported but not ranked
shk_df_cw %>% filter(is.na(s_rank)) %>% group_by(item_desc) %>% count() %>% arrange(desc(n)) %>% print(n = Inf)

shk_df_cw %>% group_by(shock_alt) %>% 
  mutate(total_shocks = n()) %>% 
  ungroup() %>% 
  group_by(shock_alt, s_rank, total_shocks) %>% 
  summarise(count = n()) %>% 
  spread(s_rank, count) %>% 
  arrange(desc(`<NA>`)) %>% 
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
  coord_flip()


# Question: How to group the shocks? For now, we will pluck out all shocks regardless of rank





# Provide a crosswalk of shocks with code to lump into categories we have established here:
# link to github categorization of shocks
shk_df %>% filter(q04 %in% c(1, 2)) %>% group_by(q02, q01) %>% tally() %>% arrange(-n) %>% print(n = Inf)

shk_df %>% group_by(q02, q01, q08_ye) %>% tally()










# Shock crosswalk mergeed in
# Located here https://github.com/tessam30/2018_Kenya/wiki/Shocks-&-Coping
shk_df_cw <- left_join(shk_df, shock_cw, by = c("q01" = "code")) 

shk_df_cw %>% group_by(shock, shock_alt) %>% tally() %>% arrange(shock) %>% print(n = Inf)

# Plot the results w/ a basic graph
shk_df_cw %>% 
  count(shock_alt,  sort = TRUE) %>% 
  mutate(shock_alt = fct_reorder(shock_alt, n)) %>% 
  ggplot(aes(shock_alt, n)) +
  geom_col()+
  coord_flip()



# Checking if any shocks are repeated within a HH -- only "Other shocks are"
shk_df_cw %>% filter(q02 != "OTHER 1") %>% 
  group_by(clid, hhid, q02) %>% tally() %>% arrange(-n) %>% print(n = 100)

# Create series of shocks types based on severity & costliness

shocks <-
  shk_df_cw %>%
  mutate(
    # Binaries flagging invalid values
    year_valid = ifelse(q08_ye <= 5, 1, 0),
    month_valid = ifelse(q08_mo <= 12, 1, 0),
    time_valid = ifelse(year_valid == 1 & month_valid == 1, 1, 0), 
    shock_time = ifelse(year_valid == 1 & month_valid == 1, ((q08_ye * 12) + q08_mo), NA),
    loss_valid = ifelse(q05 < 9999999999, 1, 0)
    loss = (),

    # Shocks by severity
    most = (q04 == 1)
  ) %>%
  group_by(clid, hhid) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(clid, hhid, shock) %>%
  mutate(shk_cat_total = n()) %>% 
  ungroup() %>% 
  group_by(clid, hhid, shock) %>% 
  mutate(count = ifelse(!is.na(shock), 1, 0),
         severe = (q04 == 1),
         cost = sum(loss_valid, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(clid, hhid)


# Plot alternative shocks with total economic loss associated with each one
shocks %>% 
  filter(q05 <9999999999) %>% 
  group_by(shock_alt) %>% 
  summarise(loss = sum(q05, na.rm = TRUE)/n(), 
            count = n()) %>% 
  mutate(shock_alt = fct_reorder(shock_alt, loss)) %>% 
  arrange(desc(loss, count)) %>% 
  ggplot(aes(shock_alt, loss)) +
  geom_col() +
  coord_flip() +
  geom_point(aes(shock_alt, count))







# How to reshape? First, try it with just the shocks -- 13,709 is the magic number I need
# The key is to use the distinct command to get duplicates removed
shocks %>% select(clid, hhid, shock, count) %>% 
  distinct %>% 
  spread(shock, count, fill = 0)




  
  # group by household level, summarizing info needed
  # BAsically need to count the max occurences of categories to get shock

tmp <- shocks %>%   
  
  # Need to filter on shock variables that result in valid shocks (last five years, )
  filter(year_valid == 1, month_valid == 1, )
  select(-matches('q0'), -shock_alt, -contains("valid") , -(shock_time:most), -severe) %>% 
  gather(variable, value, -(clid:shock)) %>% 
  distinct %>% 
  unite(shock_var, shock, variable) %>% 
    spread(shock_var, value)




         
         
shocks_dta <- 
  shk_df_cw %>%
  mutate(
    # Binaries flagging invalid values
    year_valid = ifelse(q08_ye <= 5, 1, 0),
    month_valid = ifelse(q08_mo <= 12, 1, 0),
    shock_time = ifelse(year_valid == 1 & month_valid == 1, ((q08_ye * 12) + q08_mo), NA),
    loss_valid = ifelse(q05 < 9999999999, q05, NA),
    most = ifelse(q04 == 1, 1, 0),
    
    # shocks
    ag =       ifelse(q01 %in% c(102, 103, 104), 1, 0),
    conflict = ifelse(q01 %in% c(119, 120, 123, 124), 1, 0),
    fin =      ifelse(q01 %in% c(105, 106, 107, 117), 1, 0),
    hazard =   ifelse(q01 %in% c(101, 111, 118), 1, 0),
    health =   ifelse(q01 %in% c(112, 113, 114, 115, 125), 1, 0),
    other =    ifelse(q01 %in% c(116, 121, 122, 126, 127), 1, 0),
    price =    ifelse(q01 %in% c(108, 109, 110), 1, 0),
    
    # loss value
    agloss = ifelse)


shocks_dta %>% select(clid, hhid, ag:price) %>% 
  group_by(clid, hhid) %>% 
  summarise_at(vars(ag:price), max, na.rm = TRUE)

    
    






