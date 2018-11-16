## Purpose: Take processed shocks modules and create plots and datasets for Tableau
# Apply survey weights using the survey, srvyr, and svywrangler packages
# https://github.com/gergness/srvyr
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_12
# Audience: Kenya Mission

library(corrr)
library(robustbase)
library(srvyr)



# Define shock datasets to work with --------------------------------------

# Using shocks dataframe created in the 02_KIHBS_Shocks.R file
glimpse(shocks)
shocks_geo <- shocks %>% 
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values"))) %>% 
  left_join(county_labels)

# Create a parallel data set
shocks_geo_severe <- shocks_severe %>% 
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values"))) %>% 
  left_join(county_labels)

shocks_geo_asal <- shocks %>%
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values"))) %>% 
  left_join(county_labels) %>% 
  group_by(county_name) %>% 
  mutate(count = n()) %>% 
  ungroup %>% 
  left_join(asal, by = c("county_id" = "CID"))



# How correlated are the different types of shocks ------------------------

# Check how correlated the different types of shocks are within households 
shocks %>% select(contains("_bin")) %>% cor() %>% reshape2::melt() %>% 
  #filter(Var1 != "total_shocks", Var2 != "total_shocks") %>% 
  mutate_at(vars(Var1, Var2), ~gsub("_bin", "", .)) %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "#ffffff") +
  scale_fill_viridis_c(direction = -1, option = "A") +
  geom_text(aes(label = round(value, 2), size = 0.5), colour = "#ffffff") +
  theme(legend.position =  "none") +
  ggtitle("Input prices are moderately correlated with crop and food price shocks")+
  ylab("") +
  xlab("")

# Check how correlations vary at the County level
# First check how many counties have 0 reported shocks for the sub-categories
# NOTES: There is a tradeoff between the nest() and split approach
# nest() worked better here b/c the correlate function works well with a dataframe, rather than a list

shocks_geo %>% 
  group_by(county_name) %>% 
  select(contains("_shock")) %>% 
  summarise_if(is.numeric, mean) %>% 
  print(n = 47)

corr_geo <- shocks_geo %>% 
  select(contains("_bin"), county_name) %>% 
  group_by(county_name) %>% 
  nest() %>% 
  mutate(cor = map(data, ~correlate(.))) %>% 
  unnest(cor) %>% 
  rename(shock_categ1 = rowname) %>% 
  gather(., key = "shock_categ2", value = "correlation", ag_bin:water_short_bin) %>% 
  mutate_at(vars(shock_categ1, shock_categ2), ~gsub("_shock", "", .)) 
 
# Plot the results to see how intercorrelated the shocks are at the County level
corr_geo %>% 
  ggplot(., aes(x = shock_categ1, y = shock_categ2, fill = correlation)) +
  geom_tile(color = "#ffffff") +
  scale_fill_viridis_c(direction = -1, option = "A") +
  theme(legend.position =  "none") +
  ggtitle("Shock correlations within counties")+
  ylab("") +
  xlab("") +
  facet_wrap(~county_name)
           

# Check just the core shocks now
# Can toggle between data frames
# shocks_geo and shocks_geo_severe
  shocks_geo_severe %>% 
    select(ag_shock:other_shock, county_name) %>% 
    group_by(county_name) %>% 
    nest() %>% 
    mutate(cor = map(data, ~correlate(.))) %>% 
    unnest(cor) %>% 
    rename(shock1 = rowname) %>% 
    gather(., key = "shock2", value = "correlation", ag_shock:other_shock) %>% 
    mutate_at(vars(shock1, shock2), ~gsub("_shock", "", .)) %>% 
    ggplot(., aes(x = shock1, y = shock2, fill = correlation)) +
    geom_tile(color = "#ffffff") +
    scale_fill_viridis_c(direction = -1, option = "A") +
    theme(legend.position =  "top") +
    ggtitle("Shock correlations within counties")+
    ylab("") +
    xlab("") +
    facet_wrap(~county_name)

  
# Set Sampling Weights ----------------------------------------------------
  
# --- USing the srvyr library which is a largely a wrapper for survey
# Next steps, figure ou the sampling weights and the appropriate use
# svyset clid [pw = weight], strata(strat) singleunit(centered)

# Check the sample design  
sample_design <-  svydesign(id = ~clid, strata = ~strat,  weights = ~weight, data = shocks)
summary(sample_design)

# Can also use the srvyr package which takes pipes
# Create two data frames
# 1) takes the complete range of shocks
# 2) takes only the most sever shock -- what is reported in KIHBS

shocks_geo_asal_svy <- shocks_geo_asal %>% 
  group_by(county_id) %>% 
  as_survey_design(id = clid, strata = strat,  weights = weight) 

shocks_geo_severe_svy <- shocks_geo_severe %>% 
  as_survey_design(id = clid, strata = strat, weights = weight)

# Doesn't make sense to write a function b/c of summarise_all 
# Calculate overall summary statistics for both datasets
shocks_stats_natl <- 
  shocks_geo_asal_svy %>% 
  select(contains("_shock"), - total_shocks, anyshock) %>% 
  summarise_all(survey_mean) %>% 
  select(-contains("_se")) %>% 
  gather(., key = shock, value = value) %>% 
  mutate(shock_natl = as_factor(gsub("_shock", "", shock))) %>% 
  arrange(desc(value)) %>% 
  select(-shock) 
  

shock_stats_county <- 
  shocks_geo_asal_svy %>% 
  select(contains("_shock"), - total_shocks, anyshock, county_name, county_id) %>% 
  group_by(county_name, county_id) %>% 
  summarise_all(survey_mean)%>% 
  select(-contains("_se"))%>% 
  gather(., key = shock, value = value, ag_shock:anyshock)%>% 
  mutate(shock = as_factor(gsub("_shock", "", shock))) %>% 
  left_join(shocks_stats_natl, by = c("shock" = "shock_natl")) %>%
  rename(value_county = value.x, value_natl = value.y) %>% 
  mutate(shock_dev = value_county - value_natl) %>% 
  group_by(county_name) %>% 
  mutate(maxsort = max(value_county)) %>% 
  ungroup()

# Plot the deviations and levels ------------------------------------------
# Set the theme elements for all graphs
p_stuff <- theme(legend.position = "top") +
  labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") +
  fill = guide_colourbar(tital = "Percent of households with sohck", 
                         barwidth = 0.5, barheight = 10)



# Map the deviations
counties_geo %>% 
  right_join(shock_stats_county, by = c("CID" = "county_id")) %>% 
  mutate(max = abs(max(shock_dev)),
         shock = fct_reorder(shock, value_natl, .desc = TRUE)) %>% 
  ggplot() +
  geom_sf(aes(fill = shock_dev), colour = "white", size = 0.5) +
  scale_fill_distiller(type = "div", palette = "PiYG",
                       limits = c(-0.65, 0.65)) +
  facet_wrap(~shock, nrow = 2) +
  ggtitle("Deviations from national average, by shock") +
  theme(legend.position = "top") +
  labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") 


# Show just the composition of shocks and how they add up
counties_geo %>% 
  right_join(shock_stats_county, by = c("CID" = "county_id")) %>% 
  mutate(max = abs(max(shock_dev)),
         shock = fct_reorder(shock, value_natl, .desc = TRUE), 
         county_name = fct_reorder(county_name, maxsort)) %>% 
  filter(shock != "anyshock") %>% 
  ggplot(aes(x = county_name, y = shock_dev, fill = shock)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "D", direction = -1) 
  

# Now show shocks broken out, sorted by deviations for anyshock
counties_geo %>% 
  right_join(shock_stats_county, by = c("CID" = "county_id")) %>% 
  mutate(max = abs(max(shock_dev)),
         shock = fct_reorder(shock, value_natl, .desc = TRUE), 
         county_name = fct_reorder(county_name, maxsort)) %>% 
  ggplot(aes(x = county_name, y = shock_dev, fill = shock_dev, label)) +
  geom_col() +
  coord_flip()  +
  scale_fill_distiller(type = "div", palette = "PiYG",
                     limits = c(-0.65, 0.65)) +
  facet_wrap(~shock, nrow = 2) +
  theme(legend.position = "top") +
  ggtitle("Deviations from national average, by shock") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") +
  labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") +
  xlab("") + ylab("")

# Now the levels
counties_geo %>% 
  right_join(shock_stats_county, by = c("CID" = "county_id")) %>% 
  mutate(shock = fct_reorder(shock, value_natl, .desc = TRUE)) %>% 
  ggplot() +
  geom_sf(aes(fill = value_county), colour = "white", size = 0.5) +
  scale_fill_viridis_c(alpha = 0.66, direction = -1, option = "A") +
  facet_wrap(~shock, nrow = 2) +
  ggtitle("Shock averages by county") +
  theme(legend.position = "top") +
  labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") +
  guides(fill = guide_colorbar(title = "Percent of houseohlds with shock"))

counties_geo %>% 
  right_join(shock_stats_county, by = c("CID" = "county_id")) %>% 
  mutate(max = abs(max(shock_dev)),
         shock = fct_reorder(shock, value_natl, .desc = TRUE), 
         county_name = fct_reorder(county_name, maxsort)) %>% 
  ggplot(aes(x = county_name, y = value_county, fill = value_county)) +
  geom_col() +
  geom_hline(aes(yintercept = value_natl), colour = "#D3D3D3") +
  coord_flip() +
  scale_fill_viridis_c(alpha = 0.66, direction = -1, option = "A") +
  facet_wrap(~shock, nrow = 2) +
  theme(legend.position = "none") +
  ggtitle("Percent of households with shock by county (gray line equal to national average)") +
  scale_y_continuous(labels = scales::percent) +
  xlab("") + ylab("") +
  labs(caption = "Source: Kenya Integrated Household Budget Survey 2016")














 
 

# svywrangler::calcPtEst(shocks, var = 'anyshock', 
#                        by_var = 'county', use_weights = TRUE, 
#                        psu_var = 'clid', strata_var = 'strat', 
#                        weight_var = 'weight') 
