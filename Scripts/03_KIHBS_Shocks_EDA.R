## Purpose: Take processed shocks modules and create plots and datasets for Tableau
# Apply survey weights using the survey, srvyr, and svywrangler packages
# https://github.com/gergness/srvyr
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_12
# Audience: Kenya Mission

library(corrr)
library(robustbase)
library(srvyr)

# Using shocks dataframe created in the 02_KIHBS_Shocks.R file
glimpse(shocks)
shocks_geo <- shocks %>%
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values"))) %>% 
  left_join(county_labels)

shocks_geo_asal <- shocks %>%
  mutate(county_id = parse_integer(labelled::to_factor(county, levels = "values"))) %>% 
  left_join(county_labels) %>% 
  group_by(county_name) %>% 
  mutate(count = n()) %>% 
  ungroup %>% 
  left_join(asal, by = c("county_id" = "CID"))

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

# TODO - check how correlations vary at the County level
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
  nest()%>% 
  mutate(cor = map(data, ~correlate(.)))%>% 
  unnest(cor) %>% 
  rename(shock_categ1 = rowname) %>% 
  gather(., key = "shock_categ2", value = "correlation", ag_bin:water_short_bin) %>% 
  mutate_at(vars(shock_categ1, shock_categ2), ~gsub("_shock", "", .)) 
 
# Plot the results to see how intercorrelated the shocks are at the County level
  tmp %>% 
  ggplot(., aes(x = shock_categ1, y = shock_categ2, fill = correlation)) +
  geom_tile(color = "#ffffff") +
  scale_fill_viridis_c(direction = -1, option = "A") +
  theme(legend.position =  "none") +
  ggtitle("Shock correlations within counties")+
  ylab("") +
  xlab("") +
  facet_wrap(~county_name)
           

# Check just the core shocks now
  shocks_geo %>% 
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
sample_design <-  svydesign(id = ~clid, strata = ~strat,  weights = ~weight, data = shocks)
summary(sample_design)


# Can also use the srvyr package which takes pipes
shocks_svy <- shocks_geo_asal %>% 
  group_by(county_id) %>% 
  as_survey_design(id = clid, strata = strat,  weights = weight)


# Calculating statistics by county
shocks_county <- shocks_svy %>% 
  group_by(county_id, county_name, Category) %>% 
  summarise(ag = survey_mean(ag_shock), 
            conflict = survey_mean(conflict_shock),
            hazard = survey_mean(hazard_shock),
            financial = survey_mean(financial_shock),
            health = survey_mean(health_shock),
            price = survey_mean(price_shock),
            anyshock = survey_mean(anyshock))

shocks_asal <- shocks_svy %>% 
  group_by(Category) %>% 
  summarise(ag = survey_mean(ag_shock), 
            conflict = survey_mean(conflict_shock),
            hazard = survey_mean(hazard_shock),
            financial = survey_mean(financial_shock),
            health = survey_mean(health_shock),
            price = survey_mean(price_shock),
            anyshock = survey_mean(anyshock))

 counties_geo %>% 
   right_join(shocks_county, by = c("CID" = "county_id")) %>% 
   select(-contains("_se")) %>% 
   gather(., key = shock, value = value, ag:anyshock) %>% 
   ggplot() +
   geom_sf(aes(fill = value), colour = "white", size = 0.5) +
   scale_fill_viridis_c(option = "A", direction = -1) +
   facet_wrap(~shock, nrow = 2) 
   

 
 







svywrangler::calcPtEst(shocks, var = 'anyshock', 
                       by_var = 'county', use_weights = TRUE, 
                       psu_var = 'clid', strata_var = 'strat', 
                       weight_var = 'weight') 
