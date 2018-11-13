## Purpose: Take processed shocks modules and create plots and datasets for Tableau
# Apply survey weights using the survey, srvyr, and svywrangler packages
# https://github.com/gergness/srvyr
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_12
# Audience: Kenya Mission

# Using shocks dataframe created in the 02_KIHBS_Shocks.R file
glimpse(shocks)


# Check how correlated the different types of shocks are within households 
shocks %>% select(contains("_bin")) %>% cor() %>% reshape2::melt() %>% 
  #filter(Var1 != "total_shocks", Var2 != "total_shocks") %>% 
  mutate_at(vars(Var1, Var2), ~gsub("_bin", "", .)) %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "#ffffff") +
  scale_fill_viridis_c(direction = -1, option = "A") +
  geom_text(aes(label = round(value, 2), size = 0.5), colour = "#ffffff") +
  theme(legend.position =  "none") +
  ggtitle("Input prices are moderately correlated with crop and food price shocks")

shocks %>% select(contains("_shock")) %>% cor() %>% symnum()

# Next steps, figure ou the sampling weights and the appropriate use
# svyset clid [pw = weight], strata(strat) singleunit(centered)
sample_design <-  svydesign(id = ~clid, strata = ~strat,  weights = ~weight, data = shocks)
summary(sample_design)


# Can also use the srvyr package which takes pipes
shocks_svy <- shocks %>% 
  as_survey_design(id = clid, strata = strat,  weights = weight)

shocks_svy %>% 
  group_by(county) %>% 
  summarise(ag = survey_mean(ag_shock), 
            conflict = survey_mean(conflict_shock),
            hazard = survey_mean(hazard_shock),
            financial = survey_mean(financial_shock),
            health = survey_mean(health_shock),
            price = survey_mean(price_shock)
            ) 





svywrangler::calcPtEst(shocks, var = 'anyshock', 
                       by_var = 'county', use_weights = TRUE, 
                       psu_var = 'clid', strata_var = 'strat', 
                       weight_var = 'weight') 
