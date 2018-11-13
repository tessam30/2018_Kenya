## Purpose: Take processed shocks modules and create plots and datasets for Tableau
# Apply survey weights using the survey, srvyr, and svywrangler packages
# https://github.com/gergness/srvyr
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_12
# Audience: Kenya Mission

# Using shocks dataframe created in the 02_KIHBS_Shocks.R file

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