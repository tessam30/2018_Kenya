# Purpose: Process household infrastructure for analysis & WASH Pad
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_16
# Audience: Kenya Mission & E3 in DC

# Read in the shock data 

# Load household characteristics data -------------------------------------
# Data already loaded in the 00 setup file
# Source crosswalks developed from questionnaire
source(file.path(rpath, "impr_water_cw.R"))
source(file.path(rpath, "impr_sanit_cw.R"))

# Store variable information in a new dataframe for viewing
hh_inf_meta <- add_metadata(hh_inf)

# First calculate improved water
hh_inf %>% select(j01_dr) %>% group_by(j01_dr) %>% tally()
get_labels(hh_inf$j01_dr)

# Improved codes for sanitation and water
# -- For water, there may be a 30 min threshold needed as well
  impr_water_codes <- unlist(impr_water_cw %>% filter(improved == 1) %>% select(code))
  unimpr_water_codes <- unlist(impr_water_cw %>% filter(improved == 0) %>% select(code))
  inpr_water_bottle <- unlist(impr_water_cw %>% filter(improved == 1 | code == 14) %>% select(code))
  uninpr_water_bottle <- unlist(impr_water_cw %>% filter(improved == 0 & code != 14) %>% select(code))

# -- For sanitation, the conditions must be improved and not be shared
  impr_toilet_codes = unlist(impr_sanit_cw %>% filter(improved == 'improved') %>% select(code))
  unimpr_toilet_codes = unlist(impr_sanit_cw %>% filter(improved == 'unimproved') %>% select(code))
  od_codes =  unlist(impr_sanit_cw %>% filter(improved == 'open defecation') %>% select(code))

# Select and mutate key variables for the WASH Analysis requested
hh_wash <- hh_inf %>% 
  select(clid,
         hhid,
         water_drink = j01_dr,
         water_other = j01_do, 
         water_time = j02,
         sanit_type = j10,
         sanit_share = j11,
         sanit_share_num = j12,
         hand_wash = j13,
         waste_collect = j14) %>% 
  
# -- straight classification of whether the source is improved --
# -- KIHBS classifies improved water as piped water, borehole with pump, protected spring, protected well, rain water and bottled water.
  mutate(improved_water_drink = case_when(
    water_drink %in% impr_water_codes ~ 1,
    water_drink %in% unimpr_water_codes ~ 0, 
    TRUE ~ NA_real_),
    
    improved_water_drink_bot = case_when(
      water_drink %in% inpr_water_bottle ~ 1,
      water_drink %in% uninpr_water_bottle ~ 0, 
      TRUE ~ NA_real_),
    
    improved_water_other = case_when(
      water_other %in% impr_water_codes ~ 1,
      water_other %in% unimpr_water_codes ~0,
      TRUE ~ NA_real_),
    
    # -- improved source + <= 30 min. to acquire --
    # water_time = 0== "on premises"; assumed to be < 30 min.
    improved_water_under30min = ifelse(is.na(water_time) | is.na(improved_water_drink), NA,
                                   ifelse(water_time <= 30  & improved_water_drink == 1, 1, 0)), 
  
# -- straight classification of whether the source is improved --
# From KIHBS: Human waste disposal facilities that are considered improved/adequate include;
# connection to main sewer, septic tanks, ventilated improved pit latrine, pit latrine with slab and composting toilets.
# -- Key Sentence: The analysis did not factor the issue of sharing of sanitation facilities as a measure of adequacy
 
    toilet_type = case_when(sanit_type %in% impr_toilet_codes ~ 'improved',
                            sanit_type %in% unimpr_toilet_codes ~ 'unimproved',
                            sanit_type %in% od_codes ~ 'open defecation',
                            TRUE ~ NA_character_),
 
    improved_toilet = case_when(is.na(toilet_type) ~ NA_real_,
                             (toilet_type == 'improved' & sanit_share == 2) ~ 1, # improved, unshared
                             (toilet_type == 'improved' & sanit_share == 1) ~ 0, # improved, shared
                             toilet_type %in% c('unimproved', 'open defecation') ~ 0, # unimproved or open defecation
                             TRUE ~ NA_real_), 
 
    impr_toilet_type = case_when(is.na(toilet_type) ~ NA_character_,
                              (toilet_type == 'improved' & sanit_share == 2) ~ 'improved-unshared', # improved, unshared
                              (toilet_type == 'improved' & sanit_share == 1) ~ 'improved-shared', # improved, shared
                              toilet_type == 'unimproved' ~ 'unimproved', # unimproved
                              toilet_type == 'open defecation' ~ 'open defecation', # open defecation
                              TRUE ~ NA_character_),

    improved_sanit_unsh = ifelse(toilet_type == 'improved', 1, 0))

  hh_wash_base <- left_join(hh_wash, hh_base) %>% 
    left_join(county_labels, by = c("county_id" = "county_id"))

    
  
# -- Quick summary tables --
  hh_wash %>% 
    group_by(toilet_type, improved_toilet) %>% 
    summarise(n = n()) %>% ungroup() %>% 
    mutate(pct = percent(n/sum(n), ndigits = 1))

  hh_wash %>% 
    group_by(impr_toilet_type) %>% 
    summarise(n = n()) %>%  
    mutate(pct = percent(n/sum(n), ndigits = 1))

# -- For reference, according to StatCompiler the Improved Sanitation in Kenya was only 28.1 in the 2015 MIS
# Improved water was 72
  hh_wash_base %>% 
    group_by(county_name, improved_toilet, county_id) %>% 
    summarise(n = n()) %>% 
    ungroup() %>%  
    group_by(county_name) %>% 
    mutate(pct = n/sum(n)) %>% 
    filter(improved_toilet == 1) %>% 
    ungroup() %>% 
    arrange(desc(pct)) %>% 
    print(n = 47) %>% 
    left_join(counties_geo, by = c("county_id" = "CID")) %>% 
    ggplot() +
    geom_sf(aes(fill = pct),  colour = "white", size = 0.5) +
    scale_fill_viridis_c(alpha = 0.75, option = "D", direction = -1, labels = scales::percent) +
    ggtitle("Improved sanitation lags behind in the north") +
    theme(legend.position = "top")

# Check that things check out
#hh_wash %>% group_by(improved_water_drink) %>% summarise(n())
#hh_wash %>% group_by(impr_water_under30min) %>% summarise(n())

  hh_wash_svy <- hh_wash_base %>% 
    group_by(county_id) %>% 
    as_survey_design(id = clid, strata = strat,  weights = weight) 
  


# Improved water statistics -----------------------------------------------

# -- Grab national level water stats  
# -- TODO: Write functions to do these steps as you are repeating them now   
  impr_water_natl <- 
    hh_wash_svy %>% 
    select(contains("improved_water")) %>% 
    summarise_all(survey_mean, na.rm = TRUE) %>% 
    select(-contains("_se")) %>% 
    gather(., key = improved_water_natl, value = value)

  impr_water_county <- 
    hh_wash_svy %>% 
    select(county_id, county_name, contains("improved_water")) %>%
    group_by(county_name, county_id) %>% 
    summarise_all(survey_mean, na.rm = TRUE) %>% 
    select(-contains("_se")) %>% 
    gather(., key = improved_water, value = value, -(county_id:county_name)) %>% 
    left_join(impr_water_natl, by = c("improved_water" = "improved_water_natl")) %>% 
    rename(impr_water = value.x, impr_water_natl = value.y) %>% 
    mutate(impr_water_dev = impr_water - impr_water_natl) %>% 
    group_by(county_name) %>% 
    mutate(maxsort = max(impr_water_dev)) %>% 
    ungroup()
   
# -- Notes to log
# to get a parallel color palette on diverging colors you need to use the scale_fill_gradientn
# The function allows you to define the limits, labels, and colors to get a balanced palette using the global max(abs(var))

max_dev = unlist(impr_water_county %>% summarise(max_dev = max(abs(impr_water_dev))))

  impr_water_county %>% 
    left_join(counties_geo, by = c("county_id" = "CID")) %>% 
    mutate(max_dev = max(abs(impr_water_dev))) %>% 
    ggplot() +
    geom_sf(aes(fill = impr_water_dev), colour = "white", size = 0.5) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdYlBu'),
                         limits = c(-1 * max_dev, max_dev), 
                         labels = scales::percent) +
    facet_wrap(~improved_water, nrow = 1) +
    ggtitle("Deviations from national average, by improved water source type") +
    theme(legend.position = "top") +
    labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") 
  

# Improved sanitation statistics ------------------------------------------
  # impr_sanit_natl <- 
  hh_wash_svy %>% select(improved_sanit_unsh, improved_toilet) %>% summarise_all(survey_mean, na.rm = TRUE)
    
  
  impr_sanit_natl <- 
    hh_wash_svy %>% 
    select(improved_toilet, improved_sanit_unsh) %>% 
    summarise_all(survey_mean, na.rm = TRUE) %>% 
    select(-contains("_se")) %>% 
    gather(., key = improved_sanit_natl, value = value)
  
  
  impr_sanit_county <- 
    hh_wash_svy %>% 
    select(county_id, county_name, improved_toilet, improved_sanit_unsh) %>% 
    group_by(county_name, county_id) %>% 
    summarise_all(survey_mean, na.rm = TRUE) %>% 
    select(-contains("_se")) %>% 
    gather(., key = improved_sanit, value = value, -(county_id:county_name)) %>% 
    left_join(impr_sanit_natl, by = c("improved_sanit" = "improved_sanit_natl")) %>% 
    rename(impr_sanit = value.x, impr_sanit_natl = value.y) %>% 
    mutate(impr_sanit_dev = impr_sanit - impr_sanit_natl) %>% 
    group_by(county_name) %>% 
    mutate(maxsort = max(impr_sanit_dev)) %>% 
    ungroup()
    
  max_dev_sanit = unlist(impr_sanit_county %>% summarise(max_dev = max(abs(impr_sanit_dev))))

  impr_sanit_county %>% 
    left_join(counties_geo, by = c("county_id" = "CID")) %>% 
    ggplot() +
    geom_sf(aes(fill = impr_sanit_dev), colour = "white", size = 0.5) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                         limits = c(-1 * max_dev_sanit, max_dev_sanit),
                         labels = scales::percent) +
    facet_wrap(~improved_sanit, nrow = 1) +
    ggtitle("Deviations from national average, by improved sanitation type (shared toilet or not") +
    theme(legend.position = "top") +
    labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") 
  
  
  
  
  
  impr_sanit_county %>% 
    left_join(counties_geo, by = c("county_id" = "CID")) %>% 
    ggplot() +
    geom_sf(aes(fill = impr_sanit), colour = "white", size = 0.5) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'OrRd'),
                         labels = scales::percent) +
    facet_wrap(~improved_sanit, nrow = 1) +
    ggtitle("Improved sanitation type (shared toilet or not") +
    theme(legend.position = "top") +
    labs(caption = "Source: Kenya Integrated Household Budget Survey 2016") 
  
  
  # TODO - Call DHS API to grab past values of Improved Sanitation & H20

  


