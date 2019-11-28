# Purpose: Process anthro data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_10_23
# Audience: Kenya Mission

library(zscorer)
library(llamar)

# Grab the data needed ----------------------------------------------------

hh_ind <- read_dta(file.path(kihbspath, "HH_Members_Information.dta"))
dhs <- read_dta(file.path(datapath, "2014_DHS", "KEKR71DT", "KEKR71FL.DTA"))

vtable(hh_ind)

dhs_count <- dhs %>% 
  select(ageMonths = v012) %>% 
  group_by(ageMonths) %>% 
  summarise(count = n())

# Generate an age variable converting everything to months
hh_und5 <- 
  hh_ind %>% 
  # Keeping only children 60 months or below (check below)
  filter(f01 == 1) %>% 
  mutate(ageMonths = ((b05_yy * 12) + b05_mm),
         age_flag = ifelse(ageMonths > 59 & !is.na(ageMonths), 1, 0),
         csex = b04, 
         cheight = f22,
         cweight = f21,
         anthro_flag = ifelse(cweight > 50 | cheight > 150 | cheight < 25, "True", "False"),
         
         # Need standing == 2 and laying down == 1 for the recumbent variable
         recum_recode = ifelse(f23 == 1, 2, 1))

# Check the recode numbers for missing 
# Age bandings seem to be a major issue with the data; Compare this to the DHS
hh_und5 %>% group_by(recum_recode) %>% tally() 
hh_und5 %>% filter(age_flag == 0) %>%   
  mutate(rounding = ifelse(ageMonths %in% c(12, 24, 36, 48), TRUE, FALSE)) %>% 
  group_by(ageMonths, rounding) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = ageMonths, y = count)) + 
  geom_point() +
  geom_point(data = dhs_count, aes(colour = "red"))
  

vtable(dhs)




hh_und5 %>% select(clid:hhid, ageMonths:recum_recode)

# Process DHS data on anthro data ----------------------------------------
dhs_und5 <- 
  dhs %>% 
  mutate(rural = ifelse(v025 == 2, 1, 0)) %>% 
  select(sex = b4,
         cheight = hw3,
         cweight = hw2,
         stunting = hw70,
         underweight = hw71, 
         wasting = hw72, 
         bmi = hw73, 
         age_months = hw1,
         age_group = v013) %>% 
  mutate(cheight = case_when(
    cheight > 9000 ~ NA
    
  ))


# Plot the data to check for odd values

hh_und5 %>% 
  ggplot(., aes(x = ageMonths, y = cheight, color = ifelse(cheight > 50, TRUE, FALSE))) +
  geom_point() 

hh_und5 %>% 
  ggplot(., aes(x = cheight,
                y = cweight, 
                color = anthro_flag)) + 
  facet_wrap(~ csex) +
  geom_point() +
  scale_color_viridis_d(begin = 0, end = 0.5, direction = -1,  alpha = 0.75)


# Comparison plots
ggplot(hh_und5, aes(x = cheight)) +
  geom_density(alpha = 0.5,
               fill = ftfBlue,
               colour = ftfBlue) +
  geom_density(alpha = 0.5, 
               fill = ftfOrange,
               colour = ftfOrange,
               data = hs_data) +
  theme_xygrid()+
  annotate(geom = 'text', y = 0.025, x = 101, label = "Malawi 2011 IHS",
           hjust = 0, size = 4, colour = ftfOrange) +
  annotate(geom = 'text', y = 0.025, x = 60, label = "Malawi 2010/11 DHS",
           hjust = 0, size = 4, colour = ftfBlue) 
#ggtitle("The distribution of children's heights are skewed in the 2011 IHS data") 





all <- getCohortWGS(data = hh_und5, FUN = getWGS, sexObserved = "csex", 
                    firstPart = "cheight", 
                    secondPart =  "ageMonths", 
                    index = "hfa")

(hazAll <- getCohortWGS(data = hh_und5,
                       sexObserved = "csex",
                       firstPart = "cheight",
                       secondPart = "ageMonths",
                       index = "hfa"))
  
  
  getAllWGS(data = hh_und5, 
                 sex = csex, # 1 = Male / 2 = Female
                 weight = cweight, # Weight in kilograms
                 height = cheight, # Height in centimetres
                 age = ageMonths, # Age in whole months
                 index = "wfa") # Anthropometric index (weight-for-age)




           

  
  
  
  
zscore06, a(ageMonths) s(csex) h(cheight) w(cweight) measure(recumb_recode) 


# Stunting calculation notes ----------------------------------------------

# A couple of notes for you:
#   1) You don't need to compute the z-scores for older rounds of data. The Z-scores for the older rounds of surveys are available in separate data files that can be downloaded. These are the HW (Height and Weight scores) files, for example for Benin 2001 the file is BJHW41xx.zip (where xx is the type of data file).
# 2) The reason you are not matching the z-scores in the DHS data is because of the calculation of age used in the computation of the Z-scores. HW1 only gives age rounded off to the month, but in the calculation of the z-scores we compute age to the day, as follows:
# 
# * Calculate measurement date in days
# gen mdate = mdy(hw18, hw17, hw19)
# * Calculate birth date in days
# gen bdate = mdy(b1, hw16, b2) if hw16 <= 31
# replace bdate = mdy(b1, 15, b2) if hw16 > 31
# * Calculate age in months with days expressed as decimals.
# gen age = (mdate-bdate)/30.4375
# * Compute Z-scores
# zscore06, a(age) s(b4) h(hw3_2) w(hw2_2)



# Plotting DHS modeled surfaces for stunting ------------------------------
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggpubr)


list.files(file.path(gispath, "DHS_Stunting"), 
           pattern = "MS_MEAN_v01.tif", full.names = TRUE) %>% 
  
  # Chain together a gsub command to incrementally remove parts of the name string
  set_names(gsub("Data/GIS/DHS_Stunting/", "", .) %>% 
              gsub("_CNNUTSCHA2_MS_MEAN_v01.tif", "", .))%>% 
  map(raster) %>% 
  list2env(., envir = .GlobalEnv)

# Merge the rasters together into a single raster
east_afr <- list(ET2016DHS, KE2014DHS, TZ2015DHS, UG2016DHS)
east_afr$filename = "East_Africa_Stunting.tif"
e_afr <- do.call(merge, east_afr)

# One option for plotting
levelplot(e_afr)

test_spdf <- as(e_afr, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)

p1 <- test_df %>% filter(East_Africa_Stunting > 0.01) %>% 
 ggplot(., aes(x = x, y = y, fill = East_Africa_Stunting)) +  
  geom_tile(alpha=0.8, show.legend = FALSE) + 
  scale_fill_viridis_c(option = "A", direction = -1) +
  theme_minimal()
  #scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'RdPu'))

p2 <- ggplot(test_df, aes(East_Africa_Stunting, fill = cut(East_Africa_Stunting, 100))) +
  stat_bin(bins = 100, show.legend = FALSE) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  theme_minimal()

# Arrange to show histogram below map
ggarrange(p1, p2, heights = c(2.1, 0.4),
          nrow = 2, align = "h")


# Create a smoothed palette
RdPu<- brewer.pal(9, 'RdPu')

newPal <- colorRampPalette(RdPu)
RdPu_new <- newPal(100)
