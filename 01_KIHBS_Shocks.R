# Purpose: Start processing Kenya Integrated Household Budget Survey
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_19
# Audience: Kenya Mission

# Read in the shock data 

# Load shock data ---------------------------------------------------------

shk_df <- read_dta(file.path(kihbspath, "Recent_Shocks.dta"))

# Provide a crosswalk of shocks with code to lump into categories we have established here:
# link to github categorization of shocks
shk_df %>% group_by(q02, q01) %>% tally() %>% arrange(q01) %>% print(n = Inf)

# Shock crosswalk
#---------------------------------------------------------------------
# Disaster  = 101 = drought or floods
# Ag        = 102 = crop disease or crop pests
# livestock = 103 = livestock died
# livestock = 104 = livestock were stolen
# financial = 105 = Household business failure, non-agricultural
# financial = 106 = Loss of salaried employment
# financial = 107 = end of regular assistance
# crop_price = 108 = large fall in sale prices for crops
# food_price = 109 = large rise in price of 
# input_price = 110 = large rise in agriculture input prices
# water_short = 111 = severe water shortage
# demographic = 112 = birth in the household
# demographic = 113 = death in HH head
# demographic = 114 = death of working member of household
# demographic = 115 = death of other family member
# demographic = 116 = break-up of the household
# economic = 117 = bread winner jailed
# disaster = 118 = fire
# crime    = 119 - robery/bulgary/assault
# crime    = 120 carjacking
# other    = 121 dwelling destroyed
# other    = 122 eviction
# violence = 123 ethnic clan clashes
# violence = 124 conflict
# health   = 125 HIV/Aids
# other    = 126, 127
#----------------------------------------------------------------#




# How are shocks to be lumped together?
# Disaster = 101 (drought or floods),
# 
# crop = 102 (crop disease or crop pests), 108 - large fall in sale price, 109 - input price
# livestock = 103 (died), 104 (stolen)
# ag = 102, 103, 104 -- crop + livestock 
# financial = 
#


# Water_shortage = 111 (sever water shortage)

