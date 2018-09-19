# Purpose: Start processing Kenya Integrated Household Budget Survey
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_19
# Audience: Kenya Mission

# Read in the shock data 

# Load shock data ---------------------------------------------------------

shk_df <- read_dta(file.path(kihbspath, "Recent_Shocks.dta"))

# Provide a crosswalk of shocks with code to lump into categories we have established here:
# link to github categorization of shocks
shk_df %>% group_by(q02, q01) %>% tally() %>% arrange(-n) %>% print(n = Inf)
