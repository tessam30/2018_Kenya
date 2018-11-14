# Purpose: County Budget category reshape for alinging with budget data
# Author: Tim Essam, PhD. GeoCenter
# Date: 2018_11_14
# Details: Requested by LAM team Dr. McCusker.


# Libraries and connect to data -------------------------------------------

# If first time useing R, run install.packages("tidyverse", "readxl")
library(tidyverse)
library(readxl)
source("Scripts/00_SetupData.R")

# Load the data from the appropriate directory
# Refer to the 00_SetupData.R file to see where the path objects are set. Keep paths relative whenever possible.
budget_info_wide <- read_excel(file.path(datapath, "Budget/category_crosswalk.xlsx"), 
                               sheet = "2016-17 - County Category Looku", 
                               skip = 1) 
glimpse(budget_info_wide)


## Clean up the data and transpose the columns using tidyverse::gather() function
# To reshape the data, we need to do a little tidying up. First, we need to remove the `Category/County` and the `X__1` columns as these are not needed int he reshape. Then, we will take our remaining data and reshape the Counties to be stacked alongside the budget categories defined. 

budget_info_long <- 
  budget_info_wide %>% 
  select(-X__1, - `Category/County`) %>% 
  gather(., key = County, value = Budget_Category) %>% 
  mutate(year = 2016) %>% 
  filter(!is.na(Budget_Category)) %>% 
  mutate(County = stringr::str_to_title(County))

budget_info_long %>% print(n = 40)

# Finally, let's save the resulting dataframe in a new .csv for crosswalking with the Budget data
write_csv(budget_info_long, file.path(datapath, "Budget/KEN_budget_categories_long_2016.csv"))
