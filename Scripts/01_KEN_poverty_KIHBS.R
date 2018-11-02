# Purpose: Process Poverty data from KIHBS
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_09
# Audience: Kenya Mission

# Set path and folders where data live ------------------------------------
#Source helper functions
source("00_SetupData.R")

read_path <- "KEN_IHBS_2016.xlsx"
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"

# Start the load routine --------------------------------------------------

# List of the names of the excel sheets
read_path %>%
  excel_sheets()

# To read them all at once and break apart into separate data frames
read_path %>%
  excel_sheets() %>%
  
  # Use basename with the file_path_sans_ext to strip extra info; Not NEEDED
  # set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  set_names() %>%
  map(~read_excel(path = read_path, sheet = .x), .id = "sheet") %>%
  
  # Use the list2env command to convert the list of dataframes to separate dfs using the name provided by the set_names(command). Necessary b/c not all the sheets are the same size
  list2env(., envir = .GlobalEnv)


# Write the files to the data folder for later use if needed
# Create your list first, then in second chunk write to folder
df <- excel_sheets(read_path) %>%
  set_names() %>%
  map(read_excel, path = read_path)

df %>%
  names() %>%
  map(., ~write_csv(df[[.]], paste0(datapath, "/", ., ".csv")))


# Combine like variables and merge in crosswalk ---------------------------

# ---- Food poverty

food_pov_tmp <- 
  Food_poverty %>% 
  gather(., 
         Headcount_rate_food_poverty:Severity_of_poverty,
         value = "percent", 
         key = "food_poverty_type") %>% 
  left_join(., geo_cw, by = c("CC_1" = "CC_1"))

# Extract out national numbers by indicators to create deviation metrics (could functionalize this)
pov_food <- 
  food_pov_tmp %>%
  filter(County.x == "National") %>% 
  dplyr::select(food_poverty_type, 
                nat_ave = percent) %>% 
    right_join(., food_pov_tmp, by = c("food_poverty_type")) %>% 
    mutate(deviation = (nat_ave - percent)) 


# ---- Overall poverty
poverty_tmp <- 
  Overall_poverty %>% 
  gather(., 
         Headcount_rate:Severity_of_poverity, 
         value = "percent", 
         key = "poverty_type") %>% 
  left_join(., geo_cw, by = c("CC_1" = "CC_1"))

pov <- 
  poverty_tmp %>% 
  filter(County.x == "National") %>% 
  dplyr::select(poverty_type, 
                nat_ave = percent) %>% 
  right_join(., poverty_tmp, by = c("poverty_type")) %>% 
  mutate(deviation = (nat_ave - percent)) 




`# ---- Child poverty by age groups

child_pov_pop <- 
  Proportion_poor_children %>% 
  dplyr::select(contains("population"), CC_1) %>%
  gather(., 
         population:population_0_17,
         value = "poopulation",
         key = "age_range") %>% 
  # Use case_when to get at unique combo of age_range + geography for merge
  mutate(age = case_when(
    age_range == "population"      ~ "All",
    age_range == "population_0_17" ~ "0_17",
    age_range == "population_0_5"  ~ "0_5",
    age_range == "population_6_13" ~ "6_13",
    TRUE ~ "14_17"
  )) %>% 
  rename(poverty_hc = age_range)

child_pov_pct <- 
  Proportion_poor_children %>% 
  dplyr::select(-contains("population")) %>% 
  gather(., 
         Poverty_hc_rate:Poverty_hc_rate_0_17, 
         value = "poverty",
         key = "age_range") %>% 
  mutate(age = case_when(
    age_range == "Poverty_hc_rate"      ~ "All",
    age_range == "Poverty_hc_rate_0_17" ~ "0_17",
    age_range == "Poverty_hc_rate_0_5"  ~ "0_5",
    age_range == "Poverty_hc_rate_6_13" ~ "6_13",
    TRUE ~ "14_17"
  )) %>% 
  rename(poverty_hc = age_range, 
         percent = poverty)

child_pov_dev <- 
  child_pov_pct %>% 
  filter(County == "National") %>% 
  dplyr::select(poverty_hc, nat_ave = percent) %>% 
  right_join(., child_pov_pct, by = c("poverty_hc")) %>% 
  mutate(deviation = (nat_ave - percent))


# Now combine the two to get a useable population + rate data frame
# Changed the geo_cw merge field to be a numeric to avoid string conflicts
pov_child <- 
  child_pov_dev %>% 
  left_join(child_pov_pop, by = c("CC_1", "age")) %>% 
  dplyr::select(-poverty_hc.y) %>% 
  rename(pov_hc_range = poverty_hc.x) %>% 
  left_join(., geo_cw, by = c("CC_1" = "CC_1")) %>% 
  mutate(age = fct_relevel(age, "0_5", "6_13", "14_17", "0_17", "All"))

# Clean up objects
ls(pattern = "_pov_")
rm(list = ls(pattern = "_pov_"))

# Mapping magic -----------------------------------------------------------

# Might as well go ahead and join it with a shapefile and create couple sample maps
gis_admin1 <- read_sf(file.path(gispath, "gadm36_KEN_1.shp"))

source("KEN_helper_functions.R")

# Poverty plotted by age group for children
pov_plot(pov_child, percent, "Kenya Child poverty (%) by county", age)

# Food poverty across distribution, HC, poverty gap, and serverity
pov_plot(pov_food, deviation, "Kenya food poverty (%) by county", food_poverty_type)

# Poverty overall
pov_plot(pov, percent, "Kenya poverty (%) by county", poverty_type)
pov_plot(pov, Number_poor, "Kenya poverty population (in 000's) by county", poverty_type)



# Export data for Tableau -------------------------------------------------
export_list <- list(food_poverty_tblx = pov_food , 
                    child_poverty_tblx = pov_child, 
                    poverty_tblx = pov, 
                    poverty_region_tblx = Comparison_povertyRates)

export_list %>%
  names() %>%
  map(., ~write_csv(export_list[[.]], paste0(datapath, "/", ., ".csv")))




# Extra code for plotting SRT as background -------------------------------
# http://www.francescobailo.net/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/

# Load DEM data from Raster SRTM
# dem.raster <- getData("SRTM", lat = 1, lon = 36, download = TRUE)
# 
# # Bounding box for Kenya
# #33.8935689697, -4.67677, 41.8550830926, 5.506
# my_bbox <- c(xmin = 33.8935689697,
#              xmax = 41.8550830926,
#              ymin = -4.67677,
#              ymax = 5.506)
# 
# my_bbox_m <- 
#   matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'], 
#            my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']),
#          ncol = 2)
# my_bbox_sf <- st_geometry(st_polygon(x = list(my_bbox_m)))
# st_crs(my_bbox_sf) <- 4326
# my_bbox_buff_2500_sf <- 
#   my_bbox_sf %>%
#   st_transform(crs = 32632) %>%
#   st_buffer(dist = 2500) %>% # 2.5 kilometers
#   st_transform(crs = 4326)
# 
# ggplot() + geom_sf(data = my_bbox_buff_2500_sf)
# 
# # Below did not work
# dem.raster <- crop(dem.raster, as(my_bbox_buff_2500_sf, 'Spatial'), snap='out')
# 
# 
# 
# dem.m  <-  rasterToPoints(dem.raster)
# dem.df <-  data.frame(dem.m)
# colnames(dem.df) = c("lon", "lat", "alt")
# ggplot() +
#   geom_raster(data = dem.df, aes(lon, lat, fill = alt)) +
#   scale_fill_viridis_c(option = "A")


