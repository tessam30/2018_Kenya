# Purpose: Read in IPC data and provide analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_04_22
# Audience: Kenya Mission

# Check what's pre-loaded for you
(.packages()) %>% grep("xl", .)

ipc_wide <- read_excel(file.path(ipcpath, "IPC_Sublocation_2010-2019 - Date.xlsx"))
ipc_geo <-  st_read(file.path(ipcpath, "SUBLOCATIONS_IPC_2010-2011", "SUBLOCATIONS_IPC_2010-201.shp"))

# Check for uniqueness
# So it appears that the shapefile and excel file can be joined using the FID from the excel file and the 
# JOINID from the shapefile. That said, there seems to be a bit of oddness w/ identifying unique entries.

# Shapefile first
  ipc_geo %>% group_by(ADMIN6ID, County) %>% n_groups() # 6511 unique combinations
  ipc_wide %>% count(SUBLOCID, SUB_LOCATION, COUNTY, LOCATION) %>% arrange(desc(n)) %>% print(n = 10)
  ipc_wide %>% group_by(SUBLOCID, SUB_LOCATION, COUNTY) %>% n_groups()


# Flag the Sublocations that are missing
ipc_long <- 
  ipc_wide %>% 
  mutate(subloc_flag = ifelse(is.na(SUBLOCID) == "TRUE", 1, 0)) %>% 
  gather("date", "ipc_class", `40391`:`43497`) %>% 
  mutate(ipc_date = as.Date(as.numeric(date), origin = "1899-12-30"),
         id = FID,
         famine = if_else(ipc_class == 5, 1, 0),
         emergency = if_else(ipc_class == 4, 1, 0),
         stressed = if_else(ipc_class == 3, 1, 0), 
         dates_of_interest = year(ipc_date) %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) 

ipc <- 
  ipc_long %>% 
  left_join(., ipc_geo, by = c("FID" = "JOINID"))

ipc %>% 
  #filter(ipc_class %in% c(3, 4, 5)) %>% 
  filter(dates_of_interest == "TRUE") %>% 
  ggplot() +
  geom_sf(aes(fill = factor(ipc_class)), colour = "white", size = 0.25) + 
  facet_wrap(~ipc_date) +
  scale_fill_viridis_d()

