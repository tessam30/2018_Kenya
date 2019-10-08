# Purpose: Clean up scraped data for DECA proof of concept
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_10_08
# Audience: CDD DECA team
library(readxl)

# Load data ---------------------------------------------------------------
excel_file <- file.path(datapath, "KHIBS_DECA_data.xlsx")

sheet_names <- excel_sheets(file.path(datapath, "KHIBS_DECA_data.xlsx"))


asal_geo <- st_read(file.path(datapath, "ASAL_Counties_2018.shp"))


# Batch read in the sheets, storing each one a separate datafram
sheet_names %>% 
  map(function(sheet) {
    assign(x = sheet,
           value = read_excel(path = excel_file, sheet = sheet),
           envir = .GlobalEnv) 
  })
 

View(ICT_used)
names(ICT_used)

# Function to add an index to the files read in

add_index <- function(df) {
  df <- df %>% 
    mutate(CID = row_number())
  return(df)
  
  }


deca_list <- list(ICT_used, Internet_used_where, Internet_used_why, Why_no_internet)
deca_list <- 
  deca_list %>% 
  map(., add_index)


ICT_used_long %>% 
  deca_list[[1]] %>% 
  gather(type, value, tv:internet) %>% 
  mutate(value = value/100)

Internet_used_where_long <- 
  deca_list[[2]] %>% 
  gather(type, value, `in mobility`:`other`) %>% 
  mutate(value = value/100)

Internet_used_why_long <- 
  deca_list[[3]] %>% 
  gather(type, value, `seek health info`:`other`) %>% 
  mutate(value = value/100)

Why_no_internet_long <- 
  deca_list[[4]] %>% 
  gather(type, value, `too young`:`not stated`) %>% 
  mutate(value = value/100)


datalist <- list(ICT = ICT_used, 
                 ICT_long = ICT_used_long, 
                 Internet_used = Internet_used_where, 
                 Internet_used_long = Internet_used_where_long,
                 Internet_used_why = Internet_used_why, 
                 Internet_used_why_long = Internet_used_why_long,
                 No_internet = Why_no_internet,
                 No_internet_long = Why_no_internet_long)


datalist %>% 
  names() %>% 
  map(., ~ write_csv(datalist[[.]], file.path(datapath, str_c(., ".csv"))))








# Proof of concept maps ---------------------------------------------------


# Generic plotting function for line over average plots
ict_map <- function(df, x, y) {
  asal_geo %>% 
    left_join(., df, by = "CID") %>% 
    mutate(sort_county = fct_reorder(County, value, .desc = TRUE)) %>% 
    ggplot() +
    geom_sf(aes(fill = value))
}


str(ICT_used_long
    )




hiv_line <- function(df, x, y, yave, fill, wrap) {
  df %>% 
    ggplot() +
    geom_area(aes(x = {{ x }}, y = {{ yave }}), 
              fill = grey30K, alpha = 0.50) +
    geom_line(aes(x = {{ x }}, y = {{ y }})) +
    geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }}),
               size = 3.5, shape = 21, colour = "white", stroke = 1) +
    facet_wrap(vars({{ wrap }})) +
    theme_minimal() 
}






