# Purpose: Scrape out Malnutrition data
# Author: Tim Essam
# Date: 2018_12_19
# 


# Determine universe of files to use --------------------------------------

muac_files <- list.files(file.path(datapath, "malnutrition"), pattern = "MUAC")
muac_path <- "Data/malnutrition"

muac_raw <- data_frame(filename = muac_files) %>% 
  mutate(file_contents = map(filename, 
         ~read_excel(file.path(muac_path, .), range = "B1:N72", sheet = "Data"))) %>% 
  mutate(filename = str_remove(filename, " Prices and MUAC.xlsx")) %>% 
  unnest() 

muac <- 
  muac_raw %>% 
  rename(varinfo = X__1,
         Jan = X__2,
         Feb = X__3,
         Mar = X__4,
         Apr = X__5,
         May = X__6,
         Jun = X__7,
         Jul = X__8,
         Aug = X__9,
         Sep = X__10,
         Oct = X__11,
         Nov = X__12,
         Dec = X__13, 
         county = filename) %>% 
  filter(!is.na(varinfo)) %>% 
  # Flag variable names so we know what data go w/ what variable
  mutate(ave_flag = ifelse(str_detect(varinfo, regex("average", ignore_case = TRUE)), 1, 0),
         commodity = ifelse(varinfo  %like% "Price|price|Maize|risk", varinfo, NA_character_),
         commodity_flag = ifelse(varinfo  %like% "Price|price|Maize|risk", 1, 0)) %>% 
  select(county, varinfo, commodity, commodity_flag, ave_flag, everything()) %>% 
  filter(ave_flag == 0, varinfo != "Year") %>% 
  fill(commodity)

tmp2 <- muac %>% 
  set_names() %>%  
  map(., ~read_excel(file.path(datapath, "malnutrition", .), range = "B1:N72", sheet = "Data") %>% 
        mutate(file_name = )) %>% 
  reduce(rbind)

  
map(tmp2, ~names(.))



tmp <- 
  map(tmp_list, ~read_excel(file.path(datapath, "malnutrition", .), range = "B2:", data = ))
  
  
  
tmp <- read_excel(file.path(datapath, "malnutrition", "Baringo Prices and MUAC.xlsx")) %>% 
  mutate(county = str_remove("Baringo Prices and MUAC.xlsx", " Prices and MUAC.xlsx"))
