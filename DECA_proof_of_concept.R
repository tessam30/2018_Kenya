# Purpose: Clean up scraped data for DECA proof of concept
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_10_08
# Audience: CDD DECA team
library(readxl)
library(tidytext)

# Load data ---------------------------------------------------------------
excel_file <- file.path(datapath, "DECA", "KHIBS_DECA_data.xlsx")

sheet_names <- excel_sheets(file.path(datapath, "DECA", "KHIBS_DECA_data.xlsx"))

# Batch read in the sheets, storing each one a separate datafram
sheet_names %>% 
  map(function(sheet) {
    assign(x = sheet,
           value = read_excel(path = excel_file, sheet = sheet),
           envir = .GlobalEnv) 
  })
 

#View(ICT_used)
names(ICT_used)



# Function on the fly to rescale percentages to decimals
scale2 <- function(x)(x/100)

# Function to generalize reshape process, and create an index for merging CIDS
make_long <- function(df, ...) {
  df %>% 
    mutate(CID = row_number()) %>% 
    gather(type, value, ...) %>% 
    mutate(value = scale2(value))
}



# deca_list <- list(ICT_used, Internet_used_where, Internet_used_why, Why_no_internet,
#                   `Own_phone_18+`, `Why_no_phone_18+`, `Mobile Money`,  ICT_ownership, 
#                  Internet_hh, Internet_type_hh, Internet_lack_hh)
# deca_list <- 
#   deca_list %>% 
#   map(., add_index)


# Create new dataframes for each dataset



ICT_used_long             <- make_long(ICT_used, c(`tv`:`internet`))
ICT_ownership_long        <- make_long(ICT_ownership, c(`own computer`:`own tv`))
Internet_used_where_long  <- make_long(Internet_used_where, c(`in mobility`:`other`))
Internet_used_why_long    <- make_long(Internet_used_why, c(`seek health info`:`other`)) 
Why_no_internet_long      <- make_long(Why_no_internet, c(`too young`:`not stated`))
Why_no_phone_18plus_long  <- make_long(`Why_no_phone_18+`, c(`too young`:`other`))
Internet_type_hh_long     <- make_long(Internet_type_hh, c(`wired broadband`:`other`))
Internet_lack_hh_long     <- make_long(Internet_lack_hh, c(`No need`:`other`))
Why_no_internet_age_long  <- make_long(Why_no_internet_age, c(`too young`:`not stated`))
Internet_used_where_age_long <- make_long(Internet_used_where_age, c(`in mobility`:`other`))
Internet_used_why_age_long <- make_long(Internet_used_why_age, c(`seek health info`:`other`))
mobile_money_long         <- make_long(`Mobile Money`, c(`subscribe to mobile money platform`:`subscribe to mobile banking`))
ICT_used_age_long         <- make_long(ICT_used_age, c(`tv`:`internet`))


# Age categories now


 
mobile_money <- 
  `Mobile Money` %>% 
  mutate_at(vars(contains("subscribe")), scale2)

own_phone <- 
  `Own_phone_18+` %>% 
  mutate_at(vars(c(`Own phone`)), scale2)

ICT_ownership <- ICT_ownership %>% mutate_at(vars(contains("own")), scale2)
Internet_hh <- Internet_hh %>% mutate(internet = scale2(internet))



datalist <- list(ICT = ICT_used, 
                 ICT_long = ICT_used_long,
                 ICT_ownership = ICT_ownership,
                 Internet_used = Internet_used_where, 
                 Internet_used_where_long = Internet_used_where_long,
                 Internet_used_why = Internet_used_why, 
                 Internet_used_why_long = Internet_used_why_long,
                 No_internet = Why_no_internet,
                 No_internet_long = Why_no_internet_long,
                 own_phone = own_phone,
                 Why_no_phone_18plus_long = Why_no_phone_18plus_long,
                 Internet_lack_hh_long = Internet_lack_hh_long,
                 Internet_type_hh_long = Internet_type_hh_long,
                 Internet_access_hh = Internet_hh,
                 mobile_money = mobile_money,
                 own_phone = own_phone,
                 Internet_used_where_age_long = Internet_used_where_age_long,
                 Internet_used_why_age_long = Internet_used_why_age_long,
                 Internet_used_why_age_long = Internet_used_why_age_long, 
                 ICT_used_age = ICT_used_age)


#datalist <- mget(ls(pattern = "_long"))

datalist %>% 
  names() %>% 
  map(., ~ write_csv(datalist[[.]], file.path(datapath, str_c(., ".csv"))))







# Proof of concept plots ---------------------------------------------------


# Generic plotting function for line over average plots

bar_plot <- function(df, x, y, wrap, ctitle = "Mobile phone ownership") {
  
  cpt <- df %>% select(source) %>% unique()
  
   df %>%
    mutate(csort = fct_reorder({{ x }}, {{ y }}, .desc = FALSE)) %>% 
    ggplot(aes(x = csort, {{ y }})) +
    geom_col(fill = "#949494" ) + coord_flip() +
    facet_wrap(vars({{ wrap }})) +
    theme_minimal() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    theme(strip.text = element_text(hjust = 0),
          axis.text.y = element_text(size = 7))+
    labs(x = "", y = "",
         title = ctitle, 
         caption = str_c("Source: ", cpt)) 
}

bar_plot(own_phone, x = County, y = `Own phone`, wrap = "Mobile phone ownership")


bplot_sort <- function(df, x = County, y = value, wrap = type, ctitle = "NA", rows = 2) {

  cpt <- df %>% select(source) %>% unique()
  
  df %>%
    mutate(indicator_sort = fct_reorder( {{ wrap }}, {{ y }}, .desc = TRUE),
           County_sort = reorder_within( {{ x }}, {{ y }}, {{ wrap }} )) %>% 
    ggplot(aes(y = {{ y }}, x = County_sort)) +
    geom_col(fill = "#949494") + 
    coord_flip() +  
    scale_x_reordered() +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    facet_wrap(vars(indicator_sort), scales = "free_y", nrow = rows) +
    theme_minimal() +
    labs(x = "", y = "",
         title = ctitle, 
         caption = str_c("Source: ", cpt)) +
    theme(strip.text = element_text(hjust = 0),
          axis.text.y = element_text(size = 7))
}


(ict_use_gph <- bplot_sort(ICT_used_long, ctitle = "ICT use rates"))
(ict_own_gph <- bplot_sort(ICT_ownership_long, ctitle = "ICT ownership rates", rows = 1))
(int_used_where_gph <- bplot_sort(Internet_used_where_long, ctitle = "Where is internet used"))
(int_used_why_gph <- bplot_sort(Internet_used_why_long, ctitle = "Why using the internet", rows = 3)) + theme(axis.text.y = element_text(size = 6))
(int_lack_gph <- bplot_sort(Why_no_internet_long, ctitle = "Why no internet (asked of individual)", rows = 3))
(int_lack_hh_gph <- bplot_sort(Internet_lack_hh_long, ctitle = "Why do you not have internet at home", rows = 3))
(own_phone_gph <- bar_plot(own_phone, x = County, y = `Own phone`, wrap = "Mobile phone ownership"))
(why_no_phone_gph <- bplot_sort(Why_no_phone_18plus_long, ctitle = "Why do you not own a mobile phone?"))
(mobile_money_gph <- bplot_sort(mobile_money_long, ctitle = "Do you subscribe to mobile money or banking?", rows = 1))
(int_type_hh_gph <- bplot_sort(Internet_type_hh_long, ctitle = "Type of internet connection at home (of those with internet)"))
(internet_hh_gph <- bar_plot(Internet_hh, x = County, y = internet, wrap = "Internet connection at home", ctitle = "Internet connection at home"))

# Age categories now
(ict_used_age_gph <- bplot_sort(ICT_used_age_long, x = `Age group`, ctitle = "ICT use rates by age"))
(int_used_where_age_gph <- bplot_sort(Internet_used_where_age_long,  x = `Age group`, ctitle = "Where is internet used by age"))
(int_used_why_age_gph <- bplot_sort(Internet_used_why_age_long, x = `Age group`, ctitle = "Why is internet used by age"))


# Have to set this to get ggsave to work on PC
options(bitmapType = 'cairo', device = 'png')

# put graphs into a list
graph_list <- mget(ls(pattern = "gph"))

graph_list %>% 
  names() %>% 
  map(., ~ ggsave(str_c(., ".png"),
                  graph_list[[.]],
                  scale = 1.25,
      dpi = 300))



ggsave("internet_hh_gph.png",
       internet_hh_gph,
       scale = 1.25,
       dpi = 300)



# Maps of indicators ------------------------------------------------------
map_sort <- function(df, x = County, y = value, wrap = type, ctitle = "NA", rows = 2) {
  
  cpt <- df %>% select(source) %>% unique()
  
  asal_geo %>% 
    left_join(., df, by = "CID") %>% 
    mutate(indicator_sort = fct_reorder( {{ wrap }}, {{ y }}, .desc = TRUE),
           County_sort = reorder_within( {{ x }}, {{ y }}, {{ wrap }} )) %>% 
   ggplot() +
    geom_sf(aes(fill = {{ y }}), color = "white", size = 1/4) +
    scale_fill_viridis_c(direction = -1, alpha = 0.85, option = "A",
                         labels = percent_format(accuracy = 1)) + 
    facet_wrap(vars(indicator_sort), nrow = rows) +
    theme_minimal() +
    labs(x = "", y = "",
         title = ctitle, 
         caption = str_c("Source: ", cpt),
         fill = "percent") +
    theme(strip.text = element_text(hjust = 0),
          axis.text.y = element_text(size = 7),
          legend.position = "top")
}

map_sort(ICT_used_long, ctitle = "ICT use rates")

(ict_use_map <- map_sort(ICT_used_long, ctitle = "ICT use rates"))
(ict_own_map <- map_sort(ICT_ownership_long, ctitle = "ICT ownership rates", rows = 1))
(int_used_where_map <- map_sort(Internet_used_where_long, ctitle = "Where is internet used"))
(int_used_why_map <- map_sort(Internet_used_why_long, ctitle = "Why using the internet", rows = 3)) 
(int_lack_map <- map_sort(Why_no_internet_long, ctitle = "Why no internet (asked of individual)", rows = 3))
(int_lack_hh_map <- map_sort(Internet_lack_hh_long, ctitle = "Why do you not have internet at home", rows = 3))
#(own_phone_map <- bar_plot(own_phone, x = County, y = `Own phone`, wrap = "Mobile phone ownership"))
(why_no_phone_map <- map_sort(Why_no_phone_18plus_long, ctitle = "Why do you not own a mobile phone?"))
(mobile_money_map <- map_sort(mobile_money_long, ctitle = "Do you subscribe to mobile money or banking?", rows = 1))
(int_type_hh_map <- map_sort(Internet_type_hh_long, ctitle = "Type of internet connection at home (of those with internet)"))
#(internet_hh_map <- bar_plot(Internet_hh, x = County, y = internet, wrap = "Internet connection at home", ctitle = "Internet connection at home"))



map_list <- mget(ls(pattern = "_map"))

map_list %>% 
  names() %>% 
  map(., ~ ggsave(file.path(datapath, "DECA", str_c(., ".png")),
                  map_list[[.]],
                  dpi = 300))




