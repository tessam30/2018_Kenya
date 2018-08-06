# Purpose: Set up repository for Kenya Analysis
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_08_03
# Audience: Kenya Mission

# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools")

dir.create("Data")
datapath <- "Data"
gispath <- "Data/gadm36_KEN_shp"

# Read in constituency data
# https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_KEN_shp.zip
  gis_admin2 <- read_sf(file.path(gispath, "gadm36_KEN_2.shp"))

# Download election results from Kenya Election Comission
# Per wikipedia -- results may not be up to date 
# https://en.wikipedia.org/wiki/Kenyan_general_election,_2017
  download.file("https://www.iebc.or.ke/uploads/resources/m3f8arLNjp.pdf", 
                file.path(datapath, "KEN_Election_2017.pdf"))

# Below doesn't work, so we convert the tablues with Tabula.
# elec <- pdf_text(file.path(datapath, "KEN_Election_2017.pdf"))
# First 394 rows are data from the constituencies
  elec <- read_csv(file.path(datapath, "tabula-KEN_Election_2017.csv"), n_max = 394)
  elec_cand <- read_csv(file.path(datapath, "tabula-KEN_Election_2017.csv"), skip = 410, n_max = 9)


# Clean up election results -----------------------------------------------

# First the candidate totals
  elec_cand <- 
    elec_cand[2:9, ] %>% 
    select(-"%AGE OF\rVOTES CAST") %>% 
    select(id = NO.,
           candidate = "NAME OF CANDIDATE",
           votes = "VALID VOTES IN FIGURES",
           counties_25pct = "NUMBER OF COUNTIES THE CANDIDATES HAS ATTAINED AT\rLEAST 25% OF TOTAL VALID VOTES CAST") %>% 
    mutate(votes_tot = sum(votes),
           votes_pct = votes / votes_tot,
           party = case_when(
             candidate == "JOHN EKURU LONGOGGY\rAUKOT" ~ "Thirdway Alliance Kenya",
             candidate == "MOHAMED ABDUBA DIDA"        ~ "Alliance for Real Change",
             candidate == "SHAKHALAGA KHWA JIRONGO"    ~ "United Democratic Party",
             candidate == "JAPHETH KAVINGA KALUYU"     ~ "Independent",
             candidate == "UHURU KENYATTA"             ~ "Jubilee Party of Kenya",
             candidate == "MICHAEL WAINAINA MWAURA"    ~ "Indepdendent",
             candidate == "JOSEPH WILLIAM NTHIGA\rNYAGAH" ~ "Independent",
             candidate == "RAILA ODINGA"               ~ "National Super Alliance")
           )
           

# Second, clean up the constituency level results
  elec_const <- 
    elec %>% 
    select(county_code  = "COUNTY",
           county_name  = "X2",
           consit_code  = "CONST",
           constituency = "X4",
           reg_voters   = "REGISTERED",
           Aukot        = "X6",
           Dida         = "X7",
           Jirongo      = "X8",
           Kaluyu       = "X9",
           Kenyatta     = "X10",
           Mwaura       = "X11",
           Nyagah       = "X12",
           Odinga       = "X13",
           votes        = "TOTAL VALID",
           rej_votes    = "REJECTED") %>% 
    
    # Set ID's so you can use these for the slicing and subsetting below
    mutate(id = seq_len(n()))

# Extract totals for later
  elec_tot <- elec_const %>% 
    filter(is.na(county_code)) %>% 
    filter(!(county_name %in% c("PERCENTAGE (%)", "NATIONAL %AGE"))) %>% 
    # record 13 needs to be dropped, these are values for the constituency of "CHUKA/IGAMBANG'OMBE"
    filter(!is.na(county_name)) %>% 
    # Need to coerce all the voter value columns to be numbers, they are strings as is
   mutate_at(vars(reg_voters:rej_votes), funs(as.numeric(.)))

# Check numbers
  elec_tot %>% 
    filter(county_name != "NATIONAL TOTAL") %>% 
    summarise(tot = sum(reg_voters))

# Record 86 of the elec_const did not read in correctly "CHUKA/IGAMBANG'OMBE". Values from Line 87 need to be moved up one line; Pry not the best way to do this as sort order could change, but works for now.
  y <- as.data.frame(filter(elec_const, id == 86))
  z <- as.data.frame(filter(elec_const, id == 87))
  yz <- coalesce(y, z)
  
  # Rebind to elec_tot after removing rows 86, 87
  elec_const <- 
    elec_const %>% 
    filter(!(id %in% c(86, 87))) %>% 
    rbind(., yz) %>% 
    arrange(id)

    