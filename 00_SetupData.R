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

#Source helper functions
source("strip_geom.R")

# Read in constituency data
# https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_KEN_shp.zip
  gis_admin2 <- read_sf(file.path(gispath, "gadm36_KEN_2.shp"))

# SEE here for older data --> https://github.com/mikelmaron/kenya-election-data  
  
# Download election results from Kenya Election Comission
# Per wikipedia -- results may not be up to date 
# https://en.wikipedia.org/wiki/Kenyan_general_election,_2017
# download.file("https://www.iebc.or.ke/uploads/resources/m3f8arLNjp.pdf", 
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
           

# Second, clean up the constituency level results check the info on candidates
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
  # Set aside the codes for now
  elec_codes <- 
    elec_const %>% 
    select(county_code:constituency) %>% 
    filter(!is.na(county_code))
  
  
  elec_tot <- 
    elec_const %>% 
    filter(!is.na(county_code), county_name != "COUNTY_NAME")%>% 
    #filter(!(county_name %in% c("PERCENTAGE (%)", "NATIONAL %AGE"))) %>% 
    # record 13 needs to be dropped, these are values for the constituency of "CHUKA/IGAMBANG'OMBE"
    # Need to coerce all the voter value columns to be numbers, they are strings as is
    mutate_at(vars(reg_voters:rej_votes), funs((gsub(',', '', .)))) %>% 
    mutate_at(vars(reg_voters:rej_votes), funs((gsub("-", '', . )))) %>% 
    mutate_at(vars(reg_voters:rej_votes), funs(as.numeric(.)))
  
  # ORiginally had this chunk below, not sure why
  # Extract totals for later
  # elec_tot <- elec_const %>% 
  #   filter(is.na(county_code)) %>% 
  #   filter(!(county_name %in% c("PERCENTAGE (%)", "NATIONAL %AGE"))) %>% 
  #   # record 13 needs to be dropped, these are values for the constituency of "CHUKA/IGAMBANG'OMBE"
  #   filter(!is.na(county_name)) #%>% 
  # Need to coerce all the voter value columns to be numbers, they are strings as is
  # mutate_at(vars(reg_voters:rej_votes), funs(as.numeric(.)))
  
  

  str(elec_tot)

# Check numbers
  elec_tot %>% 
    group_by(county_name) %>% 
    summarise(tot = sum(reg_voters))

# Record 86 of the elec_const did not read in correctly "CHUKA/IGAMBANG'OMBE". Values from Line 87 need to be moved up one line; Pry not the best way to do this as sort order could change, but works for now.
  y <- as.data.frame(filter(elec_const, id == 86))
  z <- as.data.frame(filter(elec_const, id == 87))
  yz <- coalesce(y, z)
  
  # Rebind to elec_tot after removing rows 86, 87; 
  elec_results <- 
    elec_const %>% 
    filter(!(id %in% c(86, 87))) %>% 
    rbind(., yz) %>% 
    arrange(id) %>% 
    filter(!is.na(county_code), county_name != "COUNTY_NAME") %>% 
    
    # Need to strip out commas b/c when you coerce as a number, it converts many to NA
    mutate_at(vars(reg_voters:rej_votes), funs((gsub(',', '', .)))) %>% 
    mutate_at(vars(reg_voters:rej_votes), funs(as.numeric(.))) %>% 
    mutate(NAME_1 = str_to_title(constituency),
           CC_2 = as.character(as.numeric(consit_code)))
  
  # Reshape the candidates so we can small multiple the results on a series of maps
  elec_results_long <- 
    elec_results %>% 
    gather(., 
           key = "Candidate", 
           value = "votes",
           Aukot:Odinga) %>% 
    group_by(Candidate) %>% 
    mutate(tot_votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(map_sort = fct_reorder(Candidate, -tot_votes)) %>% 
    group_by(constituency) %>% 
    mutate(tot_votes_const = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(const_share = votes / tot_votes_const)
  
  
  
  # Next step is to attemp to join to to the shapefile 
  # In the elce results, need to fix 100 obs that start with zero
  admin2_df <- strip_geom(gis_admin2, -geometry)
  
  
  map(list(elec_results, gis_admin2), ~ str(.))
  # Things to note: 
  # in geom_sf --> lwd controls stroke on polygons, 
  # "col" is polygon color
  p2 <- 
    gis_admin2 %>%
    left_join(., elec_results_long, by = c("CC_2")) %>%
    filter(Candidate %in% c("Kenyatta", "Odinga")) %>%
    ggplot(.) +
    geom_sf(
      lwd = 0.1, col = "white",
      aes(fill = Candidate, alpha = const_share)
    ) +
    facet_wrap(~map_sort, nrow = 1) +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  ggsave("Kenya_results.pdf",
    plot = p2
  )


  write_csv(
    elec_results_long,
    file.path(
      datapath,
      "KEN_election_results.csv"
    )
  )
  
  # Highlighting the administrative boundaries that do not have constituency codes
  fixme <- anti_join(elec_results, gis_admin2, by = c("CC_2"))
  
  p <- 
    ggplot(gis_admin2) +
    geom_sf(
      lwd = 0.3, col = "white", aes(fill = NAME_2, colour = "grey"),
      alpha = 0.5
    ) +
    scale_fill_viridis_d(option = "A") +
    theme(legend.position = "none") +
    labs(title = "Kenya constituencies ugly map")
  ggsave("Kenya_constituencies.pdf",
    plot = p2
  )
    
  # To be done: Convert constituency information to numeric for merging
  # Convert constituency names to proper for merging/match too
  # Merge data, check if constituency values even make sense
  

   # TODO: Determine what maps/viz's are most needed by customer.
