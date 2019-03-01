# Purpose: Load and process Kenya budget data
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_11_26
# Audience: Kenya Mission


# Load data and crosswalks ---------------------------------------------------------------
source(file.path(rpath, "budget_cw.R"))
source(file.path(rpath, "AHADI_focus_df.R"))

# Reading these in separately to check insheeting carefully. Data have been scraped from PDFs
  #file_name = c("County Budget Database_TE_Edits.xlsx")
  file_name = c("County Budget Database_TE_Edits_HomaBay_Version.xlsx")
  excel_sheets(file.path(budgetpath, file_name))
  
  budget_2014_in <- read_excel(file.path(budgetpath, file_name),
                               sheet = "Budget Nos 14-15")
  
  budget_2015_in <- read_excel(file.path(budgetpath, file_name), 
                               sheet = "Budget Nos 15-16")
  
  budget_2016_in <- read_excel(file.path(budgetpath, file_name), 
                                sheet = "Budget Nos 16-17")
  
  budget_2017_in <- read_excel(file.path(budgetpath, file_name),
                               sheet = "Budget Nos 17-18") 
  
  budget_list <- list(budget_2014_in, budget_2015_in, budget_2016_in, budget_2017_in)
  map(budget_list, ~str(.))

# Need to fix a few columns that are read-in as characters rather than numbers
# Acutally fixed these upstream so that Mission can have original working raw data
  budget_2014 <- budget_2014_in %>% 
    mutate(budget_year = 2014) %>% 
    select(-County)
  
  budget_2015 <- budget_2015_in %>% 
    mutate(budget_year = 2015)
  
  budget_2016 <- budget_2016_in %>% 
    mutate(budget_year = 2016)
  
  budget_2017 <- budget_2017_in %>% 
    mutate(budget_year = 2017)

# Use $ at the end to ensure that we only captures objects that end in "_in
  rm(list = ls(pattern = "*_in$"))
  
# Using previously loaded poverty data, create population figures for 2015, 2016, 2017 and 2018 based on 2.5 % growth rate
  pop_rate = 1.025
  
  pop <- pov %>% 
    mutate(pop_2014 = Population / pop_rate,
           pop_2015 = Population,
           pop_2016 = Population * pop_rate,
           pop_2017 = Population * (pop_rate^2),
           poor_2014 = Number_poor / pop_rate,
           poor_2015 = Number_poor,
           poor_2016 = Number_poor * pop_rate,
           poor_2017 = Number_poor * (pop_rate^2)) %>% 
    select(CID, County, pop_2014:poor_2017) %>% 
    gather(key = "tmp", value = "population", -c(CID, County)) %>% 
    separate(tmp, c("drop", "year"), by = "_") %>%
    mutate(population = round(population, 0)) %>% 
    spread(drop, population) %>% 
    mutate(poor_pop_mil = poor / 1e6,
           pop_mil = pop / 1e6, 
           year = as.numeric(year)) %>% 
    select(-c(County)) %>% 
    filter(CID != 0)

# Caption of data, for later use in graphics
GC_caption = c("Source: USAID GeoCenter Calculations from County Government Budget Implementation Review Reports 2014/15, 2015/16, 2016/17, 2017/18")

bar_formatr <- list(
  geom_col(),
  theme_minimal(), 
  coord_flip()
)


##%######################################################%##
#                                                          #
####                  Data Processing                   ####
#                                                          #
##%######################################################%##

# Create budget database with proper roll-ups and budget shares -----------
# Raw dataset has the original category codes, w/ hand coded mapping to budget categories
budget_raw <- 
  bind_rows(budget_2015, budget_2016, budget_2017, budget_2014) %>%
  left_join(., asal, by = c("CID" = "CID")) %>%  # add in ASAL categories
  left_join(., budget_cw, by = c("Category Code" = "Category Code")) %>%  # Add in budget category crosswalk
  select(CID, # Clean up names and rearrange for ease of viewing
         County = Counties, 
         budget_year, 
         `Category Code`,
         Budget_title, 
         budget_type = `Category.x`, 
         `ASBA Rec`:`Abs Rate Dev`,
         ASAL = `Category.y`,
         ASAL_CODE = Category_num,
         everything()) %>% 
  arrange(CID, budget_type, budget_year) %>% 
  group_by(CID, budget_year, `Category Code`) %>% 
  mutate(budget_category_count = n()) %>% # Count occurences of budget titles to see what is repeating
  ungroup() %>% 
  select(budget_category_count, everything()) %>% 
  arrange(CID, `Category Code`, budget_year) %>% 
  left_join(., pop, by = c("budget_year" = "year", "CID" = "CID")) %>% 
  mutate(Exp_dev_pc = (`Exp Dev` / pop_mil),
         Exp_dev_pc_poor = (`Exp Dev` / poor_pop_mil))

remove(list = ls(pattern = "^budget_[0-9]"))

# checking structure to ensure data types are as expected. Get some wonkiness from pdfs
  str(budget_raw)
  Hmisc::describe(budget_raw)

# Subset the raw totals - These will be used later to compare with my own calculations
# There are quite a few of the totals that are wrong, like Kisumu in 2017 due to a misplaced comma in the pdf
  budget_totals_pdf <-  
    budget_raw %>% 
    filter(`Category Code` == 13) %>% 
    mutate(budget_type = "Total") %>% 
    mutate(Expend_excheq_rec = bs_calc(`Exp Rec`, `Exq Rec`),
           Expend_excheq_dev = bs_calc(`Exp Dev`, `Exq Dev`),
           Absorption_recur = bs_calc(`Exp Rec`, `ASBA Rec`),
           Absorption_dev = bs_calc(`Exp Dev`, ASBADev)) %>% 
    group_by(budget_year) %>% 
    mutate(absorp_rank = rank(Absorption_dev)) %>% 
    ungroup()
  
  # Make a table of the budget errors encountered thus far; Posted to github on the Budget Analysis wiki
  # https://github.com/tessam30/2018_Kenya/wiki/Budget-Analysis
  budget_raw %>% filter(!is.na(Notes)) %>% arrange(CID, budget_year, `Category Code`) %>% 
    select(CID, budget_year, County, Budget_title, budget_type, Notes) %>% 
    knitr::kable()
    
##%######################################################%##
#                                                          #
####                 Visual Exploration                 ####
#                                                          #
##%######################################################%##

# Set ASAL colors an recode it
  
  
# Who planned the most, expended the most? Generalize into a function
  county_look <- function(df, x, barcolor = grey70K) {
    xvar <- enquo(x)
    
    df %>% 
      group_by(County) %>% 
      mutate(tmp = sum(!!xvar, na.rm = TRUE)) %>% 
      ungroup() %>% 
      #mutate(c_sort = County) %>% 
      mutate(c_sort = fct_reorder(County, tmp, .desc = TRUE)) %>% 
      ggplot(aes(x = budget_year, y = !!xvar)) +
      geom_col(fill = barcolor) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(hjust = 0, size = 12)) +
     facet_wrap(~ASAL_CODE + c_sort, 
                 labeller = label_wrap_gen(multi_line = FALSE)) +
      labs(x = "", y = "") #+
      #scale_fill_manual(values = c("#cc4c02", 
      #                             "#fe9929",
      #                             "#fed98e",
      #                             "#ffffd4"))
    }
  
  county_look(budget_totals_pdf, `ASBADev`)
  county_look(budget_totals_pdf, `Exp Dev`) # Kisumu numbers are wrong in 2017; Use manual calculations.
  county_look(budget_totals_pdf, `Exp_dev_pc`) +
    ggtitle("Per capita development expenditures") +
    labs(caption = GC_caption, 
         title = "Marsabit and Isiolo have the highest per capita development expenditures",
         subtitle = "estimates in ")
  
# Map out development expenditures per capita over time  
# Function to create small multiple maps based on budget_totals_pdf data
  budg_map <- function(df, xvar, leg_text = "") {
    xvar <- enquo(xvar) # grab the input text to know what to map
  
    df %>%
      left_join(asal_geo, by = c("CID" = "CID")) %>%
      ggplot(.) +
      geom_sf(aes(fill = !!xvar), colour = "white", size = 0.5) +
      facet_wrap(~budget_year, nrow = 1) +
      scale_fill_viridis_c(option = "A", direction = -1, alpha = 0.75) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.key.width = unit(1.5, "cm")
      ) + # adjust the width of the legend
      labs(
        fill = leg_text,
        caption = GC_caption
      )
  }
  
budg_map(budget_totals_pdf, Absorption_dev, leg_text = "absorption rate")
budg_map(budget_totals_pdf, Exp_dev_pc, leg_text = "Development spending per capita")


      
  # Where are the poor?
  pov_all %>% 
    left_join(asal_geo, by = c("CID" = "CID")) %>% 
    ggplot() +
    geom_sf(aes(fill = Number_poor), colour = "white") +
    scale_fill_viridis_c(option = "A", direction = -1)
  
  pov_all %>% filter(CID != 0) %>% 
    mutate(County = fct_reorder(County, Number_poor)) %>% 
    ggplot(aes(x = County, y = Number_poor)) + geom_col() +
    coord_flip() + theme_minimal()


  
  
##%######################################################%##
#                                                          #
####            Hand crafted budget totals              ####
#                                                          #
##%######################################################%##
  
# Collapsing everything down to standardized budget categories to create budget shares
# Based on yearly totals
# Need to recreate the following variables that are based on two variables  
# The bs_calc takes care of the calculation, just feed in x and y 
# Exp_exq_rec = Exp Rec / Exq Rec 
# Exp_exq_dev = Exp Dev / Exq Dev 
# Absorption_recurring =  Exp Rec / ASBA Rec 
# Absorption_development = Exp Dev / ASBADev 
  
    
  budget <- 
    budget_raw %>% 
      filter(`Category Code` != 13) %>% 
      group_by(CID, `Category Code`, budget_year, Budget_title, County, ASAL, ASAL_CODE) %>% 
      summarise_at(vars(`ASBA Rec`:`Exp Dev`), funs(sum(., na.rm = TRUE))) %>% 
    mutate(Expend_excheq_rec = bs_calc(`Exp Rec`, `Exq Rec`),#  bs_calc function to calculate relative shares
             Expend_excheq_dev = bs_calc(`Exp Dev`, `Exq Dev`),
             Absorption_recur = bs_calc(`Exp Rec`, `ASBA Rec`),
             Absorption_dev = bs_calc(`Exp Dev`, ASBADev)) %>% 
    
    # Create a count for each county / year to see how many categories are in data
    group_by(CID, budget_year) %>% 
    mutate(category_count = n()) %>% 
    ungroup() %>% 
    select(CID, category_count, `Category Code`, budget_year, Budget_title, everything()) %>% 
    
    # Created lagged budget share categories for arrows in tableau and to look at changes
    group_by(CID, `Category Code`) %>% 
    mutate_at(vars(`Exp Dev`, Absorption_dev), .funs = funs(lag = lag(. , n = 1, by = budget_year))) %>% 
    mutate(exp_higher_2016 = `Exp Dev` > `Exp Dev_lag`,
           exp_compare = case_when(
             `Exp Dev` > `Exp Dev_lag` ~ "higher than previous year",
             `Exp Dev` <= `Exp Dev_lag` ~ "lower or same than previous year",
             TRUE ~ NA_character_
           )) %>% 
    ungroup() %>% 
    group_by(CID, budget_year) %>% # Calculate overall absorption rates for recurring and dev
    mutate_at(vars(`ASBA Rec`, ASBADev, `Exp Rec`, `Exp Dev`), .funs = funs(tot_year = sum(., na.rm = TRUE))) %>% 
    mutate(CID_absorption_rec = (`Exp Rec_tot_year`/`ASBA Rec_tot_year`),
          CID_absorption_dev = (`Exp Dev_tot_year`/ASBADev_tot_year)) %>% 
    # Create development expenditure shares as share of total development expenditures
    # This is needed to do share analysis and show relative importance of each category 
    mutate(total_exp_dev = sum(`Exp Dev`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exp_dev_share = bs_calc(`Exp Dev`, total_exp_dev)) %>% 
    left_join(., AHADI_df, by = c("CID")) %>% 
    group_by(AHADI, budget_year) %>% # Calculate AHADI focus absorption rates to see if Mission support makes a diff
    mutate_at(vars(`ASBA Rec`, ASBADev, `Exp Rec`, `Exp Dev`), 
              .funs = funs(AHADI_year = sum(., na.rm = TRUE))) %>% 
    mutate(CID_absorption_rec_AHADI = (`Exp Rec_tot_year`/`ASBA Rec_tot_year`),
           CID_absorption_dev_AHADI = (`Exp Dev_tot_year`/ASBADev_tot_year)) %>% 
    ungroup() %>% 
    left_join(., pop, by = c("CID" = "CID", "budget_year" = "year")) %>%  # Add in population data for per cap budgets
    mutate(tot_dev_exp_pc = (total_exp_dev/pop_mil)) %>% 
    ungroup() %>% 
    group_by(budget_year) %>% 
    mutate(natl_budget_dev = sum(`Exp Dev`, na.rm = TRUE))
  

# AHADI program focused on certain counties, any difference? --------------
# short answer: no, not really.
  
   budget %>% group_by(AHADI, budget_year) %>% 
      summarise(mean = mean(CID_absorption_dev)) %>% spread(AHADI, mean)
  
    # Box plot of 
    ahadi_bud <- budget %>% group_by(County, budget_year, AHADI) %>% 
      summarise(testvar = mean(CID_absorption_dev)) %>% 
      mutate(AHADI = ifelse(AHADI == 1, "Treatment", "Control")) 
    
   # Appears to be no difference in each of the four budget years on absorption rates dev
     ahadi_bud %>% split(.$budget_year) %>% 
     map(~t.test(testvar ~ AHADI, data = .))
    
    ahadi_bud %>% 
      ggplot(aes(x = AHADI, y = testvar)) + geom_boxplot() +
      facet_wrap(~budget_year) + theme_minimal()
    
    # Add in AHADI info an calculate stats on that 
    budget %>% 
      mutate(c_order = fct_reorder(County, CID_absorption_dev)) %>% 
      ggplot(aes(y = CID_absorption_dev, x = c_order, fill = factor(AHADI))) +
    geom_col() + facet_wrap(~ budget_year) + coord_flip()
    

##%######################################################%##
#                                                          #
####                   Budget plots                     ####
#                                                          #
##%######################################################%##
  
  # Scatter plot function to check outliers
  budg_scatter <- function(df, xvar, yvar) {
    xvar <- enquo(xvar)
    yvar <- enquo(yvar)
    
    df %>% 
      ggplot(aes(!!xvar, !!yvar)) + geom_point() +
      theme_minimal()
  }
  
  budg_scatter(budget, CID_absorption_dev, CID_absorption_rec)  + 
    #geom_text(aes(label = str_c(County, " ", budget_year, "\n",Budget_title)),
    #              data = budget %>% filter(`Exq Dev` > 2000 | `Exp Dev` > 1500)) +
    scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1))
  
  # Machakos looks wrong, let's plot with numbers to see; Public Service Boards look really odd
  budget %>% filter(County == "Machakos") %>% 
    ggplot(aes(x = budget_year, y = Budget_title, fill = `Exp Rec`)) +
    geom_tile(colour = "white") + geom_text(aes(label = `Exp Rec`), colour = "white", size = 3) +
    coord_equal() + scale_fill_viridis_c(option = "D", direction = -1)

  budget %>% 
    ggplot(aes(x = CID_absorption_dev, y = CID_absorption_rec, 
               size = total_exp_dev, colour = budget_year)) +
    geom_point(alpha = 0.7) +
    facet_wrap(~ ASAL) +
    geom_text(aes(label = County), size = 3) +
    theme(legend.position = "none")
    #labs(title = 'Budget Year: {frame_time}') +
    #gganimate::transition_time(budget_year) +
    #transformr::ease_aes('linear')
    
  
  
  # Check how many categories are "complete" for each code
  # Need to have a sense of what categories we can trust for comparison over time
  budget %>% group_by(`Budget_title`, budget_year) %>% count() %>% spread(budget_year, n)
    
  budget %>% 
    group_by(Budget_title) %>% 
    nest() %>% 
    mutate(plot = map(data, ~ggplot(., aes(budget_year, y = Absorption_dev))+
                        geom_line(colour = "grey") +
                        geom_point(aes(colour = Absorption_dev)) +
                        scale_colour_viridis_c(direction = -1) +
                        facet_wrap(~ County) +
                        theme_minimal()
    ),
    filename = paste0(Budget_title, ".pdf")
    ) %>% 
    select(filename, plot) %>% 
    pwalk(., ggsave, path = imagepath)
  
# Overall absorption rate from the totals cateogry (# 13)
# Create a new dataset of key budget categories for mapping
  budget_summary <- 
    budget %>% 
    group_by(CID, budget_year, County, ASAL, ASAL_CODE, AHADI) %>% 
    summarise_at(vars(CID_absorption_rec, 
                      CID_absorption_dev,
                      total_exp_dev,
                      tot_dev_exp_pc,
                      poor,
                      ASBADev_tot_year),
                 funs(mean(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    group_by(budget_year) %>% # rank the vars for plotting later
    mutate_at(vars(CID_absorption_rec:tot_dev_exp_pc), .funs = funs(rank = rank(.))) %>% 
    mutate(KEN_exp_dev_tot = sum(total_exp_dev, na.rm = TRUE),
           KEN_exp_dev_ave = mean(total_exp_dev, na.rm = TRUE),
           KEN_ASBA_dev_tot = sum(ASBADev_tot_year, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(CID) %>% 
    mutate(tot_exp_dev_all_years = sum(total_exp_dev, na.rm = TRUE),
           tot_ASBADev_tot_all_years = sum(ASBADev_tot_year, na.rm = TRUE),
           CID_absorption_dev_all_years = (tot_exp_dev_all_years/tot_ASBADev_tot_all_years)) %>% 
    ungroup() 
  
 
# County budget per capita expenditures graphic   

# budget per capita graph -------------------------------------------------


  county_look(budget_summary %>% filter(AHADI %in% c(0, 1)), tot_dev_exp_pc, "#68abb8") + 
    labs(caption = GC_caption, 
         title = "PER CAPITA DEVELOPMENT EXPENDITURES") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(colour = grey20K,
                                    lineend = "square",
                                    size = 0.75),
          #panel.grid.major.y = element_blank(),
          strip.text = element_text(hjust = 0, size = 12))
  
  ggsave(file.path(imagepath, "KEN_PC_Dev_Expend_graph.pdf"),
         plot = last_plot(),
         height = 17, width = 16, units = c("in"), dpi = "retina",
         useDingbats = FALSE)
 
  #d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674 
  
  # One cut of data for per capita expenditures
budget_summary %>% 
    group_by(CID) %>% 
    mutate(tot_dev_exp_pc_ave = (sum(tot_dev_exp_pc)/4)) %>% 
    ungroup() %>% 
    select(CID, budget_year, County, tot_dev_exp_pc, tot_dev_exp_pc_ave) %>% 
    spread(budget_year, tot_dev_exp_pc) %>% 
    rename(tot_dev_exp_pc_2014 = `2014`,
           tot_dev_exp_pc_2015 = `2015`,
           tot_dev_exp_pc_2016 = `2016`,
           tot_dev_exp_pc_2017 = `2017`) %>% 
  write_csv(., file.path(budgetpath, "KEN_budget_dev_exp_pc_ave.csv"))
  
  # left_join(asal_geo, by = c("CID")) %>% 
  # ggplot() + geom_sf(aes(fill = tot_dev_exp_pc_ave), colour = "white", size = .5) +
  # scale_fill_viridis_c(direction = -1, option = "A", alpha = 0.85) +
  # theme_minimal()
  
    
  # export for maps

  
  
    write_csv(budget_summary, file.path(budgetpath, "KEN_budget_summary.csv"))
  
  
# ---- Use this if you need a print produciton quality map    
# Generic mapping function
  budget_map <- function(df, xvar)  {
    xvar = enquo(xvar)
    
    df %>% left_join(., asal_geo, by = c("CID" = "CID")) %>% 
      ggplot() +
      geom_sf(aes(fill = !!xvar), colour = "white", size = 0.5) +
      facet_wrap(~budget_year, nrow = 1) +
      theme_minimal() +
      theme(legend.position = "top",
            legend.key.width = unit(1.25, "cm"))
  }
  
budget_map(budget_summary, tot_dev_exp_pc) +
  scale_fill_viridis_c(option = "A", direction = -1, alpha = 0.75) +
                       #label = percent_format(accuracy = 2)) +
  labs(title = "Counties in the north had the highest per capita development budget expenditures",
          fill = "Development expenditures per capita") 
  ggsave(file.path(imagepath, "KEN_develompent_expenditures_per_capita_maps.pdf"),
         height = 17, width = 16)
  

# Budget Summary plots; Toggle AHADI Filter to get all Counties
  
  budget_summary %>% 
    filter(AHADI %in% c(0, 1)) %>% 
    #filter(ASAL %in% c("Arid - 85-100% Aridity")) %>% 
    mutate(csort = fct_reorder(County, CID_absorption_dev, .desc = TRUE)) %>% 
    #mutate(csort = County) %>% 
    ggplot(aes(x = budget_year, y = CID_absorption_dev)) +
    geom_area(fill = grey10K, alpha = 0.50) +
    geom_line(colour = grey50K) +
    geom_point(aes(fill = CID_absorption_dev), 
               size = 3.5, shape = 21, colour = "white", stroke = 2) + 
    facet_wrap(ASAL_CODE ~ csort, labeller = label_wrap_gen(multi_line = FALSE)) +
    theme_minimal() +
    scale_fill_viridis_c(direction = -1, option = "D") +
    theme(legend.position = "none",
          strip.text = element_text(hjust = 0, size = 9),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.25, "lines")) +
    labs(caption = GC_caption, x = "", y = "",
         title = "Development Absorption Rates By County 2014/15 - 2017/18") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.03), # Be careful in case absorption rate is > 1
                       breaks = c(0, 0.25, 0.50, 0.75, 1))
  ggsave(file.path(imagepath, "KEN_Dev_absorption_rate_timeseries.pdf"),
         plot = last_plot(), dpi = "retina", 
         height = 17, width = 16)
 
#  Check the numbers on the 14 - 17 change map  
tmp <-   budget_summary %>% 
    group_by(CID) %>% 
    mutate(absorp_lag = lag(CID_absorption_dev, n = 3, order_by = budget_year),
           absorb_chg = CID_absorption_dev - absorp_lag) %>% 
  ungroup()

absorp_chg_max = unlist(tmp %>% summarise(max_dev = max(abs(absorb_chg), na.rm = TRUE)))
  
budget_map(tmp %>% filter(budget_year == 2017), absorb_chg) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                       limits = c(-1 * absorp_chg_max, absorp_chg_max), 
                       labels = scales::percent)

  
# Try a 10 X 10 waffle chart to show budget composition by categories
  nrows <- 10
  df_grid <- expand.grid(y = 1:nrows, x = 1:nrows)
  
 grid_stat <-  budget %>% filter(CID == 1 & budget_year == 2014) %>%
    select(Budget_title, exp_dev_share) %>% 
   mutate(exp_dev_share = round(exp_dev_share * 100, 0)) %>% 
   uncount(exp_dev_share) %>% 
   cbind(df_grid)
  
library(waffle) # may be a compact way of showing budget shares across time


  
# TODO - CLEAN up below


# Incorrect budget totals -------------------------------------------------
 # Kisumu in 2017 should be 70.8 for City of Kisumu Development Expenditures - still off even after this
 # Wajir 2015/16 development expenditures do not add up to pdf total
 # Isiolo 2017/18 developmen expenditures do not add up
 # Tana River in 2017/18 is off due to a special program
 # Kirinyaga development expenditures do not add up in 2017/18 
 # Nyeri totals are 1,140,315,329 in 2017 for development expenditures, but line item budget only sums to 841
    
    
# Show which counties do not have categories across all 12 categories (47 X 12 matrix basically)
    budget %>% 
      filter(`Category Code` != 0) %>% 
      group_by(County, Budget_title) %>% 
      count() %>% 
      mutate(count = as.character(n)) %>% 
      ggplot(aes(x = Budget_title, y = County )) + 
      geom_tile(aes(fill = count), colour = "white") +
      scale_fill_brewer(palette = 13, direction = 1) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "top") +
      labs(title = "Health is the most consistent budget category across 2014/15 - 2017/18",
           y = "", x = "", caption = "Source: 2014/15, 2015/16, 2016/17 & 2017/18 Budget Data",
           fill = "Number of times budget category appears") +
      coord_flip() + 
      ggsave(file.path(imagepath, "Budget_summary.pdf"), width = 11, height = 5) 
      
# Or the purr way - saving all plots as pdfs
budget %>% 
    group_by(Budget_title) %>% 
    nest() %>% 
    mutate(plot = map(data, ~ggplot(., aes(budget_year, y = Absorption_dev))+
                        geom_line(colour = "grey") +
                        geom_point(aes(colour = Absorption_dev)) +
                        scale_colour_viridis_c(direction = -1) +
                        facet_wrap(~ County) +
                        theme_minimal()
                      ),
           filename = paste0(Budget_title, ".pdf")
           ) %>% 
    select(filename, plot) %>% 
  pwalk(., ggsave, path = imagepath)

# Heatmap maybe just as effective for showing development absorption rates across time
  budget %>% 
    filter(!is.na(Absorption_dev) & Budget_title != "Uncategorized") %>% 
    ggplot(aes(y = Budget_title, x = County)) +
    geom_tile(aes(fill = Absorption_dev), colour = grey60K) +
    facet_rep_wrap(~ budget_year, nrow = 4, repeat.tick.labels = TRUE) + 
    coord_equal() + 
    scale_fill_viridis_c(option = "A", alpha = 0.9, direction = -1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
    
# Budget notes to be resolved - updated in github on 2/4/2019
    # Homa Bay 2015 Econ Growth (3) really was 18.34
    # Machakos jumps to nearly 2 in 2016 for Transport and Infrastructure (4)
    # Tana River is very high in 2015 for Econ Growth (5); Tharaki Nithi also worth checking out
    # Kisumu and Nandi are high for Education in 2016, and 2017 respectively
    # Uasin Gishu shoots up in 2016 for Health (7); Should the 19.3 really be 193? ~ Numbers match if yes.
    # Kisumu Agriculture shoots up in 2017 (9) to nearly 4 --> see table Table 3.51 in the 2017/18 report pp141/120
    # Embu in 2016 is odd at over 60; Table Table 3.18 was scraped incorrectly in 2016/17. Need to verify everythinng


  

##%######################################################%##
#                                                          #
####          Comparing Budget Totals Part II           ####
#                                                          #
##%######################################################%##

# Now create GeoCenter totals to compare with PDFs from the Controller's Office Official Reports
# First, wrangle the 2014/15 data as it had to be scraped in a vector
  budg_tot_14 <- read_excel(file.path(budgetpath, "KEN_FY2014_15_budget_raw.xlsx"),
                            col_names = FALSE) %>% 
    rename(col_1 = "..1") %>% 
    mutate(flag = ifelse(str_detect(col_1, "[a-z]"), 1, 0))
  
# Subet counties, keepingin order, then convert remaining data into a matrix, reshape to be a 
  county14_order <- budg_tot_14 %>% filter(flag == 1)
  county14_nums  <- budg_tot_14 %>% filter(flag != 1) %>% select(-flag) %>% mutate(col_1 = as.numeric(col_1))
  mtx <- as.matrix(county14_nums)
  
  budget_14_tot <- matrix(mtx, nrow = 48, byrow = TRUE) %>% as_tibble() %>% 
    cbind(county14_order, .) %>% 
    mutate(year = 2014,
           Fyear = "2014-2015") %>% # make it compatible with the csv we read in below
    mutate_at(vars(V1:V9), funs(. / 1e6)) %>% 
    select(-c(V3, V6, V9, flag)) %>% 
    rename(`Rec_Budget Estimates` = V1,
           `Dev_Budget Estimates` = V2,
           `Rec_Exchequer` = V4,
           `Dev_Exchequer` = V5,
           `Rec_Expenditure` = V7,
           `Dev_Expenditure` = V8,
           County = col_1) %>% 
    mutate(`Exp_Exq Rec` = (Rec_Expenditure / Rec_Exchequer),
           `Exp_Exq Dev` = (Dev_Expenditure / Dev_Exchequer),
           `Recurrent Absorption Rate (%)` = (Rec_Expenditure / `Rec_Budget Estimates`),
           `Development Absorption Rate (%)` = (Dev_Expenditure / `Dev_Budget Estimates`),
           `Total_Budget Estimates` = `Rec_Budget Estimates` + `Dev_Budget Estimates`,
           Total_Expenditure = `Rec_Expenditure` + `Dev_Expenditure`) %>% 
    filter(County != "Total") %>% 
    left_join(., (county_list %>% select(CID, Counties)), by = c("County" = "Counties")) %>% 
    group_by(year) %>%
    mutate(tot_ASBArec = sum(`Rec_Budget Estimates`, na.rm = TRUE),
           tot_ASBAdev = sum(`Dev_Budget Estimates`, na.rm = TRUE),
           totl_exp_rec = sum(Rec_Expenditure, na.rm = TRUE),
           tot_exp_dev = sum(Dev_Expenditure, na.rm = TRUE),
           `Overall Absorption Rate` = (tot_exp_dev / tot_ASBAdev),
           `Overall Absorption Rate Recurring` = (totl_exp_rec / tot_ASBArec)) %>% 
    select(-c(tot_ASBArec, tot_ASBAdev, totl_exp_rec, tot_exp_dev))
             
  # Read in County Totals from Eric K.’s Github Account ---------------------

  
  
  
County_budget_allocation <- "https://raw.githubusercontent.com/kabuchanga/KE-budgetsDB/master/data_final/KEN_county_budget_totals_15-18.csv"

county_BA <- read_csv(County_budget_allocation) %>%
  mutate_at(vars(contains("Rate")), funs(. / 100)) %>%
  left_join(asal, by = c("CID" = "CID")) %>%
  mutate(year = case_when(
    FYear == "2015-2016" ~ 2015,
    FYear == "2016-2017" ~ 2016,
    FYear == "2017-2018" ~ 2017,
    TRUE ~ NA_real_
  )) %>%
  arrange(CID, year) %>% 
  bind_rows(., budget_14_tot) %>% 
  select(-c(Counties, Category)) 

budget_totals_GOK <- 
  county_BA %>% 
  group_by(CID) %>%
  mutate(
    prv_year_absorb = lag(`Overall Absorption Rate`, n = 1, order_by = year),
    prv_2year_absorb = lag(`Overall Absorption Rate`, n = 2, order_by = year),
    absorb_delta = `Overall Absorption Rate` - prv_year_absorb,
    tot_absorb_delta = `Overall Absorption Rate` - prv_2year_absorb,
    County2 = County
  ) %>%
  ungroup() %>% 
  select(-County) %>%
  left_join(., b_gc_pdf, by = c("CID" = "CID", "year" = "budget_year")) %>%
  select(County, Dev_Expenditure, total_exp_dev, `Exp Dev`, diff, everything()) %>%
  mutate(final_check = (Dev_Expenditure - total_exp_dev) %>%
    round(., 2)) %>%
  select(CID, County, year, final_check, diff, everything()) %>%
  arrange(final_check, diff) 

budget_totals_GOK %>% 
  filter(abs(final_check) > 1) %>% 
  select(County, final_check, year) %>% 
  mutate(group = ifelse(final_check > 0, "Positive", "Negative"), 
         csort = fct_reorder(County, final_check, .desc = TRUE)) %>% 
  ggplot(aes(x = csort, y = final_check, fill = group)) +
  geom_col() + coord_flip() + theme_minimal() +
  scale_fill_manual(values = c("Positive" = "#8073ac", "Negative" = "#e08214")) +
  labs(x = "", y = "Difference between GeoCenter totals (brown) and GOK Calculations (purple)",
       caption = "GeoCenter Calculations from Official GOK Budget Documents",
       title = "GeoCenter calculations differ from budget summary tables") +
  facet_wrap(~year) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"))


rm(budget_2014, budget_2015, budget_2016, budget_2017, budg_tot_14, mtx, county14_nums, county14_order)
#TODO # Plot three calculations for those with discrepancies larger than 5


##%######################################################%##
#                                                          #
####              Budget Election Analysis              ####
#                                                          #
##%######################################################%##
elec_county %>% 
  mutate(year = ifelse(YEAR == 2013, 2014, 2016)) %>% 
  right_join(budget_totals_GOK, by = c("CID" = "CID", "year" ="year")) %>% 
  filter(year %in% c(2014, 2016)) %>%
  ggplot(aes(x = TURNOUT_COUNTY, y = `tot_dev_exp_pc`, colour = factor(year), label = County)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  scale_x_symmetric(mid = 0.75) + theme_minimal() +
  geom_text() + facet_grid(~year)


# Tabular summary of absorption rates by county
county_BA %>%
  group_by(County) %>%
  mutate(ave_abs_rate = mean(`Overall Absorption Rate`)) %>%
  ungroup() %>%
  select(County, year, ave_abs_rate, `Overall Absorption Rate`) %>%
  spread(year, `Overall Absorption Rate`) %>%
  arrange(-ave_abs_rate) %>%
  print(n = 47)


yabsorp_max <- unlist(county_BA %>%
  ungroup() %>%
  summarise(max_dev = max(abs(absorb_delta), na.rm = TRUE)))

county_BA %>%
  left_join(asal_geo, by = c("CID" = "CID")) %>%
  ggplot(.) +
  geom_sf(aes(fill = absorb_delta), colour = "white", size = 0.05) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(11, "PiYG"),
    limits = c(-1 * absorp_max, absorp_max),
    labels = scales::percent
  ) +
  facet_wrap(~year)
  

##%######################################################%##
#                                                          #
####              Export data for Tableau               ####
#                                                          #
##%######################################################%##

  budget <- 
    budget %>% 
    left_join(., pov_all, by = c("CID")) %>% 
    left_join(., asal, by = c("CID")) %>% 
    select(-County.y) %>% 
    rename(County = County.x)

  export_list <- list(KEN_budget_2015_2018 = budget,
                      KEN_budget_totals_2015_2018 = budget_totals_GC,
                      county_BA = county_BA, 
                      KEN_budget_raw_2015_2018 = budget_raw)

  export_list %>%  
    names() %>% 
    map(., ~ write_csv(export_list[[.]], file.path(budgetpath, str_c(., ".csv"))))  




