##%######################################################%##
#                                                          #
####                     MSME Data                      ####
#                                                          #
##%######################################################%##
# Purpose: Plot MSME data from report on maps/graphics
# Authoer: Tim Essam, Ph.D
# Date: 2019_02_19

library(rlang)
msme <- readxl::read_excel(file.path(datapath, "MSME", "KEN_MSME_2016.xlsx"))

msme_geo <- msme %>% left_join(., asal_geo, by = c("CID" = "CID"))

msme_geo_long <- 
  msme_geo %>% 
  gather(table, 
         stat, 
         `Table 3_1 Sampled Licensed Establishments`:`Table 7_10 Employees at close Average per establishment`)

msme_plot <- function(df, x, option = 1) { 
  if (!option %in% c(1, 2)) {
    stop("Select 1 (map) or 2 (graph) as option value.")
  }
  
  xvar = enquo(x)
  
  if (option == 1) {
      ggplot(df) +
      geom_sf(aes(fill = !!xvar), colour = "white") +
      scale_fill_viridis_c(option = "C", direction = -1, alpha = 0.90) +
      theme_minimal() +
      labs(caption = "GeoCenter Calculations from MSME 2016 Report",
           title = (gsub("`", "", {rlang::quo_text(xvar)}))) 
  }
  
 else if (option == 2) {
    df %>% 
      mutate(sortvar = fct_reorder(County, !!xvar), .desc = TRUE) %>% 
      ggplot(aes(x = sortvar, y = !!xvar)) +
      geom_col() +
      coord_flip() + theme_minimal() +
      labs(caption = "GeoCenter Calculations from MSME 2016 Report",
           title = (gsub("`", "", {rlang::quo_text(xvar)})))
   }
}


msme_plot(msme_geo, `Table 3_1 Sampled Licensed Establishments`, option = 1)

#tmp <- names(msme_geo) %>% str_subset(pattern = "^Table") 
# Loop over  numeric columns to batch create maps
msme_geo %>% select_if(is.numeric) %>% select(-CID) %>%  
  map(~msme_plot(msme_geo, ., 1))

# Small multiples of every statistic
 msme_geo_long %>% 
   filter(grepl("Table 5_", table)) %>% 
   ggplot() +
   geom_sf(aes(fill = stat), colour = "white") +
   scale_fill_viridis_c(direction = -1, alpha = 0.85) +
   theme_minimal() +
   facet_wrap(~ table)

