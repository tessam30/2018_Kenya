# Kenya helper functions for election data and poverty mapping
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_11
# Audience: Kenya Mission

# Build a function to join data, and plot results based on variable input
# Inputs df = data frame with flat file data
#        x = variable to be mapped (percentage)
#        title = plot title
#       ... = facet variables

pov_plot <- function(df, x, title, ...) {
  
  # Define plot variables and faceting
  x_quo <- enquo(x)
  args <- quos(...)
  
  gis_admin1 %>%
    left_join(., 
              df,
              by = c("GID_1")) %>%
    ggplot(.) +
    geom_sf(
      lwd = 0.1,
      col = "white",
      aes(fill = !!x_quo)
    ) +
    facet_wrap(args,
               nrow = 2
    ) +
    scale_fill_viridis_c(
      option = "D",
      alpha = 0.80,
      direction = -1
    )+
    theme_basic() +
    labs(title = title, 
         caption="Source: Kenya 2016 IHBS") +
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          legend.key = element_blank())
}

# pov_plot(pov_child, poverty, "Kenya Child poverty (%) by county", age)

add_metadata <- function(df) {
  meta_df <- Hmisc::contents(hh_inf)$contents %>% 
    rownames_to_column()
  return(meta_df)
}
