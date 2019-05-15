# Purpose: PLook at proposed J2SR metrics
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_05_14
# Audience: Kenya Mission

# Look at Brent's attempt at J2RS index -----------------------------------
library(tidyverse)
library(broom)
library(knitr)
library(dendextend) # for comparing two dendrograms
library(ggfortify)
library(gridExtra)
library(ggpubr)
library(ggdendro)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)  # 30 indices for determining number of clusters
library(sparcl) # add color to dendograms

# PCA code modified from https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/

j2sr <- read_excel(file.path(datapath, "J2SR_draft.xlsx"), sheet = "Raw Data")

omit_vars <- c("CID", "Change14-17", "Any_Shock", "Ag_shocks",
               "ACLED_No_Events", "Veg_Ch", #"Sanitation",
               "DistributionPov", "Poverty_gap",
               "Severity_of_poverity", "Number_poor",
               "Wasting_dhs_2014", "Stunting_dhs_2014",
               "Employment in Licensensed MSMEs", "youth_literate_male",
               "youth_literate_female")


# Run pca on j2sr proposed data, save results in nested df for easier access
j2sr_pca <- 
  j2sr %>% 
  #select(-CID, -Veg_Ch) %>% 
  select(-omit_vars) %>% 
  #filter(County != "Nairobi City") %>% 
  nest() %>% 
  mutate(pca = map(data, ~prcomp(.x %>% 
                                 select(-County), 
                                 center = TRUE, 
                                 scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

# Create a table of the variance explained for scree plots etc
var_exp <- j2sr_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains(".fitted")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""),
         pc_num = parse_number(pc))

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc_num, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")


j2sr_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.label = "County",
                 label.repel = TRUE) +
        theme_minimal() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on select variables of J2SR dataset")
    )
  ) %>%
  pull(pca_graph)

ggsave(file.path(imagepath, "KEN_J2SR_pca_results_R.pdf"),
       plot = last_plot(),
       device = "pdf",
       height = 8.5, width = 11, dpi = 300, 
       useDingbats = FALSE)

# What does a potential index look like?


pca_max = unlist(j2sr_pca[[3]][[1]] %>% summarise(max_dev = max(abs(`.fittedPC1`))))  
  
pca_index <- j2sr_pca[[3]][[1]] %>% 
  mutate(index = (-1) * `.fittedPC1`, 
    county_sort = fct_reorder(County, index)) %>% 
  ggplot(aes(x = county_sort, y = index, fill = index)) + geom_col() +
  coord_flip() +
  labs(x = "Proposed Index",
       y = "",
       title = "PCA results reconfirm the differences among Nairobi, the North, and the rest of Kenya") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                       limits = c(-1 * pca_max, pca_max)) 


pca_map <- j2sr_pca[[3]][[1]] %>% 
  mutate(index = (-1) * `.fittedPC1`, 
         county_sort = fct_reorder(County, index),
         County = ifelse(County == "Nairobi City", "Nairobi", County)) %>% 
  left_join(asal_geo, by = c("County" = "Counties")) %>% 
  ggplot() +
  geom_sf(aes(fill = index), colour = "white") +
  theme_minimal() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                       limits = c(-1 * pca_max, pca_max)) +
  theme(legend.position = "top")

pca_index_plot <- ggarrange(pca_index, pca_map, ncol = 2) 
ggsave(file.path(imagepath, "KEN_pca_index_j2SR_R.pdf"),
       plot = pca_index_plot,
       device = "pdf",
       height = 8.5, width = 11, dpi = 300, 
       useDingbats = FALSE)

# Cluster analysis of countied ---------------------------------------------


# https://uc-r.github.io/kmeans_clust

j2sr_scaled <- 
  j2sr %>%
  select(-omit_vars) %>% 
  column_to_rownames('County') %>% 
  #select(-CID, -Veg_Ch) %>% 
  scale()
str(j2sr_scaled)  
summary(j2sr_scaled)

distance <- get_dist(j2sr_scaled)
fviz_dist(distance, gradient = list(low = "#3288bd", mid = "white", high = "#d53e4f"))

# The package has spoken, 3 is the best number of clusters
res <- NbClust(j2sr_scaled, diss = NULL, distance = "euclidean", 
               min.nc = 2, max.nc = 10, method = "complete", 
               index = "alllong", alphaBeale = 0.1)

k3 <- kmeans(j2sr_scaled, centers = 3, nstart = 25)

cluster_scatter <- fviz_cluster(k3, data = j2sr_scaled, repel = TRUE) + theme_minimal() +
  scale_colour_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) +
  scale_fill_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) +
  theme(legend.position = "top") #+
  #labs(title = "Based on the J2SR data, counties cluster into three major groups",
       #subtitle = "Nairobi City is essentially in a class of its own",
       #caption = "GeoCenter calcuations of  based on K-means clustering algorithm")


cluster_df <- k3$cluster %>% 
  tibble::enframe("County", "cluster") %>% 
  mutate(CID = seq(from = 1, to = n(), by = 1))

cluster_map <- 
  cluster_df %>% 
  left_join(., asal_geo, by = c("CID")) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(cluster)), alpha = 0.75, colour = "white") +
  scale_fill_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) +
  theme_minimal() +
  theme(legend.position = "off")

cluster_plot <- ggarrange(cluster_map, cluster_scatter, ncol = 2) %>% 
  #annotate_figure(., fig.lab = "Turkana and Kitui, on average, had the most humanitarian caseloads from 2004 - 2018")
  annotate_figure(., fig.lab = "K-means cluster analysis divides the counties into three major groups based on the J2SR proposed variables")


  ggsave(file.path(imagepath, "KEN_J2SR_cluster_analysis_R.pdf"),
         plot = cluster_plot,
         device = "pdf",
         height = 8.5, width = 11, dpi = 300, 
         useDingbats = FALSE)


# To calculate plots manually if ever needed
  # Map over 4 cluster dimensions
# clust_num <- list(2, 3, 4, 5)
# clust_df <- map(2:10, ~kmeans(j2sr_scaled, centers = ., nstart = 25))
# 
# # Looking for the kink in the total within-cluster sum of squares
# set.seed(20190514)
# 
# # function to compute total within-cluster sum of square 
# wss <- function(k) {
#   kmeans(j2sr_scaled, k, nstart = 10 )$tot.withinss
# }
# 
# set.seed(20190514)
# fviz_nbclust(j2sr_scaled, kmeans, method = "wss")
# fviz_nbclust(j2sr_scaled, kmeans, method = "silhouette")
# 
# gap_stat <- clusGap(j2sr_scaled, FUN = kmeans, nstart = 25,
#                     K.max = 10, B = 50)
# print(gap_stat, method = "firstmax")
# fviz_gap_stat(gap_stat)


# Hierarchical clustering -------------------------------------------------

# https://uc-r.github.io/hc_clustering

d <- dist(j2sr_scaled, method = "euclidean")

# Assess all the agglomerative methods
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
  
# function to compute coefficient
  ac <- function(x) {
    agnes(j2sr_scaled, method = x)$ac
  }
  
map_dbl(m, ac)
##   average    single  complete      ward 
## 0.7379371 0.6276128 0.8531583 0.9346210
  

# Hierarchical clustering using Complete Linkage
# This is the model we use below for the graphics
hc1 <- hclust(d, method = "ward.D2")

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1) 


# DIANA method - starts at the top and works down
# compute divisive hierarchical clustering
hcd <- diana(j2sr_scaled)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hcd, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Box clusters
# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 6)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

fviz_cluster(list(data = j2sr_scaled, cluster = sub_grp))

#convert cluster object to use with ggplot
dendr <- dendro_data(hc1, type = "rectangle") 

xy_pos <- dendr$labels %>% as_tibble()
# Need to merge the categories to the x / y position to plot colored labels
hc_cut <- cutree(hc1, 6) %>% 
  enframe() %>% 
  left_join(xy_pos, by = c("name" = "label"))
  
  

#your own labels (now rownames) are supplied in geom_text() and label=label
# p1 <- ggplot() + 
#   geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) +
#   geom_text(data = hc_cut, aes(x = x, y = y, label = name, hjust = 0, 
#                                colour = factor(value))) +
#   #geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
#   coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
#   theme(axis.line.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.title.y=element_blank(),
#         panel.background=element_rect(fill="white"),
#         panel.grid=element_blank()) 


dend_graphic <- ggdendrogram(hc1, rotate = TRUE, labels = FALSE) + 
  theme_minimal() +
  geom_text(data = hc_cut, 
            aes(x = x, y = (y - 3), 
                label = name, 
                hjust = 0, 
                colour = factor(value))) +
  scale_color_manual(values = c('#fb8072','#80b1d3','#fdb462','#b3de69','#bc80bd',
                                '#d9d9d9')) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "",
       y = "") 

hc_map <- 
  hc_cut %>% 
  mutate(name = ifelse(name == "Nairobi City", "Nairobi", name)) %>% 
  left_join(asal_geo, by = c("name" = "Counties")) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(value)), colour = "white", alpha = 0.75) +
  scale_fill_manual(values = c('#fb8072','#80b1d3','#fdb462','#b3de69','#bc80bd',
                               '#d9d9d9')) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Hierarchical cluster analysis yields similar results to other methods",
       subtitle = "Overall dendogram grouped to six branches",
       caption = "GeoCenter calculations based on proposed J2SR index variables.",
       fill = "Selected clusters")

  
dendo_plot <- ggarrange(hc_map, dend_graphic, ncol = 2) 

ggsave(file.path(imagepath, "KEN_J2SR_hc_analysis_R.pdf"),
       plot = dendo_plot,
       device = "pdf",
       height = 8.5, width = 11, dpi = 300, 
       useDingbats = FALSE)






