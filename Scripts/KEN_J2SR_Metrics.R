# Purpose: PLook at proposed J2SR metrics
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_05_14
# Audience: Kenya Mission

# Look at Brent's attempt at J2RS index -----------------------------------
library(tidyverse)
library(broom)
library(knitr)
library(ggfortify)

# PCA code modified from https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/

j2sr <- read_excel(file.path(datapath, "J2SR_draft.xlsx"), sheet = "Raw Data")

omit_vars <- c("CID", "Change14-17", "Any_Shock", "Ag_shocks",
               "ACLED_No_Events", "Veg_Ch", #"Sanitation",
               "DistributionPov", "Poverty_gap",
               "Severity_of_poverity", "Number_poor",
               "Wasting_dhs_2014", "Stunting_dhs_2014",
               "Employment in Licensensed MSMEs", "youth_literate_male",
               "youth_literate_female")

j2sr_pca <- 
  j2sr %>% 
  select(-CID, -Veg_Ch) %>% 
  #select(-omit_vars) %>% 
  #filter(County != "Nairobi City") %>% 
  nest() %>% 
  mutate(pca = map(data, ~prcomp(.x %>% 
                                   select(-County), 
                                 center = TRUE, 
                                 scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

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
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on J2SR dataset")
    )
  ) %>%
  pull(pca_graph)



# Cluster analysis of countie ---------------------------------------------
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)  # 30 indices for determining number of clusters

# https://uc-r.github.io/kmeans_clust


j2sr_scaled <- 
  j2sr %>%
  #select(-omit_vars) %>% 
  column_to_rownames('County') %>% 
  select(-CID, -Veg_Ch) %>% 
  scale()
str(j2sr_scaled)  
summary(j2sr_scaled)

distance <- get_dist(j2sr_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# The package has spoken, 3 is the best number of clusters
res <- NbClust(j2sr_scaled, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
               method = "complete", index = "alllong", alphaBeale = 0.1)


k3 <- kmeans(j2sr_scaled, centers = 3, nstart = 25)

fviz_cluster(k3, data = j2sr_scaled, repel = TRUE) + theme_minimal() +
  scale_colour_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) +
  scale_fill_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) +
  theme(legend.position = "top",) +
  labs(title = "Based on the J2SR data, counties cluster into three major groups",
       subtitle = "Nairobi City is essentially in a class of its own",
       caption = "GeoCenter calcuations based on K-means clustering algorithm")

cluster_df <- k3$cluster %>% 
  tibble::enframe("County", "cluster") %>% 
  mutate(CID = seq(from = 1, to = n(), by = 1))

cluster_df %>% 
  left_join(., asal_geo, by = c("CID")) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(cluster)), alpha = 0.60) +
  scale_fill_manual(values = c("#8dd3c7", "#fb8072", "#80b1d3")) 





# Map over 4 cluster dimensions
clust_num <- list(2, 3, 4, 5)
clust_df <- map(2:10, ~kmeans(j2sr_scaled, centers = ., nstart = 25))

# Looking for the kink in the total within-cluster sum of squares
set.seed(20190514)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(j2sr_scaled, k, nstart = 10 )$tot.withinss
}

set.seed(20190514)
fviz_nbclust(j2sr_scaled, kmeans, method = "wss")
fviz_nbclust(j2sr_scaled, kmeans, method = "silhouette")

gap_stat <- clusGap(j2sr_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)






