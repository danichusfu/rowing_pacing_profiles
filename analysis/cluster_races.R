#####################
##### PACKAGES ######
#####################
# If pacman is not installed, install it
# install.packages('pacman')
pacman::p_load(tidyverse, glue, lubridate, dtwclust)

races_full <- read_csv("rowing_world_championships.csv")

races_cleaned <-
  races_full %>%
  # if a race as a speed which is na or less than 2 throw out the whole race
  # there are gps errors where the average speed recorded is less than the true speed
  # this may be changed later but is a good baseline
  filter_at(vars(matches("speed")), all_vars(!is.na(.) & . > 2)) %>%
  filter(dnf == FALSE, exc == FALSE) %>%
  mutate(row_number = row_number())

races_long <- 
  races_cleaned %>%
  select(matches("speed_\\d{2}$"), matches("speed_\\d{3}$"), matches("speed_\\d{4}$"), row_number) %>%
  gather("distance", "speed", -row_number) %>%
  mutate(distance = parse_number(distance))

races_standardized <-
  races_long %>%
  group_by(row_number) %>%
  mutate(speed_standard = scale(speed)) %>%
  ungroup() %>% 
  select(-speed) %>%
  mutate(distance = paste0("speed_", distance)) %>%
  spread(distance, speed_standard) %>%
  select(row_number, matches("speed_\\d{2}$"), matches("speed_\\d{3}$"), matches("speed_\\d{4}$"))

cluster_matrix <-  
  races_standardized %>%
  select(-row_number) %>%
  as.matrix()

set.seed(14)
# k shape clustering
# http://www1.cs.columbia.edu/~jopa/Papers/PaparrizosSIGMOD2015.pdf
clusters_4 <- tsclust(cluster_matrix, k = 4, centroid = "shape", distance = "sbd")
clusters_3 <- tsclust(cluster_matrix, k = 3, centroid = "shape", distance = "sbd")
clusters_5 <- tsclust(cluster_matrix, k = 5, centroid = "shape", distance = "sbd")

cluster_centroids <- 
  tibble(centroids = clusters_4@centroids, num_clusters = 4) %>%
  bind_rows(tibble(centroids = clusters_5@centroids, num_clusters = 5)) %>%
  bind_rows(tibble(centroids = clusters_3@centroids, num_clusters = 3)) %>%
  group_by(num_clusters) %>%
  mutate(cluster = row_number()) %>%
  unite(cluster_num_clusters, cluster, num_clusters) %>%
  spread(cluster_num_clusters, centroids) %>%
  unnest() %>%
  mutate(split = row_number() * 50) %>%
  gather(cluster_num_clusters, speed, matches("\\d_\\d")) %>%
  separate(cluster_num_clusters, c("cluster", "num_clusters"), sep = "_")

write_rds(cluster_centroids, "paper/data/cluster_centroids.rds")

cluster_summary <- 
  clusters_4@clusinfo %>% 
  as_tibble() %>% 
  mutate(Cluster = row_number()) %>% 
  select(Cluster, Size = size, `Average Distance` = av_dist)

write_rds(cluster_summary, "paper/data/cluster_summary.rds")

races_clustered <- 
  races_cleaned %>%
  mutate(cluster = clusters_4@cluster,
         cluster = case_when(cluster == 1 ~ "U-Shaped",
                             cluster == 2 ~ "Even",
                             cluster == 3 ~ "Positive",
                             cluster == 4 ~ "Reverse J-Shaped")) %>%
  left_join(races_standardized, by = "row_number", suffix = c("", "_std"))
  

write_rds(races_clustered, "paper/data/races_clustered.rds")

races_clustered %>%
  count(cluster, adapt_desig) %>% 
  spread(adapt_desig,  n)

races_clustered %>%
  count(cluster, size) %>% 
  spread(size,  n)


races_plotable <- 
  races_clustered %>%
  gather("distance", "speed", matches("speed_\\d{2,4}_std")) %>%
  mutate(distance = parse_number(distance)) 

write_rds(races_plotable, "paper/data/races_plotable.rds")

races_plotable %>%
  ggplot(aes(x = distance, y = speed, colour = cluster, group = row_number)) +
  geom_line(alpha = 0.05) +
  xlab("Distance (m)") +
  ylab("Speed (m/s)") +
  scale_color_manual("Cluster", values = colour_palette) +
  facet_wrap(~ cluster)

