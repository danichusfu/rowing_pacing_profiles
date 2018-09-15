#####################
##### PACKAGES ######
#####################
# If pacman is not installed, install it
# install.packages('pacman')
# for longclust https://ms.mcmaster.ca/~paul/software.html
pacman::p_load(tidyverse, glue, lubridate, dtwclust)

races_full <- read_csv("rowing_world_championships.csv")

races_cleaned <-
  races_full %>%
  # if a race as a speed which is na or less than 2 throw out the whole race
  # there are gps errors where the average speed recorded is less than the true speed
  # this may be changed later but is a good baseline
  filter_at(vars(matches("speed")), all_vars(!is.na(.) & . > 2)) %>%
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
  select(row_number, matches("speed_\\d{2}$"), matches("speed_\\d{3}$"), matches("speed_\\d{4}$")) %>%
  mutate_at(vars(-row_number), scale)

# races_smoothed <-
#   races_long %>%
#   nest(-row_number) %>%
#   mutate(loess_model    = map(data, ~ loess(speed ~ distance, data = .x, span = 0.1)),
#          speed_smoothed = map(loess_model, predict)) %>%
#   select(-loess_model) %>%
#   unnest() %>%
#   select(-speed) %>%
#   mutate(distance = paste0("speed_", distance)) %>%
#   spread(distance, speed_smoothed) %>%
#   select(row_number, matches("speed_\\d{2}$"), matches("speed_\\d{3}$"), matches("speed_\\d{4}$"))

cluster_matrix <-  
  races_standardized %>%
  select(-row_number) %>%
  as.matrix()

# k shape clustering
# http://www1.cs.columbia.edu/~jopa/Papers/PaparrizosSIGMOD2015.pdf
clusters <- tsclust(cluster_matrix, k = 4, centroid = "shape", distance = "sbd")

tibble(clust_1 = clusters@centroids[[1]], clust_2 = clusters@centroids[[2]], 
       clust_3 = clusters@centroids[[3]], clust_4 = clusters@centroids[[4]]) %>%
  mutate(split_num = row_number()) %>%
  gather(cluster, std_speed, -split_num) %>%
  ggplot(aes(x = split_num, y = std_speed, group = cluster)) +
    geom_line()



races_cleaned %>%
  mutate(cluster = clusters@cluster) %>%
  count(cluster, size) %>% 
  spread(size,  n)

races_standardized %>%
  mutate(cluster = clusters@cluster) %>%
  select(row_number,  matches("speed_"), cluster) %>%
  gather("distance", "speed", matches("speed")) %>%
  mutate(distance = parse_number(distance),
         cluster = factor(cluster)) %>%
  filter(row_number <= 1000) %>%
  ggplot(aes(x = distance, y = speed, colour = cluster)) +
  #stat_summary(geom = "point", fun.y = "mean") +
  #geom_smooth(aes(group = row_number), color = "black", alpha = 0.1, span = 0.25) +
  #geom_line(aes(group = row_number), color = "black", alpha = 0.1) +
  stat_summary(geom = "line", fun.y = "mean", size = 1) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  xlab("Distance (m)") +
  ylab("Speed (m/s)") +
  scale_color_discrete("") +
  theme(legend.position = "bottom") #+
  facet_wrap(~ cluster)


races_cleaned %>%
  mutate(cluster = clusters@cluster) %>%
  filter(junior == "senior", adaptive == FALSE) %>%
  select(row_number,  matches("speed_"), cluster) %>%
  gather("distance", "speed", matches("speed")) %>%
  mutate(distance = parse_number(distance),
         cluster = factor(cluster)) %>%
  filter(row_number <= 1000) %>%
  ggplot(aes(x = distance, y = speed, colour = cluster)) +
  #stat_summary(geom = "point", fun.y = "mean") +
  #geom_smooth(aes(group = row_number), color = "black", alpha = 0.1, span = 0.25) +
  stat_summary(geom = "line", fun.y = "mean", size = 1) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  xlab("Distance (m)") +
  ylab("Speed (m/s)") +
  scale_color_discrete("") +
  theme(legend.position = "bottom")

