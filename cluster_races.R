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
clusters <- tsclust(cluster_matrix, k = 4, centroid = "shape", distance = "sbd")


races_cleaned %>%
  mutate(cluster = clusters@cluster) %>%
  count(cluster, size) %>% 
  spread(size,  n)


races_plotable <- 
  races_cleaned %>%
  mutate(cluster = clusters@cluster) %>%
  gather("distance", "speed", matches("speed")) %>%
  mutate(distance = parse_number(distance),
         cluster = factor(cluster)) 

races_plotable %>%
  ggplot(aes(x = distance, y = speed, colour = cluster)) +
  stat_summary(geom = "line", fun.y = "mean", size = 1) +
  xlab("Distance (m)") +
  ylab("Speed (m/s)") +
  scale_color_discrete("") +
  theme(legend.position = "bottom")


