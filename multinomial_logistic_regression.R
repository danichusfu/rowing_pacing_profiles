pacman::p_load(nnet, tidyverse, glue, lubridate)


races_clustered <- read_rds("paper/data/races_clustered.rds")


races_train <- 
  races_clustered %>% 
  select(-matches("strokes_"), - matches("speed_"), -matches("name"), -matches("birthday")) %>%
  filter(junior == "senior", adaptive == FALSE, gender != "mixed") %>%
  unite(round_rank, heat_or_final, rank_final, remove = FALSE) %>%
  mutate_all(as.factor)

model <- nnet::multinom(cluster ~ size + heat_or_final + rank_final + discipline + coxswain + gender + weight_class, data = races_train)
model <- nnet::multinom(cluster ~ size + heat_or_final + rank_final + round_rank + discipline + coxswain + gender + weight_class, data = races_train)
# model <- nnet::multinom(cluster ~ size + heat_or_final*rank_final + discipline + coxswain + gender + weight_class, data = races_train)
# model <- nnet::multinom(cluster ~ size + heat_or_final + discipline + coxswain + gender + weight_class, data = races_train)
# model <- nnet::multinom(cluster ~ size + heat_or_final + rank_final + weight_class, data = races_train)
# model <- nnet::multinom(cluster ~ 1, data = races_train)

# Model accuracy
model_fit <-
  fitted(model) %>% 
  as_tibble() %>%
  mutate(predicted_cluster = predict(model, races_train),
         actual_cluster = races_train$cluster)

model_fit %>%
  select(-predicted_cluster) %>%
  gather(cluster, value, - actual_cluster) %>%
  filter(actual_cluster == cluster) %>%
  summarise(cost_function = -1*sum(log(value)))

model$AIC

summary(model)

# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
z <- summary(model)$coefficients/summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

0.05/25 > p


model_fit %>%
  count(predicted_cluster, actual_cluster) %>%
  spread(actual_cluster, n) %>%
  rename(`Prediction/Cluster` = predicted_cluster)

model_fit %>%
  count(actual_cluster)

model_fit %>%
  count(predicted_cluster)

cross_tab_cluster_for_column(races_clustered, "rank_final")


# Conchran Manteal Hansel (CMH) Test 
# 3D Chi-Squared, check for interactions
# assign null model
# compare to log(0.5) across board
# compute for our model
# log likelihood with gender or other vars
