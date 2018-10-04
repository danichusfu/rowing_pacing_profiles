pacman::p_load(nnet, tidyverse, glue, lubridate)


races_clustered <- read_rds("paper/data/races_clustered.rds")


races_train <- 
  races_clustered %>% 
  select(-matches("strokes_"), - matches("speed_"), -matches("name"), -matches("birthday")) %>%
  filter(junior == "senior", adaptive == FALSE, gender != "mixed") %>%
  unite(round_rank, heat_or_final, rank_final, remove = FALSE) %>%
  mutate_all(as.factor) 

# full with interaction
# model <- nnet::multinom(cluster ~ size + heat_or_final*rank_final + discipline + coxswain + gender + weight_class, data = races_train)
# iteraction without coxswain (it only varies with size at the 2 boat level)
# model <- nnet::multinom(cluster ~ size +  heat_or_final + rank_final + round_rank  + discipline + gender + weight_class, data = races_train)
# race placing with heat or final
# model <- nnet::multinom(cluster ~ size + round_rank + discipline + gender + weight_class, data = races_train)
# full
model <- nnet::multinom(cluster ~ size + heat_or_final + rank_final + discipline + gender + weight_class, data = races_train)

# Intercept Model
# model <- nnet::multinom(cluster ~ 1, data = races_train)

model$AIC
model$value

# Model accuracy
model_fit <-
  fitted(model) %>% 
  as_tibble() %>%
  mutate(predicted_cluster = predict(model, races_train),
         actual_cluster = races_train$cluster)


# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
z <- coef(model)/summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

relative_odds <- exp(coef(model))
significance_test <- (0.05/27 > p)

write_rds(relative_odds, "paper/data/relative_odds.rds")
write_rds(significance_test, "paper/data/significance_test.rds")





# Conchran Manteal Hansel (CMH) Test 
# 3D Chi-Squared, check for interactions
# assign null model
# compare to log(0.25) across board
# compute for our model
# log likelihood with gender or other vars
