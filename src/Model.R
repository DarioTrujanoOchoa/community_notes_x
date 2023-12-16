
# model the data

rm(list = ls())

library(pacman)
p_load(tidyverse, 
       tidymodels, 
       recipes,
       kknn,
       yardstick,
       tune,
       ggplot2,
       ggthemes,
       rsample,
       parsnip,
       workflows,
       glmnet,
       ranger
       )

set.seed(1984)

# Load data ----
load("data/notes_merged.RData")

notes_merged <- notes_merged %>% filter(created_at > "2022-11-25 15:30:30 UTC") %>% 
  slice_sample(prop = 0.05)

  
# Split data and cross validation ----
training_percentage <- 0.75

split_notes <- initial_split(notes_merged,
                             prop = training_percentage,
                             strata = ratings)
train_notes <- training(split_notes)
test_notes <- testing(split_notes)

v_folds <- 10
notes_folds <- vfold_cv(train_notes, v = v_folds, strata = "ratings")

# Recipe ----
rec_reg <- recipe(ratings ~  ., 
              data = train_notes) %>% 
  step_rm(note_id, tweet_id, 
          created_at, current_status) %>% 
  step_dummy(w_day, hour, classification) %>% 
  step_normalize(agreement_rate) 


# Models ----
##linear model ----
linear_reg <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# KNN model
knn_mod <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("regression") %>%
  set_engine("kknn")

## Elastic net ----
# Tuning penalty and mixture
en_mod <- linear_reg(penalty = tune(), 
                           mixture = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

## Random forest ----
# Tuning mtry (number of predictors), trees, and min_n (number of minimum values in each node)
rf_mod <- rand_forest(mtry = tune(), 
                       trees = tune(), 
                       min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

# Workflow ----
## Linear ----
lm_wkflow <- workflow() %>% 
  # add model
  add_model(linear_reg) %>% 
  # add receipe
  add_recipe(rec_reg)

## KNN ----
knn_wkflow <- workflow() %>% 
  # add model
  add_model(knn_mod) %>% 
  # add receipe
  add_recipe(rec_reg)

## EN ----
en_wkflow <- workflow() %>% 
  # add model
  add_model(en_mod) %>% 
  # add receipe
  add_recipe(rec_reg)

## RF ----
rf_wkflow <- workflow() %>% 
  # add model
  add_model(rf_mod) %>% 
  # add receipe
  add_recipe(rec_reg)

# Grids for tunning parameters ----
# For the Linear model there is no parameter to tune, so I don't need a grid or tuning.
## KNN ----
knn_grid <- grid_regular(neighbors(range = c(1,15)), 
                         levels = 5)

## EN ----
en_grid <- grid_regular(penalty(range = c(-5, 5)), 
                             mixture(range = c(0,1)), 
                             levels = 10)

## RF ----
rf_grid <- grid_regular(mtry(range = c(1, 12)), 
                                  trees(range = c(20,60)), 
                                  min_n(range = c(5,20)), 
                                  levels = 8)

# Tuning ----
## KKN ----
knn_tune <- tune_grid(
  knn_wkflow,
  resamples = notes_folds,
  grid = knn_grid
)

## EN ----
en_tune <- tune_grid(
  en_wkflow,
  resamples = notes_folds,
  grid = en_grid
)

## RF ----
rf_tune_res <- tune_grid(
  rf_wkflow,
  resamples = notes_folds,
  grid = rf_grid
)

# Save tuning results ----

## KNN ----
write_rds(knn_tune, file = "data/tuned_models/knn.rds")

## EN ----
write_rds(en_tune, file = "data/tuned_models/elastic.rds")

## RF ----
write_rds(rf_tune_res, file = "data/tuned_models/rf.rds")

# Load tunning results ----
## KNN ----
read_rds(knn_tune, file = "data/tuned_models/knn.rds")

## EN ----
read_rds(elastic_tune, file = "data/tuned_models/elastic.rds")

## RF ----
read_rds(rf_tune_res, file = "data/tuned_models/rf.rds")

# Compare models ----

## Linear ----
lm_fit <- fit_resamples(lm_workflow, resamples = pokemon_folds)
lm_rmse <- collect_metrics(lm_fit) %>% 
  slice(1)

## KKN ----
knn_rmse <- collect_metrics(knn_tuned) %>% 
  arrange(mean) %>% 
  slice(6)

## EN ----
elastic_rmse <- collect_metrics(elastic_tuned) %>% 
  arrange(mean) %>% 
  slice(73)

## RF ----
rf_rmse <- collect_metrics(rf_tuned) %>% 
  arrange(mean) %>% 
  slice(513)

# Creating a tibble of all the models and their RMSE
final_compare_tibble <- tibble(
  Model = c("Linear Regression", 
            "K Nearest Neighbors", 
            "Elastic Net", 
            "Random Forest"), 
  RMSE = c(lm_rmse$mean, 
           knn_rmse$mean, 
           elastic_rmse$mean, 
           rf_rmse$mean))

# Arranging by lowest RMSE
final_compare_tibble <- final_compare_tibble %>% 
  arrange(RMSE)

final_compare_tibble

# fitting models ----
## Linear ----
fit_lm <- 
  lm_wkflow %>% 
  fit(data = train_notes)
fit_lm

## KNN ----
fit_knn <- 
  knn_wkflow %>% 
  fit(data = train_notes)
fit_knn

## EN ----
fit_en <- 
  en_wkflow %>% 
  fit(data = train_notes)
fit_knn

## RF ----
fit_knn <- 
  rf_wkflow %>% 
  fit(data = train_notes)
fit_knn


# metrics
notes_metrics <- metric_set(rmse, rsq, mae)

# linear model
notes_lm_aug <- augment(fit_lm, test_notes)
notes_metrics(notes_lm_aug, truth = ratings,
                estimate = .pred)

notes_merged %>% ggplot() +
  geom_point(aes(x=agreement_rate,y=ratings))

# knn model
notes_knn_aug <- augment(fit_knn, test_notes)
notes_metrics(notes_knn_aug, truth = ratings,
              estimate = .pred)


