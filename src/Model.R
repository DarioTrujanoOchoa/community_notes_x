
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
       ranger,
       vip
       )

set.seed(1984)

# Load data ----
load("data/notes_merged.RData")

# lets get a subset of the data

notes_merged <- notes_merged %>% 
  filter(created_at > "2022-11-25 15:30:30 UTC") %>% 
  slice_sample(prop = 0.05)

save(notes_merged, file = "data/notes_merged_sample.RData")
  
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
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 


# Models ----
##linear model ----
linear_reg <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# Polinomial Regression ----
poly_rec <- rec_reg %>% 
  step_poly(note_length, helpful_rate, not_helpful_rate,
            degree = tune())

poly_spec <- linear_reg() %>% 
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

## Polynomial Regression ----
poly_wf <- workflow() %>% 
  add_model(poly_spec) %>% 
  add_recipe(poly_rec)

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

# Grids for tuning parameters ----
# For the Linear model there is no parameter to tune, so I don't need a grid or tuning.

## Polynomial Regression ----
degree_grid <- grid_regular(degree(range = c(1,5)), levels = 5)

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

## Polynomial Regression ----
poly_tune <- tune_grid(
  poly_wf,
  resamples = notes_folds,
  grid = degree_grid
)

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
rf_tune <- tune_grid(
  rf_wkflow,
  resamples = notes_folds,
  grid = rf_grid
)

# Save tuning results ----

## Polynomial Regression ----
write_rds(poly_tune, file = "data/tuned_models/poly.rds")

## KNN ----
write_rds(knn_tune, file = "data/tuned_models/knn.rds")

## EN ----
write_rds(en_tune, file = "data/tuned_models/elastic.rds")

## RF ----
write_rds(rf_tune, file = "data/tuned_models/rf.rds")

# Load tuning results ----

## Polynomial Regression ----
poly_tune <- read_rds(file = "data/tuned_models/poly.rds")

## KNN ----
knn_tune <- read_rds(file = "data/tuned_models/knn.rds")

## EN ----
en_tune <- read_rds(file = "data/tuned_models/elastic.rds")

## RF ----
rf_tune <- read_rds(file = "data/tuned_models/rf.rds")

# Compare models ----
# collect metrics 
## Linear ----
lm_fit <- fit_resamples(lm_wkflow, resamples = notes_folds)
lm_rmse <- collect_metrics(lm_fit) %>% 
  slice(1)

## Polynomial Regression ----
poly_rmse <- show_best(poly_tune,metric = "rmse",n = 1) %>% 
  select(mean) %>% pull()

## KKN ----
knn_rmse <- show_best(knn_tune,metric = "rmse",n = 1) %>% 
  select(mean) %>% pull()

## EN ----
en_rmse <- show_best(en_tune,metric = "rmse",n = 1) %>% 
  select(mean) %>% pull()

## RF ----
rf_rmse <- show_best(rf_tune,metric = "rmse",n = 1) %>% 
  select(mean) %>% pull()

# Creating a tibble of all the models and their RMSE
final_compare_tibble <- tibble(
  Model = c(
    "Linear Regression", 
    "K Nearest Neighbors",
    "Elastic Net",
    "Random Forest",
    "Polynomial Regression"
    ), 
  RMSE = c(
   lm_rmse$mean, 
   knn_rmse,
   en_rmse,
   rf_rmse,
   poly_rmse
    ))

# Arranging by lowest RMSE
final_compare_tibble <- final_compare_tibble %>% 
  arrange(RMSE)

final_compare_tibble

# Best model ----
show_best(rf_tune, metric = 'rmse', n=1)
best_train <- select_best(rf_tune, metric = 'rmse')

## Fit to training data ----
final_workflow_train <- finalize_workflow(rf_wkflow, best_train)
final_fit_train <- fit(final_workflow_train, data = train_notes)

## Testing the model ----

# Creating the predicted vs. actual value tibble
notes_tibble <- predict(final_fit_train, new_data = test_notes %>% select(-ratings))
notes_tibble <- bind_cols(notes_tibble, test_notes %>% select(ratings))

# Creating plot of predicted values vs. actual values
notes_tibble %>% 
  ggplot(aes(x = .pred, y = ratings)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2) +
  theme_grey() +
  coord_obs_pred() +
  labs(title = "Predicted Values vs. Actual Values",
       x = "Model Prediction")

# Let's focus on the notes with not that many ratings
notes_tibble %>% 
  filter(.pred < 500,
         ratings < 500) %>% 
  ggplot(aes(x = .pred, y = ratings)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2) +
  # xlim(0, 50) +
  # ylim(0, 50) +
  theme_grey() +
  coord_obs_pred() +
  labs(title = "Predicted Values vs. Actual Values",
       x = "Model Prediction")

## VIP ----
# Using the training fit to create the VIP because the model was not actually fit to the testing data
final_fit_train %>% 
  extract_fit_engine() %>% 
  vip(aesthetics = list(fill = "red3", color = "blue3"))
