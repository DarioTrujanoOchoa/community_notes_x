
# model the data

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
       workflows
       )

load("data/notes_merged.RData")

vis_miss(slice_sample(notes_merged,prop = 0.1))

set.seed(1984)

training_percentage <- 0.75

split_notes <- initial_split(notes_merged,prop = training_percentage)
training_notes <- training(split_notes)
test_notes <- testing(split_notes)

# recipe ----
rec_reg <- recipe(ratings ~  ., 
              data = training_notes %>% select(-c(noteId,summary))) %>% 
  step_normalize(agreement_rate) 


# models ----
##linear model ----
linear_reg <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# KNN model
k = 7
knn_mod <- nearest_neighbor(neighbors = k) %>%
  set_mode("regression") %>%
  set_engine("kknn")

# Workflow ----
## linear ----
lm_wkflow <- workflow() %>% 
  # add model
  add_model(linear_reg) %>% 
  # add receipe
  add_recipe(rec_reg)

## KNNn model ----
knn_wkflow <- workflow() %>% 
  # add model
  add_model(knn_mod) %>% 
  # add receipe
  add_recipe(rec_reg)

# fitting models ----
## linear ----
fit_lm <- 
  lm_wkflow %>% 
  fit(data = training_notes)
fit_lm

## KNN ----
fit_knn <- 
  knn_wkflow %>% 
  fit(data = training_notes)
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


