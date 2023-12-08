
# model the data

library(pacman)
p_load(tidyverse, 
       tidymodels, 
       recipes,
       kknn,
       yardstick,
       tune)
p_load(ggplot2)
p_load(ggthemes)

set.seed(1984)

training_percentage <- 0.75

split_notes <- initial_split(notes_merged,prop = training_percentage)
training_notes <- training(split_notes)
test_notes <- testing(split_notes)
