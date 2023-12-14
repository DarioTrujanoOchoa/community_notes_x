# eda.R

# Dario Trujano-Ochoa

# packages 
library(pacman)
p_load(tidyverse,
       ggplo2)

load("data/notes_merged.RData")

# missing data
vis_miss(slice_sample(notes_merged,prop = 0.1))
# there is no missing data in the data set

# Analyzing the outcome variable
summary(notes_merged$ratings)

notes_merged %>% 
  filter(ratings > 500) %>% 
  ggplot() +
  geom_histogram(aes(x= ratings))

summary(notes_merged$ratings)
mean(notes_merged$ratings>500)

# The note with most ratings
notes_merged %>% filter(ratings>8000) %>% arrange(ratings) %>% 
  select(tweet_id) %>% 
  pull() %>% 
  format(scientific = F)
