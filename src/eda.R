# eda.R

# Dario Trujano-Ochoa

# packages and data ----
library(pacman)
p_load(tidyverse,
       ggplot2,
       corrplot)

load("data/notes_merged.RData")

# missing data ----
# there are 3 rows with missing values
missing_cell <- which(is.na(notes_merged), arr.ind = TRUE)
notes_merged[missing_cell[,1],]
# These are the tweet ids
tweet_ids <- notes_merged[missing_cell[,1],] %>% select(tweet_id) %>% pull() %>% format(scientific = F) %>% unique()
# There is no summary in this notes, probably this was a mistake
# There is nothing in the note 1370110240532930560 that had 8 ratings. The other two notes were never rated.

# I remove the missing values, given the content and the number of missing values this shouldn't be an issue
notes_merged <- notes_merged %>% drop_na()

# Analyzing the outcome variable ----
summary(notes_merged$ratings)
# 99% of the notes have less than 453 ratings
q_99 <- quantile(notes_merged$ratings,probs = 0.99)

notes_merged %>% 
  filter(ratings < q_99) %>% 
  ggplot() +
  geom_histogram(aes(x= ratings)) + 
  labs(
    title = "Histogram of the number of Ratings on each Note",
    subtitle = "Percentile 99 of the Ratings",
    x="Number of Ratings"
  ) +
  theme_bw()

notes_merged %>% 
  filter(ratings >= q_99) %>% 
  ggplot() +
  geom_histogram(aes(x= ratings)) + 
  labs(
    title = "Histogram of the number of Ratings on each Note",
    subtitle = "1% of Notes with more Ratings",
    x="Number of Ratings"
  ) +
  theme_bw()

# The note with most ratings
notes_merged %>% filter(ratings>6000) %>% arrange(ratings) %>% 
  select(tweet_id) %>% 
  pull() %>% 
  format(scientific = F)

# Correlations ----
# Correlation matrix
notes_cor <- cor(notes_merged %>% 
                   select(-ends_with("id")) %>% 
                   select_if(is.numeric))

# Visualization of correlation matrix
notes_corrplot <- corrplot.mixed(notes_cor, 
                                 lower = 'shade', upper = 'pie', order = 'hclust', 
                                 addCoef.col = 1, number.cex = 0.7,
                                 tl.pos = "lt"
                                 )
# There is little correlation between the variables 

# Are the rating predicting Status?

notes_merged %>% 
  filter(ratings > 1, ratings < 2000) %>% 
  ggplot() +
  geom_boxplot(aes(x=current_status, y = ratings))

notes_merged %>% group_by(current_status) %>% 
  summarise(mean(ratings), median(ratings), 
            quantile(ratings,probs = 0.1), 
            quantile(ratings,probs = 0.9)
            )

table(notes_merged$classification, notes_merged$current_status)
