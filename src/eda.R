# eda.R

# Dario Trujano-Ochoa

# packages and data ----
library(pacman)
p_load(tidyverse,
       ggplot2,
       corrplot)
rm(list=ls())

load("data/notes_merged.RData")

# missing data ----
# there are 3 rows with missing values
missing_cell <- which(is.na(notes_merged), arr.ind = TRUE)
notes_merged[missing_cell[,1],]
# These are the tweet ids
tweet_ids_missing <- notes_merged[missing_cell[,1],] %>% select(tweet_id) %>% pull() %>% format(scientific = F) %>% unique()
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


# Nate EDA ----
# Remove scientific notation/filter for tweets with ratings
notes_merged$tweet_id <- notes_merged$tweet_id %>% format(scientific = F)
notes_merged <- notes_merged %>% filter(ratings>0)
# Look at number of ratings to get a note published
notes_merged %>% group_by(current_status) %>% 
  summarise(quantile(ratings,probs = 0.25))

# Extract published notes with low ratings
notes_merged %>% 
  filter(ratings < 6, current_status != "NEEDS_MORE_RATINGS", helpful_rate >= 0.75) %>% 
  View()
# Not all notes that have enough ratings are actually published

# Grab 100 ids to use in python

# Ggplot visualisations
id_snippet <- notes_merged %>% filter(current_status == "NEEDS_MORE_RATINGS")
id_snippet <- id_snippet %>% filter(ratings > 10)
id_snippet <- id_snippet %>% select(tweet_id)
id_snippet <- id_snippet %>% format(scientific = F)
id_snippet <- id_snippet %>% unique()
View(id_snippet)
snippet_list <- paste0("[", paste(id_snippet$tweet_id, collapse = ","), "]")
write(snippet_list, file = "tweet_ids.txt")

### Far more misleading ----
ggplot(notes_merged, aes(x = classification)) +
  geom_bar() +
  labs(title = "Distribution of Classifications")

### Far more needs more ratings ----
ggplot(notes_merged, aes(x = current_status)) +
  geom_bar() +
  labs(title = "Distribution of Current Status")

### Notes need high agreement to be published ----
notes_merged %>% filter(agreement_rate>0) %>% 
ggplot(aes(x = agreement_rate)) +
  geom_histogram(binwidth = 0.05) +
  labs(title = "Distribution of Agreement Rate")

### Trustworthy sources = Misleading? ----
ggplot(notes_merged, aes(x = classification, fill = factor(trustworthy_sources))) +
  geom_bar(position = "dodge") +
  labs(title = "Classification by Trustworthy Sources",
       fill = "Trustworthy Sources")

sampled_data <- notes_merged[sample(nrow(notes_merged), 100000), ]

# Mega correlation plot
library(GGally)
ggpairs(sampled_data[, c("ratings", "agreement_rate", "helpful_rate", "not_helpful_rate", "somewhat_helpful_rate", "classification", "current_status")])

# No observed correlation between classification and helpfulness
ggplot(sampled_data, aes(x = helpful_rate, y = not_helpful_rate, color = classification)) +
  geom_point() +
  labs(title = "Helpful vs Not Helpful Rate by Classification")


### Distribution showing when notes are published ----
ggplot(notes_merged, aes(x = ratings, fill = current_status)) +
  geom_histogram(position = "dodge", binwidth = 5) +
  xlim(0,1000)+
  ylim(0,7500)+
  labs(title = "Ratings Distribution by Note Status, blue goes up to 50k when near 0 ratings")

### Helpfulness status as a measure of publishability? ----
ggplot(notes_merged, aes(x = helpful_rate, fill = current_status)) +
  geom_histogram(position = "dodge", binwidth = 0.05) +
  labs(title = "Disagreement Rate by Status")
# =^
ggplot(notes_merged, aes(x = not_helpful_rate)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ current_status) +
  labs(title = "Disagreement Rate by Status")
# = ^
ggplot(sampled_data, aes(x = helpful_rate, y = ratings, color = current_status)) +
  geom_point() +
  labs(title = "Helpfulness vs Ratings by Status")

sampled_data %>% ggplot(aes(x=agreement_rate,y=helpful_rate)) +
  geom_point()

filter(sampled_data, ratings>10) %>% ggplot(aes(x=current_status, y = ratings))+
  geom_boxplot()+
  ylim(0,100)
  
#needs more ratings outliers
# sample with specific ratings classification
notes_merged <- notes_merged %>% filter(!is.na(helpful_unbiased_language))
notes_merged <- notes_merged %>% filter(!is.na(current_status))
# Notes over time 
notes_merged %>%
  mutate(created_at = lubridate::as_date(notes_merged$created_at)) %>% 
  filter(year(created_at) >= 2024) %>% 
  ggplot(aes(x = created_at, fill = current_status))+
  geom_histogram(binwidth = 1)+
  theme_minimal()+
  labs(title = "Note Status Over Time", x = "Date", y = "Count")

#Reasons for not helpful
notes_merged %>%
  summarise(across(starts_with("not_helpful_"), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Reason", values_to = "Count") %>%
  ggplot(aes(x = reorder(Reason, Count), y = Count))+
  geom_bar(stat = "identity", fill = "red")+
  coord_flip()+
  labs(title = "Reasons for Unhelpful Notes", x = "Reason", y = "Count")

#Reasons for helpful
notes_merged %>%
  summarise(across(starts_with("helpful_"), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Reason", values_to = "Count") %>%
  ggplot(aes(x = reorder(Reason, Count), y = Count))+
  geom_bar(stat = "identity", fill = "green")+
  coord_flip()+
  labs(title = "Reasons for Unhelpful Notes", x = "Reason", y = "Count")

# Not helpful reasons grouped by status
notes_merged %>%
  group_by(current_status) %>%
  summarise(across(starts_with("not_helpful_"), mean, na.rm = TRUE)) %>%
  pivot_longer(-current_status, names_to = "Reason", values_to = "Rate") %>%
  ggplot(aes(x = Reason, y = Rate, fill = current_status))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  labs(title = "Not Helpful Reasons grouped by Note Status", x = "Reason", y = "Rate")

# helpful reasons grouped by status
notes_merged %>%
  group_by(current_status) %>%
  summarise(across(starts_with("helpful_"), mean, na.rm = TRUE)) %>%
  pivot_longer(-current_status, names_to = "Reason", values_to = "Rate") %>%
  ggplot(aes(x = Reason, y = Rate, fill = current_status))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  labs(title = "Not Helpful Reasons grouped by Note Status", x = "Reason", y = "Rate")

# Note length
notes_merged %>% ggplot(aes(x = note_length, fill = current_status))+
  geom_density(alpha = 0.5)+
  xlim(0,1000)+
  labs(title = "Note length distribution grouped by status", x = "Note Length", y = "Density")

# Correlation between new smaller variables
library(GGally)
sampled_data <- notes_merged[sample(nrow(notes_merged), 10000), ]
sampled_data %>% select(starts_with("not_helpful_")) %>% ggpairs()

# LM's
library(nnet)

lmodel <- multinom(current_status~helpful_rate+not_helpful_rate+ 
                             helpful_other+helpful_informative+helpful_clear+ 
                             not_helpful_off_topic+not_helpful_incorrect, notes_merged)

summary(lmodel)

lm(current_status~helpful_rate+not_helpful_rate+ 
                     helpful_other+helpful_informative+helpful_clear+ 
                     not_helpful_off_topic+not_helpful_incorrect, notes_merged)

#Look at currently scraped tweets
scraped <- read.csv("data/tweets_data1.csv")
scraped <- scraped %>% filter(Text != "DivN/A")
scraped <- scraped %>% rename(tweet_id = Tweet.ID.)
scraped$tweet_id. <- as.numeric(scraped$tweet_id)
notes_merged$tweet_id <- as.numeric(notes_merged$tweet_id)
merged_data <- merge(scraped, notes_merged, by = "tweet_id", all.x = TRUE)
merged_data <- merged_data %>% filter(!is.na(helpful_clear))

# Graphs for merged data
# NOTES MAKE LIKES GO DOWN
ggplot(merged_data, aes(x = Note.Published., y = as.numeric(gsub("K", "e3", Likes))))+
  geom_boxplot(fill="gray")+
  ggtitle("Likes Distribution by Note Published Status")+
  xlab("Note Published")+
  ylab("Likes")+
  theme_minimal()

# NOTHING CURRENTLY RATED HELPFUL NOT PUBLISHED AND VICE VERSA
ggplot(merged_data, aes(x = current_status, y = as.numeric(gsub("K", "e3", Likes))))+
  geom_boxplot(fill="gray")+
  ggtitle("Likes Distribution by Note Published Status")+
  xlab("Note Published")+
  ylab("Likes")+
  facet_wrap(~Note.Published.)+
  coord_flip()


# distribution of note publish times if ever published (16 hours)


#Summary:
# How many ratings needed to publish a note: 
#unknown, we need to get information on notes actually 
#being published, there seems to be no clear indication of 
#how many rates/agreement rate but at some point they are 
#definitely published because most published notes have more 
#ratings and higher agreement.
#
# How many people need to disagree: 
#similar procedure to first question, we need to see the 
#data about published notes and then use the not_helpful_rate 
#to determine publishability.

# Disagreement defined as a high not helpful rate 
#(mostly) or a low ratings score

# Agreement rate, helpful rate, and ratings are key to 
#getting a note published, need to investigate note_length 
#as well as trustworthy sources to see if there is correlation.