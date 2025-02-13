# prepare_data.R
# Merge the raw data to feed the models
# Dario Trujano-Ochoa

# packages
library(pacman)
p_load(tidyverse, 
       lubridate,
       naniar,
       janitor,
       forcats)

rm(list = ls())

# load data ----
# all the data was downloaded on December 3rd 2023
# notes
notes <- read_tsv("data/notes-00000.tsv") %>% clean_names()

## status
status <- read_tsv("data/noteStatusHistory-00000.tsv") %>% clean_names()

## ratings
r0 <- read_tsv("data/ratings-00000.tsv") %>% clean_names()
r1 <- read_tsv("data/ratings-00001.tsv") %>% clean_names()
r2 <- read_tsv("data/ratings-00002.tsv") %>% clean_names()
r3 <- read_tsv("data/ratings-00003.tsv") %>% clean_names()

# Almost all the notes in notes are in status
# only 20 notes are not there
sum(notes$note_id %in% status$note_id)

s_notes <- slice_sample(notes,n = 10000) 
s_status <- slice_sample(status,n = 10000) 

# The variables Believable, harmful, and validation difficulty were Deprecated as of 2022-10-27.
vis_miss(s_notes)
vis_miss(s_status)

# notes ----

## Authors ----
# the observations in the dataset are unique for each note
length(unique(notes$note_id))

# the Some notes are made by more than one participant
length(unique(s_notes$note_author_participant_id))
# most users publish a note only once (60% in sample) 
table(table(s_notes$note_author_participant_id)) 
barplot(table(table(s_notes$note_author_participant_id)),
        xlab = "Number of Notes",
        ylab = "Number of Authors",
        main = "Distribution of Notes Published by Author")
# ID of prolific authors
head(sort(table(s_notes$note_author_participant_id),decreasing = T), n = 25)

## Tweets ----
# some tweets have more than one note
# most of the tweets have a single note
length(unique(notes$tweet_id))

barplot(table(table(s_notes$tweet_id)),
        xlab = "Number of Notes",
        ylab = "Number of Tweets",
        main = "Distribution of Notes Published by Tweet")

# all the variables starting with misleading are answers to the reason something was misleading
# all variables starting with notMisleadnig are answers to the reason something was not misleading
# therefore cannot be used to explain why a note is misleading or not

as_tibble(s_notes) %>% 
  filter(classification == "MISINFORMED_OR_POTENTIALLY_MISLEADING") %>% 
  select_if(is.numeric) %>%
  map_dbl(sum) %>% as.data.frame()



table(s_notes$classification,s_notes$not_misleading_other)
table(s_notes$classification,s_notes$is_media_note)

table(s_notes$classification,s_notes$trustworthy_sources)

## Select the variables that will be used in the model from the notes dataset ----
notes_final <-
  notes %>% select(
    note_id,
    tweet_id,
    classification,
     trustworthy_sources,
     summary, 
    is_media_note,
    created_at_millis
     ) %>% 
  mutate(created_at = as.POSIXct(created_at_millis / 1000, origin = "1970-01-01")) %>% 
  mutate(w_day = wday(created_at, label = T),
         hour = as_factor(hour(created_at)),
         note_length = nchar(summary)) %>% 
  select(-c(summary))

# There is no missing values
vis_miss(slice_sample(notes_final,prop = 0.1))

# status ----

# the observations in the dataset are almost unique
length(unique(status$note_id))
# however they are all rated as "NEED MORE RATINGS"
duplicated_notes_status <-
status %>% group_by(note_id) %>% 
  summarise(n_notes = n()) %>% 
  filter(n_notes>1) %>% 
  pull(note_id) %>% 
  format(scientific = F)
status %>% filter(note_id %in% duplicated_notes_status) %>% View()

# Contains -1 if the note never left “Needs More Ratings” status.
# most of the notes needed more ratings
sum(status$timestamp_millis_of_first_non_nmr_status==-1)
barplot(table(status$current_status),
        ylab = "Number of Notes",
        xlab = "Current State",
        main = "Number of Notes by Status")

# most status are kept the same
table(status$current_status, status$locked_status)

# ratings ----

# there are 109,142 raters. Small for the number of notes.
# they cover 
length(unique(r0$rater_participant_id))

# only 13 people in r3 were in r1
# 42 people in r1 were in r3
# There is no more repetitions
sum(r1$rater_participant_id %in% 
       r3$rater_participant_id)

barplot(table(table(r0$rater_participant_id)),
        xlab = "Number of Ratings",
        ylab = "Number of Raters",
        main = "Distribution of Ratings Published by Rater")

## ratings in notes ----
# most of the notes are in the ratings dataset
sum(r0$note_id %in% notes$note_id)
sum(r1$note_id %in% notes$note_id)
sum(r2$note_id %in% notes$note_id)
sum(r3$note_id %in% notes$note_id)

mean(notes$note_id %in% r0$note_id)
mean(notes$note_id %in% r1$note_id)
mean(notes$note_id %in% r2$note_id)
mean(notes$note_id %in% r3$note_id)

mean(r3$note_id %in% r1$note_id)

# Most notes have few ratings
barplot(table(table(r0$note_id)),
        xlab = "Number of Ratings",
        ylab = "Number of Notes",
        main = "Distribution of Ratings Published by Note")

table(r0$helpfulness_level)

## select variables from ratings at the notes level ----
rates_summarise <-
bind_rows(r0, 
          r1,
          r2, 
          r3) %>% 
  group_by(note_id) %>% 
  summarise(
            ratings = n(),
            agreement_rate = sum(agree)/n(),
            helpful_rate = sum(helpfulness_level =="HELPFUL",na.rm = T)/n(),
            not_helpful_rate = sum(helpfulness_level =="NOT_HELPFUL",na.rm = T)/n(),
            somewhat_helpful_rate = sum(helpfulness_level =="SOMEWHAT_HELPFUL",na.rm = T)/n()
            )

rates_summarise
# There is no missing values
vis_miss(slice_sample(rates_summarise,prop = 0.1))

save(rates_summarise,file = "data/rates_summarise.RData")

# merge data ----
# Notes in both data sets are not exactly the same
# 9.3% of the notes never received ratings 
1 - mean(notes_final$note_id %in% rates_summarise$note_id)

notes_merged <- left_join(notes_final, rates_summarise, by = join_by(note_id)) %>% 
  # some notes never received ratings, let's replace them with 0
  replace_na(list(ratings = 0,
                  agreement_rate = 0,
                  helpful_rate = 0,
                  not_helpful_rate = 0,
                  somewhat_helpful_rate = 0)) 
notes_merged <- 
  left_join(x = notes_merged, 
            y = status %>% 
              # select onlye the non duplicated rows
              filter(!(note_id %in% duplicated_notes_status)) %>% 
              # I only analyze the current status
              select(note_id,current_status), 
            by = join_by(note_id))
notes_merged


save(notes_merged,file = "data/notes_merged.RData")

