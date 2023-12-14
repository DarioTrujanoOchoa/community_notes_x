# prepare_data.R
# Merge the raw data to feed the models
# Dario Trujano-Ochoa

# packages
library(pacman)
p_load(tidyverse, 
       lubridate,
       naniar,
       janitor)

# load data ----
# notes
notes <- read_tsv("data/notes-00000.tsv")

## status
status <- read_tsv("data/noteStatusHistory-00000.tsv")

## ratings
r0 <- read_tsv("data/ratings-00000.tsv")
r1 <- read_tsv("data/ratings-00001.tsv")
r2 <- read_tsv("data/ratings-00002.tsv")
r3 <- read_tsv("data/ratings-00003.tsv")

# Almost all the notes in notes are in status
# only 20 notes are not there
sum(notes$noteId %in% status$noteId)

s_notes <- slice_sample(notes,n = 10000) 
s_status <- slice_sample(status,n = 10000) 

# The variables Believable, harmful, and validation difficulty were Deprecated as of 2022-10-27.
vis_miss(s_notes)
vis_miss(s_status)

# notes ----

## Authors ----
# the observations in the dataset are unique for each note
length(unique(notes$noteId))

# the Some notes are made by more than one participant
length(unique(s_notes$noteAuthorParticipantId))
# most users publish a note only once (60% in sample) 
table(table(s_notes$noteAuthorParticipantId)) 
barplot(table(table(s_notes$noteAuthorParticipantId)),
        xlab = "Number of Notes",
        ylab = "Number of Authors",
        main = "Distribution of Notes Published by Author")
# ID of prolific authors
head(sort(table(s_notes$noteAuthorParticipantId),decreasing = T),n = 25 )

## Tweets ----
# some tweets have more than one note
# most of the tweets have a single note
length(unique(notes$tweetId))

barplot(table(table(s_notes$tweetId)),
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



table(s_notes$classification,s_notes$notMisleadingOther)
table(s_notes$classification,s_notes$isMediaNote)

table(s_notes$classification,s_notes$trustworthySources)

# select the variables that will be used in the model from the notes dataset
notes_final <-
  notes %>% select(
    noteId,
    classification,
     trustworthySources,
     summary, 
     isMediaNote
     ) %>% 
  mutate(note_length = nchar(summary))

# There is no missing values
vis_miss(slice_sample(notes_final,prop = 0.1))

# status ----

# the observations in the dataset are unique
length(unique(status$noteId))

# Contains -1 if the note never left “Needs More Ratings” status.
# most of the notes needed more ratings
sum(status$timestampMillisOfFirstNonNMRStatus==-1)
barplot(table(status$currentStatus))

table(status$currentStatus, status$lockedStatus)


# ratings ----

# there are 109,142 raters. Small for the number of notes.
# they cover 
length(unique(r0$raterParticipantId))

# only 13 people in r3 were in r1
# 42 people in r1 were in r3
# There is no more repetitions
sum(r1$raterParticipantId %in% 
       r3$raterParticipantId)

barplot(table(table(r0$raterParticipantId)),
        xlab = "Number of Ratings",
        ylab = "Number of Raters",
        main = "Distribution of Ratings Published by Rater")

## ratings in notes ----
# most of the notes are in the ratings dataset
sum(r0$noteId %in% notes$noteId)
sum(r1$noteId %in% notes$noteId)
sum(r2$noteId %in% notes$noteId)
sum(r3$noteId %in% notes$noteId)

mean(notes$noteId %in% r0$noteId)
mean(notes$noteId %in% r1$noteId)
mean(notes$noteId %in% r2$noteId)
mean(notes$noteId %in% r3$noteId)

mean(r3$noteId %in% r1$noteId)

# Most notes have few ratings
barplot(table(table(r0$noteId)),
        xlab = "Number of Ratings",
        ylab = "Number of Notes",
        main = "Distribution of Ratings Published by Note")

table(r0$helpfulnessLevel)

rates_summarise <-
bind_rows(r0, 
          r1,
          r2, 
          r3) %>% 
  group_by(noteId) %>% 
  summarise(
            ratings = n(),
            agreement_rate = sum(agree)/n(),
            helpful_rate = sum(helpfulnessLevel =="HELPFUL",na.rm = T)/n(),
            not_helpful_rate = sum(helpfulnessLevel =="NOT_HELPFUL",na.rm = T)/n(),
            somewhat_helpful_rate = sum(helpfulnessLevel =="SOMEWHAT_HELPFUL",na.rm = T)/n()
            )

rates_summarise
# There is no missing values
vis_miss(slice_sample(rates_summarise,prop = 0.1))

# merge data ----
# Notes in both data sets are not exactly the same
# 9.3% of the notes never received ratings 
1 - mean(notes_final$noteId %in% rates_summarise$noteId)

notes_merged <- left_join(notes_final, rates_summarise, by = join_by(noteId)) %>% 
  clean_names() %>% # let's clean the data names
  # some notes never received ratings, lte's replace them with 0
  replace_na(list(ratings = 0,
                  agreement_rate = 0,
                  helpful_rate = 0,
                  not_helpful_rate = 0,
                  somewhat_helpful_rate = 0))
notes_merged


save(notes_merged,file = "data/notes_merged.RData")

