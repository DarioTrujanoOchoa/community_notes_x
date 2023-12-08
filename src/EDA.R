

library(pacman)
p_load(tidyverse, 
       naniar)

# load data ----
notes <- read_tsv("data/notes-00000.tsv")
spec(notes)

status <- read_tsv("data/noteStatusHistory-00000.tsv")
spec(status)

r0 <- read_tsv("data/ratings-00000.tsv")
r1 <- read_tsv("data/ratings-00001.tsv")
r2 <- read_tsv("data/ratings-00002.tsv")
r3 <- read_tsv("data/ratings-00003.tsv")

spec(r0)

# Almost all the notes in notes are in status
# only 20 notes are not there
sum(notes$noteId %in% status$noteId)

s_notes <- slice_sample(notes,n = 10000) 
s_status <- slice_sample(status,n = 10000) 
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

notes_final <-
  notes %>% select(
    noteId,
    classification,
     trustworthySources,
     summary, 
     isMediaNote
     ) %>% 
  mutate(note_length = nchar(summary))

# status ----

# the observations in the dataset are unique
length(unique(status$noteId))

# Contains -1 if the note never left “Needs More Ratings” status.
# most of the notes needed more ratings
sum(status$timestampMillisOfFirstNonNMRStatus==-1)
barplot(table(status$currentStatus))

table(status$currentStatus, status$lockedStatus)


# ratings ----

length(unique(r0$raterParticipantId))

barplot(table(table(r0$raterParticipantId)),
        xlab = "Number of Ratings",
        ylab = "Number of Raters",
        main = "Distribution of Ratings Published by Rater")

# most of the notes are in the ratings dataset
sum(r0$noteId %in% notes$noteId)
sum(notes$noteId %in% r0$noteId)

# Most notes have few ratings
barplot(table(table(r0$noteId)),
        xlab = "Number of Ratings",
        ylab = "Number of Notes",
        main = "Distribution of Ratings Published by Note")

table(r0$helpfulnessLevel)

rates_summarise <-
r0 %>% 
  group_by(noteId) %>% 
  summarise(
            ratings = n(),
            agreement_rate = sum(agree)/n(),
            helpful = sum(helpfulnessLevel =="HELPFUL",na.rm = T),
            not_helpful = sum(helpfulnessLevel =="NOT_HELPFUL",na.rm = T)
            )

rates_summarise

# merge data ----

notes_merged <- left_join(notes_final, rates_summarise, by = join_by(noteId))
notes_merged

lm(ratings ~ 
     classification +
     trustworthySources +
     note_length,
   data = notes_merged)

