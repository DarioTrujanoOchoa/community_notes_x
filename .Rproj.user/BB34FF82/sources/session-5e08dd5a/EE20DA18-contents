

library(pacman)
p_load(tidyverse, 
       naniar)

notes <- read_tsv("data/notes-00000.tsv")
spec(notes)
s_notes <- slice_sample(notes,n = 10000) 

# all the variables starting with misleading are answers to the reason something was misleading
# therefore cannot be used to explain why a note is misleading or not

as_tibble(s_notes) %>% select_if(is.numeric) %>%
  map_dbl(sum) %>% as.data.frame()

vis_miss(s_notes)

table(s_notes$classification,s_notes$misleadingOther)
table(s_notes$classification,s_notes$misleadingFactualError)
