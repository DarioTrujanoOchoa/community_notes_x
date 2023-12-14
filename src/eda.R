# eda.R

# Dario Trujano-Ochoa

# packages 
library(pacman)
p_load(tidyverse,
       ggplo2)

load("data/notes_merged.RData")

# missing data
vis_miss(slice_sample(notes_merged,prop = 0.1))

