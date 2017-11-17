# Working on pulling x/y info from csv files? 

library(tidyverse)
library(janitor)
baby <- read_csv("../Child Data/_processed/Prosody ASL Story Clips_Prosody Group 1_ Ainsely 5y1m GREAT.csv") %>%
  select(-starts_with('AOI')) %>% clean_names()

medianame <- baby$medianame
which(is.na(medianame))

mediagaze <- baby %>%
  select(medianame, gazepointindex) %>%
  mutate(gazepointindex = case_when(
    is.na(medianame) ~ NA_integer_, 
    TRUE ~ gazepointindex
  ))

thing <- mediagaze$gazepointindex
thing2 <- zoo::na.locf(thing)

baby <- baby %>%
  mutate(medianame = tolower(medianame)) %>%
  filter(!is.na(medianame)) %>%
  group_by(medianame) %>%
  summarise()