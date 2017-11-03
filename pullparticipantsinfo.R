library(tidyverse)
library(feather)

csvpath = "../Child Data/_aoidata/"
files <- dir(path = csvpath, pattern = "_aoidata")
files <- paste(csvpath, files, sep="")
data <- files %>%
  map(read_csv) %>%
  reduce(rbind)

participants <- data %>% 
  select(-condition, -mark, -trial, -repetition, -onset, -offset, -aoi, -hits) %>%
  distinct() %>%
  mutate(participant = case_when(
    participant == "Ab07ov09_22m" ~ "Ab07ov09_32m",
    TRUE ~ participant
  ))

ages <- read_csv("childrenages.csv")
participants <- participants %>% left_join(ages, by = "participant") %>%
  select(participant, recording, analysis, group, age, gender, language)

head(participants)
write_csv(participants, 'participants.csv')
