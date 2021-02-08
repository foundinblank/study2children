# Prep data for OSF repository

library(tidyverse)
library(numform)

children <- read_csv("cleanedchildeyedata.csv") %>%
  mutate(language = case_when(
    language == "english" ~ "nonsigning",
    language =="sign" ~ "signing"
  )) %>%
  mutate(gender = case_when(
    gender == "Female" ~ "f",
    gender == "Male" ~ "m"
  )) %>%
  select(-recording, -analysis) %>%
  filter(participant != 'Ethan 10.5 y CODA GOOD')
  

children_names <- children %>% 
  select(participant) %>% 
  distinct() %>%
  rownames_to_column() %>%
  mutate(rowname = f_pad_zero(rowname)) %>%
  mutate(cleaned_participant = paste0('child_', rowname))
  

children <- children %>% 
  left_join(children_names, by = "participant") %>%
  select(-participant, -rowname, -mark, -onset, -offset) %>%
  rename(participant = cleaned_participant,
         story_secs = clip_sec,
         aoi_hits = hits,
         percent_of_story = percent,
         aoi_secs = secs) %>%
  mutate(age_group = 'infant') %>%
  select(participant, language, gender, age, age_group, group, story, story_secs, direction, trial, repetition, aoi, aoi_hits, aoi_secs, percent_of_story)



infants <- read_csv("cleanedbabyeyedata.csv") %>%
  mutate(language = case_when(
    language == "english" ~ "nonsigning",
    language =="sign" ~ "signing"
  )) %>%
  mutate(gender = case_when(
    gender == "Female" ~ "f",
    gender == "Male" ~ "m"
  )) %>%
  select(-recording, -analysis) %>%
  filter(participant != 'za02pa_1y11m') %>%
  filter(participant != 'Janie CODA 16 m')

infant_names <- infants %>% 
  select(participant) %>% 
  distinct() %>%
  rownames_to_column() %>%
  mutate(rowname = f_pad_zero(rowname)) %>%
  mutate(cleaned_participant = paste0('infant_', rowname))

infants <- infants %>% 
  left_join(infant_names, by = "participant") %>%
  select(-participant, -rowname, -mark, -onset, -offset) %>%
  rename(participant = cleaned_participant,
         story_secs = clip_sec,
         aoi_hits = hits,
         percent_of_story = percent,
         aoi_secs = secs) %>%
  mutate(age_group = 'infant') %>%
  select(participant, language, gender, age, age_group, group, story, story_secs, direction, trial, repetition, aoi, aoi_hits, aoi_secs, percent_of_story)



## Check numbers against Table 1 in manuscript

children %>%
  group_by(language, gender) %>%
  summarise(ppl = n_distinct(participant))

children %>%
  select(participant, language, age) %>%
  distinct() %>%
  group_by(language) %>%
  summarise(ppl = n(),
            age_mean = mean(age),
            age_se = sd(age)/sqrt(n()),
            age_min = min(age),
            age_max = max(age))

infants %>%
  group_by(language, gender) %>%
  summarise(ppl = n_distinct(participant))

infants %>%
  select(participant, language, age) %>%
  distinct() %>%
  group_by(language) %>%
  summarise(ppl = n(),
            age_mean = mean(age),
            age_se = sd(age)/sqrt(n()),
            age_min = min(age),
            age_max = max(age))


bind_rows(infants, children) %>%
  glimpse() %>%
  write_csv('bosworth_stone_all_data_for_osf_20210208.csv')
