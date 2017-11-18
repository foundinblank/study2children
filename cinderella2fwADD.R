# Code to add Cinderella2FW 

library(tidyverse)

cinderella2fw_from_matlab <- read_csv("../Child Data/Cinderella2FW_BelowChest_aoidata.csv") 

sums <- cinderella2fw_from_matlab %>% 
  group_by(participant,repetition) %>%
  summarise(BelowChest = sum(BelowChest))

participant_info <- cinderella2fw_from_matlab %>%
  select(-BelowChest) %>%
  distinct()

cinderella2fw <- sums %>% 
  left_join(participant_info, by = c("participant","repetition")) %>%
  ungroup() %>%
  add_column(aoi = rep("sum_AOI_BelowChest_Hit",73)) %>%
  rename(hits = BelowChest)

data <- read_feather("childrawdata.feather") %>%
  filter(participant %in% cinderella2fw$participant) %>%
  filter(condition == "Cinderella_2_FW_xvid.avi") %>%
  select(-aoi, -hits) %>%
  distinct()

thejoinedone <- cinderella2fw %>%
  select(-recording, -story, -direction) %>%
  left_join(data, by = c("participant","repetition")) %>%
  filter(!is.na(analysis))

data <- read_feather("childrawdata.feather") %>%
  bind_rows(thejoinedone)
