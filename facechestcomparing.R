library(tidyverse)


# Comparing Old and New ---------------------------------------------------
original <- read_csv("../Child Data/_aoidata/ma01wa22 10m GOOD_aoidata.csv") %>%
  separate(aoi, into = c("math", "label", "aoi", "hitlabel", "indicator"), sep = "_") %>%
  select(-math, -label, -hitlabel, -indicator) %>%
  mutate(aoi = case_when(
    str_detect(aoi, "\\d") ~ str_sub(aoi, 0, -2),
    TRUE ~ aoi
  )) %>%
  filter(str_detect(aoi, "MidFace")) %>%
  spread(aoi, hits) %>%
  group_by(trial) %>%
  mutate(origMidFaceHits = sum(MidFaceBottom, MidFaceCenter, MidFaceTop)) %>%
  select(trial, origMidFaceHits)

corrected <- read_csv("~/Desktop/ma01wa22 10m GOOD_aoidata CORRECTED.csv") %>%
  separate(aoi, into = c("math", "label", "aoi", "hitlabel", "indicator"), sep = "_") %>%
  select(-math, -label, -hitlabel, -indicator) %>%
  mutate(aoi = case_when(
    str_detect(aoi, "\\d") ~ str_sub(aoi, 0, -2),
    TRUE ~ aoi
  )) %>%
  filter(str_detect(aoi, "MidFace")) %>%
  spread(aoi, hits) %>%
  group_by(trial) %>%
  mutate(newMidFaceHits = sum(MidFaceBottom, MidFaceCenter, MidFaceTop)) %>%
  select(trial, newMidFaceHits)

compare <- left_join(original, corrected, by = "trial") %>% arrange(trial)

original <- original %>% select(trial, MidFaceBottom, MidFaceCenter, MidFaceTop) %>% arrange(trial)
corrected <- corrected %>% select(trial, MidFaceBottom, MidFaceCenter, MidFaceTop) %>% arrange(trial)



# Comparing FaceChest Variables -------------------------------------------
original <- read_csv("../Child Data/_aoidata/ma01wa22 10m GOOD_aoidata.csv") %>%
  separate(aoi, into = c("math", "label", "aoi", "hitlabel", "indicator"), sep = "_") %>%
  select(-math, -label, -hitlabel, -indicator) %>%
  mutate(aoi = case_when(
    str_detect(aoi, "\\d") ~ str_sub(aoi, 0, -2),
    TRUE ~ aoi
  )) %>%
  spread(aoi, hits) %>%
  rowwise() %>%
  mutate(face = sum(MidFaceCenter, MidFaceBottom),
         chest = sum(MidChestTop, MidChestCenter, MidChestBottom, BelowChest),
         fcr = (face - chest) / (face + chest))

corrected <- read_csv("~/Desktop/ma01wa22 10m GOOD_aoidata CORRECTED.csv") %>%
  separate(aoi, into = c("math", "label", "aoi", "hitlabel", "indicator"), sep = "_") %>%
  select(-math, -label, -hitlabel, -indicator) %>%
  mutate(aoi = case_when(
    str_detect(aoi, "\\d") ~ str_sub(aoi, 0, -2),
    TRUE ~ aoi
  )) %>%
  spread(aoi, hits) %>%
  rowwise() %>%
  mutate(face = sum(MidFaceCenter, MidFaceBottom),
         chest = sum(MidChestTop, MidChestCenter, MidChestBottom, BelowChest),
         fcr = (face - chest) / (face + chest))

library(feather)
data <- read_feather("cleanedbabyeyedata.feather")
data_mid <- data %>%
  filter(str_detect(aoi,"Mid") | aoi == "BelowChest")
data_mid <- data_mid %>%
  select(-secs, -hits) %>%
  spread(aoi,percent) %>%
  group_by(participant, trial) %>%
  mutate(face = sum(MidFaceCenter, MidFaceBottom, na.rm = TRUE),
         chest = sum(MidChestTop, MidChestCenter, MidChestBottom, BelowChest, na.rm = TRUE),
         fcr = (face - chest) / (face + chest),
         mfcr = (MidFaceBottom - MidChestTop) / (MidFaceBottom + MidChestTop))
data_mid <- data_mid %>%
  filter(language=="english") %>%
  select(participant, trial, fcr)

original <- original %>%
  select(participant, trial, fcr)
corrected <- corrected %>%
  select(participant, trial, fcr)

data_mid <- bind_rows(data_mid, original)

corrected <- corrected %>%
  mutate(participant = paste(participant, "CORRECTED", sep = "_"))

data_mid <- bind_rows(data_mid, corrected)

ggplot(data_mid, aes(x = participant, y = fcr)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
