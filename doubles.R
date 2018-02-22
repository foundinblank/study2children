library(tidyverse)
library(janitor)
doubles <- read_csv("../signgesturedata/doubles.csv") %>%
  clean_names() %>%
  mutate(recording = str_sub(recording, 0, -2)) %>%
  gather(aoi, sec, gesta_fw_belly:signd_er_rightfacetop) %>% 
  spread(period, sec) %>%
  filter(recording == "ShingYi") %>% # to choose one participant
  View()
