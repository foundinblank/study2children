library(tidyverse)
library(janitor)

df <- read_csv("/Users/adamstone/Dropbox/_UCSD/Data Analysis/Child Data/_processed/Prosody ASL Story Clips_Prosody Group 1_ Ainsely 5y1m GREAT.csv") %>% clean_names()

df1 <- df %>% select(gaze_point_index, aoi_mid_face_bottom_2_hit_3) %>%
  rename(hit = aoi_mid_face_bottom_2_hit_3) %>%
  mutate(hit = as.integer(hit))

help <- tribble(
  ~id, ~A, ~B, ~C,
  1, 0, 0, 1,
  2, 0, 0, 1,
  3, 0, 1, 0,
  4, 0, 1, 0,
  5, 1, 0, 0,
  6, 1, 0, 0,
  7, 0, 1, 0
)

help %>%
  gather(key, value, -id) %>% 
  filter(value == 1) %>%
  select(-value) %>%
  arrange(id)


df <- read_csv("/Users/adamstone/Dropbox/_UCSD/Data Analysis/Child Data/_processed/Prosody ASL Story Clips_Prosody Group 1_ Ainsely 5y1m GREAT.csv") %>% 
  clean_names() %>%
  select(-c(participant_name, studio_project_name, studio_test_name, age_value, analysis_value, 
            gender_value, language_value, recording_name, recording_date, recording_duration,
            media_pos_x_adc_spx, media_pos_y_adc_spx, media_name, media_width, media_height,
            gaze_point_x_mc_spx, gaze_point_y_mc_spx)) %>%
  mutate_if(is.character, as.integer)

aoi_values <- df %>%
  gather(aoi, value, -gaze_point_index) %>%
  filter(value == 1) %>%
  select(-value) %>%
  arrange(gaze_point_index) %>%
  mutate(aoi = str_remove(aoi, "aoi_"),
         aoi = str_remove(aoi, "_hit"),
         aoi = str_remove(aoi, "_\\d$"),
         aoi = str_remove(aoi, "left_"),
         aoi = str_remove(aoi, "mid_"),
         aoi = str_remove(aoi, "right_")) %>%
  rename(time = gaze_point_index)

df_cleaned <- read_csv("/Users/adamstone/Dropbox/_UCSD/Data Analysis/Child Data/_processed/Prosody ASL Story Clips_Prosody Group 1_ Ainsely 5y1m GREAT.csv") %>% 
  clean_names() %>%
  select(gaze_point_index, participant_name, media_name, gaze_point_y_mc_spx) %>%
  rename(time = gaze_point_index,
         y = gaze_point_y_mc_spx) %>%
  left_join(aoi_values) %>%
  filter(!is.na(time)) %>%
  group_by(time) %>%
  slice(1) %>%
  ungroup()
  
midas <- df_cleaned %>%
  filter(str_detect(media_name, "Midas")) %>% ## OOPS IT'S 4 TRIALS...
  arrange(time) %>%
  filter(aoi != "side") %>%
  # filter(!str_detect(aoi, "left")) %>%
  # filter(!str_detect(aoi, "right")) %>%
  mutate(aoi = factor(aoi, levels = c("belly","below_chest","chest_bottom",
                                      "chest_center","chest_top",
                                      "face_bottom","face_center",
                                      "face_top")))

midas_time <- select(midas, time)
midas_time <- midas_time[1:(nrow(midas_time)-1),]
midas_rest <- select(midas, -time)
midas_rest <- midas_rest[2:nrow(midas_rest),]
midas <- bind_cols(midas_time, midas_rest)

# min_midas <- min(midas$time)
# max_midas <- max(midas$time)
# seq_midas <- seq(min_midas, max_midas)
# seq_midas <- tibble(seq_midas)
# 
# midas <- midas %>% right_join(seq_midas, by = c("time" = "seq_midas"))

ggplot(midas, aes(x = time, y = aoi, group = 1)) + geom_jitter(alpha = 0.25, height = 0.2)
ggplot(midas, aes(x = time, y = aoi, group = 1)) + geom_point(alpha = 0.25, height = 0.2)
ggplot(midas, aes(x = time, y = aoi, group = 1)) + geom_point(alpha = 0.5, size = 0.5) + geom_path()
ggplot(midas, aes(x = time, y = -1*y)) + geom_path() + 
  scale_y_continuous(limits = c(-720, 0)) + ylab("y-axis")
