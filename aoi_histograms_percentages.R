ggplot(group2, aes(x = percent)) + geom_histogram() + facet_grid(aoi~story)

group2 %>%
  filter(aoi == "LeftChestBottom" & story == "Cinderella_2") %>%
  select(participant, analysis, percent) %>%
  arrange(desc(percent))

group2 %>% select(aoi) %>% distinct()