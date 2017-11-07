# Outlier Checking Script 

# We need to do this once for children then again for babies, ok? Let's remember that! 

currdata <- data_ci
currdata <- data_km
currdata <- data_3b
currdata <- data_rr

aoinames <- unique(data$aoi) %>% sort()
curraoi <- aoinames[1]

#Generate the histogram for that fairy tale and AOI  

data %>%
  filter(aoi == curraoi) %>%
  ggplot(aes(x = percent)) + geom_histogram() + 
  facet_grid(story ~ .) + 
  theme(strip.text.y = element_text(angle = 0), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  ylab("") +
  xlim(0,1) + 
  ggtitle(curraoi)

# Generate the table for that aoi 
curraoi <- aoinames[19]

currtable <- currdata %>%
  filter(aoi == curraoi) %>%
  arrange(desc(percent)) %>%
  select(participant,group,age,story,aoi,percent) %>%
  print()

# Add to outlier table
outlier2 <- currtable %>% slice(1) %>% rbind(outlier2)


# Organize outlier table
outlier2 <- outlier2 %>% arrange(participant)
write_csv(outlier2, "~/Desktop/newoutliers.csv")
