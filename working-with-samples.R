# Update the histogram to use spotify_mysterious_sample with x-axis limits from 0 to 1
ggplot(spotify_population, aes(acousticness)) +
  geom_histogram(binwidth = 0.01)