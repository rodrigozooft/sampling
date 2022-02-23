# Update the histogram to use spotify_mysterious_sample with x-axis limits from 0 to 1
ggplot(spotify_population, aes(acousticness)) +
  geom_histogram(binwidth = 0.01)

# Visualize the distribution of duration_minutes as a histogram with a binwidth of 0.5
spotify_population %>% 
    ggplot(aes(x = duration_minutes)) + 
    geom_histogram(binwidth = 0.5)

# From previous step
randoms <- data.frame(
  uniform = runif(n_numbers, min = -3, max = 3),
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of uniform values, binwidth 0.25
randoms %>%
  ggplot(aes(x = uniform)) +
  geom_histogram(binwidth = 0.25)

# From previous step
randoms <- data.frame(
  uniform = runif(n_numbers, min = -3, max = 3),
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of normal values, binwidth 0.5
randoms %>%
  ggplot(aes(x = normal)) + 
  geom_histogram(binwidth = 0.5)

# View the attrition_pop dataset
View(attrition_pop)

# Set the seed
set.seed(281)

attrition_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using simple random sampling
  slice_sample(n = 200)

# View the attrition_samp dataset
View(attrition_samp)  