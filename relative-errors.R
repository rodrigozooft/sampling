# Calculate the relative error percentage again with a sample of 100 rows
attrition_srs100 <- attrition_pop %>% slice_sample(n = 100)

mean_attrition_srs100 <- attrition_srs100 %>% summarize(mean_attrition = mean(Attrition == "Yes"))

rel_error_pct100 <- (abs(mean_attrition_pop - mean_attrition_srs100) / mean_attrition_pop) * 100

# See the result
rel_error_pct100

# Replicate this code 500 times
mean_attritions <- replicate(
  n = 500, 
  expr = 
    attrition_pop %>% 
      slice_sample(n = 20) %>% 
      summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
      pull(mean_attrition)
)

# See the result
head(mean_attritions)

# Store mean_attritions in a tibble in a column named sample_mean
sample_means <- tibble(sample_mean = mean_attritions)

# Plot a histogram of the `sample_mean` column, binwidth 0.05
ggplot(sample_means, aes(sample_mean)) + 
  geom_histogram(binwidth = 0.05

# Expand a grid representing 5 8-sided dice
dice <- expand_grid(
    die1 = 1:8,
    die2 = 1:8,
    die3 = 1:8,
    die4 = 1:8,
    die5 = 1:8
)

# See the result
dice

# From previous step
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) %>% 
  mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)

# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(factor(mean_roll))) + geom_bar()

# From previous steps
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)
sample_means <- tibble(
  sample_mean = sample_means_1000
)

# Using sample_means, draw a bar plot of sample_mean as a factor
ggplot(sample_means, aes(factor(sample_mean))) + geom_bar()

# Calculate the mean across replicates of the mean attritions in sampling_distribution_5
mean_of_means_5 <- sampling_distribution_5 %>% 
  summarize(mean_mean_attrition = mean(mean_attrition))

# Do the same for sampling_distribution_50
mean_of_means_50 <- sampling_distribution_50 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))


# ... and for sampling_distribution_500
mean_of_means_500 <- sampling_distribution_500 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# See the results
mean_of_means_5
mean_of_means_50
mean_of_means_500

# From previous step
sd_of_means_5 <- sampling_distribution_5 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))
sd_of_means_50 <- sampling_distribution_50 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))
sd_of_means_500 <- sampling_distribution_500 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))

# For comparison: population standard deviation
sd_attrition_pop <- attrition_pop %>% 
  summarize(sd_attrition = sd(Attrition == "Yes")) %>% 
  pull(sd_attrition)

# The sample sizes of each sampling distribution
sample_sizes <- c(5, 50, 500)

# Replicate this 1000 times
mean_danceability_1000 <- replicate(
  n = 1000,
  exp = {
    spotify_1_resample <- spotify_sample %>% 
      slice_sample(prop = 1, replace = TRUE)
    spotify_1_resample %>% 
      summarize(mean_danceability = mean(danceability)) %>% 
      pull(mean_danceability)
  }
)

# See the result
mean_danceability_1000

# From previous steps
mean_danceability_1000 <- load_step_4()

# Store the resamples in a tibble
bootstrap_distn <- tibble(
  resample_mean = mean_danceability_1000
)

# Draw a histogram of the resample means with binwidth 0.002

ggplot(bootstrap_distn, aes(resample_mean)) +
  geom_histogram(binwidth = 0.002)