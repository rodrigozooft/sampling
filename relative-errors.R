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