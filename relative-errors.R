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

# Generate a bootstrap distribution
mean_popularity_2000_boot <- replicate(
  # Use 2000 replicates
  n = 2000,
  expr = {
    # Start with the sample
    spotify_sample %>% 
      # Sample same number of rows with replacement
      slice_sample(n = 500, replace = TRUE) %>% 
      # Calculate the mean popularity
      summarize(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull(mean_popularity)
  }
)

# See the result
mean_popularity_2000_boot

# Calculate the true population mean popularity
pop_mean <- spotify_population %>% summarize(mean_popularity = mean(popularity))


# Calculate the original sample mean popularity
samp_mean <- spotify_sample %>% summarize(mean_popularity = mean(popularity))


# Calculate the sampling dist'n estimate of mean popularity
samp_distn_mean <- sampling_distribution %>% summarize(mean_popularity = mean(sample_mean))


# Calculate the bootstrap dist'n estimate of mean popularity
boot_distn_mean <- bootstrap_distribution %>% summarize(mean_popularity = mean(resample_mean))


# See the results
c(pop = pop_mean, samp = samp_mean, sam_distn = samp_distn_mean, boot_distn = boot_distn_mean)

# Calculate the true popluation std dev popularity
pop_sd <- spotify_population %>% summarize(sd_pop = sd(popularity))


# Calculate the true sample std dev popularity
samp_sd <- spotify_sample %>% summarize(sd_sample = sd(popularity))


# Calculate the sampling dist'n estimate of std dev popularity
samp_distn_sd <- sampling_distribution %>% summarize(sd_sample_dist = sd(sample_mean)* sqrt(500))


# Calculate the bootstrap dist'n estimate of std dev popularity
boot_distn_sd <- bootstrap_distribution %>% summarize(sd_boost_dist = sd(resample_mean)*sqrt(500))


# See the results
c(pop = pop_sd, samp = samp_sd, sam_distn = samp_distn_sd, boot_distn = boot_distn_sd)

# Generate a 95% confidence interval using the std error method
conf_int_std_error <- bootstrap_distribution %>% 
  summarize(
    point_estimate = mean(resample_mean),
    standard_error = sd(resample_mean),
    lower = qnorm(0.025, point_estimate, standard_error),
    upper = qnorm(0.975, point_estimate, standard_error)
  )

# See the result
conf_int_std_error

# Calculate the z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error

# Calculate the p-value
p_value <- pnorm(z_score, lower.tail = FALSE)
                 
# See the result
p_value

# Calculate 95% confidence interval using quantile method
conf_int_quantile <- late_shipments_boot_distn %>%
  summarize(
    lower = quantile(prop_late_shipments, 0.025)
    upper = quantile(prop_late_shipments, 0.975)
  )

# See the result
conf_int_quantile

# Calculate the numerator of the test statistic
numerator <- xbar_no - xbar_yes

# Calculate the denominator of the test statistic
denominator <- sqrt(s_no ^ 2 / n_no + s_yes ^ 2 / n_yes)

# Calculate the test statistic
t_stat <- numerator / denominator

# See the result
t_stat

# Calculate the degrees of freedom
degrees_of_freedom <- n_no + n_yes - 2

# Calculate the p-value from the test stat
p_value <- pt(t_stat, degrees_of_freedom, lower.tail = TRUE)

# See the result
p_value

# View the dem_votes_potus_12_16 dataset
View(dem_votes_potus_12_16)

# Calculate the differences from 2012 to 2016
sample_dem_data <- dem_votes_potus_12_16 %>%
    mutate(diff = dem_percent_12 - dem_percent_16)

# See the result
sample_dem_data

# From previous step
sample_dem_data <- dem_votes_potus_12_16 %>% 
  mutate(diff = dem_percent_12 - dem_percent_16)

# Using sample_dem_data, plot diff as a histogram
sample_dem_data %>% ggplot(aes(diff)) + geom_histogram(binwidth = 1)

# The paired t-test
t.test(
  x = sample_dem_data$dem_percent_12,
  y = sample_dem_data$dem_percent_16,
  paired = FALSE,
  alternative = "greater",
  mu = 0
)

# Using late_shipments, group by shipment mode, and calculate the mean and std dev of pack price
late_shipments %>%
    group_by(shipment_mode) %>%
    summarize(xbar_pack_price = mean(pack_price),s_pack_price = sd(pack_price))

# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
 ggplot(late_shipments, aes(x = shipment_mode, y = pack_price)) +
    geom_boxplot()+
    coord_flip()

# From previous step
mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, data = late_shipments)

# Perform ANOVA on the regression model
anova(mdl_pack_price_vs_shipment_mode)