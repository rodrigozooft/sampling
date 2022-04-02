# From previous step
rent_med_ci <- manhattan %>%
  specify(response = rent) %>%  
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "median")
  
# Plot the rent_med_ci statistic
ggplot(rent_med_ci, aes(stat)) +
  # Make it a histogram with a binwidth of 50
  geom_histogram(binwidth = 50)

# From previous steps
rent_med_obs <- manhattan %>%
  summarize(median_rent = median(rent)) %>%
  pull()
degrees_of_freedom <- nrow(manhattan) - 1 
t_star <- qt(0.975, df = degrees_of_freedom)

# Calculate the CI using the std error method
rent_med_ci %>%
  # Calculate the std error of the statistic
  summarize(boot_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = rent_med_obs - t_star * boot_se,
    u = rent_med_obs + t_star * boot_se
  )
  
# Recall the CI using the percentile method from step 1
rent_med_ci %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

# From previous steps
ncbirths_complete_visits <- ncbirths %>%
  filter(!is.na(visits))
visit_mean_ci <- ncbirths_complete_visits %>%
  specify(response = visits) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean")
  
# Calculate the 90% CI via percentile method
visit_mean_ci %>%
  summarize(
    l = quantile(stat, 0.05),
    u = quantile(stat, 0.95)
  )

# From previous step
visit_sd_ci <- ncbirths_complete_visits %>%
  specify(response = visits) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "sd")
  
# Calculate the 90% CI via percentile method
visit_sd_ci %>%
  summarize(
    l = quantile(stat, 0.05),
    u = quantile(stat, 0.95)
  )