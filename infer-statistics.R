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

# From previous step
n_replicates <- 15000
rent_med_ht <- manhattan %>%
  specify(response = rent) %>%
  hypothesize(null = "point", med = 2500) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "median")
rent_med_obs <- manhattan %>%
  summarize(median_rent = median(rent)) %>%
  pull()
  
rent_med_ht %>%
  # Filter for bootstrap stat greater than or equal to observed stat
  filter(stat >= rent_med_obs) %>%
  # Calculate the p-value
  summarize(p_val = n() / 15000)

# From previous steps
n_replicates <- 1500
weight_mean_ht <- ncbirths %>%
  specify(response = weight) %>%
  hypothesize(null = "point", mu = 7) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "mean")
weight_mean_obs <- ncbirths %>%
  summarize(mean_weight = mean(weight)) %>%
  pull()

# Calculate p-value
weight_mean_ht %>%
  # Filter on stat greater than or equal to weight_mean_obs
  filter(stat >= weight_mean_obs) %>%
  # p_val is twice the number of filtered rows divided by the total number of rows
  summarize(
    one_sided_p_val = n() / n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# P(T < 3) for df = 10
(x <- pt(3, df = 10))

# P(T > 3) for df = 10
(y <- 1- pt(3, df = 10))

# P(T > 3) for df = 100
(z <- 1- pt(3, df = 100))

# Comparison
y == z
y > z
y < z

# 95th percentile for df = 10
(x <- qt(0.95, df = 10))

# Upper bound of middle 95th percent for df = 10
(y <- qt(0.975, df = 10))

# Upper bound of middle 95th percent for df = 100
(z <- qt(0.975, df = 100))

# Comparison
y == z
y > z
y < z

# Filter for employed respondents
acs12_emp <- acs12 %>%
  filter(employment == "employed")

# Construct 95% CI for avg time_to_work
t.test(acs12_emp$time_to_work, conf.level = 0.95)

# Run a t-test on hrs_work and look at the CI
t.test(acs12_emp$hrs_work, conf.interval = 0.95)

# Run a t-test on diff with a 90% CI
t.test(textbooks$diff, conf.level = 0.9)

# Same with 95% CI
t.test(textbooks$diff, conf.level = 0.95)

# Same with 99% CI
t.test(textbooks$diff, conf.level = 0.99)