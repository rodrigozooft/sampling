# From previous step
rent_med_ci <- manhattan %>%
  specify(response = rent) %>%  
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "median")
  
# Plot the rent_med_ci statistic
ggplot(rent_med_ci, aes(stat)) +
  # Make it a histogram with a binwidth of 50
  geom_histogram(binwidth = 50)