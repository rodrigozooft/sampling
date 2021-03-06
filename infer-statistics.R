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

# From previous step
textdiff_med_ci <- textbooks %>%
  specify(response = diff) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "median")

# Calculate the 95% CI via percentile method
textdiff_med_ci %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

# From previous steps
n_replicates <- 15000
hsb2 <- hsb2 %>%
  mutate(diff = math - science)
scorediff_med_ht <- hsb2 %>%
  specify(response = diff) %>%
  hypothesize(null = "point", med = 0) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "median")
scorediff_med_obs <- hsb2 %>%
  summarize(median_diff = median(diff)) %>%
  pull()
  
# Calculate two-sided p-value
scorediff_med_ht %>%
  filter(stat >= scorediff_med_obs) %>%
  summarize(
    one_sided_p_val = n() / n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# From previous step
stem.cell <- stem.cell %>%
  mutate(change = after - before)
  
# Calculate observed difference in means
diff_mean <- stem.cell %>%
  # Group by treatment group
  group_by(trmt) %>%
  # Calculate mean change for each group
  summarize(mean_change = mean(change)) %>% 
  # Pull out the value
  pull() %>%
  # Calculate difference
  diff()
  
# See the result
diff_mean

# From previous step
n_replicates <- 1000
diff_mean_ht <- stem.cell %>%
  specify(change ~ trmt) %>% 
  hypothesize(null = "independence") %>%  
  generate(reps = n_replicates, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("esc", "ctrl"))
  
diff_mean_ht %>%
  # Filter for simulated test statistics at least as extreme as observed
  filter(stat >= diff_mean) %>%
  # Calculate p-value
  summarize(p_val = n() / n_replicates)

# From previous steps
ncbirths_complete_habit <- ncbirths %>%
  filter(!is.na(habit))
diff_mean_obs <- ncbirths_complete_habit %>%
  group_by(habit) %>%
  summarize(mean_weight = mean(weight)) %>%
  pull() %>%
  diff()
n_replicates <- 1000
diff_mean_ht <- ncbirths_complete_habit %>% 
  specify(weight ~ habit) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = n_replicates, type = "permute") %>%
  calculate(stat = "diff in means", order = c("nonsmoker", "smoker")) 
  
# Calculate p-value
diff_mean_ht %>%
  # Identify simulated test statistics at least as extreme as observed
  filter(stat <= diff_mean_obs)  %>%
  # Calculate p-value
  summarize(
    one_sided_p_val = n() / n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# From previous step
diff_mean_ci <- ncbirths_complete_habit %>%
  specify(weight ~ habit) %>%
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("nonsmoker", "smoker"))
  
# Calculate the 95% CI via percentile method
diff_mean_ci %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

# From previous step
ncbirths_complete_habit_weeks <- ncbirths %>%
  filter(!is.na(habit), !is.na(weeks))
diff_med_ci <- ncbirths_complete_habit_weeks %>%
  specify(weeks ~ habit) %>%
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "diff in medians", order = c("nonsmoker", "smoker"))

# Calculate the 92% CI via percentile method
diff_med_ci %>%
  summarize(
    l = quantile(stat, 0.04),
    u = quantile(stat, 0.96)
  )

# From previous steps
acs12_complete_hrlypay_citizen <- acs12 %>%
  filter(!is.na(hrly_pay), !is.na(citizen))
  
# Using acs12_complete_hrlypay_citizen, plot hrly_pay
ggplot(acs12_complete_hrlypay_citizen, aes(hrly_pay)) +
  # Add a histogram layer
  geom_histogram(binwidth = 5) +
  facet_grid(rows = vars(citizen))

# Construct 95% CI using a t-test
test_results <- t.test(hrly_pay ~ citizen, data = acs12_complete_hrlypay_citizen, conf.level = 0.95)

# See the results
test_results

# Using gss, plot wordsum
ggplot(gss, aes(wordsum)) +
  # Add a histogram layer
  geom_histogram(binwidth = 1) +
  # Facet by class
  facet_wrap(~class)

# Run an analysis of variance on wordsum vs. class
aov_wordsum_class <- aov(wordsum ~ class, data = gss)

# Tidy the model
tidy(aov_wordsum_class)

gss %>%
  # Group by class
  group_by(class) %>%
  # Calculate the std dev of wordsum as std_dev_wordsum
  summarize(std_dev_wordsum = sd(wordsum))

# Run a pairwise t-test on wordsum and class, without adjustment
t_test_results <- pairwise.t.test(gss$wordsum, gss$class, p.adjust.method = "none")

# Tidy the result
tidy(t_test_results)

# Load the mosaicData package and the RailTrail data
library(mosaicData)
data(RailTrail)

# Fit a linear model
ride_lm <- lm(volume ~ hightemp, data = RailTrail)

# View the summary of your model
summary(ride_lm)

# Print the tidy model output
tidy(ride_lm)

# Set the random number generator seed for reproducibility
set.seed(4747)

# From popdata, randomly sample 50 rows without replacement
sample1 <- popdata %>%
   sample_n(size = 50)

# Do the same again
sample2 <- popdata %>%
   sample_n(size = 50)

# Combine both samples
both_samples <- bind_rows(sample1, sample2, .id = "replicate")

# See the result
glimpse(both_samples)

# From previous step
set.seed(4747)
both_samples <- bind_rows(
  popdata %>% sample_n(size = 50), 
  popdata %>% sample_n(size = 50), 
  .id = "replicate"
)

# Using both_samples, plot response vs. explanatory, colored by replicate
ggplot(both_samples, aes(x = explanatory, y = response, color = replicate)) + 
  # Add a point layer
  geom_point() + 
  # Add a smooth trend layer, using lin. reg., no ribbon
  geom_smooth(method = "lm", se = FALSE)

# From previous step
set.seed(4747)
many_samples <- popdata %>% rep_sample_n(size = 50, reps = 100)

# Using many_samples, plot response vs. explanatory, grouped by replicate
ggplot(many_samples, aes(x = explanatory, y = response, group = replicate)) + 
  # Add a point layer
  geom_point() + 
  # Add a smooth trend line, using lin. reg., no ribbon
  geom_smooth(method = "lm", se = FALSE) 

# From previous steps
set.seed(4747)
many_samples <- popdata %>% rep_sample_n(size = 50, reps = 100)
many_lms <- many_samples %>% 
  group_by(replicate) %>% 
  do(lm(response ~ explanatory, data=.) %>% tidy()) %>%
  filter(term == "explanatory")

# Using many_lms, plot estimate
ggplot(many_lms, aes(estimate)) +
  # Add a histogram layer
  geom_histogram()

set.seed(4747)

# Generate 100 random samples of size 50
many_samples <- popdata %>% rep_sample_n(size = 50, reps = 100)

# Using many_samples, plot response vs. explanatory, grouped by replicate
ggplot(many_samples, aes(y = response, x = explanatory, group = replicate)) + 
  # Add a point layer
  geom_point() + 
  # Add a smooth  trend layer, using lin. reg., no ribbon 
  geom_smooth(method = "lm", se = FALSE)

# Update the sampling to use new_popdata
many_samples <- new_popdata %>%
  rep_sample_n(size = 50, reps = 100)

# Rerun the plot; how does it change?
ggplot(many_samples, aes(x = explanatory, y = response, group = replicate)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Update the sampling to use even_newer_popdata
many_samples <- even_newer_popdata %>%
  rep_sample_n(size = 50, reps = 100)

# Update and rerun the plot; how does it change?
ggplot(many_samples, aes(x = explanatory, y = response, group = replicate)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  # Set the x-axis limit from -17 to 17
  xlim(c(-17,17))

library(infer)

# Calculate the observed slope
# Run a lin. reg. of Foster vs. Biological on the twins data
obs_slope <- lm(Foster ~ Biological, data = twins) %>%
  # Tidy the result
  tidy() %>%   
  # Filter for rows where term equal Biological
  filter(term == "Biological") %>%
  # Pull out the estimate column
  pull(estimate) 

# See the result
obs_slope

library(infer)
set.seed(4747) 

# Simulate 10 slopes with a permuted dataset
perm_slope <- twins %>%
  # Specify Foster vs. Biological
  specify(Foster ~ Biological) %>%
  # Use a null hypothesis of independence
  hypothesize(null = "independence") %>%
  # Generate 10 permutation replicates
  generate(reps = 10, "permute") %>%
  # Calculate the slope statistic
  calculate(stat = "slope")

# See the result
perm_slope

# From previous step
perm_slope <- twins %>%
  specify(Foster ~ Biological) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "slope")

perm_slope %>% 
  # Ungroup the dataset
  ungroup() %>% 
  # Calculate summary statistics
  summarize(
    # Mean of stat
    mean_stat = mean(stat), 
    # Std error of stat
    std_err_stat = sd(stat)
  )

# From previous step
abs_obs_slope <- lm(Foster ~ Biological, data = twins) %>%
  tidy() %>%   
  filter(term == "Biological") %>%
  pull(estimate) %>%
  abs()

# Compute the p-value  
perm_slope %>% 
  # Add a column of the absolute value of the slope
  mutate(abs_perm_slope = abs(stat)) %>%
  # Calculate a summary statistic
  summarize(
    # Get prop. cases where abs. permuted slope is greater than or equal to abs. observed slope
    p_value = mean(abs_perm_slope >= abs_obs_slope)
  )

# Set the seed for reproducibility
set.seed(4747)

# Calculate 1000 bootstrapped slopes
boot_slope <- twins %>%
  # Specify Foster vs. Biological
  specify(Foster ~ Biological) %>%
  # Generate 1000 bootstrap replicates
  generate(reps = 1000, type = "bootstrap") %>%
  # Calculate the slope statistic
  calculate(stat = "slope")

# See the result  
boot_slope

set.seed(4747)

# Calculate the slope statistic 
# from 1000 bootstrap replicates of
# the Foster vs. Biological model
# of the twins dataset
boot_slope <- twins %>%
  specify(Foster ~ Biological) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope")

# Create a confidence interval of stat
# 2 std devs each side of the mean
boot_slope %>% 
  summarize(
    lower = mean(stat) - 2 * sd(stat),
    upper = mean(stat) + 2 * sd(stat)
  )

# Set alpha
alpha <- 0.05

# Set the lower percentile cutoff
p_lower <- 0.025

# Set the upper percentile cutoff
p_upper <- 0.975

# Create a confidence interval of stat using quantiles
boot_slope %>% 
  summarize(
    lower = quantile(stat, p_lower),
    upper = quantile(stat, p_upper)
  )

# From previous step
biological_perm <- twins_perm %>%
  filter(term == "Biological_perm")
degrees_of_freedom <- nrow(twins) - 2

# Using biological_perm, plot statistic
ggplot(biological_perm, aes(statistic)) + 
  # Add a histogram layer, with density on the y axis
  geom_histogram(aes(y = ..density..)) + 
  # Add a t-distribution function stat, colored red
  stat_function(fun = dt, args = list(df = degrees_of_freedom), color = "red")

model <- lm(Foster ~ Biological, data = twins)

# Get the Biological model coefficient
biological_term <- model %>% 
  # Tidy the model
  tidy() %>%
  # Filter for term equal to "Biological"
  filter(term == "Biological") 

biological_term %>%
  # Add a column of one-sided p-values
  mutate(one_sided_p_value = p.value / 2)

# Calculate the degrees of freedom of twins
degrees_of_freedom <- nrow(twins) - 2

biological_term %>%
  mutate(
    # Calculate the test statistic
    test_statistic = (estimate - 1) / std.error,
    # Calculate its one-sided p-value
    one_sided_p_value_of_test_statistic = pt(test_statistic, df = degrees_of_freedom),
    # ... and its two-sided p-value
    two_sided_p_value_of_test_statistic = one_sided_p_value_of_test_statistic * 2
  )

# The slope in the observed data and each permutation replicate
obs_slope
perm_slope

# Calculate the absolute value of the observed slope
abs_obs_slope <- abs(obs_slope)

# Find the p-value
perm_slope %>%
  # Add a column for the absolute value of stat
  mutate(abs_slope = abs(stat)) %>%
  summarize(
    # Calculate prop'n permuted values at least as extreme as observed
    p_value = mean(abs_slope >= obs_slope)
  )

# From previous step
alpha <- 0.05
degrees_of_freedom <- nrow(twins) - 2
confidence_level <- 1 - alpha
p_upper <- 1 - alpha / 2
critical_value <- qt(p_upper, df = degrees_of_freedom)

tidied_model <- lm(Foster ~ Biological, data = twins) %>% 
  # Tidy the model, with a confidence interval
  tidy(conf.int = TRUE, conf.level = confidence_level) 

tidied_model %>%
  # Manually calculate the same interval
  mutate(
    lower = estimate - critical_value * std.error,
    upper = estimate + critical_value * std.error
  )

# From previous step
alpha <- 0.05
confidence_level <- 1 - alpha
p_lower <- alpha / 2
p_upper <- 1 - alpha / 2

# Tidy the model, including a confidence interval
tidied_model <- lm(Foster ~ Biological, data = twins) %>% 
   tidy(conf.int = TRUE, conf.level = confidence_level)

# Recall the t-confidence interval
tidied_model %>%
  filter(term == "Biological") %>%
  select(conf.low, conf.high)

# Create the bootstrap confidence interval
boot_slope %>% 
  summarize(
    lower = quantile(stat, p_lower), 
    upper = quantile(stat, p_upper)
  )

# From previous steps
model <- lm(Foster ~ Biological, data = twins)
new_twins <- data.frame(Biological = seq(70, 130, 15))
augmented_model <- model %>% augment(newdata = new_twins)

augmented_model %>%
  # Calculate a confidence interval on the predicted values
  mutate(
    lower_mean_prediction = .fitted - critical_value * .se.fit,
    upper_mean_prediction = .fitted + critical_value * .se.fit
  )

# This plot is shown
ggplot(twins, aes(x = Biological, y = Foster)) + 
  geom_point() +
  geom_smooth(method = "lm") 

ggplot() + 
  # Add a point layer of Foster vs. Biological, using twins
  geom_point(aes(y = Foster, x = Biological), data = twins) +
  # Add a line layer of .fitted vs Biological, using predictions, colored blue
  geom_line(aes(y = .fitted, x = Biological), data = predictions, color = "blue") +
  # Add a ribbon layer of lower_mean_prediction to upper_mean_prediction vs Biological, 
  # using predictions, transparency of 0.2
  geom_ribbon(aes(x = Biological, ymin = lower_mean_prediction, ymax = upper_mean_prediction), data = predictions, alpha = 0.2)

  # From previous step3
twins_sigma <- model %>% glance() %>% pull(sigma)
predictions3 <- predictions %>%
  mutate(
    std_err_of_predictions = sqrt(twins_sigma ^ 2 + .se.fit ^ 2),
    lower_response_prediction = .fitted - critical_value * std_err_of_predictions,
    upper_response_prediction = .fitted + critical_value * std_err_of_predictions
  )

# Update the plot
ggplot() + 
  geom_point(aes(x = Biological, y = Foster), data = twins) +
  geom_smooth(aes(x = Biological, y = Foster), data = twins, method = "lm") + 
  # Add a ribbon layer
  geom_ribbon(
    # ... of lower_response_prediction to upper_response_prediction vs. Biological
    aes(x = Biological, ymin = lower_response_prediction, ymax = upper_response_prediction), 
    # ... using the predictions3 dataset
    data = predictions3, 
    # ... with transparency set to 0.2
    alpha = 0.2, 
    # ... and fill color red
    fill = "red"
  )

  # Run a linear regression of response vs. explanatory
model <- lm(response ~ explanatory, data = hypdata_nice)

# Augment to get the observation-level information
modeled_observations <- augment(model)

# See the result
modeled_observations

# Using modeled_observations, draw a scatter plot of .resid vs. .fitted
ggplot(modeled_observations, aes(y = .resid, x = .fitted)) + geom_point()

# Run a linear regression of response vs. explanatory
model <- lm(response ~ explanatory, data = hypdata_poor)

# Augment to get the observation-level information
modeled_observations <- augment(model)

# See the result
modeled_observations

# Using modeled_observations, draw a scatter plot 
# of residuals vs. fitted values
modeled_observations %>% ggplot(aes(y = .resid, x = .fitted)) + geom_point()

# This plot is shown
p <- ggplot(hypdata_outlier, aes(x = explanatory, y = response)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

# Filter to remove the outlier
hypdata_no_outlier <- hypdata_outlier %>% filter(response < 100)

p +
  # Add another smooth lin .reg. layer, no ribbon, 
  # hypdata_no_outlier data, colored red
  geom_smooth(method = "lm", se = FALSE, data = hypdata_no_outlier, color = "red")

# Model response vs. explanatory on hypdata_outlier and tidy it
lm(response ~ explanatory, data = hypdata_outlier) %>% tidy() %>% pull(estimate, p.value)

# Do the same on hypdata_no_outlier
lm(response ~ explanatory, data = hypdata_no_outlier) %>% tidy() %>% pull(estimate, p.value)

# Calculate the p-value with the outlier
perm_slope_out %>% 
  mutate(abs_perm_slope = abs(stat)) %>%
  summarize(p_value = mean(abs_perm_slope >= obs_slope_out))

# Calculate the p-value without the outlier
perm_slope_noout %>% 
  mutate(abs_perm_slope = abs(stat)) %>%
  summarize(p_value = mean(abs_perm_slope >= obs_slope_noout))

# Run this to see how the model looks
ggplot(hypdata_nonlinear, aes(x = explanatory + explanatory ^ 2, y = response)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Model response vs. explanatory plus explanatory squared
model <- lm(response ~ explanatory + I(explanatory ^ 2), data = hypdata_nonlinear)

# Extract observation-level information
modeled_observations <- augment(model)

# See the result
modeled_observations

# Using modeled_observations, plot residuals vs. fitted values
ggplot(modeled_observations, aes(x = .fitted, y = .resid)) +
  # Add a point layer
  geom_point() + 
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0)

# Run this to see how the model looks
ggplot(hypdata_nonequalvar, aes(x = explanatory, y = log(response))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Model log-response vs. explanatory 
model <- lm(log(response) ~ explanatory, data = hypdata_nonequalvar)

# Extract observation-level information
modeled_observations <- augment(model)

# See the result
modeled_observations

# Using modeled_observations, plot residuals vs. fitted values
ggplot(modeled_observations, aes(x = .fitted, y = .resid)) +
  # Add a point layer
  geom_point() + 
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0)

# Run this to see how the model looks
ggplot(hypdata_nonnorm, aes(x = explanatory, y = sqrt(response))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Model response vs. explanatory 
model <- lm(sqrt(response) ~ explanatory, data = hypdata_nonnorm)

# Extract observation-level information
modeled_observations <- augment(model)

# See the result
modeled_observations

# Using modeled_observations, plot residuals vs. fitted values
ggplot(modeled_observations, aes(x = .fitted, y = .resid)) +
  # Add a point layer
  geom_point() + 
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0)

  # Create a tidy model
lm(price ~ bed, data = LAhomes) %>% tidy()

# Create a tidy model using the log of both variables
lm(log(price) ~ log(bed), data = LAhomes) %>% tidy()

# Output the tidy model
lm(log(price) ~ log(sqft), data = LAhomes) %>% tidy()

# Output the tidy model
lm(log(price) ~ log(bath), data = LAhomes) %>% tidy()

# Output the tidy model
lm(log(price) ~ log(sqft) + log(bath), data = LAhomes) %>% tidy()

# Output the first model
lm(Price ~ Service, data = restNYC) %>% tidy()

# Output the second model
lm(Price ~ Service + Food + Decor, data = restNYC) %>% tidy()