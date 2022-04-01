# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(vendor_inco_term, fill = freight_cost_group)) +
    geom_bar(position = "fill") +
    ylab("proportion")

# Perform a chi-square test of independence on freight_cost_group and vendor_inco_term
test_results <- late_shipments %>% chisq_test(freight_cost_group ~ vendor_inco_term)

# See the results
test_results

# Using late_shipments, count the vendor incoterms
vendor_inco_term_counts <- late_shipments %>% count(vendor_inco_term)


# Get the number of rows in the whole sample
n_total <- nrow(vendor_inco_term_counts)

hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  # Add a column of hypothesized counts for the incoterms
   mutate(n = vendor_inco_term_counts)

# See the results
hypothesized

# From previous step
vendor_inco_term_counts <- late_shipments %>% 
  count(vendor_inco_term)
n_total <- nrow(late_shipments)
hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  mutate(n = prop * n_total)

# Using vendor_inco_term_counts, plot n vs. vendor_inco_term 
ggplot(vendor_inco_term_counts, aes(vendor_inco_term, n)) +
  # Make it a (precalculated) bar plot
  geom_col() +
  # Add points from hypothesized 
  geom_point(data = hypothesized)

hypothesized_props <- c(
  EXW = 0.75, CIP = 0.05, DDP = 0.1, FCA = 0.1
)

# Run chi-square goodness of fit test on vendor_inco_term
test_results <- late_shipments %>% chisq_test(
  response = vendor_inco_term,
  p = hypothesized_props
)

# See the results
test_results

# Extend the pipeline to declare a null hypothesis that the variables are independent
hypothesized <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence")

# See the result
hypothesized

# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Visualize the null distribution
visualize(null_distn)

# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Get the p-value
p_value <- get_p_value(
  null_distn, obs_stat,
  direction = "two sided"
)

# See the result
p_value

# From previous steps
null_distn <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
obs_stat <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))

# Get the p-value
p_value <- get_p_value(
  null_distn, obs_stat,
  direction = "less"
)

# See the result
p_value

# Run a Wilcoxon-Mann-Whitney test on weight_kilograms vs. late
test_results <- wilcox.test(
    weight_kilograms ~ late,
    data = late_shipments
)

# See the result
test_results

# Run a Kruskal-Wallace test on weight_kilograms vs. shipment_mode
test_results <- kruskal.test(
   weight_kilograms ~  shipment_mode,
   data = late_shipments
)

# See the result
test_results

# Load packages
library(ggplot2)
library(NHANES)

# What are the variables in the NHANES dataset?
colnames(NHANES)

# Create bar plot for Home Ownership by Gender
ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  # Set the position to fill
  geom_bar(position = "fill") +
  ylab("Relative frequencies")

# Density plot of SleepHrsNight colored by SleepTrouble
ggplot(NHANES, aes(x = SleepHrsNight, color = SleepTrouble)) + 
  # Adjust by 2
  geom_density(adjust = 2) + 
  # Facet by HealthGen
  facet_wrap(~ HealthGen)

  # From previous step
homes <- NHANES %>%
  select(Gender, HomeOwn) %>%
  filter(HomeOwn %in% c("Own", "Rent"))

diff_orig <- homes %>%   
  # Group by gender
  group_by(Gender) %>%
  # Summarize proportion of homeowners
  summarize(prop_own = mean(HomeOwn == "Own")) %>%
  # Summarize difference in proportion of homeowners
  summarize(obs_diff_prop = diff(prop_own)) # male - female
  
# See the result
diff_orig

# Perform 10 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 10, type = "permute") 


# Print results to console
homeown_perm

# Perform 100 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 100, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))
  
# Dotplot of 100 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_dotplot(binwidth = 0.001)

# Perform 1000 permutations
homeown_perm <- homes %>%
  # Specify HomeOwn vs. Gender, with `"Own" as success
  specify(HomeOwn ~ Gender, success = "Own") %>%
  # Use a null hypothesis of independence
  hypothesize(null = "independence") %>% 
  # Generate 1000 repetitions (by permutation)
  generate(reps = 1000, type = "permute") %>% 
  # Calculate the difference in proportions (male then female)
  calculate(stat = "diff in props", order = c("male", "female"))

# Density plot of 1000 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_density()

# Plot permuted differences, diff_perm
ggplot(homeown_perm, aes(x = diff_perm)) + 
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")

# Compare permuted differences to observed difference
homeown_perm %>%
  summarize(n_perm_le_obs = sum(diff_perm <= diff_orig))

# Create a contingency table summarizing the data
disc %>%
  # Count the rows by sex, promote
  count(sex, promote)

# Find proportion of each sex who were promoted
disc %>%
  # Group by sex
  group_by(sex) %>%
  # Calculate proportion promoted summary stat
  summarize(promoted_prop = mean(promote == "promoted"))

# Replicate the entire data frame, permuting the promote variable
disc_perm <- disc %>%
  specify(promote ~ sex, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5, type = "permute")

disc_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count(promote, sex)

disc_perm %>%
  # Calculate difference in proportion, male then female
  calculate(stat = "diff in props", order = c("male", "female"))

# From previous steps
diff_orig <- disc %>%
  group_by(sex) %>%
  summarize(prop_prom = mean(promote == "promoted")) %>%
  summarize(stat = diff(prop_prom)) %>% 
  pull()
disc_perm <- disc %>%
  specify(promote ~ sex, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("male", "female"))
  
# Using permutation data, plot stat
ggplot(disc_perm, aes(x = stat)) + 
  # Add a histogram layer
  geom_histogram(binwidth = 0.01) +
  # Add a vertical line at diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")

disc_perm %>% 
  summarize(
    # Find the 0.9 quantile of diff_perm's stat
    q.90 = quantile(stat, p = 0.9),
    # ... and the 0.95 quantile
    q.95 = quantile(stat, p = 0.95),
    # ... and the 0.99 quantile
    q.99 = quantile(stat, p = 0.99)
  )

# Use disc_perm
disc_perm %>% 
  # ... to calculate summary stats
  summarize(
    # Find the 0.01 quantile of stat
    q.01 = quantile(stat, p = 0.01),
    # ... and 0.05
    q.05 = quantile(stat, p = 0.05),
    # ... and 0.1 
    q.10 = quantile(stat, p = 0.1)
  )

# Tabulate the small dataset
disc_small %>% 
  # Select sex and promote
  count(sex, promote)
  
# Do the same for disc_big
disc_big %>%
  count(sex, promote)

# Using disc_perm_small, plot stat
ggplot(disc_perm_small, aes(x = stat)) + 
  # Add a histogram layer with binwidth 0.01
  geom_histogram(binwidth = 0.01) +
  # Add a vline layer, crossing x-axis at diff_orig_small
  geom_vline(aes(xintercept = diff_orig_small), color = "red")

# Swap the dataset to disc_perm_big
ggplot(disc_perm_big, aes(x = stat)) + 
  geom_histogram(binwidth = 0.01) +
  # Change the x-axis intercept to diff_orig_big
  geom_vline(aes(xintercept = diff_orig_big), color = "red")

calc_upper_quantiles <- function(dataset) {
  dataset %>% 
    summarize(
      q.90 = quantile(stat, p = 0.90),
      q.95 = quantile(stat, p = 0.95),
      q.99 = quantile(stat, p = 0.99)
    )
}

# Recall the quantiles associated with the original dataset
calc_upper_quantiles(disc_perm)

# Calculate the quantiles associated with the small dataset
calc_upper_quantiles(disc_perm_small)

# Calculate the quantiles associated with the big dataset
calc_upper_quantiles(disc_perm_big)

# Visualize and calculate the p-value for the original dataset
disc_perm %>%
  visualize(obs_stat = diff_orig, direction = "greater")

disc_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "greater")

# Visualize and calculate the p-value for the small dataset
disc_perm_small %>%
  visualize(obs_stat = diff_orig_small, direction = "greater")

disc_perm_small %>%
  get_p_value(obs_stat = diff_orig_small, direction = "greater")

# Visualize and calculate the p-value for the big dataset
disc_perm_big %>%
  visualize(obs_stat = diff_orig_big, direction = "greater")

disc_perm_big %>%
  get_p_value(obs_stat = diff_orig_big, direction = "greater")

# Recall the original data
disc %>% 
  count(sex, promote)

# Tabulate the new data
disc_new %>%
  count(sex, promote)

# Recall the distribution of the original permuted differences
ggplot(disc_perm, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), color = "red")

# Plot the distribution of the new permuted differences
ggplot(disc_perm_new, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig_new), color = "red")

# Recall the p-value from the original data
disc_perm %>%
  summarize(p_value = mean(diff_orig <= stat))

# Find the p-value from the new data

disc_perm_new %>%
  summarize(p_value = mean(diff_orig_new <= stat))  

# Calculate the two-sided p-value
disc_perm %>%
  summarize(p_value = 2 * mean(diff_orig <= stat))

# Tabulate the data
opportunity %>%
  count(decision, group)

# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))

# Plot group, filled by decision
ggplot(opportunity, aes(x = group, fill = decision)) + 
  # Add a bar layer, with position "fill"
  geom_bar(position = "fill")

# Calculate the observed difference in purchase rate
diff_obs <- opportunity %>%
  # Group by group
  group_by(group) %>%
  # Calculate proportion deciding to buy a DVD
  summarize(prop_buy = mean(decision == "buyDVD")) %>%
  # Calculate difference between groups
  summarize(stat = diff(prop_buy)) %>% 
  pull()

# Create data frame of permuted differences in purchase rates
opp_perm <- opportunity %>%
  # Specify decision vs. group, where success is buying a DVD
  specify(decision ~ group, success = "buyDVD") %>%
  # Set the null hypothesis to independence
  hypothesize(null = "independence") %>%
  # Generate 1000 reps of type permute
  generate(reps = 1000, type = "permute") %>%
  # Calculate the summary stat difference in proportions
  calculate(stat = "diff in props", order = c("treatment", "control"))
    
# Review the result
opp_perm

# From previous steps
diff_obs <- opportunity %>%
  group_by(group) %>%
  summarize(prop_buy = mean(decision == "buyDVD")) %>%
  summarize(stat = diff(prop_buy)) %>% 
  pull()
opp_perm <- opportunity %>%
  specify(decision ~ group, success = "buyDVD") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("treatment", "control"))
  
# Using the permuation data, plot stat
ggplot(opp_perm, aes(x = stat)) + 
  # Add a histogram layer with binwidth 0.005
  geom_histogram(binwidth = 0.005) +
  # Add a vline layer with intercept diff_obs
  geom_vline(aes(xintercept = diff_obs), color = "red")

# Visualize the statistic 
opp_perm %>%
  visualize(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `get_p_value`
opp_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `summarize`
opp_perm %>%
  summarize(p_value = mean(stat <= diff_orig))

# From previous steps
ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(stat = mean(vote == "yes"))
ex2_props <- all_polls %>%
  filter(poll == 1) %>%
  select(vote) %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
  
# Calculate variability of p-hat
ex1_props %>% 
  summarize(variability = sd(stat))
  
# Calculate variability of p-hat*
ex2_props %>% 
  summarize(variability = sd(stat))

# Combine data from both experiments
both_ex_props <- bind_rows(ex1_props, ex2_props, .id = "experiment")

# Using both_ex_props, plot stat colored by experiment
ggplot(both_ex_props, aes(stat, color = experiment)) + 
  # Add a density layer with bandwidth 0.1
  geom_density(bw = 0.1)

# Proportion of yes votes by poll
props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == "yes"))

# The true population proportion of yes votes
true_prop_yes <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_yes - true_prop_yes) < 2 * sd(prop_yes)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))

# From previous exercises
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot <- one_poll %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
  
p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(stat = mean(vote == "yes")) %>%
  pull()

# Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2 * sd(stat)
  )

# From previous step
percentile_ci <- one_poll_boot %>% 
  get_confidence_interval(level = 0.95)
  
one_poll_boot %>% 
  # Visualize in-between the endpoints given by percentile_ci
  visualize(endpoints = percentile_ci, direction = "between")

calc_t_conf_int <- function(resampled_dataset) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find the bootstrap t-confidence interval for 30 resamples
calc_t_conf_int(one_poll_boot)

# ... and for 300 resamples
calc_t_conf_int(one_poll_boot_300)

# ... and for 3 resamples
calc_t_conf_int(one_poll_boot_3)

calc_p_hat <- function(dataset) {
  dataset %>%
    summarize(stat = mean(vote == "yes")) %>%
    pull()
}
calc_t_conf_int <- function(resampled_dataset, p_hat) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find proportion of yes votes from original population
p_hat <- calc_p_hat(one_poll)

# Review the value
p_hat  

# Calculate bootstrap t-confidence interval (original 0.6 param)
calc_t_conf_int(one_poll_boot, p_hat)

# Find proportion of yes votes from new population
p_hat_0.8 <- calc_p_hat(one_poll_0.8)
  
# Review the value
p_hat_0.8  
  
# Calculate the bootstrap t-confidence interval (new 0.8 param)
calc_t_conf_int(one_poll_boot_0.8, p_hat_0.8)

# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.95) 

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.99) 

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.90)  

# Plot ci_endpoints vs. ci_percent to compare the intervals
ggplot(conf_int_data, aes(x = ci_percent, y = ci_endpoints)) +
  # Add a line layer
  geom_line()

# From previous steps
library(dplyr)
gss2016 <- gss %>%
  filter(year == 2016)
library(ggplot2)
ggplot(gss2016, aes(x = consci)) +
  geom_bar()

# Compute proportion of high conf
p_hat <- gss2016 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()

# From previous step
library(infer)
boot1 <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 1, type = "bootstrap")

# Compute proportion with high conf
boot1 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()

# From previous steps
boot_dist <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
ggplot(boot_dist, aes(x = stat)) +
  geom_density()
SE <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

# Create CI
c(p_hat - 2 * SE, p_hat + 2 * SE)

# From previous steps
boot_dist_small <- gss2016_small %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
SE_small_n <- boot_dist_small %>%
  summarize(se = sd(stat)) %>%
  pull()
boot_dist_smaller <- gss2016_smaller %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Compute and save estimate of second SE
SE_smaller_n <- boot_dist_smaller %>%
  summarize(SE_smaller_n = sd(stat)) %>%
  pull()

# Compare the results for each dataset size
message("gss2016_small has ", nrow(gss2016_small), " rows and standard error ", SE_small_n)
message("gss2016_smaller has ", nrow(gss2016_smaller), " rows and standard error ", SE_smaller_n)

# From previous steps
ggplot(gss2016, aes(x = meta_region)) +
  geom_bar()
boot_dist <- gss2016 %>%
  specify(response = meta_region, success = "pacific") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Calculate std error
SE_low_p <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

# Compare SEs
c(SE_low_p, SE)

# From previous step
n <- nrow(gss2016)
p_hat <- gss2016 %>%
  summarize(prop_pacific = mean(meta_region == "pacific")) %>%
  pull()

# Check conditions
n * p_hat >= 10
n * (1 - p_hat) >= 10

# Calculate SE
SE_approx <- sqrt(p_hat * (1 - p_hat) / n)

# Form 95% CI
c(p_hat - 2 * SE_approx, p_hat + 2 * SE_approx)

# From previous step
ggplot(gss2016, aes(x = postlife)) +
  geom_bar()

# Calculate and save proportion that believe
p_hat <- gss2016 %>%
  summarize(prop_yes = mean(postlife == "YES")) %>%
  pull()

# See the result
p_hat

# From previous steps
sim1 <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 1, type = "simulate")

# Compute proportion that believe
sim1 %>%
  summarize(prop_yes = mean(postlife == "YES")) %>%
  pull()

# From previous step
null <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 500, type = "simulate") %>%
  calculate(stat = "prop")
  
null %>%
  summarize(
    # Compute the one-tailed p-value
    one_tailed_pval = mean(stat >= p_hat),
    # Compute the two-tailed p-value
    two_tailed_pval = one_tailed_pval * 2
  ) %>%
  pull(two_tailed_pval)

# From previous step
p_hats <- gss2016 %>%
  group_by(sex) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()
  
# Compute difference in proportions
d_hat <- diff(p_hats)

# See the result
d_hat

# From previous step
null <- gss2016 %>%
  specify(cappun ~ sex, success = "FAVOR") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "diff in props", order = c("FEMALE", "MALE"))
  
# Compute two-tailed p-value
null %>%
  summarize(
    one_tailed_pval = mean(stat <= d_hat),
    two_tailed_pval = 2 * one_tailed_pval
  ) %>%
  pull(two_tailed_pval)

# From previous step
boot <- gss2016 %>%
  specify(cappun ~ sex, success = "FAVOR") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("FEMALE", "MALE"))
    
# Compute the standard error
SE <- boot %>%
  summarize(se = sd(stat)) %>%
  pull()
  
# Form the CI (lower, upper)
c(d_hat - 2 * SE, d_hat + 2 * SE)

# From previous steps
p_hats <- gssmod %>%
  group_by(coinflip) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()
d_hat <- diff(p_hats)
null <- gssmod %>%
  specify(cappun ~ coinflip, success = "FAVOR") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "diff in props", order = c("heads", "tails"))
  
# Visualize null
ggplot(null, aes(x = stat)) +
  # Add density layer
  geom_density() +
  # Add vertical red line at observed stat
  geom_vline(xintercept = d_hat, color = "red")

# From previous step
alpha <- 0.05
upper <- null %>%
  summarize(u = quantile(stat, probs = 1 - alpha / 2)) %>%
  pull()
lower <- null %>%
  summarize(l = quantile(stat, probs = alpha / 2)) %>%
  pull()
  
# Visualize cutoffs
ggplot(null, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = d_hat, color = "red") +
  # Add vertical blue line for lower cutoff
  geom_vline(xintercept = lower, color = "blue") +
  # Add vertical blue line for upper cutoff
  geom_vline(xintercept = upper, color = "blue")

# From previous step
gss_party <- gss2016 %>%
  filter(party != "Oth")
  
# Visualize distribution take 2 
ggplot(gss_party, aes(party, fill = natspac)) +
  geom_bar() 
  # Add bar layer of counts

# From previous step
Obs <- gss_party %>%
  select(natspac, party) %>%
  table()
  
# Convert table back to tidy df
Obs %>%
  # Tidy the table
  tidy() %>%
  # Expand out the counts
  uncount(n)

# From previous step
perm_1 <- gss_party %>%
  specify(natarms ~ party) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1, type = "permute")
  
# Visualize permuted data
ggplot(perm_1, aes(x = party, fill = natarms)) +
  # Add bar layer
  geom_bar()

# Compute chi-squared stat
gss_party %>%
  chisq_stat(natarms ~ party)

# Create null
null_spac <- gss_party %>%
  specify(natspac ~ party) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "Chisq")

# Visualize null
ggplot(null_spac, aes(x = stat)) +
  # Add density layer
  geom_density() +
  # Add vertical line at obs stat
  geom_vline(xintercept = chi_obs_spac, color = "red")

# Create null that natarms and party are indep
null_arms <- gss_party %>%
  specify(natarms ~ party) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "Chisq")
  
# Visualize null
ggplot(null_arms, aes(x = stat)) +
  # Add density layer
  geom_density() +
  # Add vertical red line at obs stat
  geom_vline(xintercept = chi_obs_arms, color = "red")

