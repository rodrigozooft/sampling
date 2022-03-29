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