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

# From previous step
sample_size <- 200
pop_size <- nrow(attrition_pop)
interval <- pop_size %/% sample_size

# Get row indexes for the sample
row_indexes <- seq_len(sample_size) * interval

attrition_sys_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using systematic sampling
  slice(row_indexes)

# See the result
View(attrition_sys_samp)

# Add a row ID column to attrition_pop
attrition_pop_id <- attrition_pop %>% 
  rowid_to_column()

# Using attrition_pop_id, plot YearsAtCompany vs. rowid
ggplot(attrition_pop_id, aes(y = YearsAtCompany, x = rowid)) +
  # Make it a scatter plot
  geom_point() +
  # Add a smooth trend line
  geom_smooth()

# Shuffle the rows of attrition_pop then add row IDs
attrition_shuffled <- attrition_pop %>% slice_sample(prop = 1) %>% rowid_to_column()

# Using attrition_shuffled, plot YearsAtCompany vs. rowid
# Add points and a smooth trend line
ggplot(attrition_shuffled, aes(y = YearsAtCompany, x = rowid)) +
    geom_point() + 
    geom_smooth()

# From previous steps
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))
attrition_strat <- attrition_pop %>% 
  group_by(Education) %>% 
  slice_sample(prop = 0.4) %>% 
  ungroup()

# Get the counts and percents from attrition_strat
education_counts_strat <- attrition_strat %>%
  count(Education, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

# See the results
education_counts_strat

# From previous step
attrition_eq <- attrition_pop %>%
  group_by(Education) %>% 
  slice_sample(n = 30) %>%
  ungroup()

# Get the counts and percents from attrition_eq
education_counts_eq <- attrition_eq %>%
  count(Education, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

# See the results
education_counts_eq

# From previous step
attrition_weight <- attrition_pop %>% 
  slice_sample(n = 400, weight_by = YearsAtCompany)

# Using attrition_weight, plot YearsAtCompany as a histogram with binwidth 1
ggplot(attrition_weight, aes(x = YearsAtCompany)) +
  geom_histogram(binwidth = 1)

# From previous step
attrition_weight <- attrition_pop %>%
  slice_sample(n = 400, weight_by = YearsAtCompany)

# Calculate mean YearsAtCompany using attrition_pop

attrition_pop %>% summarize(mean = mean(YearsAtCompany))

# Calculate mean YearsAtCompany using attrition_weight

attrition_weight %>% summarize(mean = mean(YearsAtCompany))

# Get unique values of RelationshipSatisfaction
satisfaction_unique <-  unique(attrition_pop$RelationshipSatisfaction)

# Randomly sample for 2 of the unique satisfaction values
satisfaction_samp <- sample(satisfaction_unique, size = 2)

# Perform cluster sampling on the selected group getting 0.25 of the population
attrition_clust <- attrition_pop %>% 
    filter(RelationshipSatisfaction %in% satisfaction_samp) %>%
    group_by(RelationshipSatisfaction) %>%
    slice_sample(n = nrow(attrition_pop) / 4) %>%
    ungroup()