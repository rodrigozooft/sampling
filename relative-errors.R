# Calculate the relative error percentage again with a sample of 100 rows
attrition_srs100 <- attrition_pop %>% slice_sample(n = 100)

mean_attrition_srs100 <- attrition_srs100 %>% summarize(mean_attrition = mean(Attrition == "Yes"))

rel_error_pct100 <- (abs(mean_attrition_pop - mean_attrition_srs100) / mean_attrition_pop) * 100

# See the result
rel_error_pct100