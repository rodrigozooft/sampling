# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(vendor_inco_term, fill = freight_cost_group)) +
    geom_bar(position = "fill") +
    ylab("proportion")

# Perform a chi-square test of independence on freight_cost_group and vendor_inco_term
test_results <- late_shipments %>% chisq_test(freight_cost_group ~ vendor_inco_term)

# See the results
test_results