# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(vendor_inco_term, fill = freight_cost_group)) +
    geom_bar(position = "fill") +
    ylab("proportion")