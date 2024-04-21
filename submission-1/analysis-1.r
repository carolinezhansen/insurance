analysis-1

ins.dat <- read_tsv("../data/acs_medicaid.txt")
ins.dat

# Problem 1 Plot the share of the adult population with direct purchase health insurance over time.
final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop)

final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Direct Purchase",
    title="Adult Population with Direct Purchase Health Insurance"
  ) +
  geom_vline(xintercept=2013.5, color="red")


# Problem 2 
# Discuss the reduction in direct purchase health insurance in later years. Can you list a couple of policies that might have affected the success of the direct purchase insurance market?

# Problem 3
# Plot the share of the adult population with Medicaid over time.

final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Medicaid",
    title="Adult Population with Medicaid"
  ) +
  geom_vline(xintercept=2013.5, color="red")


# Problem 4 
# Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.

final_data <- final.data[final.data$expand_year <= 2014, ]

ggplot(final_data, aes(x = year, y = perc_unins, color = expand_year == 2014)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red"), labels = c("Did not expand in 2014", "Expanded in 2014")) +
  labs(x = "Year", y = "Uninsured Rate", title = "Uninsured Rate Over Time by Medicaid Expansion Status") +
  theme_minimal()

ins.dat %>% group_by(year) %>% summarize(mean=mean(perc_unins)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")