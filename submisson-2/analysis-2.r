# analysis-2

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr, modelsummary, fixest)

final.data <- read_tsv("data/output/acs_medicaid.txt")

final.data

# Problem 1 Plot the share of the adult population with direct purchase health insurance over time.
final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop) %>%
         filter(! State %in% c("Puerto Rico", "District of Columbia"))


problem1 <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>%
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

problem3 <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Medicaid",
    title="Adult Population with Medicaid"
  ) +
  geom_vline(xintercept=2013.5, color="red")


# Problem 4 
# Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.


ins.plot.dat <- final.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

problem4 <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time")

final.data$expand_ever
# Problem 5
# Calculate the average percent of uninsured individuals in 2012 and 2015, separately for expansion and non-expansion states. Present your results in a basic 2x2 DD table.
# Filter the data for the years 2012 and 2015
mcaid.data_filtered <- final.data %>%
  filter(year %in% c(2012, 2015))

# Calculate the difference in the percent uninsured between 2012 and 2015 for each state
mcaid.data_diff <- mcaid.data_filtered %>%
  group_by(State) %>%
  mutate(diff_uninsured = last(uninsured / adult_pop) - first(uninsured / adult_pop))

# Group the data by expand_ever (Medicaid expansion status) and calculate the average difference-in-differences estimator
avg_did <- mcaid.data_diff %>%
  group_by(expand_ever) %>%
  summarise(avg_diff_uninsured = mean(diff_uninsured, na.rm = TRUE))

# Print the result
print(avg_did)
# Problem 6
#Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, again focusing only on states that expanded in 2014 versus those that never expanded.
reg.dat <- final.data %>% filter(expand_year==2014 | is.na(expand_year)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd.ins.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)
modelsummary(list("DD (2014)"=dd.ins.reg),
             shape=term + statistic ~ model, 
             gof_map=NA,
             coef_omit='Intercept',
             vcov=~State
         )
#Problem 7
# Include state and year fixed effects in your estimates. Try using the lfe or fixest package to estimate this instead of directly including the fixed effects.

reg.dat <- final.data %>% filter(expand_year==2014 | is.na(expand_year)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
m.dd7 <- lm(perc_unins ~ post + expand_ever + treat, data=reg.dat)
m.twfe7 <- feols(perc_unins ~ treat | State + year, data=reg.dat)
problem7 <- msummary(list("DD"=m.dd7, "TWFE"=m.twfe7),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State
         )

# Problem 8
# Repeat the analysis in question 7 but include all states (even those that expanded after 2014). Are your results different? If so, why?

reg.dat8 <- final.data %>% 
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         treat=case_when(
          year>=expand_year & !is.na(expand_year) ~ 1,
          is.na(expand_year) ~ 0,
          year<expand_year & !is.na(expand_year) ~ 0))
m.dd8 <- lm(perc_unins ~ post + expand_ever + treat, data=reg.dat8)
m.twfe8 <- feols(perc_unins ~ treat | State + year, data=reg.dat8)
problem8 <- msummary(list("DD"=m.dd8, "TWFE"=m.twfe8),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State
         )

# Problem 9
# Provide an “event study” graph showing the effects of Medicaid expansion in each year. Use the specification that includes state and year fixed effects, limited to states that expanded in 2014 or never expanded.

reg.dat <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

mod.twfe9 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat)
problem9 <- iplot(mod.twfe9, 
      xlab = 'Time to treatment',
      main = 'Event study')

# Repeat part 9 but again include states that expanded after 2014. Note: this is tricky…you need to put all states onto “event time” to create this graph.
reg.dat10 <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

mod.twfe10 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat)

save.image("submission-2/Hwk5_workspace.Rdata")
