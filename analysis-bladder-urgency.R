# Analysis - Bladder Test Urgency
# Matt Kmiecik
# Started 25 May 2022

# Purpose: analyzes the urgency ratings from the bladder test

# Prepares workspace
source("r-prep.R")

# Load
load("../output/bladder-data-wide.rda")
load("../output/bladder-data-long.rda")

###########
# URGENCY #
###########

bladder_urg_data <- 
  bladder_data_long %>% 
  filter(meas == "urg") %>%
  mutate(
    stage = fct_relevel(stage, c("fs", "fu", "mt")),
    value = as.numeric(value),
    drug_2 = ifelse(drug %in% c("OCP_CONT", "OCP_CYCL"), "OCP_YES", drug)
  )

# Histogram
ggplot(bladder_urg_data, aes(value)) +
  geom_histogram(binwidth = 5) +
  labs(x = "VAS Urgency Rating (0-100)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~stage)

# summarise
bladder_urg_data_sum <-
  bladder_urg_data %>%
  filter(complete.cases(value)) %>% # removes missing data
  group_by(drug, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Bladder urgency plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_urg_data_sum, 
  aes(factor(visit_month), m, group = drug, color = drug)
) +
  geom_point(
    data = bladder_urg_data %>% filter(complete.cases(visit_month)), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "VAS Urgency Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

# Combining OCP groups together
# summarise
bladder_urg_data_sum2 <-
  bladder_urg_data %>%
  filter(complete.cases(value)) %>% # removes missing data
  group_by(drug_2, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Bladder urgency plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_urg_data_sum2, 
  aes(factor(visit_month), m, group = drug_2, color = drug_2)
) +
  geom_point(
    data = bladder_urg_data %>% filter(complete.cases(visit_month)), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "VAS URgency Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")