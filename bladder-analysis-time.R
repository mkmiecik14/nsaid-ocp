# Analysis - Bladder Test Time
# Matt Kmiecik
# Started 25 May 2022

# Purpose: analyzes the times from the bladder test

# Prepares workspace
source("r-prep.R")

# Load
load("../output/bladder-data-wide.rda")
load("../output/bladder-data-long.rda")

########
#      #
# TIME #
#      #
########

bladder_time_data_wide <- 
  bladder_data_long %>% 
  filter(meas == "time") %>%
  mutate(
    stage = fct_relevel(stage, c("bl", "fs", "fu", "mt")),
    drug_2 = ifelse(drug %in% c("OCP_CONT", "OCP_CYCL"), "OCP_YES", drug),
    value = as.POSIXlt(value, format = "%H:%M") # creates dttm object
  ) %>%
  pivot_wider(
    id_cols = c(global_id, group, drug, notes, visit_month, study),
    names_from = c(stage, meas),
    values_from = value
  ) %>%
  # calculates time in minutes
  mutate(
    bl_timemin = difftime(bl_time, bl_time, units = "min"),
    fs_timemin = difftime(fs_time, bl_time, units = "min"),
    fu_timemin = difftime(fu_time, bl_time, units = "min"),
    mt_timemin = difftime(mt_time, bl_time, units = "min")
  )

bladder_time_data_long <-
  bladder_time_data_wide %>%
  select(-bl_time, -fs_time, -fu_time, -mt_time) %>%
  pivot_longer(
    cols = c(-global_id, -group, -drug, -notes, -visit_month, -study)
  ) %>%
  separate(name, into = c("stage", "meas")) %>%
  mutate(value = as.numeric(value)) %>%
  # ss global_id 160 is an outlier, as she has negative values for time
  # see notes: ss drank a lot of water in the morning and was at FS during
  # baseline ultrasound before water drinking; therefore, times are all
  # cattywampus
  mutate(value = ifelse(global_id == 160 & visit_month == 0, NA, value)) %>%
  mutate(
    stage = fct_relevel(stage, c("bl", "fs", "fu", "mt")),
    drug_2 = ifelse(drug %in% c("OCP_CONT", "OCP_CYCL"), "OCP_YES", drug)
  )

# Histogram
ggplot(
  bladder_time_data_long %>% filter(stage %nin% "bl"), 
  aes(value)
) +
  geom_histogram(binwidth = 5) +
  labs(x = "Time (minutes)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~stage)

# summarise
bladder_time_data_sum <-
  bladder_time_data_long %>%
  filter(complete.cases(value)) %>% # removes missing data
  group_by(drug, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Bladder time plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_time_data_sum %>% filter(stage %nin% "bl"), 
  aes(factor(visit_month), m, group = drug, color = drug)
) +
  geom_point(
    data = 
      bladder_time_data_long %>% 
      filter(complete.cases(visit_month), stage %nin% "bl"), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "Time (minutes)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

# Combining OCP groups together
# summarise
bladder_time_data_sum2 <-
  bladder_time_data_long %>%
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
  bladder_time_data_sum2 %>% filter(stage %nin% "bl"), 
  aes(factor(visit_month), m, group = drug_2, color = drug_2)
) +
  geom_point(
    data = bladder_time_data_long %>% 
      filter(complete.cases(visit_month), stage %nin% "bl"), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "Time (minutes)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")