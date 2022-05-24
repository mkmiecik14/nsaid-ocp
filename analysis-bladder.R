# Bladder Analysis
# Matt Kmiecik
# Started 12 April 2022

# Purpose: visualizes and models bladder data from the OCP/NSAID datasets

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/ss-masterlist.rda")       # masterlist
load("../output/ocp-bladder-data.rda")    # ocp bladder data
load("../output/nsaid-bladder-data.rda")  # nsaid bladder data

# Prepares data
# NSAID ss only masterlist
nsaid_masterlist <- 
  ss_masterlist %>% 
  select(global_id, nsaid_id, nsaid_r, group, drug, notes) %>%
  filter(drug == "NSAID")

# OCP ss only masterlist
ocp_masterlist <-
  ss_masterlist %>%
  select(global_id:shortannualsr, group:notes) %>%
  filter(drug %in% c("OCP_NO", "OCP_CONT", "OCP_CYCL"))

# combines nsaid bladder data with ss info
nsaid_bladder_ss <- 
  left_join(
    nsaid_masterlist,
    nsaid_bladder_data,
    by = c("nsaid_r" = "record")
  )

# combines ocp bladder data with ss info
ocp_bladder_ss <- 
  left_join(
    ocp_masterlist,
    ocp_bladder_data,
    by = c("arm1r" = "record")
  )

# Combines all bladder data together
bladder_data <- 
  bind_rows(
    nsaid_bladder_ss %>% select(-nsaid_id, -nsaid_r),
    ocp_bladder_ss %>% select(-ss, -arm1r, -shortannualsr)
  )

#########################
#                       #
# Bladder data analyses #
#                       #
#########################

# wide format for pain variables
bladder_data_wide <-
  bladder_data %>% 
  select(
    global_id:study, 
    bl_pain = bt3_baselinepain,
    fs_pain = bt10a_fspain,
    fu_pain = bt10b_fupain,
    mt_pain = bt10c_mtpain,
    fs_urg = bt10a_fsurg,
    fu_urg =  bt10b_fuurg,
    mt_urg = bt10c_mturg,
    bl_time = bt5_timefinisheddrinking,
    fs_time = bt10a_fstime,
    fu_time = bt10b_futime,
    mt_time = bt10c_mttime,
    bl_vol = bt4_baselinevol, 
    fs_vol = bt10a_fsvol,
    fu_vol = bt10b_fuvol,
    mt_vol = bt12a_mtvol,
    peehat_vol = bt12b_peehatml,
    scale_vol = bt12c_scalevol
    )

# missing data
# bladder_data_wide %>% count(global_id, drug, visit_month)
# missing_data <- bladder_pain_data %>% filter(is.na(value))
missing <- 
  bladder_data_wide %>% 
  select(-notes) %>% 
  filter(!complete.cases(.)) %>%
  filter(complete.cases(visit_month))
# figure out why some participants have random missing data points ^^^
missing_bladder_data <- 
  missing %>% 
  left_join(
    ., 
    ss_masterlist %>% select(global_id:nsaid_r, notes), 
    by = "global_id"
  ) %>%
  select(global_id, ss:notes, group:scale_vol)
# saves out (uncomment to save)
# write_csv(missing_bladder_data, "../output/missing-bladder-data.csv")

# long format for all bladder data 
bladder_data_long <-
  bladder_data_wide %>%
  pivot_longer(c(-global_id, -group, -drug, -notes, -visit_month, -study)) %>%
  separate(name, into = c("stage", "meas"))

########
# PAIN #
########

bladder_pain_data <- 
  bladder_data_long %>% 
  filter(meas == "pain") %>%
  mutate(
    stage = fct_relevel(stage, c("bl", "fs", "fu", "mt")),
    value = as.numeric(value),
    drug_2 = ifelse(drug %in% c("OCP_CONT", "OCP_CYCL"), "OCP_YES", drug)
    )

# Histogram
ggplot(bladder_pain_data, aes(value)) +
  geom_histogram(binwidth = 2) +
  labs(x = "VAS Pain Rating (0-100)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~stage)

# Density
ggplot(bladder_pain_data, aes(value, group = stage, color = stage)) +
  geom_density(aes(fill = stage), alpha = 1/3) +
  labs(x = "VAS Pain Rating (0-100)", y = "Density") +
  scale_color_ghibli_d("MononokeMedium", direction = -1) +
  scale_fill_ghibli_d("MononokeMedium", direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom")

# summarise
bladder_pain_data_sum <-
  bladder_pain_data %>%
  filter(complete.cases(value)) %>% # removes missing data
  group_by(drug, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# writes out these summary stats
# uncomment out to save
# write_csv(bladder_pain_data_sum, file = "../output/bladder-pain-data-sum.csv")

# Bladder pain plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_pain_data_sum, 
  aes(factor(visit_month), m, group = drug, color = drug)
  ) +
  geom_point(
    data = bladder_pain_data %>% filter(complete.cases(visit_month)), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
    ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

# regression - ALL
pj <- position_jitter(width = 1, height = 0)
ggplot(
  bladder_pain_data %>% filter(complete.cases(visit_month)), 
  aes(visit_month, value, group = drug, color = drug, fill = drug)
) +
  geom_point(
    data =, 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linetype = 2) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 20), minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

# regression - OCP vs. NO OCP
pj <- position_jitter(width = 1, height = 0)
ggplot(
  bladder_pain_data %>% filter(complete.cases(visit_month), drug %nin% "NSAID"), 
  aes(visit_month, value, group = drug_2, color = drug_2, fill = drug_2)
) +
  geom_point(
    data =, 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 20), minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

# regression - CONT vs. CYLC
pj <- position_jitter(width = 1, height = 0)
ggplot(
  bladder_pain_data %>% 
    filter(complete.cases(visit_month), drug %in% c("OCP_CYCL", "OCP_CONT")), 
  aes(visit_month, value, group = drug, color = drug, fill = drug)
) +
  geom_point(
    data =, 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  scale_x_continuous(breaks = seq(0, 12, 2), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 20), minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")


# Combining OCP groups together
# summarise
bladder_pain_data_sum2 <-
  bladder_pain_data %>%
  filter(complete.cases(value)) %>% # removes missing data
  group_by(drug_2, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# writes out these summary stats
# uncomment out to save
#write_csv(bladder_pain_data_sum2, file = "../output/bladder-pain-data-sum2.csv")

# Bladder pain plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_pain_data_sum2, 
  aes(factor(visit_month), m, group = drug_2, color = drug_2)
) +
  geom_point(
    data = bladder_pain_data %>% filter(complete.cases(visit_month)), 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")

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

########
# TIME #
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

############
#          #
# Modeling #
#          #
############

# FOR CLINICAL TRIALS . GOV (i.e., removes the NSAID group)
# THIS IS AN INTENT TO TREAT (ITT) ANALYSIS THAT DOES NOT ACCOUNT FOR ADHERENCE!


# Pain
pain_data <- 
  bladder_pain_data %>% 
  filter(drug %nin% "NSAID") %>%
  mutate(drug = as.factor(drug))

# establishes contrasts
contrasts(pain_data$drug) <- 
  cbind(
    .OCPvsNO = c(-1/3, -1/3, 2/3), # OCP < NO OCPs
    .CONTvsCYCL = c(-1/2, 1/2, 0) # CONT < CYCL
    )

# Linear mixed effects models 
pain_mods <-
  pain_data %>%
  nest_by(stage) %>%
  mutate(
    mod = list( 
      lmer(
        value ~ 1 + visit_month*drug + (1 | global_id),
        data = data,
        REML = TRUE
        )
    )
  )

# Extracting estimates
pain_ests <- 
  pain_mods %>%
  summarise(tidy(mod)) %>%
  ungroup() %>%
  mutate(
    sig = p.value < .05,
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "visit_month" ~ "month",
      term == "drug.OCPvsNO" ~ "OCP vs. No",
      term == "drug.CONTvsCYCL" ~ "CONT vs. CYCL",
      term == "visit_month:drug.OCPvsNO" ~ "month*OCP vs. No",
      term == "visit_month:drug.CONTvsCYCL" ~ "month*CONT vs. CYCL"
      )
    )

# writes results out
# uncomment to save out
# write_csv(pain_ests, file = "../output/pain-ests.csv")
