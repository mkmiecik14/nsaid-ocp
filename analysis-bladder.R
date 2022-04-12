# Bladder Analysis
# Matt Kmiecik
# Started 12 April 2022

# Purpose: visualizes and models bladder data from the OCP/NSAID datasets

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/ss-masterlist.rda") # masterlist
load("../output/ocp-bladder-data.rda") # ocp bladder data
load("../output/nsaid-bladder-data.rda") # nsaid bladder data

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

bladder_pain_data_wide <-
  bladder_data %>% 
  select(
    global_id:study, 
    bl_pain = bt3_baselinepain,
    fs_pain = bt10a_fspain,
    fu_pain = bt10b_fupain,
    mt_pain = bt10c_mtpain
    )

bladder_pain_data_long <-
  bladder_pain_data_wide %>%
  pivot_longer(c(-global_id, -group, -drug, -notes, -visit_month, -study)) %>%
  mutate(
    name = fct_relevel(name, c("bl_pain", "fs_pain", "fu_pain", "mt_pain")),
    value = as.numeric(value)
    ) %>%
  filter(complete.cases(value))

# Histogram
ggplot(bladder_pain_data_long, aes(value)) +
  geom_histogram(binwidth = 2) +
  labs(x = "VAS Pain Rating (0-100)", y = "Frequency") +
  theme_bw() +
  facet_wrap(~name)

# Density
ggplot(bladder_pain_data_long, aes(value, group = name, color = name)) +
  geom_density(aes(fill = name), alpha = 1/3) +
  labs(x = "VAS Pain Rating (0-100)", y = "Density") +
  scale_color_ghibli_d("MononokeMedium", direction = -1) +
  scale_fill_ghibli_d("MononokeMedium", direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom")

bladder_pain_data_sum <-
  bladder_pain_data_long %>%
  group_by(drug, visit_month, name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  )

pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(bladder_pain_data_sum, aes(factor(visit_month), m, group = drug, color = drug)) +
  geom_point(
    data = bladder_pain_data_long, 
    aes(y = value), 
    alpha = .3,
    position = pj, 
    shape = 16
    ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  #scale_color_jco() +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~name, nrow = 1) +
  theme(legend.position = "bottom")



