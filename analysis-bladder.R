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

# long format for all bladder data 
bladder_data_long <-
  bladder_pain_data_wide %>%
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
    value = as.numeric(value)
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

# missing data
# bladder_data_wide %>% count(global_id, drug, visit_month)
# missing_data <- bladder_pain_data %>% filter(is.na(value))
missing <- bladder_data_wide %>% select(-notes) %>% filter(!complete.cases(.))
# figure out why some participants have random missing data points ^^^


# Bladder pain plot
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
  bladder_pain_data_sum, 
  aes(factor(visit_month), m, group = drug, color = drug)
  ) +
  geom_point(
    data = bladder_pain_data, 
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



