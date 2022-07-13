# PPT Analysis
# Matt Kmiecik
# Started 3 June 2022

# Purpose: visualize and analyze the PPT data (see prepro-ppt.R for prep)

source("r-prep.R") # Prepare R workspace

# Loading data
load("../output/ss-masterlist.rda") # masterlist
load("../output/ppt-data.rda")

#################
#               #
# External PPTs #
#               #
#################

# Initial visualization
pj <- position_jitter(width=.1)
ggplot(
  ppt_data %>% filter(test == "PPT_ext"),
  aes(factor(trial), force)
  ) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    alpha = 1/3,
    fill = "black"
    ) +
  labs(x = "Trial", y = "Force (Newtons)") +
  facet_grid(visit_month~site) +
  theme_bw()

# Subject-wise
ext_ppt_ss <-
  ppt_data %>% 
  filter(test == "PPT_ext") %>% # only external PPTs
  filter(complete.cases(force)) %>% # removes missing data
  group_by(global_id, group, drug, visit_month, site) %>%
  summarise(
    m = mean(force),
    sd = sd(force),
    n = n()
  ) %>%
  ungroup()

# Group-wise
ext_ppt_sum <-
  ext_ppt_ss %>%
  group_by(drug, visit_month, site) %>%
  summarise(
    M = mean(m),
    SD = sd(m),
    N = n(),
    SEM = SD/sqrt(N),
    LL = quantile(m, .025),
    UL = quantile(m, .975)
  ) %>%
  ungroup()

# Plot
pj <- position_jitter(width = .5)
pd <- position_dodge(width = .2)
ggplot(ext_ppt_sum, aes(visit_month, M, group = drug, color = drug)) +
  geom_point(data = ext_ppt_ss, aes(y = m), alpha = 1/3, position = pj, shape = 16) +
  geom_path(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = M-SEM, ymax = M+SEM), 
    width = .1,
    position = pd
    ) +
  scale_x_continuous(breaks = seq(0, 12, 6), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", y = "Force (Newtons)", caption = "SEM error bars.") +
  facet_wrap(~site, nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

#################
#               #
#  Vaginal PPTs #
#               #
#################

# Initial visualization
pj <- position_jitter(width=.1)
ggplot(
  ppt_data %>% filter(test == "PPT_int"),
  aes(factor(trial), force)
  ) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    alpha = 1/3,
    fill = "black"
  ) +
  labs(x = "Trial", y = "Force (Newtons)") +
  facet_grid(visit_month~site) +
  theme_bw()

# Subject-wise
int_ppt_ss <-
  ppt_data %>% 
  filter(test == "PPT_int") %>% # only external PPTs
  filter(complete.cases(force)) %>% # removes missing data
  group_by(global_id, group, drug, visit_month, site) %>%
  summarise(
    m = mean(force),
    sd = sd(force),
    n = n()
  ) %>%
  ungroup()

# Group-wise
int_ppt_sum <-
  int_ppt_ss %>%
  group_by(drug, visit_month, site) %>%
  summarise(
    M = mean(m),
    SD = sd(m),
    N = n(),
    SEM = SD/sqrt(N),
    LL = quantile(m, .025),
    UL = quantile(m, .975)
  ) %>%
  ungroup()

# Plot
pj <- position_jitter(width = .5)
pd <- position_dodge(width = .2)
ggplot(int_ppt_sum, aes(visit_month, M, group = drug, color = drug)) +
  geom_point(data = int_ppt_ss, aes(y = m), alpha = 1/3, position = pj, shape = 16) +
  geom_path(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = M-SEM, ymax = M+SEM), 
    width = .1,
    position = pd
  ) +
  scale_x_continuous(breaks = seq(0, 12, 6), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", y = "Force (Newtons)", caption = "SEM error bars.") +
  facet_wrap(~site, nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

# Clinical trials . gov data
# These PPTs were reported
int_ppt_ss %>%
  filter(
    group %in% c("DYSB", "BPS"), # only dysb and bps 
    drug %nin% "NSAID", # no nsaid participants
    site == "12" # only site 12 o'clock
    ) %>%
  group_by(group, drug, visit_month) %>%
  summarise(
    M = mean(m),
    SD = sd(m),
    N = n(),
    SEM = SD/sqrt(N)
  ) %>%
  ungroup()

# Grand average for baseline participants
int_ppt_ss %>%
  filter(
    group %in% c("DYSB", "BPS"), # only dysb and bps 
    drug %nin% "NSAID", # no nsaid participants
    site == "12", # only site 12 o'clock
    visit_month == 0
    ) %>%
  group_by(visit_month) %>%
  summarise(
    M = mean(m),
    SD = sd(m),
    N = n(),
    SEM = SD/sqrt(N)
  ) %>%
  ungroup()

# linear mixed effect model
clin_trials_mod_data <- 
  int_ppt_ss %>%
  filter(
    group %in% c("DYSB", "BPS"), # only dysb and bps 
    drug %nin% "NSAID", # no nsaid participants
    site == "12" # only site 12 o'clock
  ) %>%
  mutate(
    ct_groups = interaction(group, drug), # creates groups for clinical trials
    ct_groups = factor(ct_groups) # factorizes it for contrasts
  ) 

clin_trials_data_missng <-
  clin_trials_mod_data %>%
  count(global_id, visit_month) %>% 
  pivot_wider(
    id_cols = global_id, 
    names_from = visit_month, 
    names_prefix = "month", 
    values_from = n
  )


# sets orthogonal contrasts
contrasts(clin_trials_mod_data$ct_groups) <- 
  cbind(
    ocp_vs_no = c(-1/4, -1/4, -1/4, 3/4), # PPT < no vs. yes
    cont_vs_cyl = c(-1/3, -1/3, 2/3, 0), # PPT < cyl vs. cont
    bps_vs_dysb = c(-1/2, 1/2, 0, 0) # PPT < bps vs. dysp
  )

# Minimum model (random intercepts only)
min_mod <- 
  lmer(
    m ~ 1 + visit_month*ct_groups + (1 | global_id), 
    data = clin_trials_mod_data
  )
summary(min_mod) # THIS WAS REPORTED ON CLINICAL TRIALS . GOV

library(interactions)
interact_plot(
  min_mod, 
  pred = visit_month, 
  modx = ct_groups, 
  plot.points = TRUE
)

## VISUALIZE THE INTERACTIONS

# Maximum model (random intercepts and slopes)
max_mod <- 
  lmer(
    m ~ 1 + visit_month*ct_groups + (1 + visit_month | global_id), 
    data = clin_trials_mod_data
  )
summary(max_mod)

# Comparing min and max models
anova(min_mod, max_mod) # max model does not improve fit


###############
#             #
#     CPM     #
#             #
###############

# Trial 1 (baseline): left shoulder and left knee PPT
# Trial 2: left knee PPT after 20s ice water bath, removed hand, then did left shoulder PPT
# Trial 3: left shoulder and left knee PPT after 5 min
# Trial 4: left shoulder and left knee PPT after 10 min

# Initial visualization
pj <- position_jitter(width=.1)
ggplot(
  ppt_data %>% filter(test == "CPM"),
  aes(factor(trial), force)
) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    alpha = 1/3,
    fill = "black"
  ) +
  labs(x = "Trial", y = "Force (Newtons)") +
  facet_grid(visit_month~site) +
  theme_bw()

# Subject-wise
cpm_ss <- ppt_data %>% filter(test == "CPM")

# Group-wise
cpm_sum <-
  cpm_ss %>%
  filter(complete.cases(force)) %>%
  group_by(drug, visit_month, trial, site) %>%
  summarise(
    M = mean(force),
    SD = sd(force),
    N = n(),
    SEM = SD/sqrt(N),
    LL = quantile(force, .025),
    UL = quantile(force, .975)
  ) %>%
  ungroup()

# CPM EFFECT SUBJECT-WISE 
# (Trial 2 - Trial 1; more positive values are desired = better CPM)
cpm_effect_ss <-
  cpm_ss %>%
  filter(complete.cases(force)) %>%
  filter(trial<3) %>%
  group_by(global_id, group, drug, visit_month, site) %>%
  summarise(
    cpm = diff(force)
  ) %>%
  ungroup()

# CPM EFFECT GROUP-WISE
cpm_effect_sum <-
  cpm_effect_ss %>%
  group_by(drug, visit_month, site) %>%
  summarise(
    M = mean(cpm),
    SD = sd(cpm),
    N = n(),
    SEM = SD/sqrt(N),
    LL = quantile(cpm, .025),
    UL = quantile(cpm, .975)
  ) %>%
  ungroup()

# CPM Effect Plot
pj <- position_jitter(width = .5)
pd <- position_dodge(width = 1)
ggplot(
  cpm_effect_sum, 
  aes(visit_month, M, group = drug, color = drug)
  ) +
  # geom_point(
  #   data = cpm_effect_ss, 
  #   aes(y = cpm), 
  #   shape = 16, 
  #   alpha = 1/3,
  #   position = pj
  #   ) +
  geom_path(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = M-SEM, ymax = M+SEM), 
    width = .1,
    position = pd
    ) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 12, 6), minor_breaks = NULL) +
  labs(
    x = "Month", 
    y = "CPM Effect (more positive values = better CPM)", 
    caption = "SEM error bars"
    ) +
  facet_wrap(~site) +
  theme_bw() +
  theme(legend.position = "bottom")

######################
#                    #
# Temporal Summation #
#                    #
######################

# the PPT force will not say anything because it is kept constant
# be sure to pull the pain ratings from redcap to see this


