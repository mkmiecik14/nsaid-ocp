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


