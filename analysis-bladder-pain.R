# Bladder Analysis
# Matt Kmiecik
# Started 12 April 2022

# Prepares workspace
source("r-prep.R")

# Load
load("../output/bladder-data-wide.rda")
load("../output/bladder-data-long.rda")

########
#      #
# PAIN #
#      #
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
clinical_trials_ss <-
  bladder_pain_data %>%
  filter(group %in% c("DYSB", "BPS"), drug %nin% "NSAID")

clinical_trials_summary <-
  bladder_pain_data %>% 
  filter(complete.cases(value)) %>%
  filter(drug %nin% "NSAID") %>%
  group_by(group, drug, visit_month, stage, meas) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# CLINICAL TRIALS DOT GOV PLOT
pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
ggplot(
 clinical_trials_summary, 
  aes(factor(visit_month), m, group = interaction(drug, group), color = interaction(drug, group)
      )
 ) +
  # geom_point(
  #   data = bladder_pain_data %>% filter(complete.cases(visit_month)), 
  #   aes(y = value), 
  #   alpha = .3,
  #   position = pj, 
  #   shape = 16
  # ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .25, position = pd) +
  labs(x = "Month", y = "VAS Pain Rating (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() +
  facet_wrap(~stage, nrow = 1) +
  theme(legend.position = "bottom")


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

# Plot for CRAMPP renewal grant
bladder_pain_data_sum3 <-
  bladder_pain_data %>%
  filter(
    complete.cases(value), # removes missing data 
    group == "DYSB" # keeps only DYSB participants
    ) %>% 
  group_by(drug_2, visit_month, stage) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

pd <- position_dodge(width = .4)
pj <- position_jitter(width = .1, height = 0)
fu_dysb_pain_plot <- 
  ggplot(
    bladder_pain_data_sum3 %>% filter(stage == "fu"), 
    aes(factor(visit_month), m, group = drug_2, color = drug_2)
  ) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .15, position = pd) +
  labs(x = "Month", y = "VAS Pain Rating @ First Urge (0-100)", caption = "SEM error bars.") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(
    breaks = seq(0, 60, 10), 
    minor_breaks = NULL, 
    expand = c(0,0)
    ) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "bottom")
fu_dysb_pain_plot
# uncomment out to save
# ggsave(
#   filename = "../output/fu-dysb-pain-plot.svg",
#   plot = fu_dysb_pain_plot,
#   width = 4.5,
#   height = 4,
#   units = "in"
#   )

# writes out n for grant
# write_csv(
#   bladder_pain_data_sum3 %>% filter(stage == "fu"),
#   file = "../output/crampp-renewal-n.csv"
# )

# repeated measures anova for grant
anova_data <- 
  bladder_pain_data %>%
  filter(
    complete.cases(value), # removes missing data 
    group == "DYSB", # keeps only DYSB participants
    stage == "fu",
    visit_month %in% c(0, 6),
    global_id %nin% c(101, 108, 118, 130, 134, 140) # participants have missing data
  ) %>%
  mutate(across(.cols = c(drug_2, visit_month, global_id), .fns = factor))
library(ez)
ezANOVA(
  data = anova_data,
  dv = value,
  wid = global_id,
  within = visit_month,
  between = drug_2,
  type = 2
)

t.test(
  value~visit_month, 
  data = anova_data %>% filter(drug_2 == "NSAID"),
  paired = TRUE
  )

t.test(
  value~visit_month, 
  data = anova_data %>% filter(drug_2 == "OCP_YES"),
  paired = TRUE
  )

t.test(
  value~visit_month, 
  data = anova_data %>% filter(drug_2 == "OCP_NO"),
  paired = TRUE
  )

ezANOVA(
  data = anova_data %>% filter(drug_2 %nin% "NSAID"),
  dv = value,
  wid = global_id,
  within = visit_month,
  between = drug_2,
  type = 2
)

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
