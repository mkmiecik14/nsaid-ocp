# Demographic Analysis for clinical trials
# Matt Kmiecik
# Started 6 July 2022

# Purpose: prepared demographic information

# Prepares workspace
source("r-prep.R")

# Load
load("../output/ocp-demo.rda") # created via `prepro-api-calls.R`
load("../output/ss-masterlist.rda")

# Retrieves the subject numbers for those in OCP trial
clinical_trial_ss <- 
  ss_masterlist %>% 
  filter(group %in% c("DYSB", "BPS")) %>%
  filter(drug %nin% "NSAID")

# joins dfs together
demo_long <- 
  left_join(clinical_trial_ss, ocp_demo, by = c("arm1r" = "record")) %>%
  separate(field_name, into = c("pre", "meas")) %>%
  select(-pre)

#######
#     #
# AGE #
#     #
#######

demo_age <- 
  demo_long %>% 
  filter(meas == "age") %>%
  mutate(value = as.numeric(value)) # converts to number

demo_age %>%
  group_by(group, drug) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    min = min(value),
    max = max(value)
  ) %>%
  ungroup()

demo_age %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    min = min(value),
    max = max(value)
  )

#############
#           #
# ETHNICITY #
#           #
#############

demo_eth <- 
  demo_long %>% 
  filter(meas == "ethnicity") %>%
  mutate(
    value = as.numeric(value), # converts to number
    value = ifelse(value == 1, "Hispanic or Latino", "Not Hispanic or Latino")
    ) 

demo_eth %>% count(group, drug, value)

########
#      #
# RACE #
#      #
########

# creates a tibble for easy joining
race_tibble <- 
  tibble(
    value = 1:5, 
    race = c(
      "American Indian or Alaskan Native", 
      "Asian",
      "Native Hawaiian or Pacific Island",
      "Black or African American",
      "White"
      )
  )

demo_race <-
  demo_long %>% 
  filter(meas == "race") %>%
  mutate(value = as.numeric(value)) %>% # converts to number
  left_join(., race_tibble, by = "value")

demo_race %>% count(group, drug, race) 

# DYSB NO_OCP 1 person identified with more than one race (global_id 131)
demo_race %>% filter(drug == "OCP_NO")
  







  



