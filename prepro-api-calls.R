# Preprocessing API data
# Matt Kmiecik
# 01 April 2022

# Purpose: preprocess and prepare the data coming out of the API calls

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/ss-masterlist.rda") # masterlist

# CRAMPP
load("../output/arm1-screenvisit-api.rda") # CRAMPP screen visit
load("../output/arm1-avisit1-api.rda") # CRAMPP avisit 1
load("../output/arm1-avisit2-api.rda") # CRAMPP avisit 2
load("../output/arm1-avisit3-api.rda") # # CRAMPP avisit 3

# NSAID
load("../output/nsaid-screenvisit-api.rda") # NSAID screen visit
load("../output/nsaid-baselinevisit-api.rda") # NSAID baseline visit
load("../output/nsaid-followupvisit-api.rda") # NSAID follow-up visit

# Combining data for efficiency - - - -

# Assessment visit data
# OCP DATA
ocp_avisit_data <- 
  bind_rows(arm1_avisit1_api, arm1_avisit2_api, arm1_avisit3_api) %>%
  mutate(
    event_id = case_when(
      event_id == "assessment_visit_1_arm_1" ~ 0,
      event_id == "assessment_visit_2_arm_1" ~ 6,
      event_id == "assessment_visit_3_arm_1" ~ 12,
    )
    ) %>%
  rename(visit_month = event_id)

# NSAID DATA
nsaid_avisit_data <-
  bind_rows(nsaid_baselinevisit_api, nsaid_followupvisit_api) %>%
  mutate(
    event_id = case_when(
      event_id == "baseline_visit_arm_1" ~ 0,
      event_id == "follow_up_visit_arm_1" ~ 6,
    )
  ) %>%
  rename(visit_month = event_id)

################
#              #
# Bladder Test #
#              #
################

# OCP bladder data
ocp_bladder_data <- 
  ocp_avisit_data %>%
  filter(grepl("bt", field_name) | grepl("bladder", field_name)) %>%
  pivot_wider(
    id_cols = c(record, visit_month), 
    names_from = field_name, 
    values_from = value
    ) %>%
  select(-ic2e_probtotal) %>% # removes column accidentally grabbed in grepl
  mutate(study = "ocp") %>%
  relocate(study, .before = bladder_testing_timestamp)
  
# NSAID bladder data
nsaid_bladder_data <- 
  nsaid_avisit_data %>%
  filter(grepl("bt", field_name) | grepl("bladder", field_name)) %>%
  pivot_wider(
    id_cols = c(record, visit_month), 
    names_from = field_name, 
    values_from = value
  ) %>%
  select(-followupic2e_probtotal, -ic2e_probtotal) %>% # removes columns
  mutate(study = "nsaid") %>%
  relocate(study, .before = bt10b_futime)
  
# Saves out data
save(ocp_bladder_data, file = "../output/ocp-bladder-data.rda")
save(nsaid_bladder_data, file = "../output/nsaid-bladder-data.rda")

####################
#                  #
# DEMOGRAPHIC DATA #
#                  #
####################

demo_vars <- c("mh2_age", "mh3_race", "mh4_ethnicity") # demo variables for extraction

#extracting race and age from ocp 
ocp_demo <-
  arm1_screenvisit_api %>% 
  filter(field_name %in% demo_vars)

#extracting age and race in nsaid
nsaid_demo <-
  nsaid_screenvisit_api %>% 
  filter(field_name %in% demo_vars)

# saves out data 
save(ocp_demo,file = "../output/ocp-demo.rda")
save(nsaid_demo, file = "../output/nsaid-demo.rda")
