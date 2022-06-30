# API Configuration
# Matt Kmiecik
# Started: 01 APRIL 2022

# Purpose: Preforms API calls to RedCap server 

# Loads packages ----
source("r-prep.R") # Prepares R workspace

# API setup ----
api_url <- "https://survey.northshore.org/api/" # Redcap URL

# Tokens ----
arm1_api_token <- 
  read_lines(
    file = "C:/Analysis/nsaid-ocp/private/EH13-094-OCP-GRANT-api-token.txt" # token
  )

arm2_api_token <- 
  read_lines(
    file = "C:/Analysis/nsaid-ocp/private/EH13-094-Arm-2-api-token.txt" # token
  )

shortened_annual_api_token <- 
  read_lines(
    file = "C:/Analysis/nsaid-ocp/private/EH13-094-Shortened-Annual-api-token.txt" # token
  )

nsaid_api_token <-
  read_lines(
    file = "C:/Analysis/nsaid-ocp/private/NSAID-EH18-128-api-token.txt" # token
  )

# Pulling data from API

##########
#        #
# CRAMPP #
#        #
##########

# Pulling CRAMPP screen visit data - arm 1
arm1_screenvisit_api <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "screen_visit_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>% 
  content(as = "parsed")

# Saves out data 
save(arm1_screenvisit_api, file = "../output/arm1-screenvisit-api.rda")
write_csv(arm1_screenvisit_api, file = "../output/arm1-screenvisit-api.csv")


# Pulling CRAMPP Assessment Visit 1 - arm 1
arm1_avisit1_api <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "assessment_visit_1_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(arm1_avisit1_api, file = "../output/arm1-avisit1-api.rda")
write_csv(arm1_avisit1_api, file = "../output/arm1-avisit1-api.csv")

# Pulling CRAMPP Assessment Visit 2 - arm 1
arm1_avisit2_api <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "assessment_visit_2_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(arm1_avisit2_api, file = "../output/arm1-avisit2-api.rda")
write_csv(arm1_avisit2_api, file = "../output/arm1-avisit2-api.csv")

# Pulling CRAMPP Assessment Visit 3 - arm 1
arm1_avisit3_api <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "assessment_visit_3_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(arm1_avisit3_api, file = "../output/arm1-avisit3-api.rda")
write_csv(arm1_avisit3_api, file = "../output/arm1-avisit3-api.csv")

##########
#        #
# NSAID  #
#        #
##########

# Pulling NSAID Screen Visit
nsaid_screenvisit_api <- 
  POST(
    url = api_url,
    body = list(
      token = nsaid_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "screen_visit_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(nsaid_screenvisit_api, file = "../output/nsaid-screenvisit-api.rda")
write_csv(nsaid_screenvisit_api, file = "../output/nsaid-screenvisit-api.csv")

# Pulling NSAID Baseline Visit
nsaid_baselinevisit_api <- 
  POST(
    url = api_url,
    body = list(
      token = nsaid_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "baseline_visit_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(nsaid_baselinevisit_api, file = "../output/nsaid-baselinevisit-api.rda")
write_csv(nsaid_baselinevisit_api, file = "../output/nsaid-baselinevisit-api.csv")

# Pulling NSAID Follow-up Visit
nsaid_followupvisit_api <- 
  POST(
    url = api_url,
    body = list(
      token = nsaid_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "follow_up_visit_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>%
  content(as = "parsed")

# Saves out data 
save(nsaid_followupvisit_api, file = "../output/nsaid-followupvisit-api.rda")
write_csv(nsaid_followupvisit_api, file = "../output/nsaid-followupvisit-api.csv")

# Remove script objects
rm(
  api_url, 
  arm1_api_token, 
  arm2_api_token, 
  nsaid_api_token, 
  shortened_annual_api_token,
  arm1_avisit1_api,
  arm1_avisit2_api,
  arm1_avisit3_api,
  arm1_screenvisit_api,
  nsaid_screenvisit_api,
  nsaid_baselinevisit_api,
  nsaid_followupvisit_api
  )


