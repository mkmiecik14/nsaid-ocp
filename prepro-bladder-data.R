# Preprocesses the bladder data further
# Matt Kmiecik
# Started 25 May 2022

# Purpose: takes the bladder data that was processed in `prepro-api-calls.R` and
# further cleans it up for analysis

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

# Saves wide data out 
save(bladder_data_wide, file = "../output/bladder-data-wide.rda")

# missing data
# bladder_data_wide %>% count(global_id, drug, visit_month)
# missing_data <- bladder_pain_data %>% filter(is.na(value))
# missing <- 
#   bladder_data_wide %>% 
#   select(-notes) %>% 
#   filter(!complete.cases(.)) %>%
#   filter(complete.cases(visit_month))
# # figure out why some participants have random missing data points ^^^
# missing_bladder_data <- 
#   missing %>% 
#   left_join(
#     ., 
#     ss_masterlist %>% select(global_id:nsaid_r, notes), 
#     by = "global_id"
#   ) %>%
#   select(global_id, ss:notes, group:scale_vol)
# saves out (uncomment to save)
# write_csv(missing_bladder_data, "../output/missing-bladder-data.csv")

# long format for all bladder data 
bladder_data_long <-
  bladder_data_wide %>%
  pivot_longer(c(-global_id, -group, -drug, -notes, -visit_month, -study)) %>%
  separate(name, into = c("stage", "meas"))

# Saves long data out 
save(bladder_data_long, file = "../output/bladder-data-long.rda")

# Cleans script objects
rm(
  bladder_data,
  bladder_data_long,
  bladder_data_wide,
  nsaid_bladder_data,
  nsaid_bladder_ss,
  nsaid_masterlist,
  ocp_bladder_data,
  ocp_bladder_ss,
  ocp_masterlist,
  ss_masterlist
)


