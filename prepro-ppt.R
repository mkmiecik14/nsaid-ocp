# Pressure Pain Thresholds (PPTs) Preprocessing Script
# Matt Kmiecik
# Started 31 May 2022

# Purpose: load raw PPT data and clean/prep for analysis
# Inspired by: https://github.com/mkmiecik14/mmh/blob/main/ppt-prepro.R

source("r-prep.R") # Prepare R workspace

############
#          #
# OCP Data #
#          #
############

# Loads in data
ppt_data <- 
  read_excel(path = "../data/ppt.xlsx", sheet = "matt-prep", na = c("MS", "ms")) %>%
  separate(ss, into = c("ss", "session")) %>% # some ss were named with visit
  mutate(
    ss = as.numeric(ss),
    visit = ifelse(is.na(session), visit, session),
    visit = as.numeric(visit),
    test = gsub(" ", "_", test) # removes space after PPTs
    ) %>%
  select(-session) %>%
  mutate(force = gsub("\\*", "_*", force)) %>% # this is to handle the *
  separate(force, into = c("force", "force_notes"), sep = "_") %>%
  mutate(
    force = as.numeric(force), 
    rate = as.numeric(rate),
    duration = as.numeric(duration)
    ) %>%
  
  ################
  #              #
  # DATA EDITING #
  #              #
  ################
  
  # data editing due to notes (see notes)
  # ss 2 seems to have a huge outlier for trial 1 of internal PPT; given that she
  # has 2 trials, the first was given an NA
  mutate(
    force = ifelse(
      ss == 2 & 
        visit == 1 & 
        test == "PPT_int" & 
        trial == 1 & 
        site == "12",
      NA,
      force
    ), 
    force = ifelse(
      ss == 13 & 
      visit == 1 & 
      test == "PPT_ext" & 
      trial == 2 & 
      site == "rShoulder",
    force + 15,
    force
    ),
    force = ifelse(
      ss == 38 & 
      visit == 1 & 
      test == "TS" & 
      trial == 2 & 
      site == "rKnee",
      NA,
      force
    ),
    force = ifelse(
      ss == 49 & 
      visit == 1 & 
      test == "TS" & 
      trial == 2 & 
      site == "rKnee",
      25,
      force
      ),
    force = ifelse(
      ss == 61 & 
        visit == 1 & 
        test == "PPT_int" & 
        site == 5,
      7.5, # see notes (replaced with middle value)
      force
    ),
    force = ifelse(
      ss == 116 & 
        visit == 1 & 
        test == "PPT_ext" & 
        site == "rForehead",
      7.5, # see notes (replaced with middle value)
      force
    ),
    force = ifelse(
      ss == 58 & 
        visit == 1 & 
        test == "PPT_int" & 
        site == 5,
      4, # see notes (replaced with middle value)
      force
    ),
    force = ifelse(
      ss == 58 & 
        visit == 1 & 
        test == "PPT_int" & 
        site == 6,
      7, # see notes (replaced with 7 as best estimate)
      force
    ),
    force = ifelse(
      ss == 59 & 
        visit == 1 & 
        test == "PPT_int" &
        trial == 1 &
        site == 5,
      3, # see notes
      force
    ),
    force = ifelse(
      ss == 322 & 
        visit == 1 & 
        test == "CPM" &
        trial == 1 &
        site == "lKnee",
      40, # see notes
      force
    )
  ) %>%
  
  # stopped data editing after ss 49, decided to visualize data for issues
  mutate(force = ifelse(force == 0, NA, force)) # force cannot be equal to zero
  
# Saving out data ----
save(ppt_data, file = "../output/ppt-data.RData") # RData
write_csv(ppt_data, file = "../output/ppt-data.csv") # CSV 

rm(ppt_data) # cleans script object(s)
