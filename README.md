# NSAID / OCP
Code repository for analyzing the OCP data from CRAMPP and the NSAID data

# Workspace Prep
`r-prep.R` - this script prepares the work space and load packages 

#API Calls
`api-calls.R` -  Preforms API calls to RedCap server

# Preprocessing Scripts
`prepro-api-calls.R` - Purpose: preprocess and prepare the data coming out of the API calls
`prepro-bladder-data.R` - takes the bladder data that was processed in `prepro-api-calls.R` and further cleans it up for analysis
`prepro-ss-masterlist.R`-  preprocesses the master list of participant IDs
`prepro-ppt.R` - load raw PPT data and clean/prep for analysis

# Analysis scripts

`analysis-bladder-pain.R`- Scripts that analyzes the bladder pain data
`analysis-bladder-urgency.R`- Scripts that analyzes the bladder urgency data
`analysis-ppt.R`- Scripts that analyzes the ppt data


