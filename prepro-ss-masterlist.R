# Master List Prepro
# Matt Kmiecik
# 01 APRIL 2022

# Purpose: preprocesses the masterlist of participant IDs

# Prepares workspace
source("r-prep.R")

# Reads in data
ss_masterlist <- read_excel("../data/ss-masterlist.xlsx", na = "NA")

# Saves out data
save(ss_masterlist, file = "../output/ss-masterlist.rda") # rda
write_csv(ss_masterlist, file = "../output/ss-masterlist.csv") # csv

# Removes script objects
rm(ss_masterlist)