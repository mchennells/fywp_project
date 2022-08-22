# FY PROJECT
# :: GENERATING MODULES LIST FOR CODING
# Combining and exporting modules for export for manual coding, reimporting for all modules currently in the dataset

setwd("~/Google Drive/Warwick/PhD/Work/WBS/TK/FYWP Data Project/fywp_project")

library(tidyverse)
library(data.table)
library(readxl)

rm(list=ls())

# Create module codes for all modules across all the datasets, export for manual coding, then import
# NOTE: some modules may no longer exist (e.g. be in manual code file but not on record)

# ==================================================================================================================================
## FILENAMES
# Update filenames for data and modules lists
path_students_occmarks <- file.path("data_files", "ugrad_students_occmarks_Aug22_ID_anonym.xlsx")
path_students_lectures <- file.path("data_files", "ugrad_students_lectures_Aug22_ID_anonym.xlsx")
path_students_seminars <- file.path("data_files", "ugrad_students_seminars_anonym_dec20.xlsx")

# Get datasets
dataset_students_lectures <- read_excel(path_students_lectures)
dataset_students_occmarks <- read_excel(path_students_occmarks)
dataset_students_seminars <- read_excel(path_students_seminars)

# ==================================================================================================================================
## MODULE LIST TO EXPORT

# Create the module list, which is then exported then externally manually coded and then read back in
modules_list_marks <- unique(dataset_students_occmarks %>% select(MODULE,MODULE_TITLE))
modules_list_lectures <- unique(dataset_students_lectures %>% select(MODULE,MODULE_TITLE))
modules_list_seminars <- unique(dataset_students_seminars %>% select(MODULE,MODULE_TITLE))

all_modules <- rbind(modules_list_marks,modules_list_lectures,modules_list_seminars)

all_modules <- unique(all_modules %>% select(MODULE,MODULE_TITLE) %>%arrange(MODULE,MODULE_TITLE))
length(all_modules$MODULE)

#all_modules_duplicate <- all_modules[!duplicated(all_modules),]

# Write list of all modules to file for manual coding
path_modules_EXPORT <- file.path("data_files", "modules/OUTPUT_modules_list_ALL.csv")  # Dataset (list) of modules
write_csv(all_modules, path_modules_EXPORT)

# ==================================================================================================================================
## IMPORT LIST OF MANUALLY CODED MODULES AND CHECK FOR ANY MISSING
# The manual coding should have been done on the exported list from above, using the following key:
# > 1: Numerical/Quantitative/Science/Engineering, 
# > 2: Social Science/Social Studies, Qualitative, 
# > 3: Languages 
# > 4: Others

# Import list of all modules for merging: this name must be manually changed in source file
path_modules_MANUALCODE_1 <- file.path("data_files", "modules/INPUT_modules_list_MANUALCODE_1.csv") # Dataset of manual coded modules
modules_list_MANUALCODE_1 <- read_csv(file = path_modules_MANUALCODE_1) %>% 
  select(MODULE, MODULE_TITLE, MODULE_TYPE)
modules_list_MANUALCODE_1 <- unique(modules_list_MANUALCODE_1)
length(modules_list_MANUALCODE_1$MODULE)

path_modules_MANUALCODE_2 <- file.path("data_files", "modules/INPUT_modules_list_MANUALCODE_2.csv") # Dataset of manual coded modules
modules_list_MANUALCODE_2 <- read_csv(file = path_modules_MANUALCODE_2) %>% 
  select(MODULE, MODULE_TITLE, MODULE_TYPE)
modules_list_MANUALCODE_2 <- unique(modules_list_MANUALCODE_2)
length(modules_list_MANUALCODE_2$MODULE)

# >> Do the above for each new list of manually coded (i.e. each time have to update)

# Bind together manually coded modules
modules_list_INPUTS <- rbind(modules_list_MANUALCODE_1, modules_list_MANUALCODE_2)
length(modules_list_INPUTS$MODULE)
length(unique(modules_list_INPUTS$MODULE))

# Check for duplicates
modules_list_INPUTS_duplicates <- modules_list_INPUTS[duplicated(modules_list_INPUTS$MODULE),]
duplicate_check <- modules_list_INPUTS %>% filter(MODULE %in% modules_list_INPUTS_duplicates$MODULE)
# Easiest here is to manually scan the list of duplicates to check for differences in MODULE TYPE by MODULE
# ... and then easiest is to manually change one or more of these in the input file rather than direct code change here

# >> Once manually fixed the duplicates, re-run the above code, check duplicates again and repeat until happy; then proceed.
# Note, might be best to leave duplicates given slight differences in TITLE etc.; just ensure TYPE is the same

# Merge module list with manually coded inputs
all_modules <- left_join(all_modules, modules_list_INPUTS, by = c('MODULE', 'MODULE_TITLE'))

# Partial coded, for reference
path_modules_list_PARTIAL <- file.path("data_files", "modules/OUTPUT_modules_list_PARTIALCODED.csv")
write_csv((all_modules %>% arrange(MODULE, MODULE_TITLE, MODULE_TYPE)),path_modules_list_PARTIAL)
# ==================================================================================================================================
## CHECK FOR ANY MISSING CODES

# Check for any modules missing manual classification (note: across all students, FY and DE, in the data)
all_modules %>% count(MODULE_TYPE)

# Export dataset of missing modules
modules_list_missing <- all_modules %>% filter(is.na(MODULE_TYPE))
length(modules_list_missing$MODULE_TYPE)
path_modules_list_MISSING <- file.path("data_files", "modules/OUTPUT_modules_list_MISSING.csv") # Dataset of still missing modules
write_csv(modules_list_missing, path_modules_list_MISSING)

# >> Update the missing list file manually, call it "INPUT_modules_list_MANUALCODE_MISSING.csv" and then proceed

# ------------------------------------
# If the missing file has been updated, then do this section; otherwise skip to the next

# Import list of missing modules which have again been manually coded in source file
path_modules_MISSING_MANUALCODE <- file.path("data_files", "modules/INPUT_modules_list_MANUALCODE_MISSING.csv") # Dataset of manual coded missing modules
modules_list_missing_MANUALCODE <- read_csv(file = path_modules_MISSING_MANUALCODE)
modules_list_missing_MANUALCODE <- modules_list_missing_MANUALCODE %>% 
  select(MODULE, MODULE_TITLE, MODULE_TYPE)
modules_list_missing_MANUALCODE <- unique(modules_list_missing_MANUALCODE)
length(path_modules_MISSING_MANUALCODE$MODULE_TYPE)

# Bind previously coded and missing coded module lists together
all_modules <- all_modules %>% filter(!is.na(MODULE_TYPE))
all_modules <- rbind(all_modules, path_modules_MISSING_MANUALCODE)

# ==================================================================================================================================
## COMBINE MODULES LIST AND MERGE

# Check there are no missings; if there are, repeat above section or convert the rest to Unknown
all_modules %>% count(MODULE_TYPE)
View(all_modules %>% filter(is.na(MODULE_TYPE)))

all_modules$MODULE_TYPE <- ifelse(is.na(all_modules$MODULE_TYPE), 4, all_modules$MODULE_TYPE)
all_modules %>% count(MODULE_TYPE)

# Export dataset of module types for analysis
write_csv(all_modules,'data_files/data_modules_list.csv')

# ==================================================================================================================================
