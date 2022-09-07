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
path_students_seminars <- file.path("data_files", "ugrad_students_seminars_Aug22_ID_anonym.xlsx")

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

# Trim white spaces, causing a merge issue later
all_modules <- all_modules %>% mutate(MODULE_TITLE = trimws(MODULE_TITLE))

#all_modules_duplicate <- all_modules[!duplicated(all_modules),]

# Export this list if a full module list (without type classification)
write_csv(all_modules,'data_files/modules/OUTPUT_modules_list_NO TYPE.csv')

# ==================================================================================================================================
## IMPORT LIST OF MANUALLY CODED MODULES AND CHECK FOR ANY MISSING
# The manual coding should have been done on the exported list from above, using the following key:
# > 1: Numerical/Quantitative/Science/Engineering, 
# > 2: Social Science/Social Studies, Qualitative, 
# > 3: Arts & Humanities, including Languages 
# > 4: Others

# Import list of all modules for merging: this name must be manually changed in source file
path_modules_MANUALCODE <- file.path("data_files", "modules/INPUT_modules_list_MANUALCODE_010922.csv") # Dataset of manual coded modules
modules_list_MANUALCODE <- read_csv(file = path_modules_MANUALCODE)
modules_list_MANUALCODE <- modules_list_MANUALCODE %>% select(MODULE, MODULE_TITLE, MODULE_TYPE)
modules_list_MANUALCODE <- unique(modules_list_MANUALCODE)
length(modules_list_MANUALCODE$MODULE)

modules_list_MANUALCODE <- modules_list_MANUALCODE %>% mutate(MODULE_TITLE = trimws(MODULE_TITLE))

# Check none are missing in manual coded file
  modules_list_MANUALCODE %>% count(MODULE_TYPE)
  #View(modules_list_MANUALCODE %>% filter(is.na(MODULE_TYPE)))

# Check none are missing given original list
module_list_full <- left_join(all_modules, modules_list_MANUALCODE, by = c("MODULE", "MODULE_TITLE") )
  module_list_full %>% count(MODULE_TYPE)
  View(module_list_full %>% filter(is.na(MODULE_TYPE)))
  View(modules_list_MANUALCODE %>% filter(MODULE %in% ( module_list_full %>% filter(is.na(MODULE_TYPE)))$MODULE))
  # Issue with formatting; manually change

module_list_full <- module_list_full %>% mutate(
  MODULE_TYPE = ifelse( MODULE == "LA3A70", 3,
                        ifelse(MODULE == "PH3560", 2 ,
                               ifelse(MODULE == "PS3550", 2 , MODULE_TYPE))))
  View(module_list_full %>% filter(is.na(MODULE_TYPE)))

  module_list_full %>% count(MODULE_TYPE)
  length(module_list_full$MODULE)
  length(unique(module_list_full$MODULE)) # Some module numbers are repeated, with slightly different titles; as long as they merge on to main dataset then type doesn't matter (and should merge, given left_join of all_modules)
  
# ------------------------------------------------------------------------------------------------------------------------
# Export dataset of module types for use in analysis
write_csv(module_list_full,'data_files/data_modules_list.csv')

# Note: if there are some types missing, then use this output list to update the type, and then rename as per the import file in line 58 (modules/INPUT_modules_list_MANUALCODE_DATE), and run the code again
  
# ==================================================================================================================================
