# FY PROJECT
# :: READ DATA
# Reading and cleaning data

setwd("~/Google Drive/Warwick/PhD/Work/WBS/TK/FYWP Data Project/fywp_project")

#sessionInfo()
library(tidyverse)
library(readxl) # Read Excel xlsx file
library(haven) # Read SPSS SAV file
library(data.table)
library(openxlsx)
library(arsenal) # for comparing dataframes

rm(list=ls())

# ==================================================================================================================================
# Set up file paths
path_students <- file.path("data_files", "ugrad_students_Aug22_ID_anonym.xlsx")
path_students_occmarks <- file.path("data_files", "ugrad_students_occmarks_Aug22_ID_anonym.xlsx")
path_students_lectures <- file.path("data_files", "ugrad_students_lectures_Aug22_ID_anonym.xlsx")
path_students_seminars <- file.path("data_files", "ugrad_students_seminars_Aug22_ID_anonym.xlsx")
path_students_supervisor <- file.path("data_files", "ugrad_students_meetings_Aug22_ID_anonym.xlsx")

# Modules list
path_modules_list <- file.path("data_files", "data_modules_list.csv")

# WP and Johnstone data
path_johnstone <- file.path("data_files", "CHECK SITS_TK_Extract for Tina Kiefer 2022_08_10_002_Aug22_ID_anonym.xlsx")

# Read data in
d_students <- read_excel(path_students)
d_marks <- read_excel(path_students_occmarks)
d_lectures <- read_excel(path_students_lectures)
d_seminars <- read_excel(path_students_seminars)
d_supervisor <- read_excel(path_students_supervisor)

d_modules_list <- read_csv(file = path_modules_list)

d_johnstone <- read_excel(path_johnstone)

# ============================================================================================================
# SAMPLE SELECTION AND CLEANING
# ============================================================================================================

## REMOVE UNNECESSARY VARIABLES

d_all <- d_students %>% 
  select(!c(
    IS_INTERCALATED, STU_NATC, DSB_CODE, YEAR_OF_BIRTH, UNIVCODE, 
  ))

# ------------------------------------------------------------------------------------------------------------
## GENERATE FY INDICATOR
# FY students who progress to UG have 2 records in the data: (1) their FY year record (2) UG record
FY_students_is_fy <- d_students %>% filter(IS_FY == 'Y') # Variable supposed to indicate they are an FY student (whether in FY or UG)
# Issue: some students have IS_FY = YES but are UG, and others have IS_FY = NO but are UG and previously were FY (according to Acronym)  
  d_students %>% filter(ID_anonym==45676949) # Example: FY one year, then on to the next, but doesn't have IS_FY = Y for both records

# Create a new indicator for FY students (given issues with IS_FY variable)
# ... identify IDs of students in their FY years
fy_id <- d_students %>% filter(ACRONYM == 'FY') 

# Dataset of record of students in their FY year > use ID from Acronym to identify both records for FY students in full dataset which students are / have been FY, given the issue in the above IS_FY variable
# ... use IDs to identify records in UG years too
d_all <- d_all %>% 
  mutate(FY_ind = ifelse(ID_anonym %in% fy_id$ID_anonym, 1, 0))
# Note: FY students will have 2 records: FY year and UG year(s)

  length((d_all %>% filter(FY_ind == 1))$ID_anonym)  # > 305 records, including both FY year record and UG record
  length(unique((d_all %>% filter(FY_ind == 1))$ID_anonym)) # > 173 unique FY students

fy_cohort <- d_all %>% filter(ACRONYM == "FY") %>% group_by(STARTYEAR) %>% tally() %>%ungroup()
fy_cohort
# can check for duplicate records after FY year: duplicated((FY_students %>% filter(ACRONYM != "FY"))$ID_anonym) # all false means no duplicates

# ------------------------------------------------------------------------------------------------------------
## CREATE FY & UNDERGRADUATE STARTYEAR
# Given issues with STARTYEAR: some FY students have FY and UG startyears the same, others are different.
# ... most FY students have their STARTYEAR (for undergrad) as their FY year, but some don't. Create new variable using FY startyear as the basis (i.e. assume STARTYEAR for FY year is correct). Assume DE students' start years are correct.
# ... and assume SCHOOLYEAR variable which identifies marks by year is correct; so academic outcomes calculated below are correct for the year in question.

# ## This piece of code shows the issues described above
# d_all %>% group_by(FY_ind) %>% tally() %>% ungroup() 
# d_all %>% group_by(FY_ind, STARTYEAR.FY.) %>% tally() %>% ungroup() 
# d_all %>% group_by(FY_ind, STARTYEAR) %>% tally() %>% ungroup()  

d_all <- d_all %>%
  mutate(
    FY_startyear = ifelse(ACRONYM == "FY", STARTYEAR, NA),   # Get FY year as base year, make rest missing
    ) %>%
  group_by(ID_anonym) %>% 
  fill(FY_startyear) %>%     # Within variable, fill missings with FY year
  ungroup()
# Note that FY_startyear and Intake Year are the same

# ------------------------------------------------------------------------------------------------------------
## FILTER COURSES
# Want to keep enough students for the demographic comparison, but not everyone - and remove here to reduce computational needs for the calculations to follow

course_filter <- d_all %>% select(ID_anonym, FY_ind, ACRONYM, ACRONYM_TITLE) %>% distinct() %>% arrange(ACRONYM)
  
course_filter <- course_filter %>% 
  filter(ACRONYM != "FY") %>%
  count(FY_ind, ACRONYM, ACRONYM_TITLE) %>% 
  pivot_wider(names_from = FY_ind, values_from = n) %>%
  rename("Non-FY students" = `0`, "FY students" = `1`)

write.xlsx(course_filter,'output/course_tally.xlsx',rowNames = F) # For manual inspection

# Identify courses FY students do, use these as a guide to filter DE students
  d_all %>% filter(FY_ind == 1) %>% count(ACRONYM)

# Only keep students from certain for analysis >> look for data file like course_tally_input and manually insert the codes below
d_all <- d_all %>%
  mutate(
    stream = ifelse(ACRONYM %in% c('MN', 'MND', 'MNDI', 'MNE', 'MNEI', 'MNF', 'MNFI', 'MNI', 'MNM', 'MNMI', 'MNUPP'), 'MN',    # BSc Management + pathways
                    ifelse(ACRONYM %in% c('AF','AFI','AFU', 'AFUPP'), 'AF',    # BSc Accounting and Finance + pathways
                           ifelse(ACRONYM %in% c('IM', 'IMD', 'IME', 'IMF', 'IMM', 'IN'), 'IM',    # International Management and International Business; 4-yr degrees
                                  ifelse(ACRONYM == 'FY', 'FY', NA))))
  ) %>%
  filter(stream %in% c('FY', 'MN', 'AF', 'IM'))
  # Note: FY students represented twice (will include 2 different records for (FY and UG)

  d_all %>% count(stream)
  print(d_all %>% count(ACRONYM), n = 50)
  length(unique((d_all %>% filter(FY_ind == 1))$ID_anonym)) # > still 173 unique FY students
  
# ------------------------------------------------------------------------------------------------------------
# More general cleaning
d_all <- d_all %>%
  mutate(
    left_ind = ifelse(STAGE_DESCRIPTION=='Left', 1, 0), # Indicator for students who have left - during FY year OR during UG (see Acronym label for which)
    timestamp = ifelse(ACRONYM == "FY", "FY", "degree"),
    SEX = replace_na(SEX, "unknown"),
    SEX = ifelse(ID_anonym == 45464177, "M", SEX),   # manually change sex of those with issues
    NATIONALITY = replace_na(NATIONALITY, "unknown"),
    NATIONALITY = ifelse(   # manually change nationality
      ID_anonym == 45464643, "British (ex. Channel Islands & Isle of Man)", ifelse(
        ID_anonym == 45464177, "British (ex. Channel Islands & Isle of Man)", NATIONALITY)),
#    start_year = as.numeric(str_sub(STARTYEAR, 0, 4)),
  )
#  %>% select(-FEESTATUS, -FEESTATUS_DESCRIPTION, -STU_NATC, -DSB_CODE, -YEAR_OF_BIRTH)   # Remove unnecessary variables

  d_all %>% count(FY_ind, left_ind) 
  # > 271 DE left, 34 FY left in either FY or UG years
  

# ------------------------------------------------------------------------------------------------------------

##### >>>>>>> This should go at the end, once everything has been counted; but just after UG startyear issues
  
## SUMMARISE PROGRESS OF FY STUDENTS
# FY students progressed to UG currently have 2 rows / records for FY year and UG year > spread to make 1 row per student with both FY and UG year recorded for each var in question
FY_students_select_spread <- d_all %>%
  select(ID_anonym, FY_ind, NATIONALITY, SEX, COUNT_MITIGATING_CIRCUMSTANCES, timestamp, STARTYEAR, 
         ACRONYM, stream, MAXSTAGE, CURRENTSTAGE, STAGE_DESCRIPTION, CURRENTSTATUS, STATUS_DESCRIPTION) %>%
  filter(FY_ind == 1) %>%
  pivot_wider(names_from = timestamp,
              values_from = c(NATIONALITY, SEX, COUNT_MITIGATING_CIRCUMSTANCES, STARTYEAR, ACRONYM, stream,
                              MAXSTAGE, CURRENTSTAGE, STAGE_DESCRIPTION, CURRENTSTATUS, STATUS_DESCRIPTION))
  length(FY_students_select_spread$ID_anonym)
  length(unique(FY_students_select_spread$ID_anonym))
  # ... these should be the same for the spread to have worked properly

FY_students_select_spread <- FY_students_select_spread %>%
  # easy way to order columns to get FY variables together and degree variables together
  select(
    ID_anonym,
    #    delay_degree_start,
    STARTYEAR_FY, STARTYEAR_degree,
    NATIONALITY_FY, SEX_FY, COUNT_MITIGATING_CIRCUMSTANCES_FY, ACRONYM_FY, stream_FY, MAXSTAGE_FY, CURRENTSTAGE_FY, STAGE_DESCRIPTION_FY, CURRENTSTATUS_FY, STATUS_DESCRIPTION_FY,
    NATIONALITY_degree, SEX_degree, COUNT_MITIGATING_CIRCUMSTANCES_degree, ACRONYM_degree, stream_degree, MAXSTAGE_degree, CURRENTSTAGE_degree, STAGE_DESCRIPTION_degree, CURRENTSTATUS_degree, STATUS_DESCRIPTION_degree
  )

# ------------------------------------------------------------------------------------------------------------
## SUMMARY DATASETS
FY_students_summary_full <- FY_students_select_spread
## --> dataset with all FY students in both their FY startyear (ACRONYM = FY) and their current stage
## Note: this does not track students over each year, just FY year and present stage / status; includes those who might have left

FY_students_summary <- FY_students_select_spread %>%
  group_by(STARTYEAR_FY, STAGE_DESCRIPTION_FY, STATUS_DESCRIPTION_FY, STARTYEAR_degree, STAGE_DESCRIPTION_degree, STATUS_DESCRIPTION_degree) %>%
  tally() %>% ungroup()

FY_students_summary_left <- FY_students_select_spread %>%
  filter(STAGE_DESCRIPTION_FY == "Left" | STAGE_DESCRIPTION_degree == "Left") # Left in FY year OR in UG year

# Export data for checking
write.xlsx(FY_students_summary_full,'output/FY_students_summary_full.xlsx',rowNames = F) # Full dataset of all FY students
write.xlsx(FY_students_summary,'output/FY_students_summary.xlsx',rowNames = F) # Summary dataset of FY student progress
write.xlsx(FY_students_summary_left,'output/FY_students_summary_left.xlsx',rowNames = F) # Summary dataset of all who left

## FYI on stage and status meanings
unique(d_all$CURRENTSTAGE)
#View(d_all %>% filter(CURRENTSTAGE %in% c('L','T')))

## Current stage & stage_description:
# X: completed
# L: left
# T: transferred
# S: suspension
# numbers (1,2,3,4,5,6): stage number

unique(d_all$CURRENTSTATUS)
#View(d_all %>% filter(CURRENTSTATUS %in% c('I','O')))

## Current status & status_description:
# NS: No show
# W: forced withdrawn
# C: voluntarily withdrawn
# N: Temporarily Withdrawn (TWD)

# J: All work submitted
# K: All work completed (FY year)
# L: Transferred
# G: awarded (degree)
# F: full
# I: Intercalated
# O: Visiting student

# 4: UG Resit
# 5: UG Resit WO Res
# 6: UG First Sit
# 7: UG First Sit WO Res


# 
# # NOTE ISSUE: some students' startyear is same for FY and UG degree, others are different
# # > assume FY year is correct - but what about those who take a break between?
# FY_students_startyear <- FY_students_select_spread %>%
#   select(ID_anonym, ACRONYM_FY, stream_FY, STARTYEAR_FY, ACRONYM_degree, stream_degree, STARTYEAR_degree, STAGE_DESCRIPTION_FY, STAGE_DESCRIPTION_degree)
# write.xlsx(FY_students_startyear,'output/FY_students_startyear.xlsx',row.names = F) # Dataset of year start to check discrepancies
# 

# ============================================================================================================
# GENERATE ACADEMIC OUTCOMES
# ============================================================================================================

## Add in module type from manual coding (see 'Module list generation.R' file)

# Recode Module Type description
d_modules_list %>% count(MODULE_TYPE)

d_modules_list <- d_modules_list %>%
  mutate(
    MODULE_TYPE = recode(MODULE_TYPE, 
                         `1` = "Numeric / Quantitative / Science / Engineering", 
                         `2` = "Social science / Social studies", 
                         `3` = "Arts & Humanities", 
                         `4` = "Other", 
                         `5` = "Unclear/unkown")
  )

## Merge module type and : MARKS
d_marks$MODULE_TITLE <- trimws(d_marks$MODULE_TITLE) # trim white spaces first, otherwise merge won't work completely
d_marks_coded <- merge(d_marks, d_modules_list, by = c('MODULE', 'MODULE_TITLE'), all.x=T)

  count(d_marks_coded, MODULE_TYPE) 
  #View(d_marks_coded %>% filter(is.na(MODULE_TYPE))) # If there are NAs, then manually inspect them

# If NAs present, means the module list MANUAL CODE is not up to date; set as Unknown for now
d_marks_coded$MODULE_TYPE <- ifelse(is.na(d_marks_coded$MODULE_TYPE), "Unclear/unkown" ,d_marks_coded$MODULE_TYPE) 

## Merge module type and : LECTURE ATTENDANCE
d_lectures$MODULE_TITLE <- trimws(d_lectures$MODULE_TITLE)
d_lectures_coded <- merge(d_lectures , d_modules_list, by = c('MODULE', 'MODULE_TITLE'), all.x=T)
  count(d_lectures_coded, MODULE_TYPE) 
  #View(d_lectures_coded %>% filter(is.na(MODULE_TYPE)))
d_lectures_coded$MODULE_TYPE <- ifelse(is.na(d_lectures_coded$MODULE_TYPE), "Unclear/unkown" ,d_lectures_coded$MODULE_TYPE) 

## Merge module type and : SEMINAR ATTENDANCE
d_seminars$MODULE_TITLE <- trimws(d_seminars$MODULE_TITLE)
d_seminars_coded <- merge(d_seminars , d_modules_list, by = c('MODULE', 'MODULE_TITLE'), all.x=T)
  count(d_seminars_coded, MODULE_TYPE) 
d_seminars_coded$MODULE_TYPE <- ifelse(is.na(d_seminars_coded$MODULE_TYPE), "Unclear/unkown" ,d_seminars_coded$MODULE_TYPE) 

# ------------------------------------------------------------------------------------------------------------
# CLEAN MARKS DATA FOR REWRITES
# > from: d_marks_coded

# Re-writes are shown by Occasion 9 or 10
# But captured with an additional row; so students who rewrote will have two rows for that particular course
# We need to include only the rewrite marks
# e.g. student ID = 45257625

  d_marks_coded %>% count(OCCASION)

# Create indicator for whether a course has a rewrite, then drop Occasion 1/2 and keep the rewrite only (Occasion 9/10)
d_marks_coded <- d_marks_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR, MODULE) %>%
  mutate(
    occasion_max = max(OCCASION) # Get occasion maximum number within student>year>module
  ) %>% ungroup() %>%
  filter(
    OCCASION == occasion_max # Keep only rewrites (in which occasion matches maximum occasion within module) or modules when no rewrite (occasion is 1 or 2)
  )
# Note: when a student has a row with rewrites under both 9 and 10 (very few), this keeps the occasion 10 rather than occasion 9

  d_marks_coded %>% count(OCCASION)

# ------------------------------------------------------------------------------------------------------------
## Side note: getting average number of modules per year

# Have to only include those in streams in question; but not contained in modules dataset, so need to merge in
d_acronym <- d_students %>% select(ID_anonym, ACRONYM) %>% distinct()

# If data collection happens part way through the year, then final marks will be artificially lower (they are missing mark inputs) and so must be excluded; rough check by comparing average marks across the years
check_marksyear <- d_marks_coded %>%
  group_by(SCHOOLYEAR) %>%
  summarise(mean_marks = mean(FINAL_MARK))

d_module_ave <- d_marks_coded %>% select(ID_anonym, SCHOOLYEAR, MODULE) %>%
  filter(SCHOOLYEAR != "2022-2023") %>% distinct() %>%  # Change year here for partially recorded marks
  left_join(d_acronym) %>%
  filter(ACRONYM %in% d_all$ACRONYM) %>% select(-ACRONYM)

d_module_ave %>% 
  group_by(ID_anonym, SCHOOLYEAR) %>% summarise(module_sum = n()) %>% ungroup() %>% # get average per individual per school year
  summarise(mean = mean(module_sum))
# 9 (8.47) modules on average

# ------------------------------------------------------------------------------------------------------------
# AVERAGE MARKS

# ... overall marks by year
d_marks_ave <- d_marks_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR) %>% 
  summarise(
    OVERALL_MARKS = mean(FINAL_MARK)    # Calculate each student's average marks for each of their schoolyear's
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, OVERALL_MARKS)
# Note: current school year only has minimal / has no marks recorded; drop later when merging.

# ... overall marks by year and module type
d_marks_ave_module <- d_marks_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR, MODULE_TYPE) %>% 
  summarise(
    OVERALL_MARKS_MODULE = mean(FINAL_MARK)    # Calculate each student's average marks for each of their schoolyear's, by module
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, MODULE_TYPE, OVERALL_MARKS_MODULE)

# Note: this means that a total average mark for each student will be obtained by averaging across their UG years, not across all modules

# ------------------------------------------------------------------------------------------------------------
# LECTURE ATTENDANCE
# > from d_lectures

# Only certain lectures are checked for attendance; this may vary between FY and DE and between students in general
# Attendance average therefore calculated by taking the average 'Yes' responses out of the total possible responses for each individual (by year and module type)
# Note: ultimately, overall average, will be average or each year's average, rather than total average. This seems fine.

d_lectures_coded <- d_lectures_coded %>%
  mutate(lecattend_ind = ifelse(d_lectures_coded$ATTENDANCE == "Y", 1 , 0))

# Average lecture attendance 
# ... by year
d_lecattend_ave <- d_lectures_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR) %>% 
  summarise(
    LECATTEND_AVE = mean(lecattend_ind)    # Calculate average lecture attendance for each schoolyear
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, LECATTEND_AVE)

# ... by year and module type
d_lecattend_ave_module <- d_lectures_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR, MODULE_TYPE) %>% 
  summarise(
    LECATTEND_AVE_MODULE = mean(lecattend_ind)    # Calculate average lecture attendance for each schoolyear & module type
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, MODULE_TYPE, LECATTEND_AVE_MODULE)

  # Note: very sporadic and not generally well spaced attendance checks between years 
  d_lectures_coded %>% count(MODULE_TYPE)
  # > weighted towards social studies (especially for FY)
  d_lectures_coded %>% count(SCHOOLYEAR)
  # > weighted towards 2017 & 2018 years??

# Note:  ACADEMIC_YEAR and SCHOOLYEAR are identical > check: View(d_lectures_coded %>% filter(SCHOOLYEAR != ACADEMIC_YEAR))

# ------------------------------------------------------------------------------------------------------------
# SEMINAR ATTENDANCE
# > from d_seminars

d_seminars_coded <- d_seminars_coded %>%
  mutate(semattend_ind = ifelse(d_seminars_coded$ATTENDANCE == "Y", 1 , 0)
  )

# Average seminar attendance 
# ... by year
d_semattend_ave <- d_seminars_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR) %>% 
  summarise(
    SEMATTEND_AVE = mean(semattend_ind)    # Calculate average lecture attendance for each schoolyear
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, SEMATTEND_AVE)

# ... by year and module type
d_semattend_ave_module <- d_seminars_coded %>% 
  group_by(ID_anonym, SCHOOLYEAR, MODULE_TYPE) %>% 
  summarise(
    SEMATTEND_AVE_MODULE = mean(semattend_ind)    # Calculate average lecture attendance for each schoolyear & module type
  ) %>%
  ungroup() %>%
  select(ID_anonym, SCHOOLYEAR, MODULE_TYPE, SEMATTEND_AVE_MODULE)

# ------------------------------------------------------------------------------------------------------------
# SUPERVISOR MEETINGS
# > from d_supervisor

# Combine various status types
d_supervisor %>% count(MEETING_STATUS)
d_supervisor <- d_supervisor %>%
  mutate(
    meeting_outcome = ifelse(
      MEETING_STATUS %in% c("accepted", "complete"), "complete", "incomplete"
    )
  )

# Calculate measure
# Note, using sums of meetings is dangerous: students might have different totals for many reasons that make comparison impossible e.g. miss a year
# Suggested measure: calculate a incomplete / complete ratio

d_supervisor_ave <- d_supervisor %>%
  group_by(ID_anonym, meeting_outcome) %>% 
  summarise(
    meeting_sum = sum(MEETING_COUNT)
  ) 

d_supervisor_ave_spread <-d_supervisor_ave %>% 
  spread(meeting_outcome, meeting_sum)
d_supervisor_ave_spread[is.na(d_supervisor_ave_spread)] <- 0

d_supervisor_ave_spread <-d_supervisor_ave_spread %>% 
  mutate(
    incomplete_complete_ratio = incomplete / complete
  )

# ============================================================================================================
# OUTCOMES FOR FY STUDENTS ----> FY YEAR ONLY
# ============================================================================================================


# Select only FY year (as base to merge)
d_FY <- d_all %>% filter(
  ACRONYM == 'FY',
#  STAGE_DESCRIPTION != "Left",    # Removes those who Left (x17), including: Forced withdrawn (x6), Voluntarily withdrawn (x3), No show (x8); still includes UG Resit WO Res(x1)
#  FY_startyear != "2020-2021",   # Remove current year (x21); given no outcomes data
  ) 

  d_FY %>% count(STARTYEAR)
  length(d_FY$ID_anonym)
  # Total: 173 students
  d_FY %>% count(STATUS_DESCRIPTION)
  #View(d_FY %>% filter(STATUS_DESCRIPTION == "Left"))

d_FY <- d_FY %>% filter(STATUS_DESCRIPTION != "No Show") # These are No Shows for the FY year; they generally have no data or meaningful outcomes so must be dropped 
  length(d_FY$ID_anonym)
  d_FY %>% count(STATUS_DESCRIPTION)


# ------------------------------------------------------------------------------------------------------------
# MARKS

## Marks: merge FY STARTYEAR (d_students) with marks from that SCHOOLYEAR (d_marks)
d_FY_marks <- left_join(d_FY, d_marks_ave, by = c("ID_anonym" = "ID_anonym")) %>%
              filter(FY_startyear == SCHOOLYEAR)   # Only select the year FY startyear is same as schoolyear
# Note: 1x UG resit, so has two marks for FY year, ID 45257625; 2nd year (rewrite was for Statistics) dropped in the filtering 

# Merge module marks by FY year and module type
d_FY_marks <- left_join(d_FY_marks, d_marks_ave_module, by = c("ID_anonym","SCHOOLYEAR"))              

  length(unique(d_FY_marks$ID_anonym))
  # 163 students (note the 10 no shows automatically dropped as they have no data on schoolyear or marks)
# ------------------------------------------------------------------------------------------------------------
# LECTURES

d_FY_lecattend <- left_join(d_FY, d_lecattend_ave, by = c("ID_anonym" = "ID_anonym")) %>%
  filter(FY_startyear == SCHOOLYEAR)

d_FY_lecattend <- left_join(d_FY_lecattend, d_lecattend_ave_module, by = c("ID_anonym","SCHOOLYEAR"))              

length(unique(d_FY_lecattend$ID_anonym))

# Check which students are in Marks dataset but not in lecture attendance dataset
# View(d_FY_marks %>% filter(!(d_FY_marks$ID_anonym %in% d_FY_lecattend$ID_anonym)))
# View(d_lectures_coded %>% filter(ID_anonym %in% c("45675372", "45672315")))
# > can see these two are missing lecture attendance data for 2015 year (i.e. their FY year) but have data for other years

# ------------------------------------------------------------------------------------------------------------
# SEMINARS

d_FY_semattend <- left_join(d_FY, d_semattend_ave, by = c("ID_anonym" = "ID_anonym")) %>%
  filter(FY_startyear == SCHOOLYEAR)

d_FY_semattend <- left_join(d_FY_semattend, d_semattend_ave_module, by = c("ID_anonym","SCHOOLYEAR"))              
# Issue: all seminar attendance in FY year seems to be for social sciences modules - same as using overall seminar attendance

length(unique(d_FY_semattend$ID_anonym))
# Much fewer students here than in Marks or Lectures, see who is missing:
# View(d_FY_marks %>% filter(!(d_FY_marks$ID_anonym %in% d_FY_semattend$ID_anonym)))
# View(d_seminars_coded %>% filter(ID_anonym %in% c("45473698", "45481160", "45475067", "45469867")))
# > as an example, use 4 from 2017-2018 FY startyear; see they are missing any data in the raw Seminars attendance dataset for this year (their FY year)

## FINAL NOTE: Both Lecture and Seminar attendance are missing for some students' FY year; next section will check if missing undergrad. 
# Can't change this, simply missing in the core dataset

# ------------------------------------------------------------------------------------------------------------
# FINAL MERGE

d_FY_full <- full_join(d_FY_marks, d_FY_lecattend)
d_FY_full <- full_join(d_FY_full, d_FY_semattend)

  length(unique(d_FY_full$ID_anonym)) # check: 163 students
  unique(d_FY_full$ACRONYM) # check: only FY year
    
  d_FY_full %>% group_by(STAGE_DESCRIPTION) %>% distinct(ID_anonym) %>% tally() %>% ungroup()
  d_FY_full %>% group_by(STATUS_DESCRIPTION) %>% distinct(ID_anonym) %>% tally() %>% ungroup()
  # Still 4 no-shows, presumably because while they didn't have marks data they did have some other seminars / lecture data
    
  # Grouping labels for use in analysis: ID_anonym; SCHOOLYEAR; MODULE_TYPE

# ============================================================================================================
# OUTCOMES FOR UG STUDENTS ----> BOTH FY AND NON-FY, ALL UG YEARS

# ------------------------------------------------------------------------------------------------------------
# Selecting UG students

d_UG <- d_all %>% filter(
  ACRONYM != 'FY',
#  STAGE_DESCRIPTION != "Left",    # Removes those who Left
) 

    # Issue:  note the issue with FY students' startyear...
    d_UG %>% filter(FY_ind == 1) %>% count(STARTYEAR) # > FY students weighted incorrectly - though happens to be correct for more recent cohorts (i.e. UG year 1 year later than FY year)
    d_UG %>% count(FY_ind)
    # Total FY who have some UG marks (not necessarily 3 years): 132

# ------------------------------------------------------------------------------------------------------------
# MARKS
d_UG_marks <- left_join(d_UG, d_marks_ave, by = c("ID_anonym" = "ID_anonym")) %>%
  filter(FY_startyear != SCHOOLYEAR | is.na(FY_startyear))  # For FY students; all years not including their FY year; DE students, missing FY startyear
# Note: there can be multiple records (rows) for each student, i.e. for each year of study

    
    
    
    
# # # # # # # # # # # # # # # # 
  ### This needs to go somewhere where it can impact the FY students too!!!
    
# # CHECK >> student startyear seems to have issues wrt FY students for their FY and UG years
# d_UG_marks_test <- d_UG_marks %>%
#   filter(FY_ind == 1) %>%
#   mutate(
#     startyear_sub = as.numeric(str_sub(STARTYEAR, 0, 4)),
#     schoolyear_sub = as.numeric(str_sub(SCHOOLYEAR, 0, 4)),
#     fy_plusone = as.numeric(str_sub(FY_startyear, 0, 4)) + 1
#   ) %>%
#   group_by(ID_anonym) %>%
#   mutate(
#     schoolyear_min = min(schoolyear_sub),
#     issue = ifelse(fy_plusone != schoolyear_min, 1,0),
#   ) %>%
#   ungroup() %>%
#   select(ID_anonym, ACRONYM, STARTYEAR, startyear_sub, FY_startyear, fy_plusone, schoolyear_min, issue, schoolyear_sub, SCHOOLYEAR, OVERALL_MARKS)
# View(d_UG_marks_test)
# 
# d_UG_marks_test %>% count(STARTYEAR)
# d_UG_marks_test %>% count(FY_startyear)
# # 
# # Issues: 
# #..  Notice that FY year 2016-2017 has same UG startyear as FY startyear
# #..  2 students with startyears later than year after FY year: IDs 45475992 (FY year 2017/18, earliest marks 2019/20), 45369489 (FY year 2018/19, earliest marks 2020/21)
# #.. issue with ID 45465811: 0% for a year (checked in d_marks and just this subject with 0 for this year); drop this year
# # Plus: 3 students have 100% for a year, 2 study abroad, 1 work placement: IDs 45573189, 45566932, 45586482
# ## >> use schoolyear_min as UG startyear, and adapt for the above students
# # # # # # # # # # # # # # # # 

# GENERATING UG_STARTYEAR 
d_UG_marks <- d_UG_marks %>%
  filter(!(ID_anonym == "45465811" & SCHOOLYEAR == "2018-2019")) %>% # Drop the 0% year for the 1 student above
  filter(!(ID_anonym %in% c(45573189, 45566932, 45586482) & SCHOOLYEAR == "2019-2020" )) # drop the year away for 3 students

# GENERATING UG_STARTYEAR 
year_key <- c(`2015` = "2015-2016", `2016` = "2016-2017", `2017` = "2017-2018", `2018` = "2018-2019", `2019` = "2019-2020", `2020` = "2020-2021", `2021` = "2021-2022", `2022` = "2022-2023", `2023` = "2023-2024")

# Use earliest year of recorded marks as UG start year (from above: have already filtered out FY years)
d_UG_marks <- d_UG_marks %>%
  mutate(
    schoolyear_sub = as.numeric(str_sub(SCHOOLYEAR, 0, 4)),
  ) %>% 
  group_by(ID_anonym) %>%
  mutate(
    schoolyear_min = min(schoolyear_sub)
  ) %>% ungroup() %>%
  mutate(
    UG_startyear = recode(as.factor(schoolyear_min), !!!year_key),
    # stream = ifelse(ACRONYM %in% c("MN", "MNI", "MNF", "MNM"), "MN",
    #                 ifelse(ACRONYM %in% c("AF","AFI","AFU", "AFUPP"), "AF",
    #                        ifelse(ACRONYM == "FY", "FY", NA)))
    )

#View(d_UG_marks %>% select(ID_anonym, FY_ind, STARTYEAR, SCHOOLYEAR, schoolyear_min, UG_startyear, stream))
d_UG_marks <- d_UG_marks %>% select(-schoolyear_sub, -schoolyear_min)

# # # # # # # # # # # # # # # # 



# Merge module marks by UG school year and module type
d_UG_marks <- left_join(d_UG_marks, d_marks_ave_module, by = c("ID_anonym","SCHOOLYEAR")) 

# ------------------------------------------------------------------------------------------------------------
# LECTURES
d_UG_lecattend <- left_join(d_UG, d_lecattend_ave, by = c("ID_anonym" = "ID_anonym")) %>%
  filter(FY_startyear != SCHOOLYEAR | is.na(FY_startyear))

d_UG_lecattend <- left_join(d_UG_lecattend, d_lecattend_ave_module, by = c("ID_anonym","SCHOOLYEAR"))              

length(unique((d_UG_lecattend %>% filter(FY_ind == 1))$ID_anonym)) # check: 96
length(unique((d_UG_lecattend %>% filter(FY_ind == 0))$ID_anonym)) 

# ------------------------------------------------------------------------------------------------------------
# SEMINARS
d_UG_semattend <- left_join(d_UG, d_semattend_ave, by = c("ID_anonym" = "ID_anonym")) %>%
  filter(FY_startyear != SCHOOLYEAR | is.na(FY_startyear))

d_UG_semattend <- left_join(d_UG_semattend, d_semattend_ave_module, by = c("ID_anonym","SCHOOLYEAR"))              

length(unique((d_UG_semattend %>% filter(FY_ind == 1))$ID_anonym)) # check: 113
length(unique((d_UG_semattend %>% filter(FY_ind == 0))$ID_anonym))

# ------------------------------------------------------------------------------------------------------------
# FINAL MERGE

d_UG_full <- full_join(d_UG_marks, d_UG_lecattend)
d_UG_full <- full_join(d_UG_full, d_UG_semattend)

length(unique(d_UG_full$ID_anonym))
unique(d_UG_full$ACRONYM) # check: no FY years

d_UG_full %>% group_by(STAGE_DESCRIPTION) %>% distinct(ID_anonym) %>% tally() %>% ungroup()
d_UG_full %>% group_by(STATUS_DESCRIPTION) %>% distinct(ID_anonym) %>% tally() %>% ungroup()


# ============================================================================================================
# Check the differences in modules across these datasets; not really aligned

# students_check <- d_students %>% filter(ID_anonym == 45464643)
# marks_check <- d_marks %>% filter(ID_anonym == 45464643) # see the issue with rewriting here - doubling of courses
# lectures_check <- d_lectures %>% filter(ID_anonym == 45464643)
# seminars_check <- d_seminars %>% filter(ID_anonym == 45464643)



#############################################
# Merge: A-Level Grades
#############################################
# FY_students_marks <- merge(FY_students_marks, dataset_FY_A_levels[,c('ID_anonym','GradesReceivedSubject','AlphabetisedGrades')], by="ID_anonym", all.x = TRUE)
# nonFY_students_marks <- merge(nonFY_students_marks, dataset_FY_A_levels[,c('ID_anonym','GradesReceivedSubject','AlphabetisedGrades')], by="ID_anonym", all.x = TRUE)

# ============================================================================================================
# COMBINE DATASETS
# Combine FY year dataset with UG dataset (latter includes both FY and DE students in their UG years)

# # Check that variables are the same between datasets
# comparedf(d_FY_full, d_UG_full)
# # d_FY_full[, !names(d_FY_full) %in% names(d_UG_full)]
# # d_UG_full[, !names(d_UG_full) %in% names(d_FY_full)]

d_full <- bind_rows(d_FY_full, d_UG_full)

# Fill in marks for students across their records
d_full <- d_full %>%
  group_by(ID_anonym, SCHOOLYEAR) %>%
  fill( c(OVERALL_MARKS, LECATTEND_AVE, SEMATTEND_AVE), .direction = c("downup")) %>% 
  ungroup() %>% 
  group_by(ID_anonym) %>%
  fill(c(UG_startyear, stream), .direction = c("downup")) %>% 
  ungroup()

length(unique(d_full$ID_anonym)) 
unique(d_full$ACRONYM)

# ------------------------------------------------------------------------------------------------------------
## DEMOGRAPHICS from Johnstone dataset  - which includes demographics for both FY and DE students
# Note: WP Indicators in a separate code file, given issues

d_johnstone_select <- d_johnstone %>% select(
  ID_anonym, 
  Gender, Disability, Ethnicity, Intake_Year,
  Entry_Tariff, New_Tariff, Award_Class, Award_Year, 
  # add in IMD Quntile here
  )

d_full_merge <- left_join(d_full, d_johnstone_select, by = "ID_anonym")

  unique(d_full_merge$Ethnicity)
  print(d_full_merge %>% count(Ethnicity), n = 30)
  # Lots of NAs
  
d_full_merge <- d_full_merge %>%
  mutate(
    Ethnicity_collapse =
      ifelse(Ethnicity %in% c("Arab"), "Arab",
             ifelse(Ethnicity %in% c("Asian - Bangladeshi or Bangladeshi British", "Asian - Indian or Indian British", "Asian - Pakistani or Pakistani British", "Any other Asian Background"), "Asian",
                    ifelse(Ethnicity %in% c("Black  - African or African British", "Black - Caribbean or Caribbean British", "Any other Black Background"), "Black",
                           ifelse(Ethnicity %in% c("Asian - Chinese or Chinese British"), "Chinese",
                                  ifelse(Ethnicity %in% c("Mixed or multiple ethnic groups - White or White British and Asian or Asian British","Mixed or multiple ethnic groups - White or White British and Black African or Black African British","Mixed or multiple ethnic groups - White or White British and Black Caribbean or Black Caribbean British", "Any other Mixed or Multiple ethnic background"), "Mixed",
                                         ifelse(Ethnicity %in% c("White", "White - English, Scottish, Welsh, Northern Irish or British", "White - Irish", "White - Gypsy or Irish Traveller", "Any other White Background"), "White", 
                                                Ethnicity
                                         )))))))
  # Left as is: Any other ethnic background; Prefer not to say / Information refused

d_full_merge %>% count(Ethnicity_collapse)

# ------------------------------------------------------------------------------------------------------------
# Write final data file
write_csv(d_full_merge,'output/data_full_merge.csv')
# ============================================================================================================

