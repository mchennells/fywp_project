# FY PROJECT
# :: DESCRIPTIVE STATISTICS
# Descriptive statistics of FY sample and Direct Entry (DE) sample

setwd("~/Google Drive/Warwick/PhD/Work/WBS/TK/FYWP Data Project/fywp_project")

#sessionInfo()
library(tidyverse)
library(data.table)
library(sjstats)
library(pwr)
library(openxlsx)
library(readxl)

# ============================================================================================================
# SETUP

rm(list=ls())

path_data <- file.path("output", "data_full_merge.csv")
raw_data <- read.csv(file = path_data ,header = T, stringsAsFactors = F)
# Note: data has multiple rows per student corresponding to FY / UG period, Schoolyear, and Module type, respectively.

# Read in WP criteria and scholarship data
path_wp_FY <- file.path("output", "d_wp_FY_final_220822.csv")
path_wp_FY_plusJ <- file.path("output", "d_wp_FY_final_plusJ_220822.csv")
path_wp_DE <- file.path("output", "d_wp_DE_final_220822.csv")

raw_data_wp_FY <- read_csv(file = path_wp_FY)
raw_data_wp_FY_plusJ <- read_csv(file = path_wp_FY_plusJ)
raw_data_wp_DE <- read_csv(file = path_wp_DE)

path_scholandcontex <- file.path("output", "d_schol_context.csv")
raw_data_scholandcontex <- read_csv(path_scholandcontex)

path_johnstone <- file.path("data_files", "CHECK SITS_TK_Extract for Tina Kiefer 2022_08_10_002_Aug22_ID_anonym.xlsx")
d_johnstone <- read_excel(path_johnstone)

# ============================================================================================================
# Data selection

d_ <- raw_data %>%
  select(
    ID_anonym, ACRONYM, stream, FY_ind, FY_startyear, UG_startyear, timestamp,
    STAGE_DESCRIPTION, STATUS_DESCRIPTION, FEESTATUS, FEESTATUS_DESCRIPTION,
    New_Tariff, Award_Class, Award_Year, Intake_Year,
    Gender, Ethnicity, Ethnicity_collapse, Disability
  )

# Note number of FY students who have left
d_left <- d_ %>% 
  filter(FY_ind == 1, STAGE_DESCRIPTION == "Left", STATUS_DESCRIPTION != "No Show") %>% 
  distinct()

d_left %>% count(timestamp, STATUS_DESCRIPTION)
# 12 left during FY year (8 FW, 4 VW) and 9 during UG (6 FW, 3 VW)
length(d_left$ID_anonym)
mean(d_left$New_Tariff)

# ============================================================================================================
# FY WP Data

# Isolate only FY students
d_FY <- d_ %>% 
  filter(
    FY_ind == 1,      # Select only FY students
    ACRONYM == "FY"   # Keep records only from FY year
  ) %>% 
  distinct()   # Remove duplicate records (duplicates given schooling years; module types)

d_FY %>% count(STATUS_DESCRIPTION)
# Note: no shows in FY year we dropped in cleaning, but we still have those who completed FY year but were no show for UG (x3) -- note they are not recorded as no shows, they just don't appear again in UG)

# # Drop No-Shows for FY year (those who didn't show up to FY year; 
# d_FY <- d_FY %>% filter(STATUS_DESCRIPTION != "No Show")

# Merge WP criteria
d_FY <- left_join(d_FY, raw_data_wp_FY_plusJ, by = c("ID_anonym", "FY_startyear"))

d_FY_nocohortone <- d_FY %>% filter(!(FY_startyear %in% c("2015-2016")))
d_FY_nocohortone %>% count(wp_sum_with_awards)
d_FY_nocohortone %>% count(wp_index)

# Export FY ethnicity data
d_FY_eth_collapse <- d_FY %>% count(Ethnicity_collapse)
write.xlsx(d_FY_eth_collapse,'output/descriptive/d_FY_eth_collapse.xlsx',rowNames = F)

# ----------------------------------------------------------------------------------------
# REMOVE THOSE WHO LEFT DURING FY YEAR; i.e. only use descriptive stats of those who completed FY year    

# d_left <- d_left %>% 
#   filter(left_ind == 1) %>%
#   select(ID_anonym, ACRONYM, FY_ind, FY_startyear, UG_startyear, CURRENTSTAGE, STAGE_DESCRIPTION, CURRENTSTATUS, STATUS_DESCRIPTION) %>% 
#   filter(FY_ind == 1)

length(d_FY$ID_anonym) #163
length(unique(d_FY$ID_anonym)) #163

d_FY %>%count(FY_startyear)  
d_FY %>% count(STAGE_DESCRIPTION)
d_FY %>% count(STAGE_DESCRIPTION, STATUS_DESCRIPTION)

d_FY <- d_FY %>% filter(STAGE_DESCRIPTION != "Left" ) # Removes 12 students who left (8x forced withdrawn; 4x voluntarily withdrawn)
length(unique(d_FY$ID_anonym)) # Check: 151 students who completed FY year, have fully submitted, or are resitting

# ### code about previous issue with duplicates, no longer a problem
#     d_FY %>%count(FY_startyear)  
#     length(d_FY$ID_anonym)  # Check: 139 students who completed FY year
#     length(unique(d_FY$ID_anonym))  
#     d_FY <- d_FY %>% mutate(idcheck = duplicated(ID_anonym))
#     #View(d_FY %>% filter(idcheck == TRUE))
#     d_FY <- d_FY %>% filter(idcheck != TRUE) %>% select(-idcheck)
# ### 

# ----------------------------------------------------------------------------------------
# FY STUDENTS DEMOGRAPHICS 

# Gender
unique(d_FY$Gender)
d_FY %>% count(Gender)
d_FY_gender <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_females = sum(Gender=="F"), tot_females_prop = round(mean(Gender=="F"), 2),
    tot_males = sum(Gender=="M"), tot_males_prop = round(mean(Gender=="M"), 2),
  ) %>%
  ungroup()
d_FY_gender
write.xlsx(d_FY_gender,'output/descriptive/d_FY_gender.xlsx',rowNames = F)

# Ethnicity
unique(d_FY$Ethnicity)
d_FY %>% count(Ethnicity)
d_FY_ethnicity <- d_FY %>%
  group_by(FY_startyear, Ethnicity) %>%
  summarize(eth_sum = n()) %>%
  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity, eth_sum, fill = 0) %>%
  ungroup()
d_FY_ethnicity
write.xlsx(d_FY_ethnicity,'output/descriptive/d_FY_ethnicity.xlsx',rowNames = F)

unique(d_FY$Ethnicity_collapse)
d_FY %>% count(Ethnicity_collapse)
d_FY_eth_collapse_yr <- d_FY %>%
  group_by(FY_startyear, Ethnicity_collapse) %>%
  summarize(eth_sum = n()) %>%
  mutate(Ethnicity_collapse = paste0(Ethnicity_collapse, "_sum")) %>%
  spread(Ethnicity_collapse, eth_sum, fill = 0) %>%
  ungroup()
d_FY_eth_collapse_yr
write.xlsx(d_FY_eth_collapse_yr,'output/descriptive/d_FY_ethnicity_collapse.xlsx',rowNames = F)

# Gender by Ethnicity
d_FY_gender_ethnicity <- d_FY %>%
  group_by(Gender, Ethnicity_collapse) %>%
  summarize(eth_sum = n()) %>%
  mutate(Ethnicity_collapse = paste0(Ethnicity_collapse, "_sum")) %>%
  spread(Ethnicity_collapse, eth_sum, fill = 0) %>%
  ungroup()
d_FY_gender_ethnicity
write.xlsx(d_FY_gender_ethnicity,'output/descriptive/d_FY_gender_ethnicity.xlsx',rowNames = F)

# ----------------------------------------------------------------------------------------
# WIDENING PARTICIPATION FLAGS - FOR FY STUDENTS
# WP INDEX: number / 5 flags for each student + awards form
# WP 1 flag: student is state In-Care program (=1 if in care program) 
# WP 2 flag: school GSCE performance (= 1 if in bottom)
# WP 3 flag: school participation in Free School Meals (FSM program) (=1 if in program)
# WP 4 flag: Lower Participation Neighbourhood (=1 if in LPN)
# WP 5 flag: student is 1st generation in higher education (=1 if first generation to be in higher education)

# ----------------------------------------------------------------------------------------
## WP 1: In-Care program
d_FY_incare <- d_FY %>% count(WP1_InCare_n)
# missing_incare <- d_FY %>% filter(is.na(InCare_Flag))
write.xlsx(d_FY_incare,'output/descriptive/d_FY_incare.xlsx',rowNames = F) 

## WP 2: School GCSE Flag
d_FY_GCSE <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_GCSE_yes = sum(WP2_GCSE_both_n==1, na.rm = TRUE), tot_GCSE_yes_prop = round(mean(WP2_GCSE_both_n==1, na.rm = TRUE), 2),
    tot_GCSE_no = sum(WP2_GCSE_both_n==0, na.rm = TRUE), tot_GCSE_no_prop = round(mean(WP2_GCSE_both_n==0, na.rm = TRUE), 2),
    tot_GCSE_NA = sum(is.na(WP2_GCSE_both_n)), tot_GCSE_NA_prop = round(mean(is.na(WP2_GCSE_both_n)), 2),
  ) %>%
  ungroup()
# missing_GCSE <- d_FY %>% filter(is.na(WP2_GCSE_both_n))
d_FY_GCSE
write.xlsx(d_FY_GCSE,'output/descriptive/d_FY_GCSE.xlsx',rowNames = F) 

## WP 3: School Free School Meals flag
d_FY_FSM <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_FSM_yes = sum(WP3_FSM_n==1, na.rm = TRUE), tot_FSM_yes_prop = round(mean(WP3_FSM_n==1, na.rm = TRUE), 2),
    tot_FSM_no = sum(WP3_FSM_n==0, na.rm = TRUE), tot_FSM_no_prop = round(mean(WP3_FSM_n==0, na.rm = TRUE), 2),
    tot_FSM_NA = sum(is.na(WP3_FSM_n)), tot_FSM_NA_prop = round(mean(is.na(WP3_FSM_n)), 2),
  ) %>%
  ungroup()
# missing_FSM <- d_FY %>% filter(is.na(WP3_FSM_n))
d_FY_FSM
write.xlsx(d_FY_FSM,'output/descriptive/d_FY_FSM.xlsx',rowNames = F) 

## WP 4: LPN FLag
unique(d_FY$WP4_LPN_n)
d_FY_LPN_flag <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_LPN_yes = sum(WP4_LPN_n==1, na.rm = TRUE), tot_LPN_yes_prop = round(mean(WP4_LPN_n==1, na.rm = TRUE), 2),
    tot_LPN_no = sum(WP4_LPN_n==0, na.rm = TRUE), tot_LPN_no_prop = round(mean(WP4_LPN_n==0, na.rm = TRUE), 2),
    tot_LPN_NA = sum(is.na(WP4_LPN_n)), tot_LPN_NA_prop = round(mean(is.na(WP4_LPN_n),na.rm = TRUE), 2),
  ) %>%
  ungroup()
# missing_LPN_flag <- d_FY %>% filter(is.na(WP4_LPN_n))
d_FY_LPN_flag
write.xlsx(d_FY_LPN_flag,'output/descriptive/d_FY_LPN_flag.xlsx',rowNames = F) 

## WP 5: student 1st generation in higher education flag
d_FY_parentHE <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_parentHE_yes = sum(WP5_1stGen_n==1, na.rm = TRUE),
    tot_parentHE_no = sum(WP5_1stGen_n == 0, na.rm = TRUE), 
    tot_parentHE_NA = sum(is.na(WP5_1stGen_n)),
  ) %>%
  ungroup()
d_FY_parentHE
write.xlsx(d_FY_parentHE,'output/descriptive/d_FY_parentHE.xlsx',rowNames = F) 

# IMD Quintile
unique(d_FY$IMD_Quintile)
d_FY_IMD <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_IMD_1 = sum(IMD_Quintile==1, na.rm = TRUE), tot_IMD_1_prop = round(mean(IMD_Quintile==1, na.rm = TRUE), 2),
    tot_IMD_2 = sum(IMD_Quintile==2, na.rm = TRUE), tot_IMD_2_prop = round(mean(IMD_Quintile==2, na.rm = TRUE), 2),
    tot_IMD_3 = sum(IMD_Quintile==3, na.rm = TRUE), tot_IMD_3_prop = round(mean(IMD_Quintile==3, na.rm = TRUE), 2),
    tot_IMD_4 = sum(IMD_Quintile==4, na.rm = TRUE), tot_IMD_4_prop = round(mean(IMD_Quintile==4, na.rm = TRUE), 2),
    tot_IMD_5 = sum(IMD_Quintile==5, na.rm = TRUE), tot_IMD_5_prop = round(mean(IMD_Quintile==5, na.rm = TRUE), 2),
    tot_IMD_NA = sum(is.na(IMD_Quintile), na.rm = TRUE), tot_IMD_NA_prop = round(mean(is.na(IMD_Quintile), na.rm = TRUE), 2)
  ) %>%
  ungroup()
# missing_IMD <- d_FY %>% filter(is.na(IMD_Quintile))
d_FY_IMD
write.xlsx(d_FY_IMD,'output/descriptive/d_FY_IMD.xlsx',rowNames = F) 

# "The Index of Multiple Deprivation score is an overall measure of deprivation experienced by people living in a certain area, and considers seven dimensions: income deprivation; employment deprivation; health deprivation and disability; education; skills and training deprivation; barriers to housing and services; and living environment deprivation and crime."
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015


# ----------------------------------------------------------------------------------------
# OLDER STATISTICS (NOT USED IN REPORT)

# LPN Quintile
unique(d_FY$LPN_Quintile)
d_FY_LPN <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_LPN_1 = sum(LPN_Quintile==1, na.rm = TRUE), tot_LPN_1_prop = round(mean(LPN_Quintile==1, na.rm = TRUE), 2),
    tot_LPN_2 = sum(LPN_Quintile==2, na.rm = TRUE), tot_LPN_2_prop = round(mean(LPN_Quintile==2, na.rm = TRUE), 2),
    tot_LPN_3 = sum(LPN_Quintile==3, na.rm = TRUE), tot_LPN_3_prop = round(mean(LPN_Quintile==3, na.rm = TRUE), 2),
    tot_LPN_4 = sum(LPN_Quintile==4, na.rm = TRUE), tot_LPN_4_prop = round(mean(LPN_Quintile==4, na.rm = TRUE), 2),
    tot_LPN_5 = sum(LPN_Quintile==5, na.rm = TRUE), tot_LPN_5_prop = round(mean(LPN_Quintile==5, na.rm = TRUE), 2),
    tot_LPN_NA = sum(is.na(LPN_Quintile), na.rm = TRUE), tot_LPN_NA_prop = round(mean(is.na(LPN_Quintile), na.rm = TRUE), 2)
  ) %>%
  ungroup()
# missing_LPN <- d_FY %>% filter(is.na(LPN_Quintile))
write.xlsx(d_FY_LPN,'output/descriptive/d_FY_LPN.xlsx',rowNames = F) 

# POL3 Quintile
unique(d_FY$Polar3_Young_HE_Participation_Quintile)
d_FY_POL3 <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_POL3_1 = sum(Polar3_Young_HE_Participation_Quintile==1, na.rm = TRUE), tot_POL3_1_prop = round(mean(Polar3_Young_HE_Participation_Quintile==1, na.rm = TRUE), 2),
    tot_POL3_2 = sum(Polar3_Young_HE_Participation_Quintile==2, na.rm = TRUE), tot_POL3_2_prop = round(mean(Polar3_Young_HE_Participation_Quintile==2, na.rm = TRUE), 2),
    tot_POL3_3 = sum(Polar3_Young_HE_Participation_Quintile==3, na.rm = TRUE), tot_POL3_3_prop = round(mean(Polar3_Young_HE_Participation_Quintile==3, na.rm = TRUE), 2),
    tot_POL3_4 = sum(Polar3_Young_HE_Participation_Quintile==4, na.rm = TRUE), tot_POL3_4_prop = round(mean(Polar3_Young_HE_Participation_Quintile==4, na.rm = TRUE), 2),
    tot_POL3_5 = sum(Polar3_Young_HE_Participation_Quintile==5, na.rm = TRUE), tot_POL3_5_prop = round(mean(Polar3_Young_HE_Participation_Quintile==5, na.rm = TRUE), 2),
    tot_POL3_NA = sum(is.na(Polar3_Young_HE_Participation_Quintile), na.rm = TRUE), tot_POL3_NA_prop = round(mean(is.na(Polar3_Young_HE_Participation_Quintile), na.rm = TRUE), 2)
  ) %>%
  ungroup()
# missing_POL3 <- d_FY %>% filter(is.na(Polar3_Young_HE_Participation_Quintile))
write.xlsx(d_FY_POL3,'output/descriptive/d_FY_POL3.xlsx',rowNames = F) 

# Low Socio-Economic Status Flag
unique(d_FY$LowSEC_Flag)
d_FY_SEC <- d_FY %>%
  group_by(FY_startyear) %>%
  summarize(
    tot_students = n(),
    tot_lowSEC_yes = sum(LowSEC_Flag=="Y", na.rm = TRUE), tot_lowSEC_yes_prop = round(mean(LowSEC_Flag=="Y"), 2),
    tot_lowSEC_no = sum(LowSEC_Flag=="N", na.rm = TRUE), tot_lowSEC_no_prop = round(mean(LowSEC_Flag=="N"), 2),
    tot_lowSEC_NA = sum(is.na(LowSEC_Flag)), tot_lowSEC_NA_prop = round(mean(is.na(LowSEC_Flag)), 2),
  ) %>%
  ungroup()
# missing_SEC <- d_FY %>% filter(is.na(LowSEC_Flag))
write.xlsx(d_FY_SEC,'output/descriptive/d_FY_SEC.xlsx',rowNames = F) 

# ----------------------------------------------------------------------------------------
# Checking School Type of FY students
d_j_schooltype <- d_johnstone %>% select(ID_anonym, School_Type)
d_FY_schooltype <- d_FY %>% left_join(d_j_schooltype, by = "ID_anonym")
fy_schooltype <- d_FY_schooltype %>% count(School_Type)


# ============================================================================================================
# DIRECT ENTRY (DE) WP flags
# ============================================================================================================

# Johnstone data is not clean, but isn't limited by the course selection
# Previously, for DE WP checks, had already removed those not doing the same courses as FY students

d_ %>% count(STATUS_DESCRIPTION)
d_ %>% count(STAGE_DESCRIPTION)
# Those who have left / no show generally missing data, so exclude like we did for FY students

d_ %>% count(FEESTATUS) # H: Home(UK) fees / HE: EU fees / O: Overseas fees
# Home(UK) fees is the correct comparison group for FY students; others missing much demographic data

d_DE <- d_ %>%
  filter(
    FY_ind == 0,
    STATUS_DESCRIPTION != "No Show",
    STAGE_DESCRIPTION != "Left",
    Intake_Year %in% c("15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22", "22/23")
  ) %>% 
  distinct()

# Keep only Home(UK) students for WP comparison
d_DE_wp <- d_DE %>% filter(FEESTATUS == "H")

# # Only include for WP comparison DE students doing the same courses 
#   d_ %>% filter(FY_ind == 1) %>% count(ACRONYM) 
# d_DE_wp <- d_DE_wp %>% filter(ACRONYM %in% c('FY', 'MN','MNI', 'MNF', 'MNM', 'AF','AFI', 'AFUPP'))


# Size and composition of the DE comparison group for WP
d_DE_wp[duplicated(d_DE_wp$ID_anonym),] # Zero duplicates
d_DE_wp <- d_DE_wp %>% filter(!duplicated(ID_anonym))

length(d_DE_wp$ID_anonym)
d_DE_wp %>% count(ACRONYM)
d_DE_wp %>% count(Intake_Year) 
# ... note large increase in home students in 21/22 intake

# ... only small increase in total number of students / not driven by non-UK increases / not driven by leavers
d_ %>% filter(FY_ind == 0, STATUS_DESCRIPTION != "No Show", STAGE_DESCRIPTION != "Left") %>% distinct() %>% count(Intake_Year)   # total number of students
d_ %>% filter(FY_ind == 0, STATUS_DESCRIPTION != "No Show", STAGE_DESCRIPTION != "Left", FEESTATUS != "H") %>% distinct() %>% count(Intake_Year) # non-UK only students
d_ %>% filter(FY_ind == 0) %>% distinct() %>% count(Intake_Year) # Relatively few leavers / no shows; that's not the driver

d_DE_wp <- left_join(d_DE_wp, raw_data_wp_DE)
# 771 students

# ----------------------------------------------------------------------------------------
# Descriptive tables

# WIDENING PARTICIPATION FLAGS
# WP INDEX: number / 5 flags for each student + awards form
# WP 1 flag: student is state In-Care program (=1 if in care program) 
# WP 2 flag: school GSCE performance (= 1 if in bottom)
# WP 3 flag: school participation in Free School Meals (FSM program) (=1 if in program)
# WP 4 flag: Lower Participation Neighbourhood (=1 if in LPN)
# WP 5 flag: student is 1st generation in higher education (=1 if first generation to be in higher education)

#var(d_DE_wp)

d_DE_incare<- d_DE_wp  %>% count(WP1_InCare_n_J)
d_DE_incare
write.xlsx(d_DE_incare,'output/descriptive/d_DE_incare.xlsx',rowNames = F)

d_DE_ALP_GCSE<- d_DE_wp  %>% count(WP2_GCSE_both_n_J)
d_DE_ALP_GCSE
write.xlsx(d_DE_ALP_GCSE,'output/descriptive/d_DE_ALP_GCSE.xlsx',rowNames = F)

d_DE_FSM<- d_DE_wp  %>% count(WP3_FSM_n_J)
d_DE_FSM
write.xlsx(d_DE_FSM,'output/descriptive/d_DE_FSM.xlsx',rowNames = F)

d_DE_LPN <- d_DE_wp  %>% count(WP4_LPN_n_J)
d_DE_LPN
write.xlsx(d_DE_LPN,'output/descriptive/d_DE_LPN.xlsx',rowNames = F)

d_DE_parentHE<- d_DE_wp  %>% count(WP5_1stGen_n_J)
d_DE_parentHE
write.xlsx(d_DE_parentHE,'output/descriptive/d_DE_parentHE.xlsx',rowNames = F)

d_DE_IMD<- d_DE_wp  %>% count(IMD_Quintile)
d_DE_IMD
write.xlsx(d_DE_IMD,'output/descriptive/d_DE_IMD.xlsx',rowNames = F)

# Additional
d_DE_SEC <- d_DE_wp  %>% count(WP6_SEC_J)
d_DE_SEC
write.xlsx(d_DE_SEC,'output/descriptive/d_DE_SEC.xlsx',rowNames = F)

# ----------------------------------------------------------------------------------------
# Check here for quality of the data over time

# % NA by intake year
# xx

# ============================================================================================================
# COMPARISON OF FY AND DE 
# ============================================================================================================

# Get DE and FY separately, then merge them together; otherwise complicated with double counting FY and UG years and differences between meaning of intake year for FY vs DE

## Check out the data
  d_DE %>% count(Intake_Year)
  d_DE %>% count(UG_startyear)
  d_DE %>% count(STAGE_DESCRIPTION)
  d_DE %>% count(FY_ind)   # Make sure no FY students snuck in
  d_DE %>% count(stream)
  d_DE %>% filter(ID_anonym %in% d_FY$ID_anonym)
  d_DE %>% count(Gender)
  d_DE %>% count(Ethnicity_collapse)
  length(unique(d_DE$ID_anonym))# 4036 comparison group (inlcudes only AF, MN and IB/IM courses). And we haven't limited to Feestutus == Home(UK)
  length(unique((d_DE %>% filter(FEESTATUS == "H") )$ID_anonym)) # 771 Home(UK) students


d_FY_select <- d_FY %>% 
  select(ID_anonym, FEESTATUS, FEESTATUS_DESCRIPTION, FY_ind, STAGE_DESCRIPTION, STATUS_DESCRIPTION, 
         Intake_Year, FY_startyear, UG_startyear, 
         ACRONYM, Gender, Ethnicity_collapse, New_Tariff)

d_DE_select <- d_DE %>% 
  select(ID_anonym, FEESTATUS, FEESTATUS_DESCRIPTION, FY_ind, STAGE_DESCRIPTION, STATUS_DESCRIPTION, 
         Intake_Year, FY_startyear, UG_startyear, 
         ACRONYM, Gender, Ethnicity_collapse, New_Tariff)  

d_combined <- bind_rows(d_FY_select, d_DE_select) %>%
  mutate(FY = ifelse(FY_ind == 1, "FY students","DE students")) %>% select(-FY_ind)

d_eth_summary <- d_combined %>%
  #  filter(FY_ind == 1 | FEESTATUS == "H") %>%   # filter to include only FY students and DE Home(UK) students if required
  select(ID_anonym, Intake_Year, FY, Ethnicity_collapse, ) %>%
  group_by(FY, Ethnicity_collapse) %>%
  summarize(eth_sum = n()) %>%
  #  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity_collapse, eth_sum, fill = 0) %>%
  ungroup()

d_eth_summary_yr <- d_combined %>%
  #  filter(FY_ind == 1 | FEESTATUS == "H") %>%   # filter to include only FY students and DE Home(UK) students if required
  select(ID_anonym, Intake_Year, FY, Ethnicity_collapse, ) %>%
  group_by(FY, Intake_Year, Ethnicity_collapse) %>%
  summarize(eth_sum = n()) %>%
  #  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity_collapse, eth_sum, fill = 0) %>%
  ungroup()

# ----------------------------------------------------------------------------------------
# TARIFF / A-LEVEL RESULTS COMPARISON
# NOTES: 
# ... dataset only includes: streams (AF, MN, IM/IB); and Students who haven't Left (STAGE_DESCRIPTION != "Left")
# ... New_Tariff is predicted tariff, not final results 


## DE_WP category
# Any students who are: 
# : Non-FY
# : With scholarships / bursaries
# : Who are Warwick Scholars
# : Who have contextual offers
# : Meet multiple WP criteria from Johnstone dataset

d_scholcont_select <- raw_data_scholandcontex %>% 
  select(ID_anonym , ind_WBS_Scholarship, ind_Warwick_Scholar, ind_Contextual_Offer)

d_tariff <- d_combined %>% left_join(d_scholcont_select) %>%
  mutate(ind_SCHOLFUL = ifelse( 
    ind_WBS_Scholarship == "Yes" | ind_Warwick_Scholar == "Yes" | ind_Contextual_Offer == "Yes", "Yes", "No"))

  #View(d_combined %>% filter (ind_SCHOLFUL == "Yes"))
  # Have some of the DE Contextual offers been removed by dropping (e.g. Left or not in course?), as there are fewer matches that in the full schol + contextual list
d_tariff %>% count(FY, ind_SCHOLFUL)
    
    testie <- d_scholcont_select %>% left_join(d_tariff) %>% 
      mutate(ind_DUP = duplicated(ID_anonym)) %>%
      select(ID_anonym, FY, starts_with("ind_"), everything())


  d_tariff %>% count(FY)
  d_tariff %>% count(FY, FEESTATUS, FEESTATUS_DESCRIPTION) 

d_tariff <- d_tariff %>%
  filter(FY == "FY students" | FEESTATUS == "H")

d_tariff %>% count(FY)
d_tariff %>% count(FY, FEESTATUS)

# In case we need school info included
d_schooltype <- d_johnstone %>% 
  select(
    ID_anonym,
    School_Type, School_Code,
    Country_Of_Domicile
  ) %>% distinct()

d_tariff <- d_tariff %>% left_join(d_schooltype)

d_tariff %>% 
  select(ID_anonym, FY, FY_startyear, UG_startyear, New_Tariff) %>%
  filter(FY == "FY students", is.na(New_Tariff))
# Missing New_Tariff for three FY students: 45566060, 45258404, 45261885

d_tariff <- d_tariff %>% 
  filter(!(ID_anonym %in% c("45674377","45681167", "45580686"))) %>%   # Remove 2 FY BTEC students (ultra high tariff), 1 very low DE student
  filter(!(UG_startyear %in% c("2014"))) %>% # remove several missing DE students
  filter(!(New_Tariff %in% c(0, NA)))   # Remove the missing FY (x3) and DE students (x48)

d_tariff %>% count(FY)
# 146 FY students (151 minus 2x BTEC % 3x missing); 720 Home(UK) students in streams (AF, MN, IM/IB)

# Plots
plot_tariffs <-  ggplot(
  d_tariff, aes(x=FY, y=New_Tariff, fill = FY)) +
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  ggtitle("Average entry tariff by student entry type") +
  coord_cartesian(ylim=c(0,200)) +
  scale_y_continuous(name = "Average entry tariff", breaks = seq(0,200,50)) +
  scale_x_discrete(name = "Undergraduate student entry type") +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_tariffs

plot_tariffs_cohort <-  ggplot(
  d_tariff %>% filter(FY == "FY students"), aes(x=FY_startyear, y=New_Tariff)) +
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  ggtitle("Average entry tariff by FY cohort year") +
  coord_cartesian(ylim=c(0,220)) +
  scale_y_continuous(name = "Average entry tariff", breaks = seq(0,250,50)) +
  scale_x_discrete(name = "FY cohort year") +
  theme_minimal(12) +
  theme(legend.position = "none")
#  scale_x_discrete(labels=c("Cohort 1","Cohort 2","Cohort 3"))
plot_tariffs_cohort

plot_tariffs_cohort_DE <-  ggplot(
  d_tariff %>% filter(FY == "DE students"), aes(x=UG_startyear, y=New_Tariff)) +
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  ggtitle("Average entry tariff by DE undergraduate starting year") +
  coord_cartesian(ylim=c(0,220)) +
  scale_y_continuous(name = "Average entry tariff", breaks = seq(0,250,50)) +
  scale_x_discrete(name = "UG starting year") +
  theme_minimal(12) +
  theme(legend.position = "none")
#  scale_x_discrete(labels=c("Cohort 1","Cohort 2","Cohort 3"))
plot_tariffs_cohort_DE

# ============================================================================================================