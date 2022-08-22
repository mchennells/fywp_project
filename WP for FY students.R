# FY PROJECT
# Generating WP Dataset and index

library(tidyverse)
library(openxlsx)
library(readxl)

rm(list=ls())

# ------------------------------------------------------------------------------------------------------------
# IMPORT FULL DATASET
# Get IDs of all FY students, for matching with WP indicators

path_data <- file.path("output", "data_full_merge.csv")
raw_data_full <- read.csv(file = path_data ,header = T, stringsAsFactors = F)

# Get IDs from FY year only (ignores FY year no shows, includes those who left during or after FY year)
d_ID <- raw_data_full %>%
  filter(
    FY_ind == 1, # Select only FY students
    ACRONYM == "FY") %>%
  select(ID_anonym, FY_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION) %>%
  distinct()

# Drop No-Shows for FY year, who don't have any data (those who didn't show up to FY year; but keeps those who completed FY year but were no show for UG (x2))
d_ID %>% count(STATUS_DESCRIPTION)
d_ID <- d_ID %>% filter(STATUS_DESCRIPTION != "No Show")
length(d_ID$ID_anonym) # 163 students

# ------------------------------------------------------------------------------------------------------------
# IMPORT WP INDICATORS
# + clean: remove from original (Tina handmade dataset) the IDs of those who have updated data (Tina's updated handmade dataset only had 2 cohorts)

# Manually combine the WP files for FY students, then import here
# > "WPcriteria cohort 2 and 3 + 21.xlsx" is a combo of "WPcriteria cohort 2 and 3_WBS_anonymized_fixed mistake160221.xlsx" and "21 Cohort Admissions Statistics TK_Aug22_ID_anonym"

path_wp_handcode <- file.path("data_files/wp files", "WPcriteria cohort 2 and 3 + 21.xlsx")
d_wp_handcode <- read_excel(path_wp_handcode)
  length(d_wp_handcode$ID_anonym)
  length(unique(d_wp_handcode$ID_anonym))

# ------------------------------------------------------------------------------------------------------------
# Those students from 2016 onwards who do NOT have data in the above TK handcoded files
path_wp_original <- file.path("data_files/wp files", "WP from WBS_2016_2020_anonymous.xlsx")
d_wp_original <- read_excel(path_wp_original)
d_wp_original <- d_wp_original %>% select(-comment, -WP_SUM, -WP_SUM_with_awards)
length(d_wp_original$ID_anonym)
length(unique(d_wp_original$ID_anonym))

# Remove those who are in this list but who already have better handcoded data above
d_wp_original_drop <- d_wp_original %>% filter(!(ID_anonym %in% d_wp_update$ID_anonym))
length(d_wp_original_drop$ID_anonym)
length(unique(d_wp_original_drop$ID_anonym))

# Bind datasets together (updated IDs with older IDs that weren't updated)
d_wp_updated <- bind_rows(d_wp_handcode, d_wp_original_drop)

# ------------------------------------------------------------------------------------------------------------
# Merge WP Indicators with IDs
d_wp_one <- left_join(d_ID, d_wp_updated, by = "ID_anonym")

# ------------------------------------------------------------------------------------------------------------
# COHORT 1 DATA
# If FY cohort year is 2015-2016 Cohort 1, thus missing in Tina's handmade data... 
# ... OR if Tina's flag data is missing ...
# then, replace with data from Johnstone Updated Dataset (which only includes data on FY students)

path_johnstone_FYonly <- file.path("data_files/wp files", "WBS_TK_Extract_20201208_amended WP flags for FY only_anoymised.xlsx")
d_johnstone_FYonly <- read_excel(path_johnstone_FYonly)
d_johnstone_FYonly_select <-d_johnstone_FYonly %>% select(
  ID_anonym,
  Polar3_Young_HE_Participation_Quintile, POLAR4_Young_HE_Participation_Quintile,
  LowSEC_Flag,LPN_Quintile,
  IMD_Quintile,
  WP_ALP_Flag, WP_FSM_Flag, WP_GCSE_Flag,
  InCare_Flag, ParentInHE_Flag,
)
length(d_johnstone_FYonly_select$ID_anonym)
length(unique(d_johnstone_FYonly_select$ID_anonym))
d_johnstone_FYonly_select$duplicate <- duplicated(d_johnstone_FYonly_select$ID_anonym)
  d_johnstone_FYonly_select %>% count(duplicate)  # Duplicates are missing information
# > Data doesn't uniquely identify students (appears to be POLAR4 variable that is sometimes different)
# > duplicate == TRUE doesn't lose any NB data, so drop accordingly

d_johnstone_FYonly_select <- d_johnstone_FYonly_select %>% filter(duplicate == 'FALSE') %>% select(-duplicate)

# Merge Johnstone data with current WP dataset
d_wp_two <- left_join(d_wp_one, d_johnstone_FYonly_select, by = "ID_anonym")
  
  length(d_wp_two$ID_anonym)
  length(unique(d_wp_two$ID_anonym))

# Manually add in the same variables as for the other FY students
d_wp_three <- d_wp_two %>%
  mutate(
    WP1_InCare_n = ifelse(FY_startyear == "2015-2016" | is.na(WP1_InCare_n), 
                            ifelse(InCare_Flag == "Yes", 1, 0),
                            WP1_InCare_n),
    
    WP2_GCSE_both_n = ifelse(FY_startyear == "2015-2016" | is.na(WP2_GCSE_both_n), 
                               ifelse(WP_GCSE_Flag == "Y" | WP_ALP_Flag == "Y", 1,0),  # GSCE Flag or A-level performance
                               WP2_GCSE_both_n),
    
    WP3_FSM_n = ifelse(FY_startyear == "2015-2016" | is.na(WP3_FSM_n), 
                         ifelse(WP_FSM_Flag == "Y", 1, 0),
                         WP3_FSM_n),
    
    WP4_LPN_n = ifelse(FY_startyear == "2015-2016" | is.na(WP4_LPN_n), 
                         ifelse(LPN_Quintile == 1, 1, 0),
                         WP4_LPN_n),
    
    WP5_1stGen_n = ifelse(FY_startyear == "2015-2016" | is.na(WP5_1stGen_n), 
                            ifelse(ParentInHE_Flag == "Yes", 1, 0),
                            WP5_1stGen_n),
  )


# WP INDEX: number / 5 flags for each student + awards form
# WP 1 flag: student is state In-Care program (=1 if in care program) 
# WP 2 flag: school GSCE performance (= 1 if in bottom)
# WP 3 flag: school participation in Free School Meals (FSM program) (=1 if in program)
# WP 4 flag: Lower Participation Neighbourhood (=1 if in LPN)
# WP 5 flag: student is 1st generation in higher education (=1 if first generation to be in higher education)


# Additional descriptive statistics which might be used: 
# Range: IMD_Quintile,
# Range: LPN_Quintile, 
# Range: Polar3_Young_HE_Participation_Quintile
# Flag: Low SEC flag
# Flag: School A-level performance

# Note: LPN quintile variable very close to Polar3 variable (though not exact); Tina LPN flag usually when LPN quintile = 1 but doesn't track perfectly
# Quintile 1 shows the lowest rate of participation. Quintile 5 shows the highest rate of participation.

# ------------------------------------------------------------------------------------------------------------
# Get WP Sum + Index
# wp_index:  low <= 2; 3 <= high

d_wp_four <- d_wp_three %>%
  mutate(
    wp_sum =  rowSums(dplyr::select(.,
                                    WP1_InCare_n, WP2_GCSE_both_n, WP3_FSM_n, WP4_LPN_n, WP5_1stGen_n
    ), na.rm = TRUE),
    wp_sum_with_awards = rowSums(dplyr::select(.,
                                               WP1_InCare_n, WP2_GCSE_both_n, WP3_FSM_n, WP4_LPN_n, WP5_1stGen_n, AWARDSform_n
    ), na.rm = TRUE),
    wp_index = ifelse(wp_sum_with_awards <= 2, "Low", "High")
  )

# ------------------------------------------------------------------------------------------------------------
# Dataset for export
d_wp_final <- d_wp_four %>% 
  select(ID_anonym, FY_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION,
         wp_sum, wp_sum_with_awards, wp_index, IMD_Quintile, 
         WP1_InCare_n, WP2_GCSE_both_n, WP3_FSM_n, WP4_LPN_n, WP5_1stGen_n, AWARDSform_n) %>% 
  arrange(wp_sum)

write.xlsx(d_wp_final,'output/d_wp_final_220822.xlsx',rowNames = F) 
write_csv(d_wp_final,'output/d_wp_final_220822')

# Note: this includes all FY students on file. Need to filter out those who've left, but this needs to be done in descriptives/analysis. 

# Dataset with all Johnstone flags too
d_wp_final_plusJ <- d_wp_four %>% 
  arrange(wp_sum)
write.xlsx(d_wp_final_plusJ,'output/d_wp_final_plusJ_220822.xlsx',rowNames = F) 
write_csv(d_wp_final_plusJ,'output/d_wp_final_plusJ_220822.csv')

