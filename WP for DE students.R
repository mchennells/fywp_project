# FY PROJECT
# Generating WP Dataset and index ====> FOR DE STUDENTS

library(tidyverse)
library(openxlsx)
library(readxl)

rm(list=ls())

merged_data <- file.path("output", "data_full_merge.csv")
raw_merged_data <- read_csv(file = merged_data)

path_warwickscholars <- file.path("data_files/scholars", "Warwick Scholars_2019_20_anonymised.xlsx")
path_scholars <- file.path("data_files/scholars", "Scholarship Overview_Aug22_ID_anonym.xlsx")
path_contextual <- file.path("data_files/scholars", "WBS Contextual Offers since 2017_Aug22_ID_anonym.xlsx")

d_warwickscholars <- read_excel(path_warwickscholars)
d_scholars <- read_excel(path_scholars)
d_contextual <- read_excel(path_contextual)

path_johnstone <- file.path("data_files", "CHECK SITS_TK_Extract for Tina Kiefer 2022_08_10_002_Aug22_ID_anonym.xlsx")
d_johnstone <- read_excel(path_johnstone)

# ----------------------------------------------------------------------------------------

d_schol_general <- d_scholars %>%
  rename(Award = `Name of award`) %>%
  select(ID_anonym, Award)

d_schol_warwick <- d_warwickscholars %>%
  mutate(Award = "Warwick Scholars Programme") %>%
  select(ID_anonym, Award)

# Bind all scholarship types together
d_schol_all <- rbind(d_schol_general, d_schol_warwick) %>%
  mutate(Scholarship = "Yes")


d_contex <- d_contextual %>%
  mutate(Context_offer = "Yes") %>%
  select(ID_anonym, Context_offer)

# Join scholarship and contextual offers together
# Note: several students with contextual offers also have scholarships
d_scholandcontex <- full_join(d_schol_all, d_contex, by = c("ID_anonym" = "ID_anonym"))
  

d_scholandcontex[duplicated(d_scholandcontex$ID_anonym),]
# 3 duplicates who are recorded as having two different scholarships: 45163911, 45262667, 45161517
d_scholandcontex <- d_scholandcontex %>% mutate(id_check = duplicated(ID_anonym))

# Need to manually remove one or more of these, as will need to merge across later
# > for now, just manually remove one for each person (will only matter for digging down into data later)
d_scholandcontex <- d_scholandcontex %>% filter(id_check == FALSE) %>% select(- id_check)

  d_scholandcontex %>% count(Award) # 50 students offered scholarships
  d_scholandcontex %>% count(Context_offer) # 51 students offered contextual offers (note not all may have taken up, so may not match to data later)

# Misc useful functions used here before:
# d_schol_contex <- d_schol_contex %>% group_by(ID_anonym) %>% fill(Scholarship, .direction = c("downup")) %>% ungroup()
#colnames(d_schol)[-1] <- paste0(colnames(d_schol)[-1], '_C' )

#---------------------------------------------------------------------------------------
# Export scholarship list
write_csv(d_scholandcontex,'output/d_scholandcontex.csv')
#---------------------------------------------------------------------------------------

# ========================================================================================================================
##  DIRECT ENTRY WP CHARACTERISTICS
# ========================================================================================================================

# DIRECT ENTRY (DE) STUDENT CHARACTERISTICS
d_DE <- raw_merged_data %>%
  filter(
    # FY_ind == 0,      # Can select only DE students for more control wrt FY students, but might be useful to have the Johnstone data tacked on as well for those who have it
  ) %>% 
  select(
    ID_anonym, UG_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION,
    FEESTATUS, FEESTATUS_DESCRIPTION, FY_ind
  ) %>% 
  distinct() # Remove duplicate records (duplicates given schooling years; module types)

  length(d_DE$ID_anonym)
  length(unique(d_DE$ID_anonym))
  # a few duplicates; in the same year, are both awarded 

d_DE <- d_DE %>% mutate(idcheck = duplicated(ID_anonym)) %>% filter(idcheck == FALSE)

#---------------------------------------------------------------------------------------
## (1) Johnstone's data approach
d_johnstone <- read_excel(path_johnstone)

d_johnstone_select <- d_johnstone %>% select(
  ID_anonym,
  LowSEC_Flag, LPN_Quintile,
  WP_ALP_Flag, WP_FSM_Flag, WP_GCSE_Flag,
  InCare_Flag, ParentInHE_Flag, IMD_Quintile,
  School_Type
) %>% distinct()

#---------------------------------------------------------------------------------------
# ## (2) Brian's data approach [IGNORE: CODE HERE FOR FUTURE REFERENCE]
# path_brian <- file.path("data_files/new", "EA Current - WBS FY WP 2020_12_14_005_anonymized.xlsx")
# d_brian <- read_excel(path_brian)
# #names(d_brian)
# 
# d_brian <-d_brian %>% select(
#   ID_anonym,
#   # `Student Gender`,
#   # starts_with("Ethnicity"),
#   #  `Academic Year`, 
#   # `Student State School Flag`,
#   "1) In Care? - Contextual Data - WP Foster Care Flag",
#   "2) GCSEs - Contextual Data - GCSE School Performance Flag",
#   "3) FSM - Contextual Data - FSM School Flag",
#   "4) LPN - Contextual Data - POLAR4 Young HE Participation Quintil",
#   "4) LPN - Contextual Data - WP Young HE Participation Flag",
#   "5) 1st Gen - EA - Student Parents Guardians Been In HE?",
#   "5) EA - Student Parents Guardians Been In HE?",
#   "Award - Contextual Data - Award (CAP UDF1)",
#   # starts_with("WP_Flag"), 
#   
# ) %>% 
#   distinct()
# 
# length(d_brian$ID_anonym)
# length(unique(d_brian$ID_anonym))
# d_brian <- d_brian %>% mutate(idcheck = duplicated(ID_anonym)) 
# # Lots of duplicates: see by looking at duplicated == TRUE that this is an issue as sometimes WP criteria are present and other times not
# 
# colnames(d_brian)[-1] <- paste0(colnames(d_brian)[-1], '_B' )
# 
# ## 
# d_full_WP <- left_join(d_full_WP, d_brian, by = "ID_anonym")
# 
# d_full_WP$duplicate <- duplicated(d_full_WP$ID_anonym)
# d_full_WP %>% count(duplicate)  # Duplicates are missing information
# d_full_WP <- d_full_WP %>% filter(duplicate == 'FALSE') %>% select(-duplicate
# Check if the following are true ...
# > Data doesn't uniquely identify students (appears to be POLAR4 variable that is sometimes different)
# > duplicate == TRUE doesn't lose any NB data, so drop accordingly


# ----------------------------------------------------------------------------------------
# WP CRITERIA GENERATION

# Combine ALP & GCSE flags
d_johnstone_select <- d_johnstone_select %>% mutate(
  ALP_GCSE = ifelse(WP_ALP_Flag == "Y" | WP_GCSE_Flag == "Y", "Y", 
                    ifelse(is.na(WP_ALP_Flag) & is.na(WP_GCSE_Flag), "NA", "N")
  ))

# Mirror FY WP criteria, but add '_J' suffix for when join together, so though names are similar we know it's from Johnstone data and not hand coded (and can remove suffix easily)
d_johnstone_select <- d_johnstone_select %>%
  mutate(
    WP1_InCare_n_J = ifelse(InCare_Flag == "Yes", 1, 0), # students spent time in local authority care
    WP2_GCSE_both_n_J = ifelse(ALP_GCSE == "Y", 1, 0), # schools with lower than average national performance
    WP3_FSM_n_J = ifelse(WP_FSM_Flag == "Y", 1, 0), # schools with free school meals
    WP4_LPN_n_J = ifelse(LPN_Quintile == 1, 1, 0), # students from lower participation quintile 1
    WP5_1stGen_n_J = ifelse(ParentInHE_Flag == "Yes", 1, 0), # Parent in higher education flag
    WP6_SEC_J = ifelse(LowSEC_Flag == "Y", 1, 0), # Students from Low Socio-Economic Status
    WP7_IMD_J = ifelse(IMD_Quintile %in% c(1,2), 1, 0), # IMD quintile 1 or 2
  )

d_johnstone_select <- d_johnstone_select %>%
  mutate(
    wp_sum_J =  rowSums(dplyr::select(.,
                                    WP1_InCare_n_J, WP2_GCSE_both_n_J, WP3_FSM_n_J, WP4_LPN_n_J, WP5_1stGen_n_J, WP6_SEC_J, WP7_IMD_J
    ), na.rm = TRUE),
    wp_ind_J = ifelse(wp_sum_J >=2, 1,0)
    )

# Number of WP criteria
d_johnstone_select %>%count(wp_sum_J)
d_johnstone_select %>%count(wp_ind_J)
mean(d_johnstone_select$wp_sum_J, na.rm = TRUE)
# >> Clearly lots missing...

#---------------------------------------------------------------------------------------
# Select data for export (to be merged in later)
d_johnstone_export <- d_johnstone_select %>% 
  select(
    ID_anonym, IMD_Quintile,
    WP1_InCare_n_J, WP2_GCSE_both_n_J, WP3_FSM_n_J, WP4_LPN_n_J, WP5_1stGen_n_J, WP6_SEC_J, WP7_IMD_J,
    wp_sum_J, wp_ind_J
  )

write_csv(d_johnstone_export,'output/d_wp_DE_final_220822.csv')
#---------------------------------------------------------------------------------------
