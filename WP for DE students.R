# FY PROJECT
# Generating WP Dataset and index ====> FOR DE STUDENTS

library(tidyverse)
library(openxlsx)
library(readxl)

rm(list=ls())

merged_data <- file.path("output", "data_full_merge.csv")
raw_merged_data <- read_csv(file = merged_data)

# WBS Contextual offers (since 2017)
path_contextual <- file.path("data_files/scholars", "WBS Contextual Offers since 2017_Aug22_ID_anonym.xlsx")

# Warwick Scholars Program (University wide admissions scheme)
path_warwickscholars <- file.path("data_files/scholars", "WBS Warwick Scholars UG_Aug22_ID_anonym.xlsx")
path_warwickscholars_2 <- file.path("data_files/scholars", "Warwick Scholars Scheme Students from Tina Kiefer 2022_09_01_001_Aug22_ID_anonym.xlsx")
# ... 2nd file is list of ALL warwick scholars whose funding was approved since the program began; use to cross check later


# Other WBS scholarship programs / pathways
path_scholars <- file.path("data_files/scholars", "Scholarship Overview_Aug22_ID_anonym.xlsx")


d_warwickscholars <- read_excel(path_warwickscholars)
d_warwickscholars_2 <- read_excel(path_warwickscholars_2)
d_scholars <- read_excel(path_scholars)
d_contextual <- read_excel(path_contextual)

# Johnstone data for additional demographics
path_johnstone <- file.path("data_files", "CHECK SITS_TK_Extract for Tina Kiefer 2022_08_10_002_Aug22_ID_anonym.xlsx")
d_johnstone <- read_excel(path_johnstone)

# ----------------------------------------------------------------------------------------
# Warwick Scholars UG program + pathways

scholcheck <- d_warwickscholars %>% filter(ID_anonym %in% d_warwickscholars_2$ID_anonym)
# Those in access program "Warwick Scholars Access" match across

d_schol_warwick <- d_warwickscholars %>%
  mutate(ind_Warwick_Scholar = "Yes") %>%
  select(ID_anonym, ind_Warwick_Scholar, everything())


# ----------------------------------------------------------------------------------------
# WBS Scholarships

 scholcheck_2 <- d_scholars %>% filter(ID_anonym %in% d_warwickscholars$ID_anonym)
 scholcheck_3 <- d_scholars %>% filter(ID_anonym %in% d_contextual$ID_anonym)

  d_scholars %>% count(`Name of award`)
# Lion Rock Bursary (FY students only, while in FY; 6 x £5k once off bursaries, eligibility to apply for the Lion Rock Studentship)
# COINS (FY students only, while in FY; 5 x £5k once off bursaries)

# Natwest (open to all UG students; 5 x £1k bursaries, access to careers/development events with Natwest)

# Young Entrepreneurs and Lion Rock studentship (1 FY Lion Rock bursary selected every year) (full fee paying scholarship + £3k bursary each year)
# Warwick Scholars Program (half fee paying)
  
# Merge DSP and MSP into WISP (£2k a year for 3 years + course program on the side)
# RBS Scholarship (£3k total, £1k per year for 3 years)
  
# ...>> Put the above scholarship notes into the data itself

d_schol_general <- d_scholars %>%
  rename(Award = `Name of award`) %>%
  mutate(
    ind_WBS_Scholarship = "Yes" ,
    Award = ifelse(Award %in% c("DSP", "MSP"), "WISP", Award),
    Award_type = ifelse(Award %in% c("WISP", "Lion Rock Bursary", "COINS", "Natwest"), "Bursary", 
                        ifelse(Award %in% c("Young Entrepreneurs", "Lion Rock Studentship", "RBS Management Scholarship"), "Full Fee Paying" , NA
                        )) 
    ) %>%
  arrange(Award) %>%
  select(ID_anonym, ind_WBS_Scholarship, everything())
#>>  The full fee paying is not quite right; some scholarships are more years but lower total amount than some bursaries which are once off

  print(d_schol_general %>% count(ID_anonym), n = 50) # there is a duplicate: 45163911 (had Natwest + Lion Rock Studentship)
  
  scholcheck <- d_schol_general %>% filter(ID_anonym %in% d_warwickscholars$ID_anonym)
  # There are some students who both have WBS scholarships AND came in through the general, Uni-wide Warwick scholars program

# ----------------------------------------------------------------------------------------
# Contextual offers

d_context <- d_contextual %>%
  mutate(ind_Contextual_Offer = "Yes") %>%
    select(ID_anonym, ind_Contextual_Offer, everything())

# ----------------------------------------------------------------------------------------

# Merge all the datasets together
  d_schol_context <- full_join(d_schol_general, d_schol_warwick) %>% full_join(d_context) %>%
    #select(ID_anonym, ind_WBS_Scholarship, ind_Warwick_Scholar,  ind_Contextual_Offer, everything()) %>%
    mutate(ID_dup = duplicated(ID_anonym))
  
# Notes: 
# .. some students can be on Warwick Scholars or Contextual offer holders and hold WBS Scholarships
# .. use ind_ to identify which dataset data comes from (follows each ind_ until the next one)
# ... it's all very messy, just all the data in there!

# This ID 45163911 has both a Lion Rock Studentship AND a Natwest Scholarship
# d_schol_full <- d_schol_full %>% filter(ID_dup == FALSE) %>% select(- ID_dup)

# Misc useful functions used here before:
# d_schol_contex <- d_schol_contex %>% group_by(ID_anonym) %>% fill(Scholarship, .direction = c("downup")) %>% ungroup()
#colnames(d_schol)[-1] <- paste0(colnames(d_schol)[-1], '_C' )

#---------------------------------------------------------------------------------------
# Export scholarship list
  
# >> What about other students who may be on Brian's WSP NEW warwick-wide dataset but aren't in the warwick scholars list we were given; check this later when merging
  
write_csv(d_schol_context,'output/d_schol_context.csv')
#---------------------------------------------------------------------------------------

# ========================================================================================================================
##  DIRECT ENTRY WP CHARACTERISTICS
# ========================================================================================================================

## (1) Johnstone data approach
d_johnstone <- read_excel(path_johnstone)

d_johnstone_select <- d_johnstone %>% select(
  ID_anonym, Intake_Year,
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
  
  d_johnstone_select %>%count(Intake_Year, wp_sum_J)

d_johnstone_sum <- d_johnstone_select %>% 
  group_by(Intake_Year, wp_sum_J) %>%
  summarise(totes = sum(wp_sum_J))

plot <- ggplot(
  d_johnstone_sum, aes(x=wp_sum_J, y=totes, fill = Intake_Year)) +
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5))
plot

# >> Clearly lots missing...

#---------------------------------------------------------------------------------------
# Select data for export (to be merged in later)
d_johnstone_export <- d_johnstone_select %>% 
  select(
    ID_anonym, Intake_Year,
    IMD_Quintile,
    WP1_InCare_n_J, WP2_GCSE_both_n_J, WP3_FSM_n_J, WP4_LPN_n_J, WP5_1stGen_n_J, WP6_SEC_J, WP7_IMD_J,
    wp_sum_J, wp_ind_J
  )

write_csv(d_johnstone_export,'output/d_wp_DE_final_070922.csv')
#---------------------------------------------------------------------------------------
