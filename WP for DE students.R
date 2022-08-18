# FY PROJECT
# Generating WP Dataset and index ====> FOR DE STUDENTS

library(tidyverse)
library(openxlsx)
library(readxl)

rm(list=ls())

path_data <- file.path("output", "data_full_merge.csv")
raw_data <- read.csv(file = path_data ,header = T, stringsAsFactors = F)


path_contextual <- file.path("data_files/WP analysis Nov 2021", "WBS Contextual Offers since 2017_with ID_anonym.xlsx")
d_contextual <- read_excel(path_contextual)

path_warscholars <- file.path("data_files/WP analysis Nov 2021", "Warwick Scholars_2019_20_anonymised.xlsx")
d_warscholars <- read_excel(path_warscholars)


# GET LIST OF CONTEXTUAL OFFER / WARWICK SCHOLARS STUDENTS
# ----------------------------------------------------------------------------------------
d_context <- d_contextual %>% select(ID_anonym, CAP_UDF1) %>% mutate(ind_c = 1)
d_warschol <- d_warscholars %>% select(ID_anonym) %>% mutate(ind_s = 1)

d_scholars <- bind_rows(d_context, d_warschol)

  length(d_scholars$ID_anonym)
  length(unique(d_scholars$ID_anonym))
  # a few duplicates; some contextual offer students were also Warwick scholars (but not all war schols are on contextual list)

d_scholars <- d_scholars %>% mutate(idcheck = duplicated(ID_anonym))
  
d_scholars <- d_scholars %>% group_by(ID_anonym) %>% fill(ind_s, .direction = c("downup")) %>% ungroup()
  
View(d_scholars %>% mutate(idcheck = duplicated(ID_anonym)))

# Easiest to manually exclude the duplicates
d_scholars <- d_scholars %>% filter(
  !(ID_anonym == 45281759 & is.na(ind_c) ) &
  !(ID_anonym == 45161517 & is.na(ind_c) ) &
  !(ID_anonym == 45179094 & is.na(ind_c) ) &
  !(ID_anonym == 45287360 & is.na(ind_c) )
) %>% select(-idcheck)


length(d_scholars$ID_anonym)
d_scholars %>% count(ind_c, ind_s)
# 58 entries in the list provided: 
# contextual but not warwick scholar: 47
# both contextual and warwick scholar: 4
# not contextual and warwick scholar: 7
# Total contextual: 51; total warwick scholar: 11
#note, not all contextual may have taken up Warwick offer / attended --> will not be matched up to data later

# All Warwick scholars should be contextual, but not the case:
d_scholars <- d_scholars %>% mutate(ind_cs = ifelse((ind_c == 1 | ind_s == 1),1,0))

#colnames(d_schol)[-1] <- paste0(colnames(d_schol)[-1], '_C' )

##  FOLLOWING IS COPY OF WP CHARACTERISTICS OF DE STUDENTS FROM fy_descriptive FILE
# ========================================================================================================================

# DIRECT ENTRY (DE) STUDENT CHARACTERISTICS
d_ <- raw_data %>%
  filter(
#    FY_ind == 0,      # Select only DE students
  ) %>% 
  select(
    ID_anonym, UG_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION,
    FEESTATUS, FEESTATUS_DESCRIPTION, FY_ind
  ) %>% 
  distinct() # Remove duplicate records (duplicates given schooling years; module types)

  length(d_$ID_anonym)
  length(unique(d_$ID_anonym))
  # a few duplicates; in the same year, are both awarded 
d_ <- d_ %>% mutate(idcheck = duplicated(ID_anonym)) %>% filter(idcheck == FALSE)

# Compare to list of scholars/contextual offers above
d_check <- d_ %>% filter(ID_anonym %in% d_scholars$ID_anonym)
d_check_2 <- d_scholars %>% filter(ID_anonym %in% d_$ID_anonym)
# 46 d_schol students (out of 58) have data
d_check_3 <- d_ %>% filter(ID_anonym %in% d_warscholars$ID_anonym)
# All 11 warwick scholars have datas

# Merge main data with contextual and warwick scholars indicators
d_schol <- left_join(d_, d_scholars, by = c("ID_anonym")) %>% select(-idcheck)
# 46 students have data from Johnstone's database

d_schol <- d_schol %>% select(ID_anonym, CAP_UDF1, ind_c, ind_s, ind_cs)
write.xlsx(d_schol,'output/ind_sc.xlsx',row.names = F) 
write.csv(d_schol,'output/ind_sc.csv',row.names = F)

# ----------------------------------------------------------------------------------------
# IMPORT AND MERGE WP

# Check no duplicates
length(d_schol$ID_anonym)
length(unique(d_schol$ID_anonym))

## (1) Johnstone data
path_johnstone <- file.path("data_files", "WBS_TK_Extract_20201202_anonymised.xlsx")
d_johnstone <- read_excel(path_johnstone)

d_johnstone <-d_johnstone %>% select(
  ID_anonym,
  LowSEC_Flag, LPN_Quintile,
  WP_ALP_Flag, WP_FSM_Flag, WP_GCSE_Flag,
  InCare_Flag, ParentInHE_Flag, IMD_Quintile,
  School_Type
) %>% distinct()

d_schol_join <- left_join(d_schol, d_johnstone, by = c("ID_anonym"))

# ## (2) Brian's data!
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
# ALP & GCSE flag combine

d_schol_join <- d_schol_join %>% mutate(
  ALP_GCSE = ifelse(WP_ALP_Flag == "Y" | WP_GCSE_Flag == "Y", "Y", 
                    ifelse(is.na(WP_ALP_Flag) & is.na(WP_GCSE_Flag), "NA", "N")
  ))

# ----------------------------------------------------------------------------------------
# RENAMING

d_wpcount <- d_schol_join

d_wpcount$WP1_InCare_n[d_wpcount$InCare_Flag == "Yes"] <- 1  # students spent time in local authority care
d_wpcount$WP2_GCSE_both_n[d_wpcount$ALP_GCSE == "Y"] <- 1  # schools with lower than average national performance
d_wpcount$WP3_FSM_n[d_wpcount$WP_FSM_Flag == "Y"] <- 1  # schools with free school meals
d_wpcount$WP4_LPN_n[d_wpcount$LPN_Quintile == 1] <- 1  # students from lower participation quintile 1
d_wpcount$WP5_1stGen_n[d_wpcount$ParentInHE_Flag == "Yes"] <- 1 # Parent in higher education flag
d_wpcount$wp6_SEC[d_wpcount$LowSEC_Flag == "Y"] <- 1  # Students from Low Socio-Economic Status
d_wpcount$WP7_IMD[d_wpcount$IMD_Quintile == 1 | d_wpcount$IMD_Quintile == 2] <- 1  # IMD quintile 1 or 2

d_wpcount <- d_wpcount %>%
  mutate(
    wp_sum =  rowSums(dplyr::select(.,
                                    WP1_InCare_n, WP2_GCSE_both_n, WP3_FSM_n, WP4_LPN_n, WP5_1stGen_n, wp6_SEC, WP7_IMD
    ), na.rm = TRUE),
    wp_ind = ifelse(wp_sum >=2, 1,0)
    )

# Number of WP criteria
d_wpcount %>%count(wp_sum)
d_wpcount %>%count(ind_c, wp_sum)
d_wpcount %>%count(ind_s, wp_sum)
d_wpcount %>%count(ind_cs, wp_sum)

d_wpcount %>%count(wp_ind)
d_wpcount %>%count(ind_cs, wp_ind)
# ind_cs + wp_ind = 236 students, for comparison calculation (Contextual inc. Warwick scholars + DE (non context/ warschol) with wp_ind)
# Won't use WP sum for Contextual offers or Warwick scholars

# Export data
d_wpcount_export <- d_wpcount %>% select(ID_anonym, CAP_UDF1, ind_c, ind_s, ind_cs, wp_sum, wp_ind)

write.csv(d_wpcount_export,'output/d_wp_export.csv',row.names = F)




# ### FY students and Home students need to be kept out 
# # ----------------------------------------------------------------------------------------
# # DROP FY STUDENTS
# # FY students have their own manual WP input (see wp_fix file) - but need the warwick scholars / provisional indicators
# 
# d_DE_schol %>% count(FY_ind)
# d_DE_schol <- d_DE_schol %>% filter(FY_ind == 0)
# 
# d_DE_schol %>% count(ind_c) # 39 contextual offers (from original list, excludes those who didn't get offer and FY students)
# d_DE_schol %>% count(ind_s) # 6 warwick scholars (means 5 were FY too)
# d_DE_schol %>% count(ind_c, ind_s) # 

# ----------------------------------------------------------------------------------------
# HOME (UK) STUDENTS ONLY
# correct comparison group for FY students; others missing much demographic data 
# ... (but maybe just take whatever we can get)

# length(d_DE$ID_anonym)
# d_DE %>% count(FEESTATUS) # H: Home(UK) fees / HE: EU fees / O: Overseas fees
# d_DE <- d_DE %>% filter(FEESTATUS == "H") # Keep only Home(UK) students
# # 485 / 3081 = approx. 15.7%
# 
# # Composition and size of DE comparison group
# d_DE %>%count(UG_startyear)  
# ----------------------------------------------------------------------------------------
