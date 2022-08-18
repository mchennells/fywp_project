# MISC analysis and checks

library(readxl) # Read Excel xlsx file

rm(list=ls())

# ==========================================================================================
# Are any of those students who left care leavers?

path_left <- file.path("output", "FY_students_summary_left.xlsx")
raw_data_left <- read_excel(path_left)

path_wp <- file.path("output", "d_wp_final.csv")
raw_data_wp <- read.csv(file = path_wp ,header = T, stringsAsFactors = F) %>%
  select(-STAGE_DESCRIPTION, -STATUS_DESCRIPTION)

d_left_care <- raw_data_left %>% left_join(raw_data_wp) %>%
  select(ID_anonym, STARTYEAR_FY, WP1_InCare_n, STAGE_DESCRIPTION_FY, STATUS_DESCRIPTION_FY, STAGE_DESCRIPTION_degree, STATUS_DESCRIPTION_degree) %>%
  filter(WP1_InCare_n == 1)

# 1 care leaver left during FY year: ID 45579369 & voluntarily withdrawn

# ==========================================================================================
# Which POLAR data are we using? 

# Get POLAR data from Johnstone updated data
path_johnstone_FYonly <- file.path("data_files/new", "WBS_TK_Extract_20201208_amended WP flags for FY only_anoymised.xlsx")
d_johnstone_FYonly <- read_excel(path_johnstone_FYonly)
d_johnstone_FYonly_select <-d_johnstone_FYonly %>% select(
  ID_anonym,
  Polar3_Young_HE_Participation_Quintile, POLAR4_Young_HE_Participation_Quintile,
  LowSEC_Flag,LPN_Quintile,
)
length(d_johnstone_FYonly_select$ID_anonym)
length(unique(d_johnstone_FYonly_select$ID_anonym))
d_johnstone_FYonly_select$duplicate <- duplicated(d_johnstone_FYonly_select$ID_anonym)
d_johnstone_FYonly_select %>% count(duplicate)  # Duplicates are missing information
# > Data doesn't uniquely identify students (appears to be POLAR4 variable that is sometimes different)
# > duplicate == TRUE doesn't lose any NB data, so drop accordingly

d_johnstone_FYonly_select <- d_johnstone_FYonly_select %>% filter(duplicate == 'FALSE') %>% select(-duplicate)

d_polar <- raw_data_wp %>% left_join(d_johnstone_FYonly_select) %>%
  select(ID_anonym, WP4_LPN_n, 
         Polar3_Young_HE_Participation_Quintile, POLAR4_Young_HE_Participation_Quintile,
         LPN_Quintile, LowSEC_Flag,
         )

write.xlsx(d_polar,'output/polarcheck.xlsx',row.names = F) 







