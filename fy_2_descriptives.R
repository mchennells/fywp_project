# FY PROJECT
# :: DESCRIPTIVE STATISTICS
# Descriptive statistics of FY sample and Direct Entry (DE) sample

#sessionInfo()
library(tidyverse)
library(data.table)
library(sjstats)
library(pwr)
library(openxlsx)
library(readxl)

rm(list=ls())

# ============================================================================================================
# READ DATA
path_data <- file.path("output", "data_full_merge.csv")
raw_data <- read.csv(file = path_data ,header = T, stringsAsFactors = F)
# Note: data has multiple rows per student corresponding to FY / UG period, Schoolyear, and Module type, respectively.

path_wp <- file.path("output", "d_wp_final.csv")
raw_data_wp <- read.csv(file = path_wp ,header = T, stringsAsFactors = F) %>%
  select(-STAGE_DESCRIPTION, -STATUS_DESCRIPTION)

# ============================================================================================================
# MERGE RAW AND WP DATA
d_ <- raw_data %>%
    select(
        ID_anonym, ACRONYM, FY_ind, FY_startyear, UG_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION,
        New_Tariff, Award_Class, Award_Year, Intake_Year,
        Gender, Ethnicity, Disability
        )

d_left <- d_ %>% filter(FY_ind == 1, STAGE_DESCRIPTION == "Left", STATUS_DESCRIPTION != "No Show") %>% distinct()
  length(d_left$ID_anonym)
  mean(d_left$New_Tariff)

# Isolate only FY students
d_FY <- d_ %>% 
  filter(
    FY_ind == 1,      # Select only FY students
    ACRONYM == "FY"   # Keep records only from FY year
    ) %>% 
  distinct()   # Remove duplicate records (duplicates given schooling years; module types)

# Drop No-Shows for FY year (those who didn't show up to FY year; but keeps those who completed FY year but were no show for UG (x2))
  d_FY %>% count(STATUS_DESCRIPTION)
d_FY <- d_FY %>% filter(STATUS_DESCRIPTION != "No Show")

# Merge WP criteria
d_FY <- left_join(d_FY, raw_data_wp, by = c("ID_anonym", "FY_startyear"))
  
d_FY_nocohortone <- d_FY %>% filter(!(FY_startyear %in% c("2015-2016")))
    d_FY_nocohortone %>% count(wp_sum_with_awards)
    d_FY_nocohortone %>% count(wp_index)

## Selecting dataset for Tina
d_FY_select <- d_FY %>% select(ID_anonym, FY_startyear, STAGE_DESCRIPTION, Gender, Ethnicity) %>%
  mutate(
    Ethnicity_collapse =
      ifelse(Ethnicity %in% c("Arab"), "Arab",
        ifelse(Ethnicity %in% c("Asian or Asian British - Bangladeshi", "Asian or Asian British - Indian", "Asian or Asian British - Pakistani", "Other Asian Background"), "Asian",
               ifelse(Ethnicity %in% c("Black or Black British - African", "Black or Black British - Caribbean", "Other Black Background"), "Black",
                      ifelse(Ethnicity %in% c("Chinese"), "Chinese",
                             ifelse(Ethnicity %in% c("Mixed - White and Asian","Mixed - White and Black African","Mixed - White and Black Caribbean", "Other Mixed Background"), "Mixed",
                                    ifelse(Ethnicity %in% c("White", "White - English Welsh Scottish Northern Irish British", "White - Irish", "Other White Background"), "White", Ethnicity
                                    )))))))
# Left as is: Other Ethnic Background; Prefer not to say / Information refused
d_FY_select %>% count(Ethnicity_collapse)
write.xlsx(d_FY_select,'output/descriptive/d_FY_select.xlsx',row.names = F)


# ----------------------------------------------------------------------------------------
# REMOVE THOSE WHO LEFT DURING FY YEAR; i.e. only use descriptive stats of those who completed FY year    
    
# d_left <- d_left %>% 
#   filter(left_ind == 1) %>%
#   select(ID_anonym, ACRONYM, FY_ind, FY_startyear, UG_startyear, CURRENTSTAGE, STAGE_DESCRIPTION, CURRENTSTATUS, STATUS_DESCRIPTION) %>% 
#   filter(FY_ind == 1)

    length(d_FY$ID_anonym)
    length(unique(d_FY$ID_anonym))
    
    d_FY %>%count(FY_startyear)  
    d_FY %>% count(STAGE_DESCRIPTION)
    d_FY %>% count(STATUS_DESCRIPTION)

d_FY <- d_FY %>% filter(STAGE_DESCRIPTION != "Left" ) # Removes 9 students who left (6x forced withdrawn; 3x voluntarily withdrawn)
    length(unique(d_FY$ID_anonym)) # Check: 138 students who completed FY year

# ### code about previous issue with duplicates, no longer a problem
#     d_FY %>%count(FY_startyear)  
#     length(d_FY$ID_anonym)  # Check: 139 students who completed FY year
#     length(unique(d_FY$ID_anonym))  
#     d_FY <- d_FY %>% mutate(idcheck = duplicated(ID_anonym))
#     #View(d_FY %>% filter(idcheck == TRUE))
#     d_FY <- d_FY %>% filter(idcheck != TRUE) %>% select(-idcheck)
# ### 
  
###>> NEED TO DO SMALL ANALYSIS ON THOSE WHO LEFT > might need to include when cleaning and then remove here (note that some no shows were already dropped in cleaning)

# ============================================================================================================
# FY STUDENTS CHARACTERISTICS 

# ----------------------------------------------------------------------------------------
# DEMOGRAPHICS

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
write.xlsx(d_FY_gender,'output/descriptive/d_FY_gender.xlsx',row.names = F)

# Ethnicity
unique(d_FY$Ethnicity)
d_FY %>% count(Ethnicity)
d_FY_ethnicity <- d_FY %>%
  group_by(FY_startyear, Ethnicity) %>%
  summarize(eth_sum = n()) %>%
  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity, eth_sum, fill = 0) %>%
  ungroup()
write.xlsx(d_FY_ethnicity,'output/descriptive/d_FY_ethnicity.xlsx',row.names = F)

# Gender by Ethnicity
d_FY_gender_ethnicity <- d_FY %>%
  group_by(Gender, Ethnicity) %>%
  summarize(eth_sum = n()) %>%
  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity, eth_sum, fill = 0) %>%
  ungroup()
write.xlsx(d_FY_gender_ethnicity,'output/descriptive/d_FY_gender_ethnicity.xlsx',row.names = F)


# ----------------------------------------------------------------------------------------
# DIRECT ENTRY 

path_johnstone <- file.path("data_files", "WBS_TK_Extract_20201202_anonymised.xlsx")
dataset_johnstone <- read_excel(path_johnstone)

# Separate out FY students from Johnstone data
d_FY_names <- d_ %>% select(ID_anonym, FY_ind) %>% filter(FY_ind == 1) %>% distinct()
d_FY_names %>%count(FY_ind)

# Remove FY students from Johnstone data
d_DE <- dataset_johnstone %>%
  select(ID_anonym, Intake_Year, Gender, Ethnicity) %>%
  filter( !(ID_anonym %in% d_FY_names$ID_anonym)) %>%
  filter( Intake_Year %in% c("16/17", "17/18", "18/19", "19/20", "20/21"))
length(d_DE$ID_anonym) # 3122 students

# Get only DE students who have ever been in UG degree (whether or not they left)
d_FY_post <- d_ %>% select(ID_anonym, FY_ind, ACRONYM, Gender, Ethnicity, FY_startyear, UG_startyear) %>%
  filter(
    FY_ind == 1, ACRONYM != "FY"
  ) %>% 
  distinct() %>% 
  mutate( Intake_Year = ifelse(UG_startyear == "2016-2017", "16/17", 
                               ifelse(UG_startyear == "2017-2018", "17/18",
                                      ifelse(UG_startyear == "2018-2019", "18/19",
                                             ifelse(UG_startyear == "2019-2020", "19/20",
                                                    ifelse(UG_startyear == "2020-2021", "20/21", "NA"
          )))))) %>%
  select(ID_anonym, FY_ind, Intake_Year, Gender, Ethnicity)

# Excludes those who left during FY years (so only includes FY students in UG years, ACRONYM != FY); 
# Excludes current 2020/21 FY cohort who are not in UG yet

# Merge datasets together
d_all <- full_join(d_DE,d_FY_post)

d_year_eth <- d_all %>% #filter(FY_ind == 1) %>%
  group_by(Intake_Year, Ethnicity) %>%
  summarize(eth_sum = n()) %>%
  #  mutate(Ethnicity = paste0(Ethnicity, "_sum")) %>%
  spread(Ethnicity, eth_sum, fill = 0) %>%
  ungroup()
print(unique(dataset_johnstone$Ethnicity))

d_FY %>%count(FY_startyear)  
length(d_FY$ID_anonym)  # Check: 138 students who completed FY year
length(unique(d_FY$ID_anonym))  

# Number of WP criteria
d_FY %>%count(wp_sum_with_awards)
mean(d_FY$wp_sum_with_awards, na.rm = TRUE)


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
d_FY_incare <- d_FY %>% group_by(WP1_InCare_n) %>% tally() %>% ungroup()
# missing_incare <- d_FY %>% filter(is.na(InCare_Flag))
write.xlsx(d_FY_incare,'output/descriptive/d_FY_incare.xlsx',row.names = F) 

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
write.xlsx(d_FY_GCSE,'output/descriptive/d_FY_GCSE.xlsx',row.names = F) 

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
write.xlsx(d_FY_FSM,'output/descriptive/d_FY_FSM.xlsx',row.names = F) 


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
write.xlsx(d_FY_LPN_flag,'output/descriptive/d_FY_LPN_flag.xlsx',row.names = F) 


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
write.xlsx(d_FY_parentHE,'output/descriptive/d_FY_parentHE.xlsx',row.names = F) 


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
write.xlsx(d_FY_IMD,'output/descriptive/d_FY_IMD.xlsx',row.names = F) 

# "The Index of Multiple Deprivation score is an overall measure of deprivation experienced by people living in a certain area, and considers seven dimensions: income deprivation; employment deprivation; health deprivation and disability; education; skills and training deprivation; barriers to housing and services; and living environment deprivation and crime."
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015

# Available for DE students?
# unique(d_$IMD_Quintile)

# # Dig into IMD4 and IMD5 WP criteria
# d_FY_IMDhigh <- d_FY %>% filter(IMD_Quintile %in% c(4,5)) %>% arrange(IMD_Quintile)
# write.xlsx(d_FY_IMDhigh,'output/descriptive/IMDhigh.xlsx',row.names = F) 
# #

# Checking School Type (1 July 2022)

d_j_schooltype <- read_excel(path_johnstone)
d_j_schooltype <- d_j_schooltype %>% select(ID_anonym, School_Type)

d_FY_schooltype <- d_FY %>% left_join(d_j_schooltype, by = "ID_anonym")

fy_schooltype <- d_FY_schooltype %>% count(School_Type)

# ----------------------------------------------------------------------------------------
# TARIFF / A-level results

d_tariff <- d_ %>% 
  filter(STAGE_DESCRIPTION != "Left" ) %>% # removes those who left during FY year and UG record of those who left during UG year
  select(
    ID_anonym, FY_ind, FY_startyear, UG_startyear, New_Tariff
  ) %>%
  distinct() %>% 
  mutate(FY = ifelse(FY_ind == 1, "FY","Direct entry"))

d_tariff %>% count(FY_ind)
d_tariff %>%count(UG_startyear)  

# Get Feestatus
path_johnstone <- file.path("data_files", "WBS_TK_Extract_20201202_anonymised.xlsx")
d_johnstone <- read_excel(path_johnstone)

d_johnstone_tariff <-d_johnstone %>% select(
  ID_anonym,
  School_Type, School_Code, 
  Country_Of_Domicile
) %>% distinct()

d_feestatus <- raw_data %>% select(ID_anonym, NATIONALITY, FEESTATUS_DESCRIPTION) %>% distinct()
length(d_feestatus$ID_anonym)
length(unique(d_feestatus$ID_anonym))
d_feestatus <- d_feestatus %>% mutate(idcheck = duplicated(ID_anonym)) %>% filter(idcheck == FALSE) %>% select(-idcheck)

### High Tariff Issue
# d_tariff_print <- d_tariff %>% left_join(d_feestatus) %>% select(-FY_ind) 
# d_tariff_print <- d_tariff_print %>% left_join(d_johnstone_tariff) 
# length(d_tariff_print$ID_anonym)
# length(unique(d_tariff_print$ID_anonym))
# write.xlsx(d_tariff_print,'output/descriptive/d_tariff_print.xlsx',row.names = F) 

d_tariff <- d_tariff %>% left_join(d_feestatus) #%>% select(-FY_ind)

d_tariff <- d_tariff %>% 
  filter(FY == "FY" | (FY == "Direct entry" & FEESTATUS_DESCRIPTION == "Home (UK) Fees"))
# Missing New_Tariff for three FY students: 45566060, 45258404, 45261885

d_tariff <- d_tariff %>% 
  filter(!(ID_anonym %in% c("45674377","45681167", "45580686"))) %>% # Remove 2 BTEC students, 1 very low DE student
  filter(!(UG_startyear %in% c("2014"))) %>% # remove several missing
  filter(!(New_Tariff %in% c(0, NA)))

d_tariff %>% count(FY_ind)
# 133 FY students (missing 5 FY students); 412 Home(UK) students 

hist(d_tariff$New_Tariff)

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
  d_tariff %>% filter(FY == "FY"), aes(x=FY_startyear, y=New_Tariff)) +
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  ggtitle("Average entry tariff by FY cohort year") +
  coord_cartesian(ylim=c(0,200)) +
  scale_y_continuous(name = "Average entry tariff", breaks = seq(0,200,50)) +
  scale_x_discrete(name = "FY cohort year") +
  theme_minimal(12) +
  theme(legend.position = "none")
#  scale_x_discrete(labels=c("Cohort 1","Cohort 2","Cohort 3"))
plot_tariffs_cohort



# ----------------------------------------------------------------------------------------
# OLDER STATISTICS

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
write.xlsx(d_FY_LPN,'output/descriptive/d_FY_LPN.xlsx',row.names = F) 

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
write.xlsx(d_FY_POL3,'output/descriptive/d_FY_POL3.xlsx',row.names = F) 

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
write.xlsx(d_FY_SEC,'output/descriptive/d_FY_SEC.xlsx',row.names = F) 




# ============================================================================================================
# DIRECT ENTRY (DE) STUDENT CHARACTERISTICS
# ============================================================================================================

d_DE <- raw_data %>%
  filter(
    FY_ind == 0,      # Select only DE students
  ) %>% 
  select(
    ID_anonym, UG_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION,
    FEESTATUS, FEESTATUS_DESCRIPTION,
  ) %>% 
  distinct() # Remove duplicate records (duplicates given schooling years; module types)

length(d_DE$ID_anonym)
length(unique(d_DE$ID_anonym))
# a few duplicates; in the same year, are both awarded 
d_DE <- d_DE %>% mutate(idcheck = duplicated(ID_anonym)) %>% filter(idcheck == FALSE)

# # Remove those who left, often missing data
# d_DE <- d_DE %>% filter(STAGE_DESCRIPTION != "Left" ) # Removes 9 students who left (6x forced withdrawn; 3x voluntarily withdrawn)
# length(unique(d_DE$ID_anonym)) # Check: 138 students who completed FY year

# # Remove those who have left
# d_DE %>% count(STATUS_DESCRIPTION)
d_DE <- d_DE %>% filter(STATUS_DESCRIPTION != "No Show")
d_DE %>% count(STAGE_DESCRIPTION)
d_DE <- d_DE %>% filter(STAGE_DESCRIPTION != "Left")

# ----------------------------------------------------------------------------------------
# HOME (UK) STUDENTS ONLY
# correct comparison group for FY students; others missing much demographic data

length(d_DE$ID_anonym)
d_DE %>% count(FEESTATUS) # H: Home(UK) fees / HE: EU fees / O: Overseas fees
d_DE <- d_DE %>% filter(FEESTATUS == "H") # Keep only Home(UK) students
# 462 / 2632 = approx. 17.5%
# minus left = 444 / 2549 = approx 17.4%

# Composition and size of DE comparison group
d_DE %>%count(UG_startyear)  
# Note: remember already excluded are those students who have left for various reasons

# ----------------------------------------------------------------------------------------
# IMPORT AND MERGE WP

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

d_DE_J <- left_join(d_DE, d_johnstone, by = c("ID_anonym"))

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

d_DE_J <- d_DE_J %>% mutate(
  ALP_GCSE = ifelse(WP_ALP_Flag == "Y" | WP_GCSE_Flag == "Y", "Y", 
                    ifelse(is.na(WP_ALP_Flag) & is.na(WP_GCSE_Flag), "NA", "N")
))

# ----------------------------------------------------------------------------------------
# DESCRIPTIVES

d_wpcount <- d_DE_J

d_wpcount$wp_SEC[d_wpcount$LowSEC_Flag == "Y"] <- 1
d_wpcount$wp_ALPGCSE[d_wpcount$ALP_GCSE == "Y"] <- 1
d_wpcount$wp_FSM[d_wpcount$WP_FSM_Flag == "Y"] <- 1
d_wpcount$wp_parent[d_wpcount$ParentInHE_Flag == "Yes"] <- 1
d_wpcount$wp_incare[d_wpcount$InCare_Flag == "Yes"] <- 1

d_wpcount <- d_wpcount %>%
  mutate(
    wp_sum =  rowSums(dplyr::select(.,
                                    wp_SEC, wp_ALPGCSE, wp_FSM, wp_parent, wp_incare
    ), na.rm = TRUE))

# Number of WP criteria
d_wpcount %>%count(wp_sum)
mean(d_wpcount$wp_sum, na.rm = TRUE)

# Tables
d_UG_SEC <- d_DE_J  %>% count(LowSEC_Flag)
  write.xlsx(d_UG_SEC,'output/descriptive/d_UG_SEC.xlsx',row.names = F)
d_UG_LPN <- d_DE_J  %>% count(LPN_Quintile)
  write.xlsx(d_UG_LPN,'output/descriptive/d_UG_LPN.xlsx',row.names = F)

d_UG_ALP<- d_DE_J  %>% count(WP_ALP_Flag)
  write.xlsx(d_UG_ALP,'output/descriptive/d_UG_ALP.xlsx',row.names = F)
d_UG_GCSE<- d_DE_J  %>% count(WP_GCSE_Flag)
  write.xlsx(d_UG_ALP,'output/descriptive/d_UG_GCSE.xlsx',row.names = F)
d_UG_ALP_GCSE<- d_DE_J  %>% count(ALP_GCSE)
  write.xlsx(d_UG_ALP_GCSE,'output/descriptive/d_UG_ALP_GCSE.xlsx',row.names = F)
  
  
d_UG_FSM<- d_DE_J  %>% count(WP_FSM_Flag)
  write.xlsx(d_UG_FSM,'output/descriptive/d_UG_FSM.xlsx',row.names = F)
d_UG_parentHE<- d_DE_J  %>% count(ParentInHE_Flag)
  write.xlsx(d_UG_parentHE,'output/descriptive/d_UG_parentHE.xlsx',row.names = F)
d_UG_incare<- d_DE_J  %>% count(InCare_Flag)
  write.xlsx(d_UG_incare,'output/descriptive/d_UG_incare.xlsx',row.names = F)
d_UG_IMD<- d_DE_J  %>% count(IMD_Quintile)
  write.xlsx(d_UG_IMD,'output/descriptive/d_UG_IMD.xlsx',row.names = F)
  
d_UG_schooltype<- d_DE_J  %>% count(School_Type)

# ============================================================================================================
