# FY REPORT
# 3 :: Analysing FY data for Report presentation

setwd("~/Google Drive/Warwick/PhD/Work/WBS/TK/FYWP Data Project/fywp_project")

#sessionInfo()
library(tidyverse)
library(data.table)
library(sjstats)
library(pwr)
library(readxl)
library(ggplot2)
library(openxlsx)

rm(list=ls())

# ============================================================================================================
# SETUP
path_data <- file.path("output", "data_full_merge.csv")
raw_data <- read_csv(file = path_data)

path_wp_FY <- file.path("output", "d_wp_FY_final_220822.csv")
path_wp_FY_plusJ <- file.path("output", "d_wp_FY_final_plusJ_220822.csv")
path_wp_DE <- file.path("output", "d_wp_DE_final_220822.csv")

raw_data_wp_FY <- read_csv(file = path_wp_FY)
raw_data_wp_FY_plusJ <- read_csv(file = path_wp_FY_plusJ)
raw_data_wp_DE <- read_csv(file = path_wp_DE)

path_scholandcontex <- file.path("output", "d_schol_context.csv")
raw_data_scholandcontex <- read_csv(path_scholandcontex)


# ============================================================================================================

#raw_data[raw_data == -99] <- NA

# ____________________________________________________
# Data selection for analysis
d_ <- raw_data %>%
  filter(stream != "non-WBS")   # only keep if need to look at non-WBS stream students

    d_ %>% group_by(FY_ind) %>% distinct(ID_anonym) %>% tally() %>% ungroup()
    
    d_ %>% 
      filter(ACRONYM == "FY") %>%
      select(ID_anonym, FY_startyear) %>%
      distinct() %>%
      count(FY_startyear)

    d_ %>% 
      filter(ACRONYM == "FY") %>%
      select(ID_anonym, UG_startyear) %>%
      distinct() %>%
      count(UG_startyear)
    
    # See all 39 NA, why they are missing
    View(
      d_ %>% filter(ACRONYM == "FY", is.na(UG_startyear)) %>%
        select(ID_anonym, ACRONYM, STAGE_DESCRIPTION, STATUS_DESCRIPTION, STARTYEAR, FY_startyear, UG_startyear) %>%
        distinct() %>% arrange(STAGE_DESCRIPTION, FY_startyear) 
         )
    # 4 have FY year COMPLETED but have no UG year; must have not continued / were no show for UG
    # 3 are 20/21 FY startyear but still in stage 1, haven't progressed to undergrad, they are RESIT 
    # 21/22 startyear have no UG marks
    # Rest left during FY year (no show or withdrawn vol/forced)

# ----------------------------------------------------------------------------------------
# FY STUDENTS WHO HAVE LEFT
# Note: STAGE_DESCRIPTION states current - i.e. not year by year - stage: so students who have left will show this across all their records / rows in our dataset

## Students who left during FY year
    d_ %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION) %>% distinct() %>% count(STATUS_DESCRIPTION)
    d_ %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear, STAGE_DESCRIPTION, STATUS_DESCRIPTION) %>% distinct() %>% count(FY_startyear, STATUS_DESCRIPTION)
      # 16 students (8x forced withdrawn, 4x voluntarily withdrawn, 4 x no shows)

# Some FY students finished their FY year but never started UG or left during UG years
    d_ %>% filter(ACRONYM != "FY") %>% 
      select(ID_anonym, FY_ind, UG_startyear, STAGE_DESCRIPTION) %>% distinct() %>% 
      count(FY_ind, STAGE_DESCRIPTION)
      # 8 FY students left during UG years
    
## Remove those who have left during FY year or UG years
    
    #View(d_ %>% filter(FY_ind == 1 & STAGE_DESCRIPTION == "Left")) # Eyeball those who've left
    
    # Issues with using "Left"
    d_ %>% filter(STAGE_DESCRIPTION == "Left") %>% select(ID_anonym, STATUS_DESCRIPTION) %>% distinct() %>% count(STATUS_DESCRIPTION)
      # 2 students recorded as having Left nonetheless have their degrees Full or Awarded; must be a mistake and not FY students
    d_ %>% filter(STAGE_DESCRIPTION != "Left") %>% select(ID_anonym, STATUS_DESCRIPTION) %>% distinct() %>% count(STATUS_DESCRIPTION)
      # View(d_ %>% filter(STAGE_DESCRIPTION != "Left" & STATUS_DESCRIPTION %in% c("Voluntarily Withdrawn", "TWD")))
      # 16 students not recorded as Left have Voluntary withdrawn (x4) or Temporary Withdrawal (x 12); none are FY students 

d_ <- d_ %>% filter(
  STAGE_DESCRIPTION != "Left" & 
    !(STATUS_DESCRIPTION %in% c("Voluntarily Withdrawn", "TWD")))
# Note: 
# ... (1) this removes the records for YY FY students and ZZ FY students who left during FY and UG years respectively (and obviously those DE students who left during UG year)
# ... (2) this still keeps the FY year for those FY students who left during UG years (e.g. ID_anonym == 45577875 or 45464177)

# 11 FY students who passed FY year left during UG year for voluntary / forced withdrawn and 2 no shows

    d_ %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear) %>% distinct() %>% count(FY_startyear)
    length(unique((d_ %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear) %>% distinct())$ID_anonym))

# ----------------------------------------------------------------------------------------
# ADDITINAL SOME MORE CLEANING TO SELECT RIGHT STUDENTS AND GET CORRECT YEAR COMPARISON GROUP


# Some students like 45143577 are still in FY year though they are a year behind; e.g. starting FY in 2020-2021, but repeating it in 2021-2022, with stage UG Resit WO Res, current status = 5
# >> They won't be included in the marks etc. comparison as they don't have a UG_startyear
            
# ----------------------------------------------------------------------------------------
# ###>> NB Issue with UG marks, 2019/2020 marks are dragging everything down
# 
# path_check <- file.path("data_files", "ugrad_students_occmarks_anonym_dec20.xlsx")
# raw_data_check <- read_excel(path_check)
# 
# names_check <- d_ %>% distinct(ID_anonym)
# # limiting data check to only those we are concerned with here; i.e. DE who started the same year as the FY students
# names_check_FY <- d_ %>% filter(FY_ind == 1) %>% distinct(ID_anonym)
# names_check_startyear <- d_ %>% filter(UG_startyear == "2019-2020") %>% distinct(ID_anonym)
# names_check_startyear_FY <- d_ %>% filter(UG_startyear == "2019-2020" & FY_ind == 1)  %>% distinct(ID_anonym)
# 
# raw_check <- raw_data_check %>%
#   filter(
#     ID_anonym %in% names_check$ID_anonym & SCHOOLYEAR == "2019-2020"
#   )
# 
# raw_check_FY <- raw_data_check %>%
#   filter(
#     ID_anonym %in% names_check_FY$ID_anonym & SCHOOLYEAR == "2019-2020"
#   )
# 
# raw_check_startyear <- raw_data_check %>%
#   filter(
#     ID_anonym %in% names_check_startyear$ID_anonym & SCHOOLYEAR == "2019-2020"
#   )
# raw_check_startyear %>% count(OCCASION)
# raw_check_startyear_FY <- raw_data_check %>%
#   filter(
#     ID_anonym %in% names_check_startyear_FY$ID_anonym & SCHOOLYEAR == "2019-2020"
#   )
# 
# write.xlsx(raw_check_startyear,'output/FY2019marksissues.xlsx',row.names = F) 
# 
# 
# mean(raw_check$FINAL_MARK)
# mean(raw_check_FY$FINAL_MARK)
# mean(raw_check_startyear$FINAL_MARK)
# 
# hist(raw_check $FINAL_MARK, breaks = 40)
# hist(raw_check_FY $FINAL_MARK, breaks = 40)
# hist(raw_check_startyear $FINAL_MARK, breaks = 40)
# hist(raw_check_startyear_FY $FINAL_MARK, breaks = 40)

# ----------------------------------------------------------------------------------------
# ADDITIONAL CLEANING

# Cleaning: remove 1x 2014 schoolyear and the few that are missing
d_ <- d_ %>% filter(
  SCHOOLYEAR != "2014-2015" & !is.na(SCHOOLYEAR))

# Select school years for analysis: 
  # 1. Only include UG years for which there are marks for students which can be compared against FY (i.e. from first FY UG cohort: 2016/17, 2017/18, 2018/19, 2019/20)
  # 2. However, because of issues identified above, don't include all students beginning UG in 2019/2020 (note: drops FY 2018/2019 cohort too)
  # 3. Exclude current SCHOOLYEAR as lacks data (marks are only partially captured)

d_ <- d_ %>% filter(
  UG_startyear %in% c("2016-2017","2017-2018","2018-2019","2020-2021", "2021-2022") &  
  !(SCHOOLYEAR %in% c("2022-2023"))
  )
# Note: this will still include the marks for the 2019/2020 year, just not for students who began UG in that year

# If still in stage 1 (given year selection above), then there is insufficient data for analysis
#View(d_ %>% filter(CURRENTSTAGE == 1)) # no students
#d_ <- d_ %>% filter(CURRENTSTAGE > 1)

# Add in DE - WP criteria
d_scholcont_select <- raw_data_scholandcontex %>% 
  select(ID_anonym , ind_WBS_Scholarship, ind_Warwick_Scholar, ind_Contextual_Offer)

# New tariff dataset
d_ <- d_ %>% left_join(d_scholcont_select) %>%
  mutate(ind_SCHOLFUL = ifelse( 
    ind_WBS_Scholarship == "Yes" | ind_Warwick_Scholar == "Yes" | ind_Contextual_Offer == "Yes", "Yes", "No"))

d_ <- d_ %>%
  mutate(
    FY = ifelse(FY_ind == 1, "FY students","Direct entry"), 
    student_type = ifelse(FY_ind == 1, "FY", 
                          ifelse((FY_ind == 0 & ind_SCHOLFUL == "Yes"), "DE - WP students", "DE students"))) %>%
  mutate(
    student_type = ifelse(is.na(student_type),"DE students", student_type)   # weird, had to do this because 'else' part wasn't working above
  )

  d_ %>% count(student_type)
  d_ %>% count(FY)

# ----------------------------------------------------------------------------------------
# NB NOTE: we are not removing those that didn't pass - we are taking all into account, as long as they haven't left
# So those who are repeating are included in two years for the same stage
# e.g. ID_anonym == 45361802 ; stage 2 for two years in a row, first year had fail marks

# ----------------------------------------------------------------------------------------
# STUDENT SUMMARY
d_stats <- d_ %>% select(ID_anonym,  FY_ind, FY_startyear, UG_startyear) %>% distinct()
  d_stats %>% count(FY_ind) # 102 FY students (from 5 cohorts) in UG years compared to 2,388 DE students
  d_stats %>% count(FY_ind, FY_startyear) # FY students by FY year cohort
  d_stats %>% count(FY_ind, UG_startyear) # FY students by UG year cohort
  # Note: includes the FY years for those who finished FY but then left during FY (in order to calculate FY marks later)

# ==========================================================================================
### MARKS
# Q: how well are FW students doing? (1) Overall (2) numbers modules (3) social sciences modules

# ----------------------------------------------------------------------------------------

## FY YEAR

# ... by FY cohort
marks_FYyear <- d_ %>% filter(ACRONYM == "FY") %>%
  select(ID_anonym, FY_ind, ACRONYM, FY_startyear, SCHOOLYEAR,
         OVERALL_MARKS) %>% distinct() %>%
  group_by(FY_startyear) %>% summarise(mean_marks = mean(OVERALL_MARKS, na.rm = TRUE)) %>% ungroup()

# ... by module type
marks_FYyear_module <- d_ %>% filter(ACRONYM == "FY") %>%
  select(ID_anonym, FY_ind, ACRONYM, FY_startyear, SCHOOLYEAR,
         OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE)  %>%
  group_by(MODULE_TYPE) %>% summarise(mean_marks = mean(OVERALL_MARKS_MODULE, na.rm = TRUE)) %>% ungroup()

# ... by FY cohort and module type
marks_FYyear_module_year <- d_ %>% filter(ACRONYM == "FY") %>%
  select(ID_anonym, FY_ind, ACRONYM, FY_startyear, SCHOOLYEAR,
         OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE)  %>%
  group_by(FY_startyear, MODULE_TYPE) %>% summarise(mean_marks = mean(OVERALL_MARKS_MODULE, na.rm = TRUE)) %>% ungroup()

# ----------------------------------------------------------------------------------------
## UNDERGRADUATE YEARS
# ... by FY UG startyear

# Remove duplicates present from multiple modules by year
d_marks_UG <- d_ %>% 
  filter(ACRONYM != "FY") %>% 
  select(ID_anonym, Gender, Ethnicity, Ethnicity_collapse, Disability, FY, student_type, FEESTATUS, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, OVERALL_MARKS) %>% distinct()

  d_marks_UG %>% select(ID_anonym, FY,student_type, FY_startyear, UG_startyear) %>% distinct() %>% count(FY, UG_startyear) 

# ... note this also drops the records from (2) earlier: records of FY students who left only during UG year (9, including 7 forced/voluntary withdrawn and 2 no shows) [see code below to see this]
# /////////// >> this was to find issues in students dropped in the comment above
# check_1 <- d_ %>% filter(FY == "FY")
# length(unique(check_1$ID_anonym))
# check_2 <- d_marks_UG %>% filter(FY == "FY")
# length(unique(check_2$ID_anonym))
# 
# d_marks_UG %>% select(ID_anonym, FY, FY_startyear, UG_startyear) %>% distinct() %>% count(FY, UG_startyear) 
# 
# missing_check <- check_1 %>%
#     filter(
#       !(ID_anonym %in% check_2$ID_anonym)
#     )
# 
# unique(missing_check$ID_anonym)
# 
# # 45671263 : forced withdrawn UG
# # 45683240 : forced withdrawn UG
# # 45668892: missing UG years even though passed FY year
# # 45579195 : voluntary withdrawn UG
# # 45577875 : voluntary withdrawn UG
# # 45579448 : missing UG years even though passed FY year
# # 45475067 : forced withdrawn UG
# # 45472808 : forced withdrawn UG
# # 45464177 : forced withdrawn UG
#  

# Get one average mark per student (i.e. collapse)
d_marks_UG_collapse <- d_marks_UG %>%
  group_by(ID_anonym, Gender, Ethnicity, Ethnicity_collapse, Disability, FY, student_type, FEESTATUS, ACRONYM, stream, UG_startyear) %>% 
  summarise( mean_marks = mean(OVERALL_MARKS, na.rm = TRUE)) %>% ungroup()

# 
# View(d_marks_UG_collapse %>% arrange(mean_marks))
# ## Low marks issue: 2 FY students have exceptionally low marks (ID 45465811 & 45475992), both UG startyear 2018-2019; check original marks dataset (occmarks)
#  path_check_marks <- file.path("data_files", "ugrad_students_occmarks_anonym_dec20.xlsx")
#  raw_data_check_marks <- read_excel(path_check_marks)
#  View(raw_data_check_marks %>% filter(ID_anonym %in% c("45465811","45475992")) %>% arrange(ID_anonym, SCHOOLYEAR))
# # Missing any marks for 2018/19 year (barring 1 module), supposed UG startyear (see readdata script for info on this issue)
# # Seems they only started UG in 2019, so they need to be dropped too (given the issues with 2019/2020 year marks)
# 
d_marks_UG_collapse <- d_marks_UG_collapse %>% filter(!(ID_anonym %in% c("45465811","45475992")))

#
  d_marks_UG_collapse %>% select(ID_anonym, student_type, UG_startyear) %>% distinct() %>% count(student_type, UG_startyear) 

# Comparing FY and DE marks
# ... overall
d_marks_overall <- d_marks_UG_collapse %>% 
  group_by(UG_startyear, FY) %>% 
  summarise(
    marks_overall_mean = mean(mean_marks),
    students_sum = n()) %>%
  ungroup()
# Note: this just separates the marks into those starting in different years; doesn't track individuals over time - we can do this here in the collapsing above

# ... by gender
d_marks_overall_gen <- d_marks_UG_collapse %>% 
  group_by(FY, Gender) %>% 
  summarise(
    marks_overall_mean = mean(mean_marks),
    students_sum = n()) %>%
  ungroup()

# ... by ethnicity
d_marks_overall_eth <- d_marks_UG_collapse %>% 
  group_by(student_type, Ethnicity_collapse) %>% 
  summarise(
    marks_overall_mean = mean(mean_marks, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup() %>% 
  pivot_wider(names_from = student_type, values_from = c(marks_overall_mean, students_sum))

  d_marks_UG_collapse %>% count(Ethnicity_collapse)
  d_marks_UG_collapse %>% count(UG_startyear)
  
with(data = d_marks_UG_collapse %>% 
       filter(
         student_type == "DE students",   # DE students only
         Ethnicity_collapse %in% c("Black", "White"),   # Difference between white and black DE students
              ),    
     wilcox.test(mean_marks ~ Ethnicity_collapse, paired = FALSE))
with(data = d_marks_UG_collapse %>% 
       filter(
         student_type == "FY",   # FY students only
         Ethnicity_collapse %in% c("Black", "White"),
       ),    
     wilcox.test(mean_marks ~ Ethnicity_collapse, paired = FALSE))

# ----------------------------------------------------------------------------------------
# Plot overall marks

d_marks_UG_collapse$student_type <- as_factor(d_marks_UG_collapse$student_type)
#
plot_marks_overall <-  ggplot(
    d_marks_UG_collapse, aes(x=student_type, y=mean_marks, fill = student_type)) + 
    stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
    stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
    stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
    ylab("Average marks (%)") +
    xlab("Student entry type") +
    ggtitle("Average UG Overall marks across all modules") +
    coord_cartesian(ylim=c(0,80)) +
    theme_minimal(12) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#3399FF", "#E69F00", "#006633"))
plot_marks_overall

  d_marks_UG_collapse %>% count(student_type)

with(data = d_marks_UG_collapse %>% filter(student_type != "FY"),    # Difference between DE and DE-WP (FY excluded)
     wilcox.test(mean_marks ~ student_type, paired = FALSE))
with(data = d_marks_UG_collapse %>% filter(student_type != "DE - WP students"),    # Difference between DE and FY (DE-WP excluded)
     wilcox.test(mean_marks ~ student_type, paired = FALSE))
with(data = d_marks_UG_collapse %>% filter(student_type != "DE students"),    # Difference between DE-WP and FY (DE excluded)
     wilcox.test(mean_marks ~ student_type, paired = FALSE))


plot_marks_overall_startyear <-  ggplot(
  d_marks_UG_collapse, aes(x=UG_startyear, y=mean_marks, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  ylab("Average marks (%)") +
  xlab("UG startyear") +
  ggtitle("Average UG Overall marks across all modules") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_marks_overall_startyear

#with(data = d_marks_UG_collapse, wilcox.test(mean_marks ~ FY, paired = FALSE))

plot_marks_overall_stream_FY <-  ggplot(
    d_marks_UG_collapse, aes(x=FY, y=mean_marks, fill = stream)) + 
    stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
    stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
    stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
    ylab("Average marks (%)") +
    xlab("Student entry type") +
    ggtitle("Average UG Overall marks between streams") +
    coord_cartesian(ylim=c(0,80)) +
    theme_minimal(12)  +
    scale_fill_brewer(palette = "Dark2")
  #  theme(legend.position = "none")
plot_marks_overall_stream_FY

plot_marks_overall_stream <-  ggplot(
  d_marks_UG_collapse, aes(x=student_type, y=mean_marks, fill = stream)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  ylab("Average marks (%)") +
  xlab("Student entry type") +
  ggtitle("Average UG Overall marks between streams") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12)
#  theme(legend.position = "none") 
plot_marks_overall_stream


# International vs home
d_marks_UG_collapse_int <- d_marks_UG_collapse %>% 
  mutate(home_int = ifelse(FEESTATUS == "H", "Home(UK)", "EU / International")
         )
  d_marks_UG_collapse_int %>% count(home_int)

plot_marks_overall_int <-  ggplot(
  d_marks_UG_collapse_int %>% filter(!is.na(home_int), FY == "Direct entry"), aes(x=home_int, y=mean_marks, fill = home_int)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  ylab("Average marks (%)") +
  xlab("Fee Status") +
  ggtitle("Average UG Overall marks between Home(UK) and International/EU") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2")
plot_marks_overall_int

# ----------------------------------------------------------------------------------------
# PLOT ATTAINMENT GAP

d_marks_AG <- d_marks_UG_collapse %>% 
  filter(
    stream != "non-WBS",
    Ethnicity_collapse %in% c("Asian", "Black", "White"),
    )
  
  d_marks_AG %>% count(Ethnicity_collapse)
  length(d_marks_AG$ID_anonym)
  length(unique(d_marks_AG$ID_anonym))

  
ggplot(data = d_marks_AG, aes (x = Ethnicity_collapse, y = mean_marks, colour = student_type)) +
  geom_boxplot(
    outlier.shape = NA, 
    position = position_dodge(width = 1),
    ) +
  geom_point(position = position_jitterdodge(dodge.width = 1), alpha = 0.4 ) +
  stat_summary(
    fun=mean, geom="point", shape=4, size=3, # color="black" to stand out more
    position = position_dodge2(width = 1, preserve = "single")
  ) +
  scale_y_continuous(
    limits = c(
      max( min(d_marks_AG$mean_marks) - 10, 0),
      min( max(d_marks_AG$mean_marks) + 10, 100)
    ),
    breaks = seq(0, 100, 10)
  ) + 
  ylab("Average marks (%)") +
  xlab("Ethnicity") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())
  #   stat_summary(fun = mean, geom="point", shape=20, size=3, color="black") +
  #   stat_summary(fun.data = mean_se, geom="errorbar", width = 0.2, color="black") # can also use mean_cl_boot in place of mean_se
  # #  stat_summary(fun = mean, geom = 'line')
  

# ----------------------------------------------------------------------------------------
# PLOT DISABILITY DIFFERENCES
  
  d_marks_UG_collapse %>% count(ACRONYM)
  d_marks_UG_collapse %>% count(stream)

d_dis <- d_marks_UG_collapse %>%
  select(ID_anonym, UG_startyear, FEESTATUS, FY, student_type, Ethnicity_collapse, stream, Disability, mean_marks) %>% 
  distinct()

  d_dis %>% count(student_type, UG_startyear)
  length(d_dis$ID_anonym)
  length(unique(d_dis$ID_anonym))

d_dis_spread <- d_dis %>% 
  count(student_type, Disability) %>%
  pivot_wider(names_from = student_type, values_from = n) %>%
  arrange(Disability)

d_dis_spread_marks <- d_dis %>% 
  group_by(Disability) %>%
  summarise(
    n = n(),
    mean = mean(mean_marks)
  ) %>% ungroup() %>%
  arrange(Disability)


plot_marks_dis <-  ggplot(
  d_dis %>% filter(!is.na(Disability)), aes(x= Disability, y=mean_marks, fill = Disability)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  ylab("Average marks (%)") +
  xlab("Disability") +
  ggtitle("Average UG Overall marks by disability") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_marks_dis



# ----------------------------------------------------------------------------------------
### BY MODULES 

# Remove duplicates present from multiple modules by year
d_marks_UG_modules <- d_ %>% filter(ACRONYM != "FY") %>% 
  select(ID_anonym, Gender, Ethnicity, FY, student_type, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, MODULE_TYPE, OVERALL_MARKS_MODULE)

d_marks_UG_modules <- d_marks_UG_modules %>% filter(!(ID_anonym %in% c("45465811","45475992")))

# Get one average mark per student (i.e. collapse )
d_marks_UG_modules_collapse <- d_marks_UG_modules %>%
  group_by(ID_anonym, Gender, Ethnicity, FY, student_type, ACRONYM, stream, UG_startyear, MODULE_TYPE) %>% 
  summarise( 
    mean_marks = mean(OVERALL_MARKS_MODULE, na.rm = TRUE)
    ) %>% ungroup()

  d_marks_UG_modules_collapse %>% count(MODULE_TYPE)

## Numeric-related modules
plot_marks_numeric <-  ggplot(
      d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Numeric / Quantitative / Science / Engineering"), 
      aes(x=FY, y=mean_marks, fill = FY)) + 
      stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
      stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
      stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                      
      ylab("Average marks (%)") +
      xlab("Student entry type") +
      ggtitle("Average UG marks in Numeric modules") +
      coord_cartesian(ylim=c(0,80)) +
      theme_minimal(12) +
      theme(legend.position = "none")
plot_marks_numeric
#with(data = d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Numeric"), wilcox.test(mean_marks ~ FY, paired = FALSE))

# # ... by year
# plot_marks_numeric_year <-  ggplot(
#       d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Numeric"), 
#       aes(x=UG_startyear, y=mean_marks, fill = FY)) + 
#       stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#       stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
#       stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -1) +
#       ylab("Average marks (%)") +
#       xlab("FY Cohort") +
#       ggtitle("Average UG marks in Numeric related modules") +
#       coord_cartesian(ylim=c(0,80)) +
#       theme_minimal(12) +
#       theme(legend.title = element_blank()) +
#       scale_x_discrete(labels=c("Cohort 1","Cohort 2","Cohort 3", "Cohort 4"))
# plot_marks_numeric_year


## Social sciences-related modules
plot_marks_social <-  ggplot(
      d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Social science / Social studies"), 
      aes(x=FY, y=mean_marks, fill = FY)) + 
      stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
      stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
      stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
      ylab("Average marks (%)") +
      xlab("Student entry type") +
      ggtitle("Average UG marks in Social studies modules") +
      coord_cartesian(ylim=c(0,80)) +
      theme_minimal(12) +
      theme(legend.position = "none")
plot_marks_social
#with(data = d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), wilcox.test(mean_marks ~ FY, paired = FALSE))

# # ... by year
# plot_marks_social_year <-  ggplot(
#       d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), 
#       aes(x=UG_startyear, y=mean_marks, fill = FY)) + 
#       stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#       stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
#       stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -1) +                
#       ylab("Average marks (%)") +
#       xlab("FY Cohort") +
#       ggtitle("Average UG marks in Social studies related modules") +
#       coord_cartesian(ylim=c(0,80)) +
#       theme_minimal(12) +
#       theme(legend.title = element_blank()) +
#       scale_x_discrete(labels=c("Cohort 1","Cohort 2","Cohort 3", "Cohort 4"))
# plot_marks_social_year

# 
# ## Other module marks
# plot_marks_other <-  ggplot(
#   d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Other"), 
#   aes(x=FY, y=mean_marks, fill = FY)) + 
#   stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#   stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
#   stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
#   ylab("Average marks (%)") +
#   xlab("Student entry type") +
#   ggtitle("Average UG marks in Other modules") +
#   coord_cartesian(ylim=c(0,80)) +
#   theme_minimal(12) +
#   theme(legend.position = "none")
# plot_marks_other
# 
# 
# ## Unknown module marks
# plot_marks_unknown <-  ggplot(
#   d_marks_UG_modules_collapse %>% filter(MODULE_TYPE == "Unknown"), 
#   aes(x=FY, y=mean_marks, fill = FY)) + 
#   stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#   stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
#   stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
#   ylab("Average marks (%)") +
#   xlab("Student entry type") +
#   ggtitle("Average UG marks in Unknown modules") +
#   coord_cartesian(ylim=c(0,80)) +
#   theme_minimal(12) +
#   theme(legend.position = "none")
# plot_marks_unknown
#

# ----------------------------------------------------------------------------------------
# Plot marks by SCHOOLYEAR and FY

d_marks_year <- d_ %>% filter(ACRONYM != "FY") %>% 
  select(ID_anonym, FY, student_type, stream, FY_startyear, UG_startyear, SCHOOLYEAR, OVERALL_MARKS) %>% distinct()

d_marks_year_collapse <- d_marks_year %>%
  group_by(ID_anonym, FY, student_type, stream, SCHOOLYEAR) %>% 
  summarise( mean_marks = mean(OVERALL_MARKS, na.rm = TRUE)) %>% ungroup()

d_marks_year_collapse <- d_marks_year_collapse %>% filter(!(ID_anonym %in% c("45465811","45475992")))
  
d_marks_year_overall <- d_marks_year_collapse %>% 
  group_by(SCHOOLYEAR, FY) %>% 
  summarise(
    marks_overall_mean = mean(mean_marks, na.rm = TRUE),
    #students_sum = n()
    ) %>%
  ungroup() %>% 
  pivot_wider(names_from = FY, values_from = marks_overall_mean, names_prefix = "Overall marks_") %>%
  mutate(marks_diff = `Overall marks_Direct entry` - `Overall marks_FY students`)

#d_marks_year_collapse$student_type <- as_factor(d_marks_year_collapse$student_type)
#
plot_marks_year <-  ggplot(
  d_marks_year_collapse, aes(x=SCHOOLYEAR)) + 
  stat_summary(aes(y=mean_marks, group = FY, color = FY), fun = mean, geom='line', size=1) +
  stat_summary(aes(y=mean_marks, group = FY, color = FY), fun.data = mean_se, geom='errorbar', width=0.2) +
  stat_summary(aes(y=mean_marks, group = FY, color = FY, label=round(..y..,0)), 
               fun=mean, geom="text", vjust = -2) +
  stat_summary(data = d_marks_year_overall, aes(y=marks_diff),
               fun = mean, geom='bar', 
               width=0.3, position=position_dodge(0.5), fill = "grey") +
  stat_summary(data = d_marks_year_overall, aes(y=marks_diff, label=round(..y..,0)), 
               fun = mean, geom="text", vjust = -2) +
  xlab("School Year") +
  ggtitle("Average overall marks by year, and difference between group averages") +
  coord_cartesian(ylim=c(0,90)) +
  scale_y_continuous(name = "Average marks (%)",  breaks = seq(0,90,10)) +
  theme_minimal(12) +
  theme(legend.justification = c(0, 1), legend.position = c(0.75, 1), legend.title=element_blank()) +
  scale_color_manual(values=c("#3399FF", "#006633"))
plot_marks_year

plot_marks_year_type <-  ggplot(
  d_marks_year_collapse, aes(x=SCHOOLYEAR)) + 
  stat_summary(aes(y=mean_marks, group = student_type, color = student_type), fun = mean, geom='line', size=1) +
  stat_summary(aes(y=mean_marks, group = student_type, color = student_type), fun.data = mean_se, geom='errorbar', width=0.2) +
  stat_summary(aes(y=mean_marks, group = student_type, color = student_type, label=round(..y..,0)), 
               fun=mean, geom="text", vjust = -2) +
  stat_summary(data = d_marks_year_overall, aes(y=marks_diff),
               fun = mean, geom='bar', 
               width=0.3, position=position_dodge(0.5), fill = "grey") +
  stat_summary(data = d_marks_year_overall, aes(y=marks_diff, label=round(..y..,0)), 
               fun = mean, geom="text", vjust = -2) +
  xlab("School Year") +
  ggtitle("Average overall marks by year, and difference between group averages") +
  coord_cartesian(ylim=c(0,90)) +
  scale_y_continuous(name = "Average marks (%)",  breaks = seq(0,90,10)) +
  theme_minimal(12) +
  theme(legend.justification = c(0, 1), legend.position = c(0.75, 1), legend.title=element_blank())
plot_marks_year_type

# ----------------------------------------------------------------------------------------
## FY AND PREDICTING UG MARKS
  
  d_ %>% count(FY)
  d_ %>% count(student_type)

d_marks_FYUG <- d_ %>% filter(student_type == "FY" & !(ID_anonym %in% c("45465811","45475992"))) %>%
  select(ID_anonym, Gender, Ethnicity_collapse, FY, ACRONYM, stream, New_Tariff, FY_startyear, UG_startyear, SCHOOLYEAR, timestamp, OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE
         ) %>% 
  distinct() 

# Remove FY year of those who left during UG (and who only have FY years in this dataset)
d_remove <- d_marks_FYUG %>% select(ID_anonym, timestamp) %>% distinct %>% group_by(ID_anonym) %>% mutate(ind_sum =  n()) %>% ungroup()
  d_remove %>% count(ind_sum)
  # Those with only 1 record are FY students who completed FY year but Left during undergrad (x7 students, see above); have confirmed in original students record data (students_anonymised)
d_remove <- d_remove %>% filter(ind_sum ==2)

### >>> GET THE IDS OF THESE STUDENTS TO CHECK

# Keep only those who have both FY and UG records
d_marks_FYUG <- d_marks_FYUG %>% filter(ID_anonym %in% d_remove$ID_anonym)

# ----------------------------------------------------------------------------------------
# Overall UG marks by FY marks

d_marks_FYUG_overall <- d_marks_FYUG %>% select(ID_anonym, Ethnicity_collapse, New_Tariff, FY_startyear, timestamp, OVERALL_MARKS) %>% distinct() %>%
  group_by(ID_anonym,Ethnicity_collapse, New_Tariff, FY_startyear, timestamp) %>% 
  summarise(mean_marks = mean(OVERALL_MARKS, na.rm = TRUE)) %>% 
  ungroup()
d_marks_FYUG_overall %>% count(timestamp)

# Overall FY and UG marks by FY cohort
d_marks_FYUG_overall_sum <- d_marks_FYUG_overall %>% 
  group_by(FY_startyear, timestamp) %>% 
  summarise(
    marks_overall_mean = mean(mean_marks),
    students_sum = n()) %>%
  ungroup()

d_marks_FYUG_pivot <- d_marks_FYUG_overall %>% pivot_wider(names_from= timestamp, values_from = mean_marks, names_prefix = "overall_marks_" )

# Module UG marks by FY marks
d_marks_FYUG_module <- d_marks_FYUG %>% select(ID_anonym, FY_startyear, New_Tariff, timestamp, MODULE_TYPE, OVERALL_MARKS_MODULE) %>% distinct() %>%
  group_by(ID_anonym,New_Tariff,  FY_startyear, timestamp, MODULE_TYPE) %>% 
  summarise(mean_marks = mean(OVERALL_MARKS_MODULE, na.rm = TRUE)) %>% 
  ungroup()
d_marks_FYUG_module %>% count(timestamp)

d_marks_FYUG_module_pivot <- d_marks_FYUG_module %>% pivot_wider(names_from = c(timestamp, MODULE_TYPE), values_from = mean_marks)
#, names_prefix = "module_marks_" 

d_marks_FYUG_final <- left_join(d_marks_FYUG_pivot, d_marks_FYUG_module_pivot)

# , by = c("ID_anonym" = "ID_anonym", "FY_startyear"="FY_startyear", "New_Tariff"="New_Tariff")

# ----------------------------------------------------------------------------------------
# ANALYSIS
# By stream! Ethnicity, Gender.

d_marks_FYUG_final <- d_marks_FYUG_final %>% 
  filter(!(ID_anonym %in% c("45674377","45681167"))) # Remove 2 BTEC students, 1 very low DE student

d_marks_FY_eth <- d_marks_FYUG_final %>% 
  group_by(Ethnicity_collapse) %>% 
  summarise(
    FY_marks_overall_mean = mean(overall_marks_FY, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

with(data = d_marks_FYUG_final %>% 
    filter(
      Ethnicity_collapse %in% c("Black", "Asian"),   # Difference between white and black DE students
    ),    
  wilcox.test(overall_marks_FY ~ Ethnicity_collapse, paired = FALSE))


# ANALYSIS OF FY MARKS
# note: this is limited to the 95 FY students under consideration, though could be expanded to all

### >> Can add in regression of Predicted A-levels (which FY are selected on) and FY marks

m_fy <- lm(overall_marks_FY ~  New_Tariff, data = d_marks_FYUG_final)
summary(m_fy)
effectsize::cohens_f(m_fy) 

m_fy_num <- lm(FY_Numeric ~  New_Tariff, data = d_marks_FYUG_final)
summary(m_fy_num)
effectsize::cohens_f(m_fy_num) 

m_fy_soc <- lm(`FY_Social studies` ~  New_Tariff, data = d_marks_FYUG_final)
summary(m_fy_soc)
effectsize::cohens_f(m_fy_soc)

# ANALYSIS OF DEGREE MARKS
m_fy_ug <- lm(overall_marks_degree ~  overall_marks_FY + New_Tariff, data = d_marks_FYUG_final)
summary(m_fy_ug)
anova(m_fy_ug)
effectsize::cohens_f(m_fy_ug) # cohen's f: 0.53
#model_fig$predicted_overall <- predict(model_fig)

m_fy_ug_numeric <- lm(degree_Numeric ~ overall_marks_FY + New_Tariff, data = d_marks_FYUG_final)
summary(m_fy_ug_numeric)
anova(m_fy_ug_numeric)
effectsize::cohens_f(m_fy_ug_numeric) # cohen's f: 0.39

m_fy_ug_social <- lm(`degree_Social studies` ~ overall_marks_FY + New_Tariff, data = d_marks_FYUG_final)
summary(m_fy_ug_social)
anova(m_fy_ug_social)
effectsize::cohens_f(m_fy_ug_social) # cohen's f: 0.44

## ... in more detail wrt to FY module types

m_fy_ug_break <- lm(overall_marks_degree ~ FY_Numeric + `FY_Social studies` + New_Tariff , data = d_marks_FYUG_final)
summary(m_fy_ug_break)
anova(m_fy_ug_break)
effectsize::cohens_f(m_fy_ug_break) # cohen's f: 0.40
# FY social studies marks predict UG degree overall mark success

m_fy_ug_num_break <- lm(degree_Numeric ~ FY_Numeric + `FY_Social studies` + New_Tariff , data = d_marks_FYUG_final)
summary(m_fy_ug_num_break)
anova(m_fy_ug_num_break)
effectsize::cohens_f(m_fy_ug_num_break)
# Numeric FY success predicts UG numeric success

m_fy_ug_soc_break <- lm(`degree_Social studies` ~ FY_Numeric + `FY_Social studies` + New_Tariff , data = d_marks_FYUG_final)
summary(m_fy_ug_soc_break)
anova(m_fy_ug_soc_break)
effectsize::cohens_f(m_fy_ug_soc_break)
# Social studies success predicts UG social studies success

## Note: no effects of tariff throughout; i.e. once FY year marks accounted for

# # Correlations between tariffs and marks
# with(data = d_marks_FYUG_final, cor.test(New_Tariff,overall_marks_degree, method = "spearman"))
# with(data = d_marks_FYUG_final, cor.test(New_Tariff,degree_Numeric, method = "spearman"))
# with(data = d_marks_FYUG_final, cor.test(New_Tariff,`degree_Social studies`, method = "spearman"))
# 

# Figure: FY marks on UG marks
plot_fy_ug <- ggplot(d_marks_FYUG_final, 
                     aes(x = overall_marks_FY, y = overall_marks_degree )) +
  geom_point(aes(colour = FY_startyear), size=2) +
  #  geom_line(aes(y = predicted)) +
  geom_smooth(method = lm, se = FALSE, level = 0.95, fullrange = TRUE, colour = 'black') +
  coord_cartesian(ylim=c(30,70), xlim = c(40,80)) +
  labs(title = "Individual student FY performance and UG performance", colour = "Cohort") +
  ylab("Student average UG mark (%)") +
  xlab("Student average FY mark (%)")
plot_fy_ug

#hist(d_marks_FYUG_final$New_Tariff, breaks = 20)

# ==========================================================================================
# COHORTS WHO'VE GRADUATED COMPARISON ONLY
d_grad <- d_ %>% 
  filter(ACRONYM != "FY", STATUS_DESCRIPTION == "Awarded", 
         Award_Year %in% c("18/19","19/20")) %>%
  select(ID_anonym, FY, FY_startyear, stream, Award_Class, Award_Year, New_Tariff, SCHOOLYEAR, OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE) %>%
  pivot_wider(names_from = MODULE_TYPE, 
              values_from = OVERALL_MARKS_MODULE)

# Compare DE and FY students for years in question
d_grad %>% select(ID_anonym, FY, Award_Year, Award_Class) %>% distinct() %>% count(Award_Year, FY, Award_Class)

# ... get breakdown by FY cohort only
d_grad %>% filter(FY == "FY") %>%
  select(ID_anonym, FY_startyear, Award_Year, Award_Class) %>% distinct() %>% 
  count(FY_startyear, Award_Class, Award_Year)

#View(d_grad %>% filter(FY == "FY"))

# d_grad_export <- d_grad %>% filter(FY == "FY") %>% select(ID_anonym, FY_startyear, Award_Year, Award_Class) %>% distinct()
# write.xlsx(d_grad_export,'output/d_grad_export.xlsx',row.names = F) 


d_grad_final <- d_grad %>%
  group_by(ID_anonym, FY, New_Tariff) %>% 
    summarise( 
      marks_overall = mean(OVERALL_MARKS, na.rm = TRUE),
      marks_numeric = mean(Numeric, na.rm = TRUE),
      marks_social = mean(`Social studies`, na.rm = TRUE),
      marks_unkown = mean(Unknown, na.rm = TRUE),
      marks_other = mean(Other, na.rm = TRUE),
      tariff = mean(New_Tariff, na.rm = TRUE)
      ) %>% 
  ungroup()

# Check no duplicates
length(d_grad_final$ID_anonym)
length(unique(d_grad_final$ID_anonym))

d_grad_final %>% count(FY)
#View(d_ %>% filter(ID_anonym %in% d_grad_final$ID_anonym))

d_grad_final_sum <- d_grad_final %>%
  group_by(FY) %>%
    summarise(
      sum = n(),
      marks_overall = mean(marks_overall),
      marks_numeric = mean(marks_numeric, na.rm = TRUE),
      marks_social = mean(marks_social, na.rm = TRUE),
      marks_unkown = mean(marks_unkown, na.rm = TRUE),
      marks_other = mean(marks_other, na.rm = TRUE),
      tariff = mean(tariff, na.rm = TRUE)
    )

## Here, I can look at the award marks and use to compare with DE students, proportion getting a certain result
## But need to confirm that the award corresponds to what we have on file already

# In occstudents dataset: use STATUS_DESCRIPTION == Awarded (or check for All Work Submitted)
# In Johnstone dataset: use Award_Class & Award_Year



# d_grad_final <- d_ %>% 
#   filter(ACRONYM != "FY", UG_startyear == "2016-2017", STATUS_DESCRIPTION == "Awarded") %>%
#   select(ID_anonym, FY, New_Tariff, SCHOOLYEAR, OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE) %>%
#   pivot_wider(names_from = MODULE_TYPE, 
#               values_from = OVERALL_MARKS_MODULE) %>%
#   group_by(ID_anonym, FY, New_Tariff) %>% 
#   summarise( 
#     marks_overall = mean(OVERALL_MARKS, na.rm = TRUE),
#     marks_numeric = mean(Numeric, na.rm = TRUE),
#     marks_social = mean(`Social studies`, na.rm = TRUE),
#     marks_unkown = mean(Unknown, na.rm = TRUE),
#     marks_other = mean(Other, na.rm = TRUE),
#     tariff = mean(New_Tariff, na.rm = TRUE)
#   ) %>% 
#   ungroup() 


# ==========================================================================================
# ATTENDANCE - LECTURES

# Remove duplicates present from multiple modules by year
d_attend_lec <- d_ %>% 
  filter(ACRONYM != "FY" & !(ID_anonym %in% c("45465811","45475992"))) %>% 
  select(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, LECATTEND_AVE) %>% distinct()

d_attend_lec %>% select(ID_anonym, FY, FY_startyear, UG_startyear) %>% distinct() %>% count(FY) 
d_attend_lec %>% select(ID_anonym, FY, FY_startyear, UG_startyear) %>% distinct() %>% count(FY, UG_startyear) 

# Get one average lecture attendance per student (i.e. collapse )
d_attend_lec_collapse <- d_attend_lec %>%
  group_by(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, UG_startyear) %>% 
  summarise(mean_attend_lec = mean(LECATTEND_AVE, na.rm = TRUE)) %>% ungroup()

d_attend_lec_collapse %>% count(FY) 

d_attend_lec_overall <- d_attend_lec_collapse %>% 
  group_by(UG_startyear, FY) %>% 
  summarise(
    attend_lec_overall_mean = mean(mean_attend_lec, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# Plot overall lecture attendance
plot_attend_lec_overall <-  ggplot(
  d_attend_lec_collapse, aes(x=FY, y=mean_attend_lec, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  ylab("Average lecture attendance (%)") +
  xlab("Student entry type") +
  ggtitle("Average UG Overall lecture attendance across all modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attend_lec_overall


# ----------------------------------------------------------------------------------------
# BY MODULES

# Remove duplicates present from multiple modules by year
d_attend_lec_modules <- d_ %>% 
  filter(ACRONYM != "FY" & !(ID_anonym %in% c("45465811","45475992"))) %>% 
  select(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, MODULE_TYPE, LECATTEND_AVE_MODULE) %>% distinct()

# Get one average mark per student (i.e. collapse )
d_attend_lec_modules_collapse <- d_attend_lec_modules %>%
  group_by(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, UG_startyear, MODULE_TYPE) %>% 
  summarise( 
    mean_attend_lec = mean(LECATTEND_AVE_MODULE, na.rm = TRUE)
  ) %>% ungroup()

# Plot attendance in numeric-related lectures
plot_attendance_lec_numeric <-  ggplot(
  d_attend_lec_modules_collapse %>% filter(MODULE_TYPE == "Numeric"),
  aes(x=FY, y=mean_attend_lec, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -4) +
#  ylab("Average attendance (proportion)") +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)",  breaks = seq(0,1,0.2)) +
  xlab("Student entry type") +
  ggtitle("Average Lecture attendance - Numeric modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attendance_lec_numeric
with(data = d_attend_lec_modules_collapse %>% filter(MODULE_TYPE == "Numeric"), wilcox.test(mean_attend_lec ~ FY, paired = FALSE))

# Plot attendance in social sciences-related lectures
plot_attendance_lec_social <-  ggplot(
  d_attend_lec_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), 
  aes(x=FY, y=mean_attend_lec, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -4) +
#  ylab("Average attendance (proportion)") +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)",  breaks = seq(0,1,0.2)) +
  xlab("Student entry type") +
  ggtitle("Average Lecture attendance - Social studies modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attendance_lec_social
with(data = d_attend_lec_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), wilcox.test(mean_attend_lec ~ FY, paired = FALSE))

# # CORRELATIONS: Overall lecture attendance vs Overall marks (UG)
# with(data = d_ , cor.test(OVERALL_MARKS.UG., ATTENDANCE_MEAN.UG., method = "spearman"))
# 
# # Overall attendance and overall marks
# with(data = d_ %>% filter(FY == "FY"), cor.test(OVERALL_MARKS.UG., ATTENDANCE_MEAN.UG., method = "spearman"))
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(OVERALL_MARKS.UG., ATTENDANCE_MEAN.UG., method = "spearman"))
# 
# # Numeric attendance and marks
# with(data = d_ %>% filter(FY == "FY"), cor.test(Lecture_Numerical_mean_marks.UG., Lecture_Numerical_attendance_count.UG., method = "spearman"))
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(Lecture_Numerical_mean_marks.UG., Lecture_Numerical_attendance_count.UG., method = "spearman"))
# 
# # Non-numeric attendance and marks
# with(data = d_ %>% filter(FY == "FY"), cor.test(Lecture_Social_studies_mean_marks.UG., Lecture_Social_studies_count.UG., method = "spearman"))
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(Lecture_Social_studies_mean_marks.UG., Lecture_Social_studies_count.UG., method = "spearman"))


# ==========================================================================================
# ATTENDANCE - SEMINARS

# Remove duplicates present from multiple modules by year
d_attend_sem <- d_ %>% 
  filter(ACRONYM != "FY" & !(ID_anonym %in% c("45465811","45475992"))) %>% 
  select(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, SEMATTEND_AVE) %>% distinct()
d_attend_sem %>% select(ID_anonym, FY, FY_startyear, UG_startyear) %>% distinct() %>% count(FY) 

# Get one average seminar attendance per student (i.e. collapse )
d_attend_sem_collapse <- d_attend_sem %>%
  group_by(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, UG_startyear) %>% 
  summarise(mean_attend_sem = mean(SEMATTEND_AVE, na.rm = TRUE)) %>% ungroup()

d_attend_sem_collapse %>% count(FY) 

d_attend_sem_overall <- d_attend_sem_collapse %>% 
  group_by(UG_startyear, FY) %>% 
  summarise(
    attend_sem_overall_mean = mean(mean_attend_sem, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# Plot overall seminar attendance
plot_attend_sem_overall <-  ggplot(
  d_attend_sem_collapse, aes(x=FY, y=mean_attend_sem, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +                  
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  xlab("Student entry type") +
  ggtitle("Average UG Overall seminar attendance across all modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attend_sem_overall

# ----------------------------------------------------------------------------------------
# BY MODULE

# Remove duplicates present from multiple modules by year
d_attend_sem <- d_ %>% 
  filter(ACRONYM != "FY" & !(ID_anonym %in% c("45465811","45475992"))) %>% 
      select(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, FY_startyear, UG_startyear, SCHOOLYEAR, MODULE_TYPE, SEMATTEND_AVE_MODULE) %>% distinct()

# Get one average mark per student (i.e. collapse )
d_attend_sem_modules_collapse <- d_attend_sem %>%
  group_by(ID_anonym, Gender, Ethnicity, FY, ACRONYM, stream, UG_startyear, MODULE_TYPE) %>% 
  summarise( 
    mean_attend_sem = mean(SEMATTEND_AVE_MODULE, na.rm = TRUE)
  ) %>% ungroup()

# Plot attendance in numeric-related seminars
plot_attendance_sem_numeric <-  ggplot(
  d_attend_sem_modules_collapse %>% filter(MODULE_TYPE == "Numeric"),
  aes(x=FY, y=mean_attend_sem, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
#  ylab("Average attendance (proportion)") +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  xlab("Student entry type") +
  ggtitle("Average Seminar attendance - Numeric modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attendance_sem_numeric
wilcox.test(mean_attend_sem ~ FY, data = d_attend_sem_modules_collapse %>% filter(MODULE_TYPE == "Numeric"), paired = FALSE)

# Plot attendance in non-numeric-related seminars
plot_attendance_sem_social <-  ggplot(
  d_attend_sem_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), 
  aes(x=FY, y=mean_attend_sem, fill = FY)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
#  ylab("Average attendance (proportion)") +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  xlab("Student entry type") +
  ggtitle("Average Seminar attendance - Social studies modules") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_attendance_sem_social
wilcox.test(mean_attend_sem ~ FY, data = d_attend_sem_modules_collapse %>% filter(MODULE_TYPE == "Social studies"), paired = FALSE)

# d_ <- d_ %>% 
#   mutate(seminar_overall = (Seminar_Numerical_attendance_count.UG. + Seminar_Social_studies_count.UG.)/2
#            )
# 
# # Overall attendance and overall marks
# with(data = d_ , cor.test(OVERALL_MARKS.UG., seminar_overall, method = "spearman"))
# with(data = d_ , cor.test(Lecture_Numerical_mean_marks.UG., Seminar_Numerical_attendance_count.UG., method = "spearman"))
# with(data = d_ , cor.test(Lecture_Social_studies_mean_marks.UG., Seminar_Social_studies_count.UG., method = "spearman"))
# 
#  with(data = d_ %>% filter(FY == "FY"), cor.test(OVERALL_MARKS.UG., seminar_overall, method = "spearman"))
#  with(data = d_ %>% filter(FY == "Direct entry"), cor.test(OVERALL_MARKS.UG., seminar_overall, method = "spearman"))
# 
# # Numeric attendance and marks
# with(data = d_ %>% filter(FY == "FY"), cor.test(Lecture_Numerical_mean_marks.UG., Seminar_Numerical_attendance_count.UG., method = "spearman"))
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(Lecture_Numerical_mean_marks.UG., Seminar_Numerical_attendance_count.UG., method = "spearman"))
# 
# # Non-numeric attendance and marks
# with(data = d_ %>% filter(FY == "FY"), cor.test(Lecture_Social_studies_mean_marks.UG., Seminar_Social_studies_count.UG., method = "spearman"))
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(Lecture_Social_studies_mean_marks.UG., Seminar_Social_studies_count.UG., method = "spearman"))
# 

# ==========================================================================================
# NUMBER OF MITIGATING CIRCUMSTANCES

d_mit <- d_ %>%
  filter(
    ACRONYM != "FY" & !(ID_anonym %in% c("45465811","45475992")),
    stream != "non-WBS"
    ) %>% 
  mutate(
    count_mit_ave = ifelse(UG_startyear == "2016-2017", COUNT_MITIGATING_CIRCUMSTANCES/6,
                           ifelse(UG_startyear == "2017-2018", COUNT_MITIGATING_CIRCUMSTANCES/5,
                                  ifelse(UG_startyear == "2018-2019",COUNT_MITIGATING_CIRCUMSTANCES/4, 
                                         ifelse(UG_startyear == "2019-2020",COUNT_MITIGATING_CIRCUMSTANCES/3,
                                                ifelse(UG_startyear == "2020-2021",COUNT_MITIGATING_CIRCUMSTANCES/2,
                                                       ifelse(UG_startyear == "2021-2022",COUNT_MITIGATING_CIRCUMSTANCES, 999))))))
  ) %>% 
  select(ID_anonym, UG_startyear, FY, student_type, Ethnicity_collapse, stream, COUNT_MITIGATING_CIRCUMSTANCES, count_mit_ave) %>% 
  distinct() %>%
  mutate(student_type_coll = ifelse(student_type %in% c("DE - WP students", "FY"), "FY / WP students", "DE students"))

  d_mit %>% count(UG_startyear)
  d_mit %>% count(student_type, UG_startyear)

  d_mit %>% count(student_type)
  d_mit %>% count(student_type_coll)
  
  length(d_mit$ID_anonym)
  length(unique(d_mit$ID_anonym))
  
  summary(d_mit$count_mit_ave)

d_mitigating_overall <- d_mit %>% 
  group_by(student_type) %>% 
  summarise(
    mitigating_mean = mean(count_mit_ave, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

d_mitigating_overall_year <- d_mit %>% 
  group_by(UG_startyear, student_type) %>% 
  summarise(
    mitigating_mean = mean(count_mit_ave, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

chisq.test(d_mit$count_mit_ave, d_mit$student_type, correct = TRUE)

 d_mit <- d_mit %>%
   mutate(
     student_type = fct_relevel(student_type,"DE students", "DE - WP students", "FY"),
     student_type_coll = fct_relevel(student_type_coll,"DE students", "FY / WP students")
          
          ) 

# Plot mitigating factors
plot_mitigating_overall <-  ggplot(
  d_mit, aes(x=student_type, y=count_mit_ave, fill = student_type)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -7) +                  
  ylab("Average number of mitigating factors per year") +
  xlab("Student entry type") +
  ggtitle("Mitigating factors, by student type") +
  coord_cartesian(ylim=c(0,0.6)) +
  theme_minimal(12) +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#3399FF", "#E69F00", "#006633"))
plot_mitigating_overall

plot_mitigating_year <-  ggplot(
  d_mit, aes(x=UG_startyear, y=count_mit_ave, fill = student_type)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -7) +                  
  ylab("Average number of mitigating factors per year") +
  xlab("UG startyear") +
  ggtitle("Mitigating factors, by student type and UG start year") +
  coord_cartesian(ylim=c(0,0.8)) +
  theme_minimal(12) +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#3399FF", "#E69F00", "#006633")) +
  theme(legend.position = "bottom", legend.title = element_blank())
plot_mitigating_year

  d_mit %>% count(Ethnicity_collapse)
  d_mit %>% count(student_type)
  
plot_mitigating_eth <-  ggplot(
  d_mit %>% filter(Ethnicity_collapse %in% 
                     c("Arab", "Asian", "Black", "Chinese", "Mixed", "White")),
  aes(x=Ethnicity_collapse, y=count_mit_ave, fill = Ethnicity_collapse)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -7) +                  
  ylab("Average number of mitigating factors per year") +
  xlab("Student ethnicity type") +
  ggtitle("Mitigating factors, by student ethnicity") +
  coord_cartesian(ylim=c(0,0.6)) +
  theme_minimal(12) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")
plot_mitigating_eth


plot_mitigating_eth_2 <-  ggplot(
  d_mit %>% filter(Ethnicity_collapse %in% 
    c("Arab", "Asian", "Black", "Chinese", "Mixed", "White")),
  aes(x=Ethnicity_collapse, y=count_mit_ave, fill = student_type_coll)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -7) +                  
  ylab("Average number of mitigating factors per year") +
  xlab("Student ethnicity type") +
  ggtitle("Mitigating factors, by student ethnicity and category") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2")
plot_mitigating_eth_2


plot_mitigating_stream <-  ggplot(
  d_mit, aes(x=stream, y=count_mit_ave, fill = student_type_coll)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -7) +                  
  ylab("Average number of mitigating factors per year") +
  xlab("Stream") +
  ggtitle("Mitigating factors, by stream and student type") +
  coord_cartesian(ylim=c(0,1.2)) +
  theme_minimal(12) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2")
  # scale_fill_manual(values=c("#3399FF", "#E69F00", "#006633"))
plot_mitigating_stream

# 
# # with(data = d_, cor.test(COUNT_MITIGATING_CIRCUMSTANCES,OVERALL_MARKS.UG., method = "spearman"))
# # with(data = d_, cor.test(COUNT_MITIGATING_CIRCUMSTANCES,ATTENDANCE_MEAN.UG., method = "spearman"))
# # with(data = d_, cor.test(COUNT_MITIGATING_CIRCUMSTANCES,seminar_overall, method = "spearman"))
# 
# # Corr: mitigating x marks OVERALL - FY students
# with(data = d_ %>% filter(FY == "FY"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,OVERALL_MARKS.UG., method = "spearman"))
# # Corr: mitigating x marks OVERALL - DE students
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,OVERALL_MARKS.UG., method = "spearman"))
# 
# # Corr: mitigating x attendance OVERALL - FY students
# with(data = d_ %>% filter(FY == "FY"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,ATTENDANCE_MEAN.UG., method = "spearman"))
# # Corr: mitigating x attendance OVERALL - DE students
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,ATTENDANCE_MEAN.UG., method = "spearman"))
# 
# aov_mit <- lm(ATTENDANCE_MEAN.UG. ~ COUNT_MITIGATING_CIRCUMSTANCES, data = d_ %>% filter(FY == "FY"))
# summary(aov_mit)
# 
# 
# # Similar impact on FY and DE wrt to mitigating circumstances and attendance, though FY not sig (low power)
# 
# # Corr: mitigating x attendance NUMERIC - FY students
# with(data = d_ %>% filter(FY == "FY"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,Lecture_Numerical_attendance_count.UG., method = "spearman"))
# # Corr: mitigating x attendance NUMERIC - DE students
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,Lecture_Numerical_attendance_count.UG., method = "spearman"))
# 
# # Corr: mitigating x attendance SOCIAL - FY students
# with(data = d_ %>% filter(FY == "FY"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,Lecture_Social_studies_count.UG., method = "spearman"))
# # Corr: mitigating x attendance SOCIAL - DE students
# with(data = d_ %>% filter(FY == "Direct entry"), cor.test(COUNT_MITIGATING_CIRCUMSTANCES,Lecture_Social_studies_count.UG., method = "spearman"))
# 
# # --> only valid in social sciences, though findings might be because most FY students who have mitigating factors are in Management program with social studies
# 
# 


# ==========================================================================================
# WIDENING PARTICIPATION => WP_INDEX
d_WPindex <- raw_data_wp

## WP Index and FY marks
d_FYonly <- raw_data %>%
  select(ID_anonym, Gender, Ethnicity, FY_ind, ACRONYM, stream, New_Tariff, STAGE_DESCRIPTION, FY_startyear, UG_startyear, SCHOOLYEAR, timestamp, OVERALL_MARKS)

d_FYonly <- d_FYonly %>% filter(STAGE_DESCRIPTION != "Left")
  d_FYonly %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear, STAGE_DESCRIPTION) %>% distinct() %>% count(FY_startyear)
d_FYonly <- d_FYonly %>% filter(ACRONYM == "FY") %>% select(ID_anonym, FY_startyear, OVERALL_MARKS, STAGE_DESCRIPTION) %>% distinct()
d_FYonly %>% count(FY_startyear)

# Only look at marks of those who have completed their FY year
d_FYonly <- d_FYonly %>% filter(STAGE_DESCRIPTION == "Completed")

d_FYonly_wp <- left_join(d_FYonly, d_WPindex)  %>% 
  filter(FY_startyear != "2015-2016") # filter out, given poor data during this year
d_FYonly_wp %>% count(FY_startyear) # Note current year removed (through stage description above) and 2015/16 year removed too
# total: 91 students

plot_wpindex_marks_FY <-  ggplot(
  d_FYonly_wp, 
  aes(x=wp_index, y=OVERALL_MARKS)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average FY year marks (%)") +
  scale_x_discrete(name = "Widening Participation Index") +
  ggtitle("Average Foundation Year marks by WP Index") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_marks_FY

d_FYonly_wp %>% group_by(wp_index) %>% summarise(mean = mean(OVERALL_MARKS))

plot_wpindex_marks_FY <-  ggplot(
  d_FYonly_wp, 
  aes(x=wp_sum_with_awards, y=OVERALL_MARKS)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  #  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average FY year marks (%)") +
  scale_x_continuous(name = "Widening Participation Index") +
  ggtitle("Average Foundation Year marks by WP Index") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_marks_FY


# ----------------------------------------------------------------------------------------
## WP Index and UG marks
# Limited to the 29 students under consideration from earlier, minus 2015-2016 cohort who are missing data
d_marks_wpindex <- left_join(d_marks_FYUG_final, d_WPindex)  %>% filter(FY_startyear != "2015-2016")
length(d_marks_wpindex$ID_anonym)

plot_wpindex_marks_degree <-  ggplot(
  d_marks_wpindex , 
  aes(x=wp_index, y=overall_marks_degree)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average undergraduate marks (%)") +
#  scale_x_continuous(name = "Widening Participation Index",  breaks = seq(0,5,1)) +
  scale_x_discrete(name = "Widening Participation Index") +
  ggtitle("Average undergraduate degree marks by WP Index") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_marks_degree

plot_wpindex_marks_degree <-  ggplot(
  d_marks_wpindex , 
  aes(x=wp_index, y=overall_marks_degree)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average undergraduate marks (%)") +
#  scale_x_continuous(name = "Widening Participation Index",  breaks = seq(0,5,1)) +
  scale_x_discrete(name = "Widening Participation Index") +
  ggtitle("Average undergraduate degree marks by WP Index") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_marks_degree

### WP Index and attendance
d_attend_lec_FY <- d_attend_lec_collapse %>% filter(FY == "FY") %>% filter(UG_startyear != "2016-2017")
d_attend_lec_wpindex <- left_join(d_attend_lec_FY, d_WPindex)
d_attend_lec_FY %>% count(UG_startyear) # 29 students

d_attend_sem_FY <- d_attend_sem_collapse %>% filter(FY == "FY") %>% filter(UG_startyear != "2016-2017")
d_attend_sem_wpindex <- left_join(d_attend_sem_FY, d_WPindex)
d_attend_sem_FY %>% count(UG_startyear) # 29 students

plot_wpindex_attend_lec <-  ggplot(
  d_attend_lec_wpindex, 
  aes(x=wp_sum_with_awards, y=mean_attend_lec)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Widening Participation Index",  breaks = seq(0,5,1)) +
  ggtitle("Lecture attendance during undergraduate degree by WP Index") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_attend_lec

plot_wpindex_attend_sem <-  ggplot(
  d_attend_sem_wpindex %>% filter(UG_startyear != "2016-2017"), 
  aes(x=wp_sum_with_awards, y=mean_attend_sem)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "Widening Participation Index",  breaks = seq(0,5,1)) +
  ggtitle("Seminar attendance in undergraduate degree by WP Index") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_attend_sem

## WP Index and Mitigating Circumstances
d_mit_FY <- d_mit %>% filter(FY == "FY")
d_mit_wpindex <- left_join(d_mit_FY, d_WPindex) %>% 
  filter(UG_startyear != "2016-2017")
d_mit_wpindex %>% count(UG_startyear) # 29 students

plot_wpindex_mit <-  ggplot(
  d_mit_wpindex, 
  aes(x=wp_index, y=count_mit_ave, fill = wp_index)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -6) +
  ylab("Average no. mitigating circumstances per year of undergrad") +
  scale_x_discrete(name = "Number of Widening Participation Criteria") +
  ggtitle("Mitigating circumstances per undergraduate year by WP level") +
  coord_cartesian(ylim=c(0,0.8)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_wpindex_mit


# ----------------------------------------------------------------------------------------
### IMD IMD IMD IMD

plot_IMD_marks_degree <-  ggplot(
  d_marks_wpindex , 
  aes(x=IMD_Quintile, y=overall_marks_degree)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average undergraduate marks (%)") +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Average undergraduate degree marks by WP Index") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_marks_degree

plot_IMD_attend_lec <-  ggplot(
  d_attend_lec_wpindex %>% filter(UG_startyear != "2016-2017"), 
  aes(x=IMD_Quintile, y=mean_attend_lec)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Lecture attendance in undergraduate degree by WP Index") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_attend_lec

plot_IMD_attend_sem <-  ggplot(
  d_attend_sem_wpindex %>% filter(UG_startyear != "2016-2017"), 
  aes(x=IMD_Quintile, y=mean_attend_sem)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Seminar attendance in undergraduate degree by WP Index") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_attend_sem

plot_IMD_mit <-  ggplot(
  d_mit_wpindex, 
  aes(x=IMD_Quintile, y=count_mit_ave)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  ylab("Average no. mitigating circumstances per year of undergrad") +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Mitigating circumstances per year by WP Index") +
  coord_cartesian(ylim=c(0,0.8)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_mit

# ==========================================================================================
# IMD for all students

path_johnstone <- file.path("data_files", "WBS_TK_Extract_20201202_anonymised.xlsx")
d_johnstone <- read_excel(path_johnstone)
d_IMD <-d_johnstone %>% select(
  ID_anonym,
  IMD_Quintile,
#  Polar3_Young_HE_Participation_Quintile, POLAR4_Young_HE_Participation_Quintile,
#  LowSEC_Flag,LPN_Quintile,
#  WP_ALP_Flag, WP_FSM_Flag, WP_GCSE_Flag,
#  InCare_Flag, ParentInHE_Flag,
) %>% 
  mutate(IMD_hilo = ifelse(is.na(IMD_Quintile), "N/A", 
                           ifelse(IMD_Quintile <=2, "Low IMD Quintile (1 & 2)", "High IMD Quintile (3-5)"))
  )
         
d_IMD$IMD_hilo <- factor(d_IMD$IMD_hilo, levels=c("Low IMD Quintile (1 & 2)", "High IMD Quintile (3-5)"))

# IMD and mitigating circumstances
d_mit_IMD <- d_mit %>% left_join(d_IMD)
d_mit_IMD %>% count(FY, IMD_Quintile)
# 963 / 1240 students missing data; analysis only on 277 students

## By IMD Quintile
plot_IMD_mit <-  ggplot(
  d_mit_IMD, 
  aes(x=IMD_Quintile, y=count_mit_ave)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -5) +
  ylab("Average no. mitigating circumstances per year of undergrad") +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Mitigating circumstances per year by IMD Quntile") +
  coord_cartesian(ylim=c(0,0.6)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_mit

# IMD and UG marks
d_marks_IMD <- d_marks_UG_collapse %>% left_join(d_IMD)
d_marks_IMD %>% count(FY, IMD_Quintile)

plot_IMD_marks_degree <-  ggplot(
  d_marks_IMD , 
  aes(x=IMD_Quintile, y=mean_marks)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average undergraduate marks (%)") +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Average undergraduate degree marks by IMD Quintile") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_marks_degree

# IMD and lecture attendance
d_lec_IMD <- d_attend_lec_collapse %>% left_join(d_IMD)
d_lec_IMD %>% count(FY, IMD_Quintile)

plot_IMD_attend_lec <-  ggplot(
  d_lec_IMD, 
  aes(x=IMD_Quintile, y=mean_attend_lec)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Lecture attendance in undergraduate degree by WP Index") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_attend_lec

# IMD and seminar attendance
d_sem_IMD <- d_attend_sem_collapse %>% left_join(d_IMD)
d_sem_IMD %>% count(FY, IMD_Quintile)

plot_IMD_attend_sem <-  ggplot(
  d_sem_IMD, 
  aes(x=IMD_Quintile, y=mean_attend_sem)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -2) +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)",  breaks = seq(0,1,0.2)) +
  scale_x_continuous(name = "IMD Quintile",  breaks = seq(0,5,1)) +
  ggtitle("Seminar attendance in undergraduate degree by IMD Quintile") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  theme(legend.position = "none")
plot_IMD_attend_sem
                             

## By IMD Level
# ------

# ... Mitigating circumstances
plot_mit_IMD_hilo <-  ggplot(
  d_mit_IMD %>% filter(IMD_hilo != "N/A"), 
  aes(x=FY, y=count_mit_ave, fill = IMD_hilo)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  scale_y_continuous(name = "Average no. mitigating circumstances per year of undergrad") +
  scale_x_discrete(name = "Student entry type") +
  ggtitle("Mitigating circumstances per year by IMD Quntile") +
  coord_cartesian(ylim=c(0,0.5)) +
  theme_minimal(12) +
  labs(fill = "IMD Indicator") 
plot_mit_IMD_hilo
d_mit_IMD %>% filter(IMD_hilo != "N/A") %>% count(FY)

# ... Overall marks
plot_marks_IMD_hilo <-  ggplot(
  d_marks_IMD %>% filter(IMD_hilo != "N/A"), 
  aes(x=FY, y=mean_marks, fill = IMD_hilo)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,0)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  scale_y_continuous(name = "Average undergraduate marks (%)") +
  scale_x_discrete(name = "Student entry type") +
  ggtitle("Average undergraduate degree marks by IMD Quintile") +
  coord_cartesian(ylim=c(0,80)) +
  theme_minimal(12) +
  labs(fill = "IMD Indicator") 
plot_marks_IMD_hilo
d_marks_IMD %>% filter(IMD_hilo != "N/A") %>% count(FY)


# ... Lecture attendance
plot_lec_IMD_hilo <-  ggplot(
  d_lec_IMD %>% filter(IMD_hilo != "N/A"), 
  aes(x=FY, y=mean_attend_lec, fill = IMD_hilo)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  scale_y_continuous(name = "Average attendance (proportion of all lectures attended)") +
  scale_x_discrete(name = "Student entry type") +
  ggtitle("Lecture attendance in undergraduate degree by IMD Quintile") +
  coord_cartesian(ylim=c(0,0.8)) +
  theme_minimal(12) + 
  labs(fill = "IMD Indicator") 
plot_lec_IMD_hilo
d_lec_IMD %>% filter(IMD_hilo != "N/A") %>% count(FY)


# ... Seminar attendance
plot_sem_IMD_hilo <-  ggplot(
  d_sem_IMD %>% filter(IMD_hilo != "N/A"), 
  aes(x=FY, y=mean_attend_sem, fill = IMD_hilo)) + 
  stat_summary(fun = mean, geom='bar', width=0.5, position=position_dodge(0.5)) +
#  stat_summary(fun.data = mean_se, geom='errorbar', position=position_dodge(0.5), width=0.2) +
  stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", position=position_dodge(0.5), vjust = -3) +
  scale_y_continuous(name = "Average attendance (proportion of all seminars attended)") +
  scale_x_discrete(name = "Student entry type") +
  ggtitle("Seminar attendance in undergraduate degree by IMD Quintile") +
  coord_cartesian(ylim=c(0,1)) +
  theme_minimal(12) +
  labs(fill = "IMD Indicator") 
plot_sem_IMD_hilo
d_sem_IMD %>% filter(IMD_hilo != "N/A") %>% count(FY)

plot_mit_IMD_hilo
plot_marks_IMD_hilo
plot_lec_IMD_hilo
plot_sem_IMD_hilo

# ==========================================================================================
# VARIOUS REGRESSIONS

# Merge collapsed datasets for analysis
d_marks <- d_marks_UG_collapse %>% select(ID_anonym, FY, UG_startyear, mean_marks, Gender, stream)
d_marks %>% count(FY)

d_attend_lec <- d_attend_lec_collapse %>% select(ID_anonym, FY, UG_startyear, mean_attend_lec)
d_attend_lec %>% count(FY)

d_attend_sem <- d_attend_sem_collapse %>% select(ID_anonym, FY, UG_startyear, mean_attend_sem)
d_attend_sem %>% count(FY)

d_mitigating <- d_mit %>% select(-COUNT_MITIGATING_CIRCUMSTANCES)


d_merge <- left_join(d_marks, d_attend_lec) %>% 
  left_join(d_attend_sem) %>% 
  left_join(d_mitigating)

d_merge %>% count(FY) 


# Analysis
m_marks_attend <- lm(mean_marks ~  mean_attend_lec +  mean_attend_sem + FY, 
                     data = d_merge)
  summary(m_marks_attend)
  anova(m_marks_attend)
  effectsize::cohens_f(m_marks_attend)


# Mitigating circumstances and outcomes
m_marks_mit <- lm(mean_marks ~  count_mit_ave * FY, data = d_merge)
  summary(m_marks_mit)
  anova(m_marks_mit)
  effectsize::cohens_f(m_marks_mit)

m_lec_mit <- lm(mean_attend_lec ~  count_mit_ave * FY, data = d_merge)
  summary(m_lec_mit)
  anova(m_lec_mit)
  effectsize::cohens_f(m_lec_mit)

m_sem_mit <- lm(mean_attend_sem ~  count_mit_ave * FY, data = d_merge)
  summary(m_sem_mit)
  anova(m_sem_mit)
  effectsize::cohens_f(m_sem_mit)

# Note: effect size for this is larger than effect size for FY


# All built in for regression on all students, with FY as dummy
  m_marks_reg <- lm(mean_marks ~  mean_attend_lec + mean_attend_sem + count_mit_ave + FY, 
                       data = d_merge)
  summary(m_marks_reg)
  anova(m_marks_reg)
  effectsize::cohens_f(m_marks_reg)
  
  
# Main regression for FY on UG marks
  d_fyug <- d_merge %>% filter(FY == "FY")
  d_fy_temp <-d_marks_FYUG_final %>% select(ID_anonym, New_Tariff, overall_marks_FY, `degree_Social studies`, degree_Numeric)
  
  d_merge_fyug <- left_join(d_fyug, d_fy_temp) 
  
# Overall
   m_fy_ug_full <- lm(mean_marks ~ overall_marks_FY + New_Tariff 
                     + mean_attend_lec + mean_attend_sem + count_mit_ave
                     + Gender + stream, 
                     data = d_merge_fyug)
  summary(m_fy_ug_full)
  anova(m_fy_ug_full)
  effectsize::cohens_f(m_fy_ug_full)
  
  
# Numeric
  m_fy_ug_full_num <- lm(degree_Numeric ~ overall_marks_FY + New_Tariff 
                         + mean_attend_lec + mean_attend_sem + count_mit_ave 
                         + Gender + stream, 
                         data = d_merge_fyug)
  summary(m_fy_ug_full_num)
  anova(m_fy_ug_full_num)
  effectsize::cohens_f(m_fy_ug_full_num)

# Social studies
  m_fy_ug_full_soc <- lm(`degree_Social studies` ~ overall_marks_FY + New_Tariff 
                     + mean_attend_lec + mean_attend_sem + count_mit_ave
                     + Gender + stream, 
                     data = d_merge_fyug)
  summary(m_fy_ug_full_soc)
  anova(m_fy_ug_full_soc)
  effectsize::cohens_f(m_fy_ug_full_soc)
  

# ==========================================================================================
# WIDENING PARTICIPATION STUDENT OUTCOMES BREAKDOWN

### NOTE CHECK WHICH YEARS ARE INCLUDED IN COMPARISON YEARS

# Import WP indicators and groups 
path_wpind <- file.path("output", "d_wp_export.csv")
raw_data_wp <- read.csv(file = path_wpind ,header = T, stringsAsFactors = F) 

## Checking data

# Are WS scholars in the raw data?
test <- raw_data %>% filter(ID_anonym %in% (raw_data_wp %>% filter(ind_s == 1))$ID_anonym)
length(unique(test$ID_anonym)) # all 11 there
test_s <- test %>% select(ID_anonym, FY_startyear, UG_startyear, FY_ind)
length(unique(test_s$ID_anonym)) # all 11 there
test_s2 <- test %>% select(ID_anonym, FY_startyear, UG_startyear, FY_ind) %>% distinct()
length(unique(test_s2$ID_anonym)) # all 11 there

# How many WS are not FY?
test_2 <- raw_data %>% filter(FY_ind == 0 & (ID_anonym %in% (raw_data_wp %>% filter(ind_s == 1))$ID_anonym))
length(unique(test_2$ID_anonym)) # 6 students are WS but not FY
test_2 %>% select(ID_anonym, UG_startyear) %>% distinct() %>% count(UG_startyear) # 2 (19/20), 4 (20/21)

# How many contextual offers are there?
test_3 <- raw_data %>% filter((ID_anonym %in% (raw_data_wp %>% filter(ind_c == 1))$ID_anonym))
length(unique(test_3$ID_anonym)) # 39 contextual, but not WS; 7 WS not on contextual list
test_3 %>% select(ID_anonym, UG_startyear) %>% distinct() %>% count(UG_startyear) # 5 (17/18), 6 (18/19), 11 (19/20), 17 (20/21)

# ----------------------------------------------------------------------------------------
# WP AND MITIGATING CIRCUMSTANCES (AVERAGE MIT CIRCS/YEAR)
# Note: excludes those who left; only includes three years 16/17 to 18/19; doesn't do anything for Home vs away

d_mit_wp_DE <- d_mit %>% left_join(raw_data_wp)

d_mit_wp_DE %>% count(ind_cs, FY)
d_mit_wp_DE %>% count(ind_c, ind_s, FY)
d_mit_wp_DE %>% count(FY, ind_cs, wp_ind)
d_mit_wp_DE %>% count(FY, ind_cs, wp_sum)
d_mit_wp_DE %>% count(ind_cs, UG_startyear)
# 20 contextual students, of which 2 are WS scholars
# 76 FY students
# 1571 DE students, of which 14 are wp_ind (@ wp_sum >= 3)

#d_mit_wp_DE <- d_mit_wp_DE %>% filter(FY == "Direct entry")

d_mit_wp_DE <- d_mit_wp_DE %>% mutate(ind_wp_cs = ifelse( FY == "Direct entry" & (!is.na(ind_cs) | wp_ind == 1), 1, 0)) 
d_mit_wp_DE %>% count(ind_wp_cs)

# Contextual only
d_mit_wp_c <- d_mit_wp_DE %>% 
  group_by(FY, ind_cs) %>% 
  summarise(
    mitigating_mean = mean(count_mit_ave, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# DE non-contextual students
d_mit_wp_DEonly <- d_mit_wp_DE %>% filter(is.na(ind_c)) %>%
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(count_mit_ave, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# All DE WP students
d_mit_wp_c_both <- d_mit_wp_DE %>% 
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(count_mit_ave, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

d_mit_wp_DE_test <- d_mit_wp_DE %>% filter(FY == "Direct entry")
chisq.test(d_mit_wp_DE_test$count_mit_ave, d_mit_wp_DE_test$ind_wp_cs, correct = TRUE)


# ----------------------------------------------------------------------------------------
# WP AND AVERAGE MARKS
# Note: excludes those who left; only includes three years 16/17 to 18/19; doesn't do anything for Home vs away
    
#d_marks  d_marks_UG_collapse

d_marks_wp_DE <- d_marks %>% left_join(raw_data_wp)

d_marks_wp_DE %>% count(ind_cs, FY)
d_marks_wp_DE %>% count(ind_c, ind_s, FY)
d_marks_wp_DE %>% count(FY, ind_cs, wp_ind)
d_marks_wp_DE %>% count(ind_cs, UG_startyear)

d_marks_wp_DE <- d_marks_wp_DE %>% mutate(ind_wp_cs = ifelse( FY == "Direct entry" & (!is.na(ind_cs) | wp_ind == 1), 1, 0)) 
d_marks_wp_DE %>% count(ind_wp_cs)


# Contextual only
d_marks_wp_c <- d_marks_wp_DE %>% 
  group_by(FY, ind_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_marks, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# DE non-contextual students
d_marks_wp_DEonly <- d_marks_wp_DE %>% filter(is.na(ind_c)) %>%
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_marks, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# All DE WP students
d_marks_wp_c_both <- d_marks_wp_DE %>% 
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_marks, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

d_marks_wp_DE_test <- d_marks_wp_DE %>% filter(FY == "Direct entry")
chisq.test(d_marks_wp_DE_test$mean_marks, d_marks_wp_DE_test$ind_wp_cs, correct = TRUE)


# ----------------------------------------------------------------------------------------
# WP AND LECTURE ATTENDANCE

d_att_lec_wp_DE <- d_attend_lec %>% left_join(raw_data_wp)

d_att_lec_wp_DE %>% count(ind_cs, FY)
d_att_lec_wp_DE %>% count(ind_c, ind_s, FY)
d_att_lec_wp_DE %>% count(FY, ind_cs, wp_ind)
d_att_lec_wp_DE %>% count(ind_cs, UG_startyear)

d_att_lec_wp_DE <- d_att_lec_wp_DE %>% mutate(ind_wp_cs = ifelse( FY == "Direct entry" & (!is.na(ind_cs) | wp_ind == 1), 1, 0)) 
d_att_lec_wp_DE %>% count(ind_wp_cs)

# Contextual only
d_att_lec_wp_c <- d_att_lec_wp_DE %>% 
  group_by(FY, ind_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_lec, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# DE non-contextual students
d_att_lec_wp_DEonly <- d_att_lec_wp_DE %>% filter(is.na(ind_c)) %>%
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_lec, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# All DE WP students
d_att_lec_wp_c_both <- d_att_lec_wp_DE %>% 
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_lec, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

d_att_lec_wp_DE_test <- d_att_lec_wp_DE %>% filter(FY == "Direct entry")
chisq.test(d_att_lec_wp_DE_test$mean_attend_lec, d_att_lec_wp_DE_test$ind_wp_cs, correct = TRUE)


# ----------------------------------------------------------------------------------------
# WP AND SEMINAR ATTENDANCE

d_att_sem_wp_DE <- d_attend_sem %>% left_join(raw_data_wp)

d_att_sem_wp_DE %>% count(ind_cs, FY)
d_att_sem_wp_DE %>% count(ind_c, ind_s, FY)
d_att_sem_wp_DE %>% count(FY, ind_cs, wp_ind)
d_att_sem_wp_DE %>% count(ind_cs, UG_startyear)

d_att_sem_wp_DE <- d_att_sem_wp_DE %>% mutate(ind_wp_cs = ifelse( FY == "Direct entry" & (!is.na(ind_cs) | wp_ind == 1), 1, 0)) 
d_att_sem_wp_DE %>% count(ind_wp_cs)

# Contextual only
d_att_sem_wp_c <- d_att_sem_wp_DE %>% 
  group_by(FY, ind_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_sem, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# DE non-contextual students
d_att_sem_wp_DEonly <- d_att_sem_wp_DE %>% filter(is.na(ind_c)) %>%
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_sem, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

# All DE WP students
d_att_sem_wp_c_both <- d_att_sem_wp_DE %>% 
  group_by(FY, ind_wp_cs) %>% 
  summarise(
    mitigating_mean = mean(mean_attend_sem, na.rm = TRUE),
    students_sum = n()) %>%
  ungroup()

d_att_sem_wp_DE_test <- d_att_sem_wp_DE %>% filter(FY == "Direct entry")
chisq.test(d_att_sem_wp_DE_test$mean_attend_sem, d_att_sem_wp_DE_test$ind_wp_cs, correct = TRUE)


# ----------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
  
  ### END HERE
  
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

