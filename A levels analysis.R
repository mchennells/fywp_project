# Additional analysis on FY students A levels and marks
# 11 Feb 2020

library(dplyr)
library(tidyr)
library(readxl) # Read Excel xlsx file
library(ggplot2)

rm(list=ls())

# Read file with A-levels results
path_alevels <- file.path("Feb 2020", "a-levels and degree classification_copy.xlsx")
raw_alevels <- read_excel(path_alevels) %>% rename(ID_anonym = ID_anonymous)

# Get award year and class
path_data <- file.path("output", "data_full_merge.csv")
raw_data <- read.csv(file = path_data ,header = T, stringsAsFactors = F)
d_award <- raw_data %>% select(ID_anonym, Award_Year, Award_Class) %>% distinct(ID_anonym, ID_anonym, Award_Year, Award_Class)

length(unique(d_award$ID_anonym))
length(d_award$ID_anonym)

# 
#   d_grad <- d_ %>% 
#   filter(ACRONYM != "FY", STATUS_DESCRIPTION == "Awarded", 
#          Award_Year %in% c("18/19","19/20")) %>%
#   select(ID_anonym, FY, FY_startyear, stream, Award_Class, Award_Year, New_Tariff, SCHOOLYEAR, OVERALL_MARKS, MODULE_TYPE, OVERALL_MARKS_MODULE) %>%
#   pivot_wider(names_from = MODULE_TYPE, 
#               values_from = OVERALL_MARKS_MODULE)

# # Compare DE and FY students for years in question
# d_grad %>% select(ID_anonym, FY, Award_Year, Award_Class) %>% distinct() %>% count(Award_Year, FY, Award_Class)

# Merge award year and class onto A level results
d_a_levels <- raw_alevels %>% left_join(d_award) %>% 
  select(ID_anonym, `A-points`,`A-level summary`, Award_Class)
  
d_a_levels %>% count(Award_Class)

# Final cleaning
d_a_levels <- d_a_levels %>% 
  filter(!is.na(Award_Class)) %>%   # only include those who have been awarded degree: excludes x2 withdrawn, x2 still in progress
  filter(!(ID_anonym %in% c("45674377","45681167", "45580686"))) %>% # Remove 2 BTEC students, 1 very low DE student  
  group_by(`A-level summary`, `A-points`, Award_Class) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  mutate( # for ordering the classification (x axis)
    grade_order = ifelse(Award_Class == "Third", 0,
                         ifelse(Award_Class == "Two:Two", 1, 
                                ifelse(Award_Class == "Two:One", 2, 
                                       ifelse(Award_Class == "First", 3, "N/A")
  )))) %>%
  arrange(`A-points`, grade_order, `A-level summary`) %>%  
  mutate(number= row_number()) # for ordering A levels (y axis)

# Plot figure 
plot_apoints <-  ggplot(
  d_a_levels, aes(x = number, y = grade_order)) + 
  geom_point(aes(size = N)) +
  scale_size(name = "N",
             breaks = c(1,2),
             range = c(1,2)) +
  coord_flip() +
  xlab("Students' predicted A-level results") +
  scale_x_continuous(breaks = d_a_levels$number, labels = d_a_levels$`A-level summary`) +
  ylab("UG degree classification awarded") + 
  scale_y_discrete(breaks = c(0,1,2,3), labels = c("3","2:2","2:1", "1")) +
#  geom_text(aes(label=ifelse(grade_order < 1,as.character(Award_Class),'')), hjust=-0.05, vjust=0) +
  theme_minimal(12) + theme(panel.grid.minor= element_blank()) +
  theme(legend.position = c(0.95, 0.15),legend.background = element_rect(fill="white"))
#   theme(legend.title = c("check")) +
#  ggtitle("A-level results and UG degree award classification awarded")
plot_apoints


# Compare tariff levels for robustness check
d_tariff <- raw_data %>% select(ID_anonym, New_Tariff) %>% distinct()
d_a_levels <- d_a_levels %>% left_join(d_tariff)

d_a_levels$group <- ifelse(d_a_levels$grade_order >= 2, "High","Low")

d_a_levels_group <- d_a_levels %>%
  group_by(group) %>% 
  summarise(
    mean_tariff = mean(New_Tariff),
    median_tariff = median(New_Tariff)
  )

write.xlsx(d_a_levels,'output/alevelscheck.xlsx',row.names = F) 








