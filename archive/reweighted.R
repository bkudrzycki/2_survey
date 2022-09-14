# Package names
packages <- c("survey", "tidyverse", "gtsummary")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)))
rm(installed_packages, packages)

## ---- load_panel data
setwd("~/polybox/Youth Employment/Thesis")
load("data/youth_survey/ys_panel.rda")

# baseline data only, convert variables for table to numeric
ys_baseline <- ys_panel %>% filter(wave == 0) %>% 
  mutate_at(c('YS3_8', 'YS6_6', 'YS6_2', 'YS6_11_1', 'YS6_11_2', 'YS6_11_5', 'YS6_11_8', 'YS3_15', 'YS3_17_6', 'YS3_13'), as.numeric) 

ys_baseline <- ys_baseline %>% 
  mutate_at(vars(starts_with("YS3_17")), ~ if_else(is.na(.), '0', '1')) %>%  # replace missing values with 0s
  mutate(sex = recode(sex, `0` = "Female",
                      `1` = "Male")) %>% 
  haven::as_factor()

#`haven::as_factor()`, `labelled::to_factor()`, `labelled::unlabelled()`, and `unclass()

ys_baseline$yeduc <- factor(ys_baseline$yeduc, levels = c("None", "<Primary", "Primary", "Collège", "Lycée", "Tertiary"))

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = ys_baseline, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(gtsummary.tbl_summary.percent_fun = function(x) style_number(x, digits = 3)) #two digits for percentages
options(survey.lonely.psu="adjust") # circumvent small strata problem for regions

#svymean(~censusage, sstrat, na.rm=TRUE)

sstrat %>% 
  tbl_svysummary(
    by=status, 
    # summarize a subset of the columns
    include = c(sex, baseline_age, YS3_8, YS6_6, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8, YS3_15, yeduc, YS3_17_6, YS3_13, status),
    # adding labels to table
    label = list(sex = "Sex",
                 YS3_8 = "Children",
                 YS6_6  = "People in household",
                 YS6_2 = "Home electrified",
                 YS6_11_1 = "Cell Phone",
                 YS6_11_2 = "Smartphone",
                 YS6_11_5 = "Motorcycle",
                 YS6_11_8 = "Television",
                 YS3_15 = "Years of schooling",
                 yeduc = "Highest education level",
                 YS3_17_6 = "Baccalauréate diploma",
                 YS3_13 = "Completed apprenticeship"),
    type = list(c(YS3_8, YS6_6, YS6_2) ~ "continuous",
                c(YS3_17_6, YS3_13) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}",
                     all_continuous() ~ "{mean}")
  ) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** N={n_unweighted} ({round(p_unweighted,2)}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Occupation**") %>% 
  add_overall(col_label = "**Overall**, N={N_unweighted}") %>% 
  modify_table_body(
    mutate,
    group_variable = case_when(variable %in% c("sex", "baseline_age", "YS3_8") ~ "Demographics",
                               variable %in% c("YS6_6", "YS6_2") ~ "Household",
                               variable %in% c("YS6_11_1", "YS6_11_2", "YS6_11_5", "YS6_11_8") ~ "Asset ownership",
                               variable %in% c("YS3_15", "yeduc", "YS3_17_6", "YS3_13") ~ "Education")) %>%
  modify_table_body(group_by, group_variable) %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  italicize_labels() %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()

# Descriptive table by gender

options(gtsummary.tbl_svysummary.percent_fun = function(x) style_number(x * 100, digits = 1))



sstrat %>% 
  tbl_svysummary(
    by=sex, 
    # summarize a subset of the columns
    include = c(sex, status, baseline_age, YS3_8, YS6_6, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8, YS3_15, yeduc, YS3_17_6, YS3_13),
    # adding labels to table
    label = list(status = "Employment Status",
                 YS3_8 = "Children",
                 YS6_6  = "People in household",
                 YS6_2 = "Home electrified",
                 YS6_11_1 = "Cell Phone",
                 YS6_11_2 = "Smartphone",
                 YS6_11_5 = "Motorcycle",
                 YS6_11_8 = "Television",
                 YS3_15 = "Years of schooling",
                 yeduc = "Highest education level",
                 YS3_17_6 = "Baccalauréate diploma",
                 YS3_13 = "Completed apprenticeship"),
    type = list(c(YS3_8, YS6_6, YS6_2) ~ "continuous",
                c(YS3_17_6, YS3_13) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}",
                     all_continuous() ~ "{mean}")
  ) %>% 
  modify_header(update = all_stat_cols() ~ "**{level}** N={n_unweighted} ({round(p_unweighted,2)})%") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>% 
  add_overall(col_label = "**Overall**, N={N_unweighted}") %>% 
  modify_table_body(
    mutate,
    group_variable = case_when(variable == "status" ~ "",
                               variable %in% c("baseline_age", "YS3_8") ~ "Demographics",
                               variable %in% c("YS6_6", "YS6_2") ~ "Household",
                               variable %in% c("YS6_11_1", "YS6_11_2", "YS6_11_5", "YS6_11_8") ~ "Asset ownership",
                               variable %in% c("YS3_15", "yeduc", "YS3_17_6", "YS3_13") ~ "Education")) %>%
  modify_table_body(group_by, group_variable) %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  italicize_labels() %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()

rm(sstrat)
