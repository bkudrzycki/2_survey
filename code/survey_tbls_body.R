## ---- tbl-surveydesc --------

df <- ys_baseline %>% zap_labels() %>% mutate(sex = recode(sex, `0` = "Female",
                                                           `1` = "Male"))

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(survey.lonely.psu="adjust")

sstrat %>% 
  tbl_svysummary(
    by=sex, 
    # summarize a subset of the columns
    include = c(baseline_age, status, yeduc, YS3_15, YS3_17_6, YS3_13, YS3_8, YS6_6, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8),
    missing = "no",
    # adding labels to table
    label = list(status = "Activity",
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
    type = list(c(YS3_8, YS6_6) ~ "continuous",
                c(YS3_17_6, YS3_13) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean}")
  ) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\nN={n_unweighted} ({round(p_unweighted*100,2)}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>% 
  add_overall(col_label = "**Overall**\nN={N_unweighted}") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive Statistics",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 8,
                         end_row = 14,
                         group_label = "Education") %>% 
  kableExtra::group_rows(start_row = 18,
                         end_row = 20,
                         group_label = "Household") %>% 
  kableExtra::group_rows(start_row = 21,
                         end_row = 24,
                         group_label = "Assets") %>% 
  footnote(general = "Mean; \\\\%. Calculated using responses from baseline survey. Sample weighting applied as described in the text.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "") %>% 
  kableExtra::kable_styling(latex_options="scale_down")


## ---- tbl-aspirations --------

df <- ys_panel %>% zap_labels() %>% filter(as.factor(status) %in% c(2:4) & wave == 0) %>% mutate(fiveyrs = case_when(YS8_36 == 7 | YS8_36 == 9 | YS9_37 == 6 | YS10_27 == 4 | YS10_27 == 7 ~ 5, # other
                                               YS8_36 == 5 | YS8_36 == 6 | YS9_37 == 4 | YS10_27 == 5 | YS10_27 == 6 ~ 4, # in education/training
                                               YS8_36 == 3 | YS9_37 == 1 | YS10_27 == 2 ~ 3, # (still) self-employed
                                               YS8_36 == 2 | YS9_37 == 2 | YS10_27 == 3 ~ 2, # different/new employer
                                               YS8_36 == 1 ~ 1, # same employer
                                               YS10_27 == 1 ~ 0))

df$status <- factor(df$status, levels = c(2:4), labels=c("NEET", "Self-Employed", "Employed"))
df$fiveyrs <- factor(df$fiveyrs, levels = c(0:5), labels = c("Still looking for work", "Same employer", "Different/new employer", "(Still) self-employed", "In education/training", "Other"))

df %>% tbl_summary(by = "status", 
                   include = "fiveyrs", 
                   missing = "no", 
                   label = fiveyrs ~ "Where do you see yourself in five years?",
                   statistic = list(all_categorical() ~ "{p}%")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\nN={n}") %>% 
  modify_header(label = " ") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  modify_table_body(~.x %>% 
                      dplyr::mutate(stat_1 = ifelse(stat_1 == "0%", "-", stat_1),
                                    stat_2 = ifelse(stat_2 == "0%", "-", stat_2),
                                    stat_3 = ifelse(stat_3 == "0%", "-", stat_3))) %>% 
  as_kable_extra(caption = "Youth Aspirations",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  footnote(general = "\textit{Note:} Calculated using responses from baseline survey.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "") %>% 
  kableExtra::kable_styling(latex_options="scale_down")

