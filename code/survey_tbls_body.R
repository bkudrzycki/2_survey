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
