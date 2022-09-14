## ---- tbl-eductable --------

df <- ys_baseline %>% zap_labels()

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(survey.lonely.psu="adjust")

sstrat %>% 
  tbl_svysummary(
    by=status, 
    # summarize a subset of the columns
    include = c(sex, baseline_age, yeduc, YS3_15, YS3_17_6, YS3_13, YS3_8, YS6_6, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8,status),
    missing = "no",
    # adding labels to table
    label = list(sex = "Male",
                 YS3_8 = "No. of children",
                 YS6_6  = "People in household",
                 YS6_2 = "Home electrified",
                 YS6_11_1 = "Cell Phone",
                 YS6_11_2 = "Smartphone",
                 YS6_11_5 = "Motorcycle",
                 YS6_11_8 = "Television",
                 YS3_15 = "Years of school",
                 yeduc = "Highest educ. level",
                 YS3_17_6 = "Baccalauréate",
                 YS3_13 = "Past apprenticeship"),
    type = list(c(YS3_8, YS6_6) ~ "continuous",
                c(YS3_17_6, YS3_13) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean}")
  ) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\nN={n_unweighted}\n({round(p_unweighted*100,0)}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Occupation**") %>% 
  add_overall(col_label = "**Overall**\nN={N_unweighted}") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive statistics by baseline activity",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 3,
                         end_row = 12,
                         group_label = "Education") %>% 
  kableExtra::group_rows(start_row = 13,
                         end_row = 15,
                         group_label = "Household") %>% 
  kableExtra::group_rows(start_row = 16,
                         end_row = 19,
                         group_label = "Assets") %>% 
  footnote(general = "Mean; \\\\%. Calculated using responses from baseline survey. Sample weighting applied as described in the text.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "") %>% 
  kableExtra::kable_styling(latex_options="scale_down")
