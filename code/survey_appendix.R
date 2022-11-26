## ---- tbl-attrition --------

df <- ys_panel_labels %>% 
  mutate(yos = ifelse(as.numeric(YS3_15) < 50, as.numeric(YS3_15), NA))

df$baseline_activity <- factor(ys_panel_labels$baseline_activity, levels=c("Apprentice", "In School", "Employed", "Self-Employed", "NEET"))

df$wave <- factor(ys_panel_labels$wave, labels=c("Baseline", "Remote 1", "Remote 2", "Remote 3", "Endline"))

df %>% 
  select(wave, status, baseline_activity, sex, baseline_age, yos) %>% 
  tbl_summary(by = wave,
              missing = "no",
              label = list(status ~ "Activity",
                           baseline_activity ~ "Baseline activity",
                           sex ~ "Male",
                           baseline_age ~ "Age",
                           yos ~ "Years of Schooling"),
              statistic = list(sex ~ "{p}%",
                               baseline_age ~ "{mean}",
                               all_continuous() ~ "{mean} ({sd})")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\nN={n}") %>% 
  add_p(list(status ~ NULL,
             baseline_activity ~ "kruskal.test",
             sex ~ "chisq.test",
             baseline_age ~ "kruskal.test",
             yos ~ "kruskal.test"),
        pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  as_kable_extra(caption = "Sample Composition and Attrition",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::kable_styling(latex_options="scale_down")

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

## ---- tbl-eductable --------

df <- ys_baseline %>% zap_labels() %>% 
  mutate(yos = ifelse(YS3_15 != 99, YS3_15, 0),
         app = YS3_13,
         cep = YS3_17_2,
         bepc = YS3_17_4,
         bac = YS3_17_6,
         cap = YS3_17_8,
         licence = YS3_17_11,
         master = YS3_17_12,
         fathapp = YS3_9,
         fath_primary = ifelse(YS3_10 > 2, 1, 0),
         fsecplus = ifelse(YS3_10 > 4 & YS3_10 != 10, 1, 0),
         mothapp = YS3_11,
         moth_primary = ifelse(YS3_12 > 2, 1, 0),
         msecplus = ifelse(YS3_12 > 4 & YS3_12 != 10, 1, 0),
         married = ifelse(YS3_6 == 1, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(YS3_4_4), 1, 0),
         christian = ifelse(!is.na(YS3_5_1) | !is.na(YS3_5_2) | !is.na(YS3_5_3) | !is.na(YS3_5_4), 1, 0),
         city = ifelse(YS3_3 == 4, 1, 0),
         total = 1) %>% 
  mutate(status = recode(status, "Self-Employed" = "Self-**\n**Employed"))


sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(survey.lonely.psu="adjust")

sstrat %>% 
  tbl_svysummary(
    by=status, 
    # summarize a subset of the columns
    include = c(total, sex, baseline_age, beninese, fon, christian, city, yos, app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married,  YS3_8, YS6_6, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8,status),
    missing = "no",
    # adding labels to table
    label = list(total = "N",
                 sex = "Male (=1)",
                 yos = "Years of Schooling",
                 app = "Completed apprenticeship (=1)",
                 cep = "Primary diploma: CEP (=1)",
                 bepc = "Junior high diploma: BEPC (=1)",
                 bac = "Baccalauréat: BAC (=1)",
                 cap = "Vocational certificate: CAP (=1)",
                 licence = "2nd cycle university: Licence (=1)",
                 master = "3rd cycle university: Maîtrise (=1)",
                 fathapp = "Father was apprentice (=1)",
                 fath_primary = "Father completed primary (=1)",
                 fsecplus = "Father completed secondary (=1)",
                 mothapp = "Mother was apprentice (=1)",
                 moth_primary = "Mother completed primary (=1)",
                 msecplus = "Mother completed secondary (=1)",
                 married = "Married (=1)",
                 YS3_8 = "No. of children",
                 YS6_6  = "People in household",
                 beninese = "Nationality: Beninese (=1)",
                 fon = "Ethnicity: Fon (=1)",
                 christian = "Religion: Christian (=1)",
                 city = "Grew up in a city (=1)",
                 YS6_2 = "Home electrified (=1)",
                 YS6_11_1 = "Cell Phone (=1)",
                 YS6_11_2 = "Smartphone (=1)",
                 YS6_11_5 = "Motorcycle (=1)",
                 YS6_11_8 = "Television (=1)"),
    type = list(c(YS3_8, YS6_6) ~ "continuous",
                c(app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean}",
                     total ~ "{N_unweighted}")
  ) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\n({round(p_unweighted, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Baseline Activity**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive statistics by baseline activity",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 7,
                         end_row = 14,
                         group_label = "Education") %>% 
  kableExtra::group_rows(start_row = 15,
                         end_row = 20,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 21,
                         end_row = 24,
                         group_label = "Household") %>% 
  kableExtra::group_rows(start_row = 25,
                         end_row = 29,
                         group_label = "Assets") %>% 
  footnote(general = "Mean; \\\\%. Calculated using responses from baseline survey. Sample weighting applied.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "") %>% 
  kableExtra::kable_styling(font_size = 7)

## ---- fig-survival --------

df <- ys_panel %>% 
  filter(wave == 0) %>% 
  mutate(id = row_number(), 
         weights = 1/prob,
         right_censored = 1-right_censored) %>%  #by convention, 0 = alive (i.e. censored) and 1 = dead (not censored)
  select(id, sex, baseline_age, starts_with("occ"), graduation_age, first_employment_age, first_employment_duration, right_censored, prob) %>% 
  mutate(sex = recode(sex, `1` = "Male", `0` = "Female"))

df <- df %>% mutate(weights = 1/prob)

surv_object <- Surv(time = df$first_employment_duration, event = df$right_censored)

fit1 <- survfit(surv_object ~ sex, data = df)
#fit2 <- survfit(surv_object ~ sex, data = df, weights = weights)

ggsurvplot(fit1, data = df, pval = FALSE, risk.table ="percentage", palette = "aaas", fontsize = 4, conf.int = TRUE, risk.table.height = .3, risk.table.title = "Percentage at risk", ggtheme = theme_survminer(base_size = 10, base_family = "Palatino"), tables.theme = theme_survminer(base_size = 10, base_family = "Palatino", font.main = 14), legend.labs=c("Female" ,"Male")) + guides(colour = "none", fill = "none") + labs(x = "Duration in Years")

## ---- tbl-transm01 --------

df <- ys_panel_labels %>% select(IDYouth, wave, status) %>% pivot_wider(id_cols = IDYouth, names_from = wave, values_from = status, names_prefix = "wave_")

x <- tabyl(df, wave_YS, wave_F1U, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(df, wave_YS, wave_F1U, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'wave_YS'] <- 'Baseline'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','Follow-up 1'),
                                colwidths = c(6,1)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Baseline and follow-up wave 1") %>% 
  add_footer_lines("Row % (column %)") %>% 
  fontsize(size = 9, part = 'all')

## ---- tbl-transm12 --------

x <- tabyl(df, wave_F1U, wave_F2U, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(df, wave_F1U, wave_F2U, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'wave_F1U'] <- 'Follow-up 1'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','Follow-up 2'),
                                colwidths = c(6,1)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Follow-up wave 1 and follow-up wave 2") %>% 
  add_footer_lines("Row % (column %)") %>% 
  fontsize(size = 9, part = 'all')


## ---- tbl-transm23 --------

x <- tabyl(df, wave_F2U, wave_F3U, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(df, wave_F2U, wave_F3U, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'wave_F2U'] <- 'Follow-up 2'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','Follow-up 3'),
                                colwidths = c(6,1)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Follow-up wave 2 and follow-up wave 3") %>% 
  add_footer_lines("Row % (column %)") %>% 
  fontsize(size = 9, part = 'all')


## ---- tbl-transm34 --------

x <- tabyl(df, wave_F3U, wave_F4U, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(df, wave_F3U, wave_F4U, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'wave_F3U'] <- 'Follow-up\n3'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','Endline'),
                                colwidths = c(6,1)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Follow-up wave 3 and endline") %>% 
  add_footer_lines("Row % (column %)") %>% 
  fontsize(size = 9, part = 'all')


## ---- tbl-transm04 --------

x <- tabyl(df, wave_YS, wave_F4U, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(df, wave_YS, wave_F4U, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'wave_YS'] <- 'Baseline'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','Endline'),
                                colwidths = c(6,1)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Baseline and endline") %>% 
  add_footer_lines("Row % (column %)") %>% 
  fontsize(size = 9, part = 'all')




