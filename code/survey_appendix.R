## ---- tbl-census ----

df <- data.frame(" " = c("", "In School", "Other", "Self-Employed", "Employed", "Apprentice"), "Aged 15-19" = c("1417 (71.64)", "125 (6.32)", "95 (4.80)", "35 (1.77)", "306 (15.47)", "1978 (100.00)"), "Aged 20-29" = c("1144 (31.07)", "635 (17.25) ", "1183 (32.13) ", "33 (11.76)", " 287 (7.79)", "3682 (100.00)"), "Aged 30 and above" = c("87 (1.35)", "574 (24.35)", "664 (56.68)", "117 (17.28)", "22 (0.34)", "6464 (100.00)"), check.names = FALSE)

flextable(df) %>% 
  theme_booktabs() %>%
  autofit(part = "all") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Census of 13 zones de dénombrement") %>% 
  add_footer_lines("n, %.") %>% 
  fontsize(size = 9, part = 'all') %>% 
  width(width = .8) %>%
  width(j = 1, width = 1)

## ---- tbl-attrition --------

df <- ys_panel_labels %>% 
  mutate(yos = ifelse(as.numeric(YS3_15) < 50, as.numeric(YS3_15), NA))

df$status <- factor(ys_panel_labels$status, levels=c("Apprentice", "In School", "Employed", "Self-Employed", "NEET"))
df$baseline_activity <- factor(ys_panel_labels$baseline_activity, levels=c("Apprentice", "In School", "Employed", "Self-Employed", "NEET"))

df$wave <- factor(ys_panel_labels$wave, labels=c("Baseline", "Follow-up 1", "Follow-up 2", "Follow-up 3", "Endline"))

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
  add_p(list(status ~ NULL,
             baseline_activity ~ "kruskal.test",
             sex ~ "chisq.test",
             baseline_age ~ "kruskal.test",
             yos ~ "kruskal.test"),
        pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\nN={n}") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Sample Composition and Attrition",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  footnote(general = "\\\\tiny{n (\\\\%); \\\\%; Mean (SD). Calculated using responses from baseline survey.}",
           threeparttable = T,
           fixed_small_size = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 8) %>% 
  column_spec(2:7, width = "7em")



## ---- tbl-descgender --------

df <- ys_panel_labels %>% filter(wave == "YS") %>% zap_labels() %>% 
  mutate(sex = recode(sex, `0` = "Female",
                      `1` = "Male"),
         yos = ifelse(as.numeric(YS3_15) != 99, YS3_15, 0),
         app = YS3_13,
         cep = YS3_17_2,
         bepc = YS3_17_4,
         bac = YS3_17_6,
         cap = YS3_17_8,
         licence = YS3_17_11,
         master = YS3_17_12,
         fathapp = YS3_9,
         fath_primary = ifelse(as.numeric(YS3_10) > 2, 1, 0),
         fsecplus = ifelse(as.numeric(YS3_10) > 4 & as.numeric(YS3_10) != 10, 1, 0),
         mothapp = YS3_11,
         moth_primary = ifelse(as.numeric(YS3_12) > 2, 1, 0),
         msecplus = ifelse(as.numeric(YS3_12) > 4 & as.numeric(YS3_12) != 10, 1, 0),
         married = ifelse(as.numeric(YS3_6) == 1, 1, 0),
         withparents = ifelse(as.numeric(YS6_1) == 3, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(as.numeric(YS3_4_4)), 1, 0),
         christian = ifelse(!is.na(as.numeric(YS3_5_1)) | !is.na(as.numeric(YS3_5_2)) | !is.na(as.numeric(YS3_5_3)) | !is.na(as.numeric(YS3_5_4)), 1, 0),
         city = ifelse(as.numeric(YS3_3) == 4, 1, 0),
         total = 1,
         YS3_8 = as.numeric(YS3_8),
         YS6_6 = as.numeric(YS6_6))

df %>% tbl_summary(
  by=sex, 
  # summarize a subset of the columns
  include = c(total, sex,  baseline_age, beninese, fon, christian, city, status, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, withparents, YS3_8, YS6_6, wealth_quintile, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8),
  missing = "no",
  # adding labels to table
  label = list(total = "N",
               status = "Activity at baseline",
               graduation_age = "Graduation age",
               first_employment_age = "Age at first employment",
               first_employment_duration = "Duration of transition in years",
               yos = "Years of schooling",
               app = "Completed apprenticeship (=1)",
               cap = "Vocational certificate: CAP (=1)",
               cep = "Primary diploma: CEP (=1)",
               bepc = "Junior high diploma: BEPC (=1)",
               bac = "Baccalauréat: BAC (=1)",
               licence = "2nd cycle university: Licence (=1)",
               master = "3rd cycle university: Maîtrise (=1)",
               fathapp = "Father was an apprentice (=1)",
               fath_primary = "Father completed primary (=1)",
               fsecplus = "Father completed secondary (=1)",
               mothapp = "Mother was an apprentice (=1)",
               moth_primary = "Mother completed primary (=1)",
               msecplus = "Mother completed secondary (=1)",
               married = "Married (=1)",
               withparents = "Living with parents (=1)",
               YS3_8 = "No. of children",
               YS6_6  = "People in household",
               beninese = "Nationality: Beninese (=1)",
               fon = "Ethnicity: Fon (=1)",
               christian = "Religion: Christian (=1)",
               city = "Grew up in a city (=1)",
               wealth_quintile = "Wealth index quintile",
               YS6_2 = "Home electrified (=1)",
               YS6_11_1 = "Cell Phone (=1)",
               YS6_11_2 = "Smartphone (=1)",
               YS6_11_5 = "Motorcycle (=1)",
               YS6_11_8 = "Television (=1)"),
  value = list(beninese = "Béninois"),
  type = list(c(YS3_8, YS6_6, first_employment_duration, wealth_quintile) ~ "continuous",
              c(beninese, app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
  statistic = list(all_categorical() ~ "{p}%",
                   all_continuous() ~ "{mean} ({median})",
                   total ~ "{N}")
) %>% 
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** ({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Summary Statistics - By Gender",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 7,
                         end_row = 15,
                         group_label = "Employment Status") %>% 
  kableExtra::group_rows(start_row =16,
                         end_row = 23,
                         group_label = "Education") %>%
  kableExtra::group_rows(start_row = 24,
                         end_row =29,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 30,
                         end_row = 39,
                         group_label = "Household Characteristics and Assets") %>% 
  footnote(general = "\\\\scriptsize{Mean (median); \\\\%. Calculated using responses from baseline survey.}",
           number = c("To first employment."),
           threeparttable = T,
           fixed_small_size = F,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 8) %>%
  column_spec(2:5, width = "7em")

## ---- tbl-surveydesc_strat --------

df <- ys_panel_labels %>% filter(wave == "YS") %>% zap_labels() %>% 
  mutate(sex = recode(sex, `0` = "Female",
                      `1` = "Male"),
         yos = ifelse(as.numeric(YS3_15) != 99, YS3_15, 0),
         app = YS3_13,
         cep = YS3_17_2,
         bepc = YS3_17_4,
         bac = YS3_17_6,
         cap = YS3_17_8,
         licence = YS3_17_11,
         master = YS3_17_12,
         fathapp = YS3_9,
         fath_primary = ifelse(as.numeric(YS3_10) > 2, 1, 0),
         fsecplus = ifelse(as.numeric(YS3_10) > 4 & as.numeric(YS3_10) != 10, 1, 0),
         mothapp = YS3_11,
         moth_primary = ifelse(as.numeric(YS3_12) > 2, 1, 0),
         msecplus = ifelse(as.numeric(YS3_12) > 4 & as.numeric(YS3_12) != 10, 1, 0),
         married = ifelse(as.numeric(YS3_6) == 1, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(as.numeric(YS3_4_4)), 1, 0),
         christian = ifelse(!is.na(as.numeric(YS3_5_1)) | !is.na(as.numeric(YS3_5_2)) | !is.na(as.numeric(YS3_5_3)) | !is.na(as.numeric(YS3_5_4)), 1, 0),
         city = ifelse(as.numeric(YS3_3) == 4, 1, 0),
         total = 1,
         YS3_8 = as.numeric(YS3_8),
         YS6_6 = as.numeric(YS6_6))

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(survey.lonely.psu="adjust")

sstrat %>% 
  tbl_svysummary(
    by=sex, 
    # summarize a subset of the columns
    include = c(total, sex,  baseline_age, beninese, fon, christian, city, status, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married,  YS3_8, YS6_6, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8),
    missing = "no",
    # adding labels to table
    label = list(total = "N",
                 status = "Activity at baseline",
                 graduation_age = "Graduation age",
                 first_employment_age = "Age at first employment",
                 first_employment_duration = "Duration of transition in years",
                 yos = "Years of schooling",
                 app = "Completed apprenticeship (=1)",
                 cap = "Vocational certificate: CAP (=1)",
                 cep = "Primary diploma: CEP (=1)",
                 bepc = "Junior high diploma: BEPC (=1)",
                 bac = "Baccalauréat: BAC (=1)",
                 licence = "2nd cycle university: Licence (=1)",
                 master = "3rd cycle university: Maîtrise (=1)",
                 fathapp = "Father was an apprentice (=1)",
                 fath_primary = "Father completed primary (=1)",
                 fsecplus = "Father completed secondary (=1)",
                 mothapp = "Mother was an apprentice (=1)",
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
    value = list(beninese = "Béninois"),
    type = list(c(YS3_8, YS6_6, first_employment_duration) ~ "continuous",
                c(beninese, app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean}",
                     total ~ "{N_unweighted}")
  ) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** ({round(p_unweighted, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive Statistics, Weighted",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 7,
                         end_row = 15,
                         group_label = "Employment Status") %>% 
  kableExtra::group_rows(start_row =16,
                         end_row = 23,
                         group_label = "Education") %>%
  kableExtra::group_rows(start_row = 24,
                         end_row =28,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 29,
                         end_row = 37,
                         group_label = "Household Characteristics and Assets") %>% 
  footnote(general = "\\\\scriptsize{Mean; \\\\%. Calculated using responses from baseline survey. Sample weighting applied.}",
           number = c("To first employment."),
           threeparttable = T,
           fixed_small_size = F,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 7) %>%
  column_spec(2:4, width = "7em")

# footnote(general = "Mean; \\\\%. Calculated using responses from baseline survey. Sample weighting applied.",
#          threeparttable = T,
#          fixed_small_size = F,
#          escape = F,
#          general_title = "") %>% 

## ---- tbl-propensities ----

df <- ys_panel_labels %>%
  select(IDYouth, wave, age, status, formal, informal, underemp, regular, casual, employer, independent, sex) %>% 
  mutate(age_cat2 = case_when(age %in% c(19:21) ~ "19-21",
                              age %in% c(22:24) ~ "22-24",
                              age %in% c(25:27) ~ "25-27",
                              age %in% c(28:30) ~ "28-30")) %>%
  pivot_wider(id_cols = IDYouth, names_from = wave, values_from = c(age_cat2, status, formal, informal, underemp, regular, casual, employer, independent, sex), names_prefix = "wave_")

# formal

t1 <- df %>% select(status_wave_YS, formal_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(formal_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = formal_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, formal_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(formal_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = formal_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, formal_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(formal_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = formal_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, formal_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(formal_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = formal_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y[[1]] <- y[[1]] %>% mutate(From = "    Female")
y[[2]] <- y[[2]] %>% mutate(From = "    Male")
z[[1]] <- z[[1]] %>% mutate(From = "    19-21")
z[[2]] <- z[[2]] %>% mutate(From = "    22-24")
z[[3]] <- z[[3]] %>% mutate(From = "    25-27")
z[[4]] <- z[[4]] %>% mutate(From = "    28-30")

formal <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% rename("Formal" = `1`)

# informal

t1 <- df %>% select(status_wave_YS, informal_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(informal_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = informal_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, informal_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(informal_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = informal_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, informal_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(informal_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = informal_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, informal_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(informal_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = informal_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

informal <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Informal" = `1`)

# regular

t1 <- df %>% select(status_wave_YS, regular_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(regular_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = regular_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, regular_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(regular_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = regular_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, regular_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(regular_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = regular_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, regular_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(regular_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = regular_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

regular <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Regular" = `1`)

# casual

t1 <- df %>% select(status_wave_YS, casual_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(casual_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = casual_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, casual_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(casual_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = casual_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, casual_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(casual_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = casual_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, casual_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(casual_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = casual_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

casual <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Casual" = `1`)

# underemp

t1 <- df %>% select(status_wave_YS, underemp_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(underemp_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = underemp_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, underemp_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(underemp_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = underemp_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, underemp_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(underemp_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = underemp_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, underemp_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(underemp_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = underemp_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

underemp <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Under-\nemployed" = `1`)

# employer

t1 <- df %>% select(status_wave_YS, employer_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(employer_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = employer_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, employer_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(employer_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = employer_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, employer_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(employer_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = employer_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, employer_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(employer_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = employer_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

employer <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Employer" = `1`)

# independent

t1 <- df %>% select(status_wave_YS, independent_wave_F1U, sex_wave_F1U, age_cat2_wave_F1U) %>% filter(independent_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = independent_wave_F1U, "sex" = sex_wave_F1U, "age" = age_cat2_wave_F1U)

t2 <- df %>% select(status_wave_F1U, independent_wave_F2U, sex_wave_F2U, age_cat2_wave_F2U) %>% filter(independent_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = independent_wave_F2U, "sex" = sex_wave_F2U, "age" = age_cat2_wave_F2U)

t3 <- df %>% select(status_wave_F2U, independent_wave_F3U, sex_wave_F3U, age_cat2_wave_F3U) %>% filter(independent_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = independent_wave_F3U, "sex" = sex_wave_F3U, "age" = age_cat2_wave_F3U)

t4 <- df %>% select(status_wave_F3U, independent_wave_F4U, sex_wave_F4U, age_cat2_wave_F4U) %>% filter(independent_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = independent_wave_F4U, "sex" = sex_wave_F4U, "age" = age_cat2_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

z <- tabyl(ttot, From, to, age, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

independent <- rbind(x, y[[1]], y[[2]], z[[1]], z[[2]], z[[3]], z[[4]]) %>% select(-"From") %>% rename("Indep." = `1`)

tab1 <- cbind(formal, informal, regular, casual, underemp, employer, independent)
tab1 <- tab1[c(1,7,13,19,25,31,37,2,8,14,20,26,32,38,3,9,15,21,27,33,39,4,10,16,22,28,34,40,5,11,17,23,29,35,41),]
tab1[15,1] <- "Self-Emp."

flextable(tab1) %>%
  theme_booktabs() %>%
  autofit(part = "all") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  add_header_row(values = c('','To'),
                 colwidths = c(4,4)) %>% 
  set_caption("Transition Rates into Different Types of Work") %>% 
  add_footer_lines("Row % reported, but do not add up to 100% as activities are not exclusive.") %>% 
  fontsize(size = 9, part = 'all') %>% 
  width(width = .7) %>%
  width(j = 8, width = .4) %>% 
  bold(i = c(1,8,15,22,29), bold = TRUE)

## ---- tbl-entry --------

df <- ys_panel_labels %>% filter(wave == "YS") %>% zap_labels() %>% 
  mutate(sex = recode(sex, `0` = "Female",
                      `1` = "Male"),
         yos = ifelse(as.numeric(YS3_15) != 99, as.numeric(YS3_15), 0),
         app = YS3_13,
         cep = YS3_17_2,
         bepc = YS3_17_4,
         bac = YS3_17_6,
         cap = YS3_17_8,
         licence = YS3_17_11,
         master = YS3_17_12,
         fathapp = YS3_9,
         fath_primary = ifelse(as.numeric(YS3_10) > 2, 1, 0),
         fsecplus = ifelse(as.numeric(YS3_10) > 4 & as.numeric(YS3_10) != 10, 1, 0),
         mothapp = YS3_11,
         moth_primary = ifelse(as.numeric(YS3_12) > 2, 1, 0),
         msecplus = ifelse(as.numeric(YS3_12) > 4 & as.numeric(YS3_12) != 10, 1, 0),
         married = ifelse(as.numeric(YS3_6) == 1, 1, 0),
         withparents = ifelse(as.numeric(YS6_1) == 3, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(as.numeric(YS3_4_4)), 1, 0),
         christian = ifelse(!is.na(as.numeric(YS3_5_1)) | !is.na(as.numeric(YS3_5_2)) | !is.na(as.numeric(YS3_5_3)) | !is.na(as.numeric(YS3_5_4)), 1, 0),
         city = ifelse(as.numeric(YS3_3) == 4, 1, 0),
         total = 1,
         YS3_8 = as.numeric(YS3_8),
         YS6_6 = as.numeric(YS6_6)) %>% 
  mutate(entry = recode(entry, "Self-Employed" = "Self-**\n**Employed"))

df %>% tbl_summary(
  by=entry, 
  # summarize a subset of the columns
  include = c(total, sex, married, YS3_8, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus),
  missing = "no",
  # adding labels to table
  label = list(total = "N",
               sex = "Male",
               status = "Activity at baseline",
               graduation_age = "Graduation age",
               first_employment_age = "Age at first employment",
               first_employment_duration = "Duration of transition in years",
               yos = "Years of schooling",
               app = "Completed apprenticeship (=1)",
               cap = "Vocational certificate: CAP (=1)",
               cep = "Primary diploma: CEP (=1)",
               bepc = "Junior high diploma: BEPC (=1)",
               bac = "Baccalauréat: BAC (=1)",
               licence = "2nd cycle university: Licence (=1)",
               master = "3rd cycle university: Maîtrise (=1)",
               fathapp = "Father was an apprentice (=1)",
               fath_primary = "Father completed primary (=1)",
               fsecplus = "Father completed secondary (=1)",
               mothapp = "Mother was an apprentice (=1)",
               moth_primary = "Mother completed primary (=1)",
               msecplus = "Mother completed secondary (=1)",
               married = "Married (=1)",
               YS3_8 = "No. of children"),
  value = list(beninese = "Béninois",
               sex = "Male"),
  type = list(c(first_employment_duration, YS3_8, yos) ~ "continuous",
              c(sex, app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
  statistic = list(all_categorical() ~ "{p}%",
                   all_continuous() ~ "{mean}",
                   total ~ "{N}")) %>% 
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** ({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Status at Labour Market Entry**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Summary Statistics - Labour Market Entry",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 5,
                         end_row = 7,
                         group_label = "Transition") %>% 
  kableExtra::group_rows(start_row =8,
                         end_row = 15,
                         group_label = "Education") %>%
  kableExtra::group_rows(start_row = 16,
                         end_row =21,
                         group_label = "Parents' Education") %>% 
  footnote(general = "\\\\tiny{Mean; \\\\%. Calculated using responses from baseline survey.}",
           threeparttable = T,
           fixed_small_size = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 9) %>%
  column_spec(2:6, width = "5em")

## ---- tbl-wage ----

df <- ys_panel %>% zap_labels() %>% 
  mutate(total = 1,
         sex = recode(sex, `0` = "Female",
                      `1` = "Male"),
         formal = ifelse(!is.na(formal), formal, 0),
         informal = ifelse(!is.na(informal), informal, 0),
         underemp = ifelse(!is.na(underemp), underemp, 0),
         fulltime = ifelse(!is.na(fulltime), fulltime, 0),
         regular = ifelse(!is.na(regular), regular, 0),
         casual = ifelse(!is.na(casual), casual, 0),
         employer = ifelse(!is.na(employer), employer, 0),
         independent = ifelse(!is.na(independent), independent, 0),
         job_type = case_when(YS8_4 %in% c(5,6) ~ "Family worker",
                              YS8_4 == 3 ~ "Multiple employers, irregular", 
                              YS8_4 == 2 ~ "One employer, irregular basis",
                              YS8_4 == 1 ~ "One employer, regular basis"),
         job_type = factor(job_type, levels = c("One employer, regular basis", "One employer, irregular basis", "Multiple employers, irregular", "Family worker")), 
         YS8_10 = recode(YS8_10, `1` = "Written", `2` = "Verbal", `3` = "None"),
         YS8_11 = ifelse(wave == 4, YE7_6, YS8_11),
         YS8_15 = ifelse(wave == 4, YE7_9, YS8_15),
         YS8_24 = ifelse(wave == 4, YE7_18, YS8_24),
         YS8_33 = ifelse(wave == 4, YE7_22, YS8_33),
         YS8_35 = ifelse(wave == 4, YE7_25, YS8_35),
         wage = case_when(YS8_24 %in% c(0:4) ~ "<35,000 FCFA",
                          YS8_24 == 5 ~ "35,000-54,999 FCFA",
                          YS8_24 %in% c(6:8) ~ "55,000-149.999 FCFA",
                          YS8_24 %in% c(9:10) ~ ">150,000 FCFA"),
         wage = factor(wage, levels = c("<35,000 FCFA", "35,000-54,999 FCFA", "55,000-149.999 FCFA", ">150,000 FCFA")), 
         unlimited_contract = ifelse(YS8_11 == 2, 1, 0),
         YS9_7 = ifelse(wave == 4, YE8_5, YS9_7),
         YS9_8_1 = ifelse(wave == 4, YE8_6_1, YS9_8_1),
         YS9_8_2 = ifelse(wave == 4, YE8_6_2, YS9_8_2),
         YS9_8_3 = ifelse(wave == 4, YE8_6_3, YS9_8_3),
         YS9_8_4 = ifelse(wave == 4, YE8_6_4, YS9_8_4),
         YS9_8_5 = ifelse(wave == 4, YE8_6_5, YS9_8_5),
         YS9_8_6 = ifelse(wave == 4, YE8_6_6, YS9_8_6),
         registered = ifelse(YS9_7 == 1 |
                               YS9_8_1 == 1 |
                               YS9_8_2 == 1 |
                               YS9_8_3 == 1 |
                               YS9_8_4 == 1 |
                               YS9_8_5 == 1 |
                               YS9_8_6 == 1, 1, 0),
         YS9_9_0 = ifelse(wave == 4, YE8_7_0, YS9_9_0),
         YS9_9_1 = ifelse(wave == 4, YE8_7_1, YS9_9_1),
         YS9_9_2 = ifelse(wave == 4, YE8_7_2, YS9_9_2),
         YS9_9_3 = ifelse(wave == 4, YE8_7_3, YS9_9_3),
         YS9_9_4 = ifelse(wave == 4, YE8_7_4, YS9_9_4),
         YS9_9_5 = ifelse(wave == 4, YE8_7_5, YS9_9_5),
         taxes = case_when(YS9_9_0 == 1 ~ 0,
                           YS9_9_1 == 1 ~ 1,
                           YS9_9_2 == 1 ~ 1,
                           YS9_9_3 == 1 ~ 1,
                           YS9_9_4 == 1 ~ 1,
                           YS9_9_5 == 1 ~ 1),
         YS9_10 = ifelse(wave == 4, YE8_8, YS9_10),
         solo = ifelse(YS9_15 == 1, 1, 0),
         employees = ifelse(YS9_15 == 1, NA, YS9_15-1),
         YS9_18 = ifelse(wave == 4, YE8_10, YS9_18),
         YS9_23 = ifelse(wave == 4, YE8_21, YS9_23),
         profits = case_when(YS9_23 %in% c(0:1) ~ "<20,000 FCFA",
                             YS9_23 == 2 ~ "20,000-39,999 FCFA",
                             YS9_23 %in% c(3:4) ~ "40,000-124.999 FCFA",
                             YS9_23 %in% c(5:10) ~ ">125,000 FCFA"),
         profits = factor(profits, levels = c("<20,000 FCFA", "20,000-39,999 FCFA", "40,000-124.999 FCFA", ">125,000 FCFA")), 
         YS9_24 = ifelse(wave == 4, YE8_17, YS9_24),
         YS9_34 = ifelse(wave == 4, YE8_24, YS9_34),
         YS9_36 = ifelse(wave == 4, YE8_23, YS9_36))


t1 <- df %>% filter(wave == 0, status == "Employed") %>% 
  tbl_summary(by = "sex",
              include = c(total, job_type, YS8_9, YS8_15, wage, YS8_33, YS6_21, YS8_35),
              missing = "no",
              label = list(total = "N",
                           job_type = "Working arrangement",
                           YS8_9 = "Number of workers¹",
                           YS8_15 = "Months worked²",
                           wage = "Wage (previous month)",
                           YS8_33 = "Job satisfaction (out of 5)³",
                           YS6_21 = "Life satisfaction (out of 5)³",
                           YS8_35 = "Actively looking for new job"),
              value = list(sex = 1),
              type = list(c(YS8_9, YS8_15, YS6_21, YS8_33) ~ "continuous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA)

t2 <- df %>% filter(wave == 0, status == "Employed") %>% 
  tbl_summary(by = "age_cat2",
              include = c(total, job_type, YS8_9, YS8_15, wage, YS6_21, YS8_33, YS8_35),
              missing = "no",
              # adding labels to table
              label = list(total = "N",
                           job_type = "Working arrangement",
                           YS8_9 = "Number of workers¹",
                           YS8_15 = "Months worked²",
                           wage = "Wage (previous month)",
                           YS8_33 = "Job satisfaction (out of 5)³",
                           YS6_21 = "Life satisfaction (out of 5)³",
                           YS8_35 = "Actively looking for new job"),
              value = list(sex = 1),
              type = list(c(YS8_9, YS8_15, YS6_21, YS8_33) ~ "continuous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  modify_footnote(update = everything() ~ NA)

tbl_merge(list(t1, t2), tab_spanner = FALSE) %>% 
  as_kable_extra(caption = "Summary Statistics - Wage Employed",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  footnote(general = "Calculated using responses from baseline survey.",
           number = c("Primary employer. Includes surveyed worker.", "Of past 12 months.", "Likert scale, 1 = Very dissatisfied, 5 = Very satisfied."),
           threeparttable = T,
           escape = F,
           fixed_small_size = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 9) %>%
  column_spec(2:8, width = "4em")

## ---- tbl-self ----

t1 <- df %>% filter(wave == 0, status == "Self-Employed") %>% 
  tbl_summary(by = "sex",
              include = c(total, registered, taxes, YS9_10, solo, employees, YS9_18, profits, YS9_24, YS9_34, YS6_21, YS9_36),
              missing = "no",
              label = list(total = "N",
                           registered = "Registered business¹",
                           taxes = "Pays taxes²",
                           YS9_10 = "Trade association member",
                           employees = "Number of employees³",
                           profits = "Profits (previous month)",
                           solo = "Works alone (no employees)",
                           YS9_18 = "Months worked of past 12",
                           YS9_24 = "Apprentices trained",
                           YS9_34 = "Job Satisfaction (out of 5, Likert scale)",
                           YS6_21 = "Life satisfaction (out of 5, Likert scale)",
                           YS9_36 = "Looking for new job"),
              type = list(c(employees, YS9_18, YS9_24, YS6_21, YS9_34) ~ "continuous",
                          c(YS9_36) ~ "dichotomous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA)

t2 <- df %>% filter(wave == 0, status == "Self-Employed") %>% 
  tbl_summary(by = "age_cat2",
              include = c(total, registered, taxes, YS9_10, solo, employees, YS9_18, profits, YS9_24, YS9_34, YS6_21, YS9_36),
              missing = "no",
              # adding labels to table
              label = list(total = "N",
                           registered = "Registered business¹",
                           taxes = "Pays taxes²",
                           YS9_10 = "Trade association member",
                           employees = "Number of employees³",
                           profits = "Profits (previous month)",
                           solo = "Works alone (no employees)",
                           YS9_18 = "Months worked of past 12",
                           YS9_24 = "Apprentices trained",
                           YS9_34 = "Job Satisfaction (out of 5, Likert scale)",
                           YS6_21 = "Life satisfaction (out of 5, Likert scale)",
                           YS9_36 = "Looking for new job"),
              type = list(c(employees, YS9_18, YS9_24, YS6_21, YS9_34) ~ "continuous",
                          c(YS9_36) ~ "dichotomous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  modify_footnote(update = everything() ~ NA)

tbl_merge(list(t1, t2), tab_spanner = FALSE) %>% 
  as_kable_extra(caption = "Summary Statistics - Self-Employed",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  footnote(general = "Calculated using responses from baseline survey.",
           number = c("Either registered with Benin Chamber of Commerce and Industry (CCIB), Register of Commerce and Personal Property Transaction (RCCM), National Social Security Fund (CNSS) or National Institute of Statistics and Economic Analysis (INSAE) or in possession of a professional card (carte professionnelle de commerçant, CPC) or a Unique Fiscal Identifier (IFU).", "Paying either Synthetic Professional Tax (Taxe Professionnelle Synthètique, TPS), taxes for public space usage (e.g. patente foraine), or any other local taxes.", "Not including the business owner (i.e. the survey respondent))."),
           threeparttable = T,
           escape = F,
           fixed_small_size = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 9) %>%
  column_spec(2:8, width = "4em")


## ---- tbl-aspirations ----

df <- ys_panel %>% zap_labels() %>% filter(as.factor(status) %in% c("NEET", "Self-Employed", "Employed") & wave == 0) %>% mutate(fiveyrs = case_when(YS8_36 == 7 | YS8_36 == 9 | YS9_37 == 6 | YS10_27 == 4 | YS10_27 == 7 ~ 5, # other
                                                                                                                                                     YS8_36 == 5 | YS8_36 == 6 | YS9_37 == 4 | YS10_27 == 5 | YS10_27 == 6 ~ 4, # in education/training
                                                                                                                                                     YS8_36 == 3 | YS9_37 == 1 | YS10_27 == 2 ~ 3, # (still) self-employed
                                                                                                                                                     YS8_36 == 2 | YS9_37 == 2 | YS10_27 == 3 ~ 2, # different/new employer
                                                                                                                                                     YS8_36 == 1 ~ 1, # same employer
                                                                                                                                                     YS10_27 == 1 ~ 0))

df$fiveyrs <- factor(df$fiveyrs, levels = c(0:5), labels = c("Still looking for work", "Working for same employer", "Different/new employer", "(Still) self-employed", "In education/training", "Other"))

df$status <- factor(df$status)

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
  footnote(general = "Calculated using responses from baseline survey.",
           threeparttable = T,
           escape = F,
           fixed_small_size = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 9) %>%
  column_spec(2:4, width = "8em")

## ---- tbl-clustertbl ----
df <- ys_baseline %>% zap_labels() %>% filter(!is.na(act13)) %>% select(IDYouth, "act13", "act14", "act15", "act16", "act17", "act18", "act19", "act19.2", "act19.3", contains("act2"), sex, baseline_age, beninese, fon, christian, city, status, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, YS3_8, YS6_6, wealth_quintile, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8) %>% mutate(across(c("act13":"act19"), ~ case_when(. == 1 | . == 2 | . == 3 ~ "In School", . == 0 | . == 8 | . == 99 ~ "NEET", . == 7 ~ "Self-Employed", . == 4 | . == 5 ~ "Apprentice", . == 6 ~ "Employed"))) %>% 
  rename(`2013` = act13,
         `2014` = act14,
         `2015` = act15,
         `2016` = act16,
         `2017` = act17,
         `2018` = act18,
         `2019` = act19,
         `2019_2` = act19.2,
         `2019_3` = act19.3,
         `2020` = act20.1,
         `2020_2` = act20.2,
         `2021` = act21) %>% 
  mutate(`2013_2` = `2013`,
         `2013_3` = `2013`,
         `2014_2` = `2014`,
         `2014_3` = `2014`,
         `2015_2` = `2015`,
         `2015_3` = `2015`,
         `2016_2` = `2016`,
         `2016_3` = `2016`,
         `2017_2` = `2017`,
         `2017_3` = `2017`,
         `2018_2` = `2018`,
         `2018_3` = `2018`,
         `2020_3` = `2020_2`,
         `2021_2` = `2021`,
         `2021_3` = `2021`)

cols <- c("2013", "2013_2", "2013_3", "2014", "2014_2", "2014_3", "2015", "2015_2", "2015_3", "2016", "2016_2", "2016_3", "2017", "2017_2", "2017_3", "2018", "2018_2", "2018_3", "2019", "2019_2", "2019_3", "2020", "2020_2", "2020_3", "2021", "2021_2", "2021_3")

labs <- c("2013", "2013", "2013", "2014", "2014", "201_3", "2015", "2015", "2015", "2016", "2016", "2016", "2017", "2017", "2017", "2018", "2018", "2018", "2019", "2019", "2019", "2020", "2020", "2020", "2021", "2021", "2021")

col_order <- c("IDYouth", cols, "sex", "baseline_age", "beninese", "fon", "christian", "city", "status", "graduation_age", "first_employment_age", "first_employment_duration", "yos", "app", "cap", "cep", "bepc", "bac", "licence", "master", "fathapp", "fath_primary", "fsecplus", "mothapp", "moth_primary", "msecplus", "married", "YS3_8", "YS6_6", "wealth_quintile", "YS6_2",  "YS6_11_1", "YS6_11_2", "YS6_11_5", "YS6_11_8")

df <- df[, col_order]

df.alphab <- c("Employed", "Self-Employed", "In School", "Apprentice", "NEET")

df.seq <- seqdef(df, 2:28, xtstep = 1, alphabet = df.alphab)

df.om <- seqdist(df.seq, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)

clusterward <- agnes(df.om, diss = TRUE, method = "ward")

df.cl5 <- cutree(clusterward, k = 5)

cl5.lab <- factor(df.cl5, labels = c("Apprenticeship", "Schooling", "Employed", "Self-Employed", "NEET"))

df$cluster <- df.cl5

df <- df %>% mutate(cluster = recode(cluster, `1` = "TRAIN",
                                     `2` = "SCHOOL",
                                     `3` = "WAGE",
                                     `4` = "SELF",
                                     `5` = "NEET"),
                    total = 1)


tbl_summary(df,
            include = c(cluster, total, sex, baseline_age, beninese, city, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, YS3_8, YS6_6, wealth_quintile, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8), 
            by = cluster,
            missing = "no",
            # adding labels to table
            label = list(total = "N",
                         sex = "Male",
                         status = "Activity at baseline",
                         graduation_age = "Graduation age",
                         first_employment_age = "Age at first employment",
                         first_employment_duration = "Duration of transition in years",
                         yos = "Years of schooling",
                         app = "Completed apprenticeship (=1)",
                         cap = "Vocational certificate: CAP (=1)",
                         cep = "Primary diploma: CEP (=1)",
                         bepc = "Junior high diploma: BEPC (=1)",
                         bac = "Baccalauréat: BAC (=1)",
                         licence = "2nd cycle university: Licence (=1)",
                         master = "3rd cycle university: Maîtrise (=1)",
                         fathapp = "Father was an apprentice (=1)",
                         fath_primary = "Father completed primary (=1)",
                         fsecplus = "Father completed secondary (=1)",
                         mothapp = "Mother was an apprentice (=1)",
                         moth_primary = "Mother completed primary (=1)",
                         msecplus = "Mother completed secondary (=1)",
                         married = "Married (=1)",
                         YS3_8 = "No. of children",
                         YS6_6  = "People in household",
                         beninese = "Nationality: Beninese (=1)",
                         city = "Grew up in a city (=1)",
                         wealth_quintile = "Wealth index quintile",
                         YS6_2 = "Home electrified (=1)",
                         YS6_11_1 = "Cell Phone (=1)",
                         YS6_11_2 = "Smartphone (=1)",
                         YS6_11_5 = "Motorcycle (=1)",
                         YS6_11_8 = "Television (=1)"),
            value = list(beninese = 1,
                         sex = 1),
            type = list(c(first_employment_duration, YS3_8, yos, wealth_quintile) ~ "continuous",
                        c(sex, app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8) ~ "dichotomous"),
            statistic = list(all_categorical() ~ "{p}%",
                             all_continuous() ~ "{mean}",
                             total ~ "{N}")) %>% 
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** \n ({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "Cluster") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Comparison of Clusters",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 6,
                         end_row = 8,
                         group_label = "Transition") %>% 
  kableExtra::group_rows(start_row =9,
                         end_row = 16,
                         group_label = "Education") %>%
  kableExtra::group_rows(start_row = 17,
                         end_row =22,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 23,
                         end_row =31,
                         group_label = "Household and Assets") %>% 
  footnote(general = "\\\\tiny{Mean; \\\\%. Calculated using responses from baseline survey.}",
           threeparttable = T,
           fixed_small_size = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 8) %>%
  column_spec(2:8, width = "5em")

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
#mean(ys_panel$first_employment_duration, na.rm = T)
#print(fit1, print.rmean=T)

ggsurvplot(fit1, data = df, pval = FALSE, risk.table ="percentage", palette = "aaas", fontsize = 4, conf.int = TRUE, risk.table.height = .3, risk.table.title = "Percentage at risk", ggtheme = theme_survminer(base_size = 10, base_family = "Palatino"), tables.theme = theme_survminer(base_size = 10, base_family = "Palatino", font.main = 14), legend.labs=c("Female" ,"Male")) + guides(colour = "none", fill = "none") + labs(x = "Duration in Years")

## ---- tbl-fullmatrix --------

t1 <- ys_baseline %>% select(act13, act14) %>% rename("From" = act13, "to" = act14)
t2 <- ys_baseline %>% select(act14, act15) %>% rename("From" = act14, "to" = act15)
t3 <- ys_baseline %>% select(act15, act16) %>% rename("From" = act15, "to" = act16)
t4 <- ys_baseline %>% select(act16, act17) %>% rename("From" = act16, "to" = act17)
t5 <- ys_baseline %>% select(act17, act18) %>% rename("From" = act17, "to" = act18)
t6 <- ys_baseline %>% select(act18, act19) %>% rename("From" = act18, "to" = act19)
t7 <- ys_baseline %>% select(act18, act19) %>% rename("From" = act18, "to" = act19)
t8 <- ys_baseline %>% select(act19, act19.2) %>% rename("From" = act19, "to" = act19.2)
t9 <- ys_baseline %>% select(act19.2, act19.3) %>% rename("From" = act19.2, "to" = act19.3)
t10 <- ys_baseline %>% select(act19.3, act20.1) %>% rename("From" = act19.3, "to" = act20.1)
t11 <- ys_baseline %>% select(act20.1, act20.2) %>% rename("From" = act20.1, "to" = act20.2)
t12 <- ys_baseline %>% select(act20.2, act21) %>% rename("From" = act20.2, "to" = act21)


ttot <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) %>% 
  mutate(to = case_when(to == 1 | to == 2 | to == 3 ~ "In School",
                        to == 0 | to == 8 | to == 99 ~ "NEET",
                        to == 7 ~ "Self-Employed",
                        to == 4 | to == 5 ~ "Apprentice",
                        to == 6 ~ "Employed"),
         From = case_when(From == 1 | From == 2 | From == 3 ~ "In School",
                          From == 0 | From == 8 | From == 99 ~ "NEET",
                          From == 7 ~ "Self-Employed",
                          From == 4 | From == 5 ~ "Apprentice",
                          From == 6 ~ "Employed"))

ttot$From <- factor(ttot$From, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))
ttot$to <- factor(ttot$to, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'From'] <- 'From'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','To'),
                                colwidths = c(3,4)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Combined data, 2013-2021") %>% 
  add_footer_lines("Row %\n(Column %)") %>% 
  fontsize(size = 7.5, part = 'all')
## ---- tbl-historymatrix --------

t1 <- ys_baseline %>% select(act13, act14) %>% rename("From" = act13, "to" = act14)
t2 <- ys_baseline %>% select(act14, act15) %>% rename("From" = act14, "to" = act15)
t3 <- ys_baseline %>% select(act15, act16) %>% rename("From" = act15, "to" = act16)
t4 <- ys_baseline %>% select(act16, act17) %>% rename("From" = act16, "to" = act17)
t5 <- ys_baseline %>% select(act17, act18) %>% rename("From" = act17, "to" = act18)
t6 <- ys_baseline %>% select(act18, act19) %>% rename("From" = act18, "to" = act19)

ttot <- rbind(t1, t2, t3, t4, t5, t6) %>% 
  mutate(to = case_when(to == 1 | to == 2 | to == 3 ~ "In School",
                        to == 0 | to == 8 | to == 99 ~ "NEET",
                        to == 7 ~ "Self-Employed",
                        to == 4 | to == 5 ~ "Apprentice",
                        to == 6 ~ "Employed"),
         From = case_when(From == 1 | From == 2 | From == 3 ~ "In School",
                          From == 0 | From == 8 | From == 99 ~ "NEET",
                          From == 7 ~ "Self-Employed",
                          From == 4 | From == 5 ~ "Apprentice",
                          From == 6 ~ "Employed"))

ttot$From <- factor(ttot$From, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))
ttot$to <- factor(ttot$to, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'From'] <- 'From'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','To'),
                                colwidths = c(3,4)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Event History, 2013-2019") %>% 
  add_footer_lines("Row %\n(Column %)") %>% 
  fontsize(size = 9, part = 'all')

## ---- tbl-pooledmatrix --------

df <- ys_panel_labels %>% select(IDYouth, wave, status) %>% pivot_wider(id_cols = IDYouth, names_from = wave, values_from = c(status), names_prefix = "wave_")

t1 <- df %>% select(wave_YS, wave_F1U) %>% rename("From" = wave_YS, "to" = wave_F1U)

t2 <- df %>% select(wave_F1U, wave_F2U) %>% rename("From" = wave_F1U, "to" = wave_F2U)

t3 <- df %>% select(wave_F2U, wave_F3U) %>% rename("From" = wave_F2U, "to" = wave_F3U)

t4 <- df %>% select(wave_F3U, wave_F4U) %>% rename("From" = wave_F3U, "to" = wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = c("row", "col")) %>% adorn_pct_formatting(digits = 2)

x[-1] <- paste0(as.matrix(x[-1]), "\n(", as.matrix(y[-1]), ")")

names(x)[names(x) == 'From'] <- 'From'
x[6,7] <- ''

flextable(x) %>% add_header_row(values = c('','To'),
                                colwidths = c(3,4)) %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  set_caption("Activity transition matrix: Panel data, pooled, 2019-2021") %>% 
  add_footer_lines("Row %\n(Column %)") %>% 
  fontsize(size = 9, part = 'all')

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
  add_footer_lines("Row %\n(Column %)") %>% 
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
  add_footer_lines("Row %\n(Column %)") %>% 
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
  add_footer_lines("Row %\n(Column %)") %>% 
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
  add_footer_lines("Row %\n(Column %)") %>% 
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
  add_footer_lines("Row %\n(Column %)") %>% 
  fontsize(size = 9, part = 'all')




