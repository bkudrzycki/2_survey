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
  include = c(total, sex,  baseline_age, beninese, fon, christian, city, status, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, withparents, YS3_8, YS6_6, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8),
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
                   total ~ "{N}")
) %>% 
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}** ({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive Statistics by Gender",
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
                         end_row = 38,
                         group_label = "Household Characteristics and Assets") %>% 
  footnote(general = "\\\\scriptsize{Mean; \\\\%. Calculated using responses from baseline survey.}",
           number = c("To first employment."),
           threeparttable = T,
           fixed_small_size = F,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 7) %>%
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

## ---- fig-clusters --------

df <- ys_baseline %>% zap_labels() %>% filter(!is.na(act13)) %>% select(IDYouth, "act13", "act14", "act15", "act16", "act17", "act18", "act19", "act19.2", "act19.3", contains("act2"), sex, baseline_age, beninese, fon, christian, city, status, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, YS3_8, YS6_6, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8) %>% mutate(across(c("act13":"act19"), ~ case_when(. == 1 | . == 2 | . == 3 ~ "In School", 
                                                                                                                                                                                                                                                 . == 0 | . == 8 | . == 99 ~ "NEET",
                                                                                                                                                                                                                                                 . == 7 ~ "Self-Employed",
                                                                                                                                                                                                                                                 . == 4 | . == 5 ~ "Apprentice",
                                                                                                                                                                                                                                                 . == 6 ~ "Employed"))) %>% 
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

col_order <- c("IDYouth", cols, "sex", "baseline_age", "beninese", "fon", "christian", "city", "status", "graduation_age", "first_employment_age", "first_employment_duration", "yos", "app", "cap", "cep", "bepc", "bac", "licence", "master", "fathapp", "fath_primary", "fsecplus", "mothapp", "moth_primary", "msecplus", "married", "YS3_8", "YS6_6", "YS6_2",  "YS6_11_1", "YS6_11_2", "YS6_11_5", "YS6_11_8")

df <- df[, col_order]

df.alphab <- c("Employed", "Self-Employed", "In School", "Apprentice", "NEET")

df.seq <- seqdef(df, 2:28, xtstep = 1, alphabet = df.alphab)

df.om <- seqdist(df.seq, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)

clusterward <- agnes(df.om, diss = TRUE, method = "ward")

df.cl5 <- cutree(clusterward, k = 5)

cl5.lab <- factor(df.cl5, labels = c("TRAIN", "SCHOOL", "WAGE", "SELF", "NEET"))

seqdplot(df.seq, group = cl5.lab, border = NA, xtlab = labs)

# seqiplot(df.seq, border = NA, with.legend = "right", xtlab = labs)
# 
# seqplot(df.seq, type="f", idxs = 1:50)

# df.seqe <- seqecreate(df.seq)
# fsubseq <- seqefsub(df.seqe, pMinSupport=0.05)
# plot(fsubseq[2:11], col="cyan")

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
  fontsize(size = 9, part = 'all')
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
  fontsize(size = 8, part = 'all')

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
  fontsize(size = 8, part = 'all')

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
  fontsize(size = 8, part = 'all')

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
  fontsize(size = 8, part = 'all')


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
  fontsize(size = 8, part = 'all')


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
  fontsize(size = 8, part = 'all')


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
  fontsize(size = 8, part = 'all')




