## ---- tbl-desc ----

df <- ys_panel %>% filter(wave == 0) %>% zap_labels() %>% 
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
         withparents = ifelse(as.numeric(YS6_1) == 3, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(YS3_4_4), 1, 0),
         christian = ifelse(!is.na(YS3_5_1) | !is.na(YS3_5_2) | !is.na(YS3_5_3) | !is.na(YS3_5_4), 1, 0),
         city = ifelse(YS3_3 == 4, 1, 0),
         total = 1) %>% 
  mutate(status = recode(status, "Self-Employed" = "Self-**\n**Employed"))

df %>% 
  tbl_summary(
    by=status, 
    # summarize a subset of the columns
    include = c(total, sex, baseline_age, beninese, fon, christian, city, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, withparents, YS3_8, YS6_6, wealth_quintile, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8, status),
    missing = "no",
    # adding labels to table
    label = list(total = "N",
                 sex = "Male (=1)",
                 graduation_age = "Graduation age",
                 first_employment_age = "Age at first employment",
                 first_employment_duration = "Duration of transition in years¹",
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
    type = list(c(YS3_8, YS6_6, first_employment_duration, wealth_quintile) ~ "continuous",
                c(app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean} ({median})",
                     total ~ "{N}"),
    digits = list(all_continuous() ~ c(2,0))
  ) %>%  
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\n({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Baseline Activity**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive Statistics by Baseline Activity",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 8,
                         end_row = 10,
                         group_label = "Employment Status") %>% 
  kableExtra::group_rows(start_row = 11,
                         end_row = 18,
                         group_label = "Education") %>% 
  kableExtra::group_rows(start_row = 19,
                         end_row = 24,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 25,
                         end_row = 34,
                         group_label = "Household Characteristics and Assets") %>% 
  footnote(general = "\\\\scriptsize{Mean (median); \\\\%. Calculated using responses from baseline survey.}",
           number = c("To first employment."),
           threeparttable = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(latex_options="scale_down", full_width = FALSE) %>%
  column_spec(2:7, width = "5em")


## ---- tbl-firstempreg ----

df <- ys_baseline %>% mutate(first_emp = first_employment_age)

m1 <- lm(first_emp ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

df <- ys_baseline %>% mutate(first_emp = first_employment_duration)

m2 <- lm(first_emp ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)


df3 <- ys_panel %>% filter(wave == 0, entry %in% c("NEET", "Employed")) 
df3$entry = relevel(factor(df3$entry), ref=1)

m3 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df3, trace = F) # trace = F to suppress output message

df4 <- ys_panel %>% filter(wave == 0, entry %in% c("NEET", "Self-Employed"))
df4$entry = relevel(factor(df4$entry), ref=1)

m4 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df4, trace = F)

df5 <- ys_panel %>% filter(wave == 0, entry %in% c("Employed", "Self-Employed")) 
df5$entry = relevel(factor(df5$entry), ref=2)

m5 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df5, trace = F)

stargazer(m1, m2, m3, m4, m5, df = FALSE, font.size= "scriptsize", column.sep.width = "3pt",
          no.space = TRUE, single.row = FALSE, digits = 2, header = FALSE, table.placement = "H",
          notes.align = "r",
          notes.append = TRUE,
          covariate.labels = c("Male (=1)",
                               "Years of Schooling",
                               "Completed apprenticeship (=1)",
                               "Primary school diploma: CEP (=1)",
                               "Junior high diploma: BEPC (=1)",
                               "Baccalauréat: BAC (=1)",
                               "Lower vocational: CAP (=1)",
                               "2nd cycle university: Licence (=1)",
                               "3rd cycle university: Maîtrise (=1)",
                               "Father was apprentice (=1)",
                               "Father completed primary (=1)",
                               "Father completed secondary (=1)",
                               "Mother was apprentice (=1)",
                               "Mother completed primary (=1)",
                               "Mother completed secondary (=1)",
                               "Married (=1)",
                               "Beninese (=1)",
                               "Ethnicity: Fon (=1)",
                               "Religion: Christian (=1)",
                               "Grew up in a city (=1)"),
          title = "Transition Into First Employment",
          omit.stat=c("n", "bic", "adj.rsq", "ser"),
          column.labels = c("\\shortstack{Transition  \\\\ Age}", "\\shortstack{Transition \\\\ Duration}", "\\shortstack{Wage vs \\\\ NEET}", "\\shortstack{Self vs \\\\ NEET}", "\\shortstack{Wage vs \\\\ Self}"), 
          model.names = FALSE,
          dep.var.labels = c("", "\\underline{Labor market status at entry}"),
          model.numbers = TRUE,
          dep.var.caption = "",
          add.lines = list(c("Observations", 417, 417, nrow(residuals(m3)), nrow(residuals(m5)), nrow(residuals(m5)))),
          label = "tab:tbl-firstempreg")

## ---- fig-ageplot ----

df <- ys_panel_labels %>%
  filter(wave == "YS", sex == 0) %>% 
  pivot_longer(cols = starts_with("occ"),
               names_to = "actage",
               names_prefix = "occ",
               values_to = "activity") %>% 
  mutate(actage = as.numeric(actage))

df <- df %>% 
  filter(activity != 99) %>% ## no apprentices, drop "don't want to say"
  mutate(activity = dplyr::recode(activity, `2` = 1),
         activity = dplyr::recode(activity, `3` = 1),
         activity = dplyr::recode(activity, `5` = 4),## merge formal and traditional apprenticeship
         activity = dplyr::recode(activity, `0` = 8)) %>% 
  group_by(actage, activity) %>%
  summarise(n = sum(activity, na.rm = TRUE)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!is.na(activity)) %>% 
  ungroup() %>% 
  rbind(c(13,4,0,0),
        c(13,6,0,0),
        c(13,7,0,0),
        c(14,6,0,0),
        c(14,7,0,0),
        c(15,6,0,0))

f <- ggplot(df, aes(x=actage, y=prop, fill=factor(activity))) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_x_continuous(breaks=c(13,16,19,22,25,28)) +
  xlab("Age - Females") +
  ylab("Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette="Greys", name = "", labels = c("Schooling", "Apprenticeship", "Wage Employment", "Self-Employment", "NEET")) 

df <- ys_panel_labels %>%
  filter(wave == "YS", sex == 1) %>% 
  pivot_longer(cols = starts_with("occ"),
               names_to = "actage",
               names_prefix = "occ",
               values_to = "activity") %>% 
  mutate(actage = as.numeric(actage))

df <- df %>% 
  filter(activity != 99) %>% ## no apprentices, drop "don't want to say"
  mutate(activity = dplyr::recode(activity, `2` = 1),
         activity = dplyr::recode(activity, `3` = 1),
         activity = dplyr::recode(activity, `5` = 4),## merge formal and traditional apprenticeship
         activity = dplyr::recode(activity, `0` = 8)) %>% 
  group_by(actage, activity) %>%
  summarise(n = sum(activity, na.rm = TRUE)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!is.na(activity)) %>% 
  ungroup() %>% 
  rbind(c(13,6,0,0),
        c(13,7,0,0),
        c(13,8,0,0),
        c(14,6,0,0),
        c(14,7,0,0),
        c(14,8,0,0),
        c(15,6,0,0),
        c(15,7,0,0))

m <- ggplot(df, aes(x=actage, y=prop, fill=factor(activity))) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_x_continuous(breaks=c(13,16,19,22,25,28)) +
  xlab("Age - Males") +
  ylab("") +
  theme_minimal() +
  scale_fill_brewer(palette="Greys", name = "", labels = c("Schooling", "Apprenticeship", "Wage Employment", "Self-Employment", "NEET"))

ggarrange(f,m, ncol=2, common.legend = TRUE, legend="bottom")

## ---- tbl-matrix ----

t1 <- ys_baseline %>% select(act13, act14, age = age13, sex) %>% rename("From" = act13, "to" = act14)
t2 <- ys_baseline %>% select(act14, act15, age = age14, sex) %>% rename("From" = act14, "to" = act15)
t3 <- ys_baseline %>% select(act15, act16, age = age15, sex) %>% rename("From" = act15, "to" = act16)
t4 <- ys_baseline %>% select(act16, act17, age = age16, sex) %>% rename("From" = act16, "to" = act17)
t5 <- ys_baseline %>% select(act17, act18, age = age17, sex) %>% rename("From" = act17, "to" = act18)
t6 <- ys_baseline %>% select(act18, act19, age = age18, sex) %>% rename("From" = act18, "to" = act19)
t7 <- ys_baseline %>% select(act19, act19.2, age = age19.1, sex) %>% rename("From" = act19, "to" = act19.2)
t8 <- ys_baseline %>% select(act19.2, act19.3, age = age19.2, sex) %>% rename("From" = act19.2, "to" = act19.3)
t9 <- ys_baseline %>% select(act19.3, act20.1, age = age19.3, sex) %>% rename("From" = act19.3, "to" = act20.1)
t10 <- ys_baseline %>% select(act20.1, act20.2, age = age20.1, sex) %>% rename("From" = act20.1, "to" = act20.2)
t11 <- ys_baseline %>% select(act20.2, act21, age = age20.2, sex) %>% rename("From" = act20.2, "to" = act21)


ttot <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) %>% 
  mutate(to = case_when(to == 1 | to == 2 | to == 3 ~ "In School",
                        to == 0 | to == 8 | to == 99 ~ "NEET",
                        to == 7 ~ "Self-Employed",
                        to == 4 | to == 5 ~ "Apprentice",
                        to == 6 ~ "Employed"),
         From = case_when(From == 1 | From == 2 | From == 3 ~ "In School",
                          From == 0 | From == 8 | From == 99 ~ "NEET",
                          From == 7 ~ "Self-Employed",
                          From == 4 | From == 5 ~ "Apprentice",
                          From == 6 ~ "Employed"),
         age_cat = case_when(age %in% c(14:18) ~ "14-18",
                             age %in% c(19:24) ~ "19-24",
                             age %in% c(25:30) ~ "25-30"),
         )

ttot$From <- factor(ttot$From, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))
ttot$to <- factor(ttot$to, levels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

x <- tabyl(ttot, From, to, show_na = FALSE) %>% 
                   adorn_percentages("row")

M <- as.data.frame(-sweep(-data.matrix(x)[,-1], MARGIN=1, 1 / diag(data.matrix(x)[,-1]), `*`)) %>% mutate(From = "Conditional")

x1 <- data.matrix(tabyl(ttot %>% filter(sex == 0), From, to, show_na = FALSE) %>% 
                   adorn_percentages("row"))[,-1]

M1 <- as.data.frame(-sweep(-x1, MARGIN=1, 1 / diag(x1), `*`)) %>% mutate(From = "Female") # r_ij = -q_ij/q_ii

x2 <- data.matrix(tabyl(ttot %>% filter(sex == 1), From, to, show_na = FALSE) %>% 
                    adorn_percentages("row"))[,-1]

M2 <- as.data.frame(-sweep(-x2, MARGIN=1, 1 / diag(x2), `*`)) %>% mutate(From = "Male")

x3 <- data.matrix(tabyl(ttot %>% filter(age_cat == "14-18"), From, to, show_na = FALSE) %>% 
                    adorn_percentages("row"))[,-1]

M3 <- as.data.frame(-sweep(-x3, MARGIN=1, 1 / diag(x3), `*`)) %>% mutate(From = "14-18")

x4 <- data.matrix(tabyl(ttot %>% filter(age_cat == "19-24"), From, to, show_na = FALSE) %>% 
                    adorn_percentages("row"))[,-1]

M4 <- as.data.frame(-sweep(-x4, MARGIN=1, 1 / diag(x4), `*`)) %>% mutate(From = "19-24")

x5 <- data.matrix(tabyl(ttot %>% filter(age_cat == "25-30"), From, to, show_na = FALSE) %>% 
                    adorn_percentages("row"))[,-1]

M5 <- as.data.frame(-sweep(-x5, MARGIN=1, 1 / diag(x5), `*`)) %>% mutate(From = "25-30")


y <- rbind(x, M, M1, M2, M3, M4, M5)

y <- y[c(seq(1, 35, 5), seq(2, 35, 5),  seq(3, 35, 5),  seq(4, 35, 5),  seq(5, 35, 5)),] %>% adorn_pct_formatting(digits = 2) %>% mutate(across(everything(), ~replace(., . ==  "100.00%" , "-")))

flextable(y) %>%
  theme_booktabs() %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  add_header_row(values = c('','To'),
                 colwidths = c(3,3)) %>%
  set_caption("Activity transition matrix: Combined data, 2013-2021") %>% 
  add_footer_lines("Row %. First row for each activity refers to unconditional transition rate; remaining rates are conditional.") %>%
  fontsize(size = 9, part = 'all') %>% 
  align(align = "center") %>% 
  align(j=1, align = "left") %>% 
  bold(i = c(1,8,15,22,29), bold = TRUE) %>% 
  width(width = .8) %>%
  width(j = 1, width = 1.2)

# 
# fontsize(size = 8, part = 'all') %>% 
#   width(width = .7) %>%
#   width(j = 6, width = .4) %>% 
#   autofit(part = "all") %>%
  

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

## ---- tbl-transreg ----

# for those who did not transition in past seven years, transitioned when all subsequent periods are in employment:
df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(transitioned_0 = ifelse(status_0 %in% c("Employed", "Self-Employed") & status_1 %in% c("Employed", "Self-Employed", NA) & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0), 
         transitioned_1 = ifelse(status_1 %in% c("Employed", "Self-Employed") & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_2 = ifelse(status_2 %in% c("Employed", "Self-Employed") & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_3 =ifelse(status_3 %in% c("Employed", "Self-Employed") & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_4 =ifelse(status_4 %in% c("Employed", "Self-Employed"), 1, 0)) %>%
  pivot_longer(cols = starts_with("trans"),
               names_to = "wave",
               names_prefix = "transitioned_",
               values_to = "transitioned2") %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, transitioned2)

df <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

x <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(sw_0 = ifelse(act19 == 7 & status_0 == "Employed",1,0),
         sw_1 = ifelse(status_0 == "Self-Employed" & status_1 == "Employed",1,0),
         sw_2 = ifelse(status_1 == "Self-Employed" & status_2 == "Employed",1,0),
         sw_3 = ifelse(status_2 == "Self-Employed" & status_3 == "Employed",1,0),
         sw_4 = ifelse(status_3 == "Self-Employed" & status_4 == "Employed",1,0)) %>% 
  pivot_longer(cols = starts_with("sw"),
               names_to = "wave",
               names_prefix = "sw_",
               values_to = "sw") %>% 
  mutate(sw = replace_na(sw, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, sw)

df <- left_join(df, x, by = c("IDYouth", "wave"))

y <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(ws_0 = ifelse(act19 == 7 & status_0 == "Self-Employed",1,0),
         ws_1 = ifelse(status_0 == "Employed" & status_1 == "Self-Employed",1,0),
         ws_2 = ifelse(status_1 == "Employed" & status_2 == "Self-Employed",1,0),
         ws_3 = ifelse(status_2 == "Employed" & status_3 == "Self-Employed",1,0),
         ws_4 = ifelse(status_3 == "Employed" & status_4 == "Self-Employed",1,0)) %>% 
  pivot_longer(cols = starts_with("ws"),
               names_to = "wave",
               names_prefix = "ws_",
               values_to = "ws") %>% 
  mutate(ws = replace_na(ws, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, ws)

df <- left_join(df, y, by = c("IDYouth", "wave"))

z <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(nw_0 = ifelse(act19 %in% c(0,8) & status_0 %in% c("Self-Employed","Employed"),1,0),
         nw_1 = ifelse(status_0 == "NEET" & status_1 %in% c("Self-Employed","Employed"),1,0),
         nw_2 = ifelse(status_1 == "NEET" & status_2 %in% c("Self-Employed","Employed"),1,0),
         nw_3 = ifelse(status_2 == "NEET" & status_3 %in% c("Self-Employed","Employed"),1,0),
         nw_4 = ifelse(status_3 == "NEET" & status_4 %in% c("Self-Employed","Employed"),1,0)) %>% 
  pivot_longer(cols = starts_with("nw"),
               names_to = "wave",
               names_prefix = "nw_",
               values_to = "nw") %>% 
  mutate(nw = replace_na(nw, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, nw)

df <- left_join(df, z, by = c("IDYouth", "wave"))

for_plm <- df %>% filter(!is.na(YS6_21)) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, status, lagged_status, sw, ws, nw, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm2 <- df %>% filter(!is.na(YS6_21),
                                transitioned == 0) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, status, lagged_status, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm$YS6_21 <- as.double(for_plm$YS6_21)
for_plm2$YS6_21 <- as.double(for_plm2$YS6_21)

op1 <- pglm(YS6_21 ~ transitioned2 + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op1 <- YS6_21 ~ transitioned2 + age + sex + yos
#phtest(form_op1, data = for_plm)
m1 <- extract.pglm(op1)

op2 <- pglm(YS6_21 ~ sw + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op2 <- YS6_21 ~ sw + age + sex + yos
#phtest(form_op2, data = for_plm)
m2 <- extract.pglm(op2)

op3 <- pglm(YS6_21 ~ ws + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op3 <- YS6_21 ~ ws + age + sex + yos
#phtest(form_op3, data = for_plm)
m3 <- extract.pglm(op3)

op4 <- pglm(YS6_21 ~ nw + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = "IDYouth", model = "random", na.action=na.omit)
form_op4 <- YS6_21 ~ nw + age + sex + yos
#phtest(form_op4, data = for_plm)
m4 <- extract.pglm(op4)

texreg(list(m1, m2, m3, m4),
       override.coef = list(exp(m1@coef),exp(m2@coef),exp(m3@coef),exp(m4@coef)),
       override.se = list(exp(m1@se),exp(m2@se),exp(m3@se),exp(m4@se)),
       custom.coef.names = c("Intercept", "Transitioned", "Age", "Male", "Years of Schooling", "Self->Wage", "Wage->Self", "NEET->Work"),
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
       omit.coef = c('mu_1|mu_2|mu_3|sigma'),
       #custom.gof.rows = list("Covariates" = c("YES", "YES", "YES", "YES")),
       fontsize = "small",
       label = "tab:tbl-transreg",
       custom.note = paste("\\small{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$. Odds ratios reported.}"),
       float.pos = "H",
       caption = "Probit Regression of Life Satisfaction")

## ---- tbl-transreg2 ----

op1 <- pglm(YS6_21 ~ transitioned2 + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "pooling", na.action=na.omit)
form_op1 <- YS6_21 ~ transitioned2 + age + sex + yos
#phtest(form_op1, data = for_plm)
m1 <- extract.pglm(op1)

op2 <- pglm(YS6_21 ~ status + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "pooling", na.action=na.omit)
form_op2 <- YS6_21 ~ transitioned2 + status + age + sex + yos
#phtest(form_op2, data = for_plm)
m2 <- extract.pglm(op2)

op3 <- pglm(YS6_21 ~ status + lagged_status + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "pooling", na.action=na.omit)
form_op3 <- YS6_21 ~ transitioned2 + status + lagged_status + age + sex + yos
#phtest(form_op3, data = for_plm)
m3 <- extract.pglm(op3)

op4 <- pglm(YS6_21 ~ status*lagged_status + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = "IDYouth", model = "pooling", na.action=na.omit)
form_op4 <- YS6_21 ~ nw + age + sex + yos
#phtest(form_op4, data = for_plm)
m4 <- extract.pglm(op4)

texreg(list(m1, m2, m3, m4),
       override.coef = list(exp(m1@coef),exp(m2@coef),exp(m3@coef),exp(m4@coef)),
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
       custom.coef.names = c("Any Transition", "Age", "Male", "Years of Schooling", "Omitted Current Status: In School \\\\ \\hspace{1em}Current:NEET", "\\hspace{1em}Current:Self-Employed", "\\hspace{1em}Current:Employed", "\\hspace{1em}Current:Apprentice", "Omitted Previous Status: In School \\\\ \\hspace{1em}Previous:NEET", "\\hspace{1em}Previous:Self-Employed", "\\hspace{1em}Previous:Employed", "\\hspace{1em}Previous:Apprentice", "Interactions Previous$\\rightarrow$Current \\\\ \\hspace{1em}NEET$\\rightarrow$NEET", "\\hspace{1em}NEET$\\rightarrow$Self-Employed", "\\hspace{1em}NEET$\\rightarrow$Employed", "\\hspace{1em}Self-Employed$\\rightarrow$Employed", "\\hspace{1em}Employed$\\rightarrow$Self-Employed", "\\hspace{1em}Employed$\\rightarrow$Employed"),
       omit.coef = c('Intercept|statusNEET:lagged_statusSelf-Employed|statusNEET:lagged_statusEmployed|statusNEET:lagged_statusApprentice|statusSelf-Employed:lagged_statusSelf-Employed|statusApprentice:lagged_statusSelf-Employed|statusApprentice:lagged_statusNEET|statusEmployed:lagged_statusApprentice|statusApprentice:lagged_statusEmployed|statusSelf-Employed:lagged_statusApprentice|statusApprentice:lagged_statusApprentice|mu_1|mu_2|mu_3|sigma'),
       #custom.gof.rows = list("Covariates" = c("YES", "YES", "YES", "YES")),
       fontsize = "small",
       single.row = TRUE,
       label = "tab:reg_table",
       custom.note = paste("\\small{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$. Odds ratios reported. Not all interactions shown.}"),
       float.pos = "H",
       caption = "Probit Regression of Life Satisfaction")

## ---- tbl-employed ----

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
         neet = ifelse(status == "NEET", 1, 0),
         wage = case_when(YS8_24 %in% c(0:4) ~ "<35,000 FCFA",
                          YS8_24 == 5 ~ "35,000-54,999 FCFA",
                          YS8_24 %in% c(6:8) ~ "55,000-149.999 FCFA",
                          YS8_24 %in% c(9:10) ~ ">150,000 FCFA"),
         wage = factor(wage, levels = c("<35,000 FCFA", "35,000-54,999 FCFA", "55,000-149.999 FCFA", ">150,000 FCFA")), 
         profits = case_when(YS9_23 %in% c(0:1) ~ "<20,000 FCFA",
                             YS9_23 == 2 ~ "20,000-39,999 FCFA",
                             YS9_23 %in% c(3:4) ~ "40,000-124.999 FCFA",
                             YS9_23 %in% c(5:10) ~ ">125,000 FCFA"),
         profits = factor(profits, levels = c("<20,000 FCFA", "20,000-39,999 FCFA", "40,000-124.999 FCFA", ">125,000 FCFA")), 
         fiveyrs = case_when(YS8_36 == 7 | YS8_36 == 9 | YS9_37 == 6 | YS10_27 == 4 | YS10_27 == 7 ~ 5, # other
                             YS8_36 == 5 | YS8_36 == 6 | YS9_37 == 4 | YS10_27 == 5 | YS10_27 == 6 ~ 4, # in education/training
                             YS8_36 == 3 | YS9_37 == 1 | YS10_27 == 2 ~ 3, # (still) self-employed
                             YS8_36 == 2 | YS9_37 == 2 | YS10_27 == 3 ~ 2, # different/new employer
                             YS8_36 == 1 ~ 1, # same employer
                             YS10_27 == 1 ~ 0),
         satisfaction = coalesce(YS8_33, YS9_34)
         )


df$fiveyrs <- factor(df$fiveyrs, levels = c(0:5), labels = c("Still looking for work (NEET only)", "Working for same employer (wage employed only)", "Different/new employer", "(Still) self-employed", "In education/training", "Other"))


t1 <- df %>% filter(wave == 0 & (status %in% c("Self-Employed", "Employed") | YS10_8 == 1)) %>% 
  tbl_summary(by = "sex",
              include = c(total, formal, informal, fulltime, underemp, regular, casual, employer, independent, neet, wage, profits, wealth_quintile, satisfaction, YS6_21, fiveyrs),
              missing = "no",
              # adding labels to table
              label = list(total = "N",
                           formal = "Formal employment",
                           informal = "Informal employment",
                           fulltime = "Working full time",
                           underemp = "Underemployed",
                           regular = "Regular employment",
                           casual = "Casual worker",
                           employer = "Employer",
                           independent = "Independent",
                           neet = "Unemployed, looking for work",
                           wage = "Wage (of wage employed)",
                           profits = "Profits (of self-employed)",
                           wealth_quintile = "Wealth index quintile",
                           satisfaction = "Job Satisfaction (of wage and self-employed)¹",
                           YS6_21 = "Life satisfaction¹",
                           fiveyrs = "Where do you see yourself in five years?"),
              value = list(sex = 1),
              type = list(c(formal, informal, underemp, regular, casual, employer, independent) ~ "dichotomous",
                          c(wealth_quintile, satisfaction, YS6_21) ~ "continuous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA)
  

t2 <- df %>% filter(wave == 0 & (status %in% c("Self-Employed", "Employed") | YS10_8 == 1)) %>% 
  tbl_summary(by = "age_cat2",
              include = c(total, formal, informal, fulltime, underemp, regular, casual, employer, independent, neet, wage, profits, YS6_21, satisfaction, fiveyrs),
              missing = "no",
              # adding labels to table
              label = list(total = "N",
                           formal = "Formal employment",
                           informal = "Informal employment",
                           fulltime = "Working full time",
                           underemp = "Underemployed",
                           regular = "Regular employment",
                           casual = "Casual worker",
                           employer = "Employer",
                           independent = "Independent",
                           neet = "Unemployed, looking for work",
                           wage = "Wage (of wage employed)",
                           profits = "Profits (of self-employed)",
                           satisfaction = "Job Satisfaction (of wage and self-employed)¹",
                           YS6_21 = "Life satisfaction¹",
                           fiveyrs = "Where do you see yourself in five years?"),
              value = list(sex = 1),
              type = list(c(formal, informal, underemp, regular, casual, employer, independent) ~ "dichotomous",
                          c(YS6_21, satisfaction) ~ "continuous"),
              statistic = list(all_categorical() ~ "{p}%",
                               all_continuous() ~ "{mean}",
                               total ~ "{N}")) %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**") %>% 
  modify_footnote(update = everything() ~ NA)

tbl_merge(list(t1, t2), tab_spanner = FALSE) %>% 
  as_kable_extra(caption = "Youth Participating in Labor Market - Summary Statistics",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  footnote(general = "Calculated using responses from baseline survey.",
           number = c("Likert scale, 1 = Very dissatisfied, 5 = Very satisfied."),
           threeparttable = T,
           escape = F,
           fixed_small_size = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 8) %>%
  column_spec(2:8, width = "4em") %>% 
  row_spec(4:5,background = "#EEEEEEEE") %>% 
  row_spec(8:9,background = "#EEEEEEEE")
