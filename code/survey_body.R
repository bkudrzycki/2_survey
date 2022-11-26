## ---- tbl-aspirations --------

df <- ys_panel %>% zap_labels() %>% filter(as.factor(status) %in% c("NEET", "Self-Employed", "Employed") & wave == 0) %>% mutate(fiveyrs = case_when(YS8_36 == 7 | YS8_36 == 9 | YS9_37 == 6 | YS10_27 == 4 | YS10_27 == 7 ~ 5, # other
                                               YS8_36 == 5 | YS8_36 == 6 | YS9_37 == 4 | YS10_27 == 5 | YS10_27 == 6 ~ 4, # in education/training
                                               YS8_36 == 3 | YS9_37 == 1 | YS10_27 == 2 ~ 3, # (still) self-employed
                                               YS8_36 == 2 | YS9_37 == 2 | YS10_27 == 3 ~ 2, # different/new employer
                                               YS8_36 == 1 ~ 1, # same employer
                                               YS10_27 == 1 ~ 0))

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


## ---- tbl-firstempreg --------

df <- ys_panel %>% filter(wave == 0) %>% 
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
  )


m1 <- lm(first_employment_age ~ yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary, data = df)

m2 <- lm(first_employment_age ~ yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

ys_panel$first_employment_duration <- ys_panel$first_employment_duration+1

m3 <- lm(first_employment_duration ~ yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus, data = df)

m4 <- lm(first_employment_duration ~ yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

stargazer(m1, m2, m3, m4, df = FALSE, font.size= "small", column.sep.width = "-8pt",
          no.space = TRUE, digits = 2, header = F, table.placement = "H",
          notes = c("Omitted category: CQP Selected.",
                    "$^1$Prior to 2019.",
                    "$^2$Excluding apprentices"),
          notes.align = "r",
          notes.append = TRUE,
          covariate.labels = c("Years of Schooling",
                               "Completed apprenticeship in the past (=1)",
                               "Primary school diploma: CEP (=1)",
                               "Junior high diploma: BEPC (=1)",
                               "Baccalauréat: BAC (=1)",
                               "Lower vocational certificate: CAP (=1)",
                               "2nd cycle university: Licence (=1)",
                               "3rd cycle university diploma: Maîtrise (=1)",
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
          title = "Transition into first employment",
          omit.stat=c("aic", "bic", "adj.rsq", "ser"),
          dep.var.labels = c("Transition Age", "Transition Duration"),
          model.names = FALSE,
          dep.var.caption = "",
          label = "tab:tbl-firstempreg")


## ---- tbl-historymatrix --------

t1 <- ys_panel %>% select(act13, act14) %>% rename("From" = act13, "to" = act14)
t2 <- ys_panel %>% select(act14, act15) %>% rename("From" = act14, "to" = act15)
t3 <- ys_panel %>% select(act15, act16) %>% rename("From" = act15, "to" = act16)
t4 <- ys_panel %>% select(act16, act17) %>% rename("From" = act16, "to" = act17)
t5 <- ys_panel %>% select(act17, act18) %>% rename("From" = act17, "to" = act18)
t6 <- ys_panel %>% select(act18, act19) %>% rename("From" = act18, "to" = act19)

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
  set_caption("Event history transition matrix, 2013-2019") %>% 
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

## ---- tbl-propensities --------

df <- ys_panel_labels %>% select(IDYouth, wave, status, formal, informal, underemp, regular, casual, employer, independent, sex) %>% pivot_wider(id_cols = IDYouth, names_from = wave, values_from = c(status, formal, informal, underemp, regular, casual, employer, independent, sex), names_prefix = "wave_")

# formal

t1 <- df %>% select(status_wave_YS, formal_wave_F1U, sex_wave_F1U) %>% filter(formal_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = formal_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, formal_wave_F2U, sex_wave_F2U) %>% filter(formal_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = formal_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, formal_wave_F3U, sex_wave_F3U) %>% filter(formal_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = formal_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, formal_wave_F4U, sex_wave_F4U) %>% filter(formal_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = formal_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y[[1]] <- y[[1]] %>% mutate(From = "    Female")
y[[2]] <- y[[2]] %>% mutate(From = "    Male")

formal <- rbind(x, y[[1]], y[[2]]) %>% rename("Formal" = `1`)

# informal

t1 <- df %>% select(status_wave_YS, informal_wave_F1U, sex_wave_F1U) %>% filter(informal_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = informal_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, informal_wave_F2U, sex_wave_F2U) %>% filter(informal_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = informal_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, informal_wave_F3U, sex_wave_F3U) %>% filter(informal_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = informal_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, informal_wave_F4U, sex_wave_F4U) %>% filter(informal_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = informal_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

informal <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Informal" = `1`)

# regular

t1 <- df %>% select(status_wave_YS, regular_wave_F1U, sex_wave_F1U) %>% filter(regular_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = regular_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, regular_wave_F2U, sex_wave_F2U) %>% filter(regular_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = regular_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, regular_wave_F3U, sex_wave_F3U) %>% filter(regular_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = regular_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, regular_wave_F4U, sex_wave_F4U) %>% filter(regular_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = regular_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

regular <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Regular" = `1`)

# casual

t1 <- df %>% select(status_wave_YS, casual_wave_F1U, sex_wave_F1U) %>% filter(casual_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = casual_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, casual_wave_F2U, sex_wave_F2U) %>% filter(casual_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = casual_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, casual_wave_F3U, sex_wave_F3U) %>% filter(casual_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = casual_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, casual_wave_F4U, sex_wave_F4U) %>% filter(casual_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = casual_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

casual <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Casual" = `1`)

# underemp

t1 <- df %>% select(status_wave_YS, underemp_wave_F1U, sex_wave_F1U) %>% filter(underemp_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = underemp_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, underemp_wave_F2U, sex_wave_F2U) %>% filter(underemp_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = underemp_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, underemp_wave_F3U, sex_wave_F3U) %>% filter(underemp_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = underemp_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, underemp_wave_F4U, sex_wave_F4U) %>% filter(underemp_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = underemp_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

underemp <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Under-\nemployed" = `1`)

# employer

t1 <- df %>% select(status_wave_YS, employer_wave_F1U, sex_wave_F1U) %>% filter(employer_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = employer_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, employer_wave_F2U, sex_wave_F2U) %>% filter(employer_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = employer_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, employer_wave_F3U, sex_wave_F3U) %>% filter(employer_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = employer_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, employer_wave_F4U, sex_wave_F4U) %>% filter(employer_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = employer_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

employer <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Employer" = `1`)

# independent

t1 <- df %>% select(status_wave_YS, independent_wave_F1U, sex_wave_F1U) %>% filter(independent_wave_F1U == 1) %>% rename("From" = status_wave_YS, "to" = independent_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, independent_wave_F2U, sex_wave_F2U) %>% filter(independent_wave_F2U == 1) %>% rename("From" = status_wave_F1U, "to" = independent_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, independent_wave_F3U, sex_wave_F3U) %>% filter(independent_wave_F3U == 1) %>% rename("From" = status_wave_F2U, "to" = independent_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, independent_wave_F4U, sex_wave_F4U) %>% filter(independent_wave_F4U == 1) %>% rename("From" = status_wave_F3U, "to" = independent_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("col") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

independent <- rbind(x, y[[1]], y[[2]]) %>% select(-"From") %>% rename("Indep." = `1`)

tab1 <- cbind(formal, informal, regular, casual, underemp, employer, independent)
tab1 <- tab1[c(1,7,13,2,8,14,3,9,15,4,10,16,5,11,17),]
tab1[7,1] <- "Self-Emp."

flextable(tab1) %>%
  theme_booktabs() %>%
  autofit(part = "all") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>% 
  add_header_row(values = c('','To'),
                 colwidths = c(4,4)) %>% 
  set_caption("Transition Propensities into Different Types of Work") %>% 
  fontsize(size = 9, part = 'all') %>% 
  width(width = .7) %>%
  width(j = 8, width = .4) %>% 
  bold(i = c(1,4,7,10,13), bold = TRUE)
  

## ---- tbl-transreg --------

ys_panel$transition_ready <- 0
ys_panel$transitioned <- 0

dflist <- ys_baseline %>% 
  dplyr::select(starts_with("act1")) %>% 
  names()

#only those who have been in school can transition; only those who haven't transitioned will be taken into account
for (k in 1:3021){
  for (i in 1:7){
    y <- dflist[i]
    if(ys_panel[[y]][[k]] %in% c(1:5)) {
      ys_panel[["transition_ready"]][[k]] <- 1
    }
    if(ys_panel[[y]][[k]] %in% c(6:7)) {
      ys_panel[["transitioned"]][[k]] <- 1
    }
  }
}

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

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

df <- ys_panel %>% 
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

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

df <- ys_panel %>% 
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

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

df <- ys_panel %>% 
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

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

ys_panel <- ys_panel %>% mutate(beninese = ifelse(YS3_1 == 1, 1, 0),
       fathapp = YS3_9,
       mothapp = YS3_11,
       siblings = as.numeric(YS3_7),
       fon = ifelse(YS3_4_4 == 1, 1, 0),
       fon = ifelse(is.na(fon), 0, 1),
       christian = ifelse(YS3_5_1 == 1 | YS3_5_2 == 1 | YS3_5_3 == 1 | YS3_5_4 == 1, 1, 0),
       christian = ifelse(is.na(christian), 0, 1),
       secplus = ifelse(YS3_16 > 4 & YS3_16 != 10, 1, 0),
       bac = ifelse(is.na(YS3_17_6), 0, 1),
       fsecplus = ifelse(YS3_10 > 4 & YS3_10 != 10, 1, 0),
       msecplus = ifelse(YS3_12 > 4 & YS3_12 != 10, 1, 0),
       city = ifelse(YS3_3 == 4, 1, 0),
       yos = ifelse(YS3_15 != 99, YS3_15, 0),
       yos = ifelse(YS3_15 < 50, YS3_15, NA))

for_plm <- ys_panel %>% filter(!is.na(YS6_21)) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, sw, ws, nw, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm2 <- ys_panel %>% filter(!is.na(YS6_21),
                                transitioned == 0) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm$YS6_21 <- as.double(for_plm$YS6_21)
for_plm2$YS6_21 <- as.double(for_plm2$YS6_21)

extract.pglm <- function (model, include.nobs = TRUE, include.loglik = TRUE, ...) {
  s <- summary(model, ...)
  coefficient.names <- rownames(s$estimate)
  coefficients <- s$estimate[, 1]
  standard.errors <- s$estimate[, 2]
  significance <- s$estimate[, 4]
  loglik.value <- s$loglik
  n <- nrow(model$model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    gof <- c(gof, loglik.value)
    gof.names <- c(gof.names, "Log-Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                     se = standard.errors, pvalues = significance, gof.names = gof.names, 
                     gof = gof, gof.decimal = gof.decimal)
  return(tr)
}

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
       custom.coef.names = c("Intercept", "Transitioned", "Age at Baseline", "Male", "Years of Schooling", "Self->Wage", "Wage->Self", "NEET->Work"),
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
       omit.coef = c('mu_1|mu_2|mu_3|sigma'),
       #custom.gof.rows = list("Covariates" = c("YES", "YES", "YES", "YES")),
       fontsize = "small",
       label = "tab:reg_table",
       custom.note = paste("\\small{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$. Odds ratios reported.}"),
       float.pos = "H",
       caption = "Random effects probit regression of life satisfaction on successful transition")


