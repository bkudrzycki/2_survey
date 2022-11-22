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
  

