library("TraMineR")
library("cluster")


df <- ys_baseline %>% filter(!is.na(act13)) %>% select(IDYouth, "act13", "act14", "act15", "act16", "act17", "act18", "act19", "act19.2", "act19.3", contains("act2"), sex, fath_primary, bac)

ys_panel <- ys_panel %>% 
  mutate(status2 = ifelse(YS4_1 == 1 | (YS1_2 == 1 & wave == 0), 5, 
                         ifelse(YS8_4 %in% c(1,2,3,5,6,8), 4,
                                ifelse(YS8_4 == 4, 3, NA)))) %>% 
  mutate(status2 = ifelse(is.na(status2) & YS7_1 == 1, 1, status2)) %>%
  mutate(status2 = ifelse(is.na(status2) & F3U2_0a == 1 & YS8_4 %in% c(1,2,3,5,6,8), 4, status2)) %>% 
  mutate(status2 = ifelse(is.na(status2) & YE3_5 == 1 & YS8_4 %in% c(1,2,3,5,6,8), 4, status2)) %>% 
  mutate(status2 = ifelse(is.na(status2) & YE3_5 == 1 & YS8_4 == 4, 3, status2)) %>% 
  mutate(status2 = ifelse(is.na(status2), 2, status2))

ys_panel$status19 <- NA
ys_panel$status19.2 <- NA
ys_panel$status19.3 <- NA
ys_panel$status20.1 <- NA
ys_panel$status20.2 <- NA
ys_panel$status21 <- NA


ys_panel <- ys_panel %>% mutate(across(c("status19":"act21"), ~ case_when(YS7_1 == 1 & YS7_2 == 1 ~ 1, # primary school
                                                                          YS7_1 == 1 & YS7_2 %in% c(2:6) ~ 2, # secondary/technical school
                                                                          YS7_1 == 1 & YS7_2 %in% c(7,8) ~ 3, # university
                                                                          YS4_1 == 1 & YS4_2 %in% c(1,2) ~ 4, # apprenticeship (traditional)
                                                                          YS4_1 == 1 & YS4_2 %in% c(3,4) ~ 5, # apprenticeship (formal)
                                                                          
                                                                          )))
                                                                       . == 0 | . == 8 | . == 99 ~ "NEET",
                                                                       . == 7 ~ "Self-Employed",
                                                                       . == 4 | . == 5 ~ "Apprentice",
                                                                       . == 6 ~ "Employed"))) %>% 
  
  
  
  status19 = case_when(YS7_1 == 1 & YS7_2 == 1 ~ 1))
  
  
  . == 1 | . == 2 | . == 3 ~ "In School", 
                                                                       . == 0 | . == 8 | . == 99 ~ "NEET",
                                                                       . == 7 ~ "Self-Employed",
                                                                       . == 4 | . == 5 ~ "Apprentice",
                                                                       . == 6 ~ "Employed")))

# 1
# Primary Schooling	
# 2
# Secondary Schooling	
# 3
# University	
# 4
# Apprenticeship (traditional)	
# 5
# Apprenticeship (formal)	
# 6
# Employment	
# 7
# Self-employment	
# 8
# Unemployment	
# 0
# None (stay at home)	
# 99
# Don't want to say


df <- ys_baseline %>% filter(!is.na(act13)) %>% select(IDYouth, "act13", "act14", "act15", "act16", "act17", "act18", "act19", "act19.2", "act19.3", contains("act2"), sex, fath_primary, bac) %>% mutate(across(c("act13":"act19"), ~ case_when(. == 1 | . == 2 | . == 3 ~ "In School", 
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
         `2021` = act21)



df.alphab <- c("Employed", "Self-Employed", "In School", "Apprentice", "NEET")

df.seq <- seqdef(df, 2:13, xtstep = 1, alphabet = df.alphab)

df.om <- seqdist(df.seq, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)

clusterward <- agnes(df.om, diss = TRUE, method = "ward")

df.cl4 <- cutree(clusterward, k = 6)

cl4.lab <- factor(df.cl4, labels = paste("Cluster", 1:6))

seqdplot(df.seq, group = cl4.lab, border = NA)


entropies <- seqient(df.seq)
lm.ent <- lm(entropies ~ sex + fath_primary + bac, df)





df <- ys_panel_labels %>% select(IDYouth, wave, status, formal, informal, underemp, regular, casual, employer, independent, sex) %>% pivot_wider(id_cols = IDYouth, names_from = wave, values_from = c(status, formal, informal, underemp, regular, casual, employer, independent, sex), names_prefix = "wave_")

# formal

t1 <- df %>% select(status_wave_YS, formal_wave_F1U, sex_wave_F1U) %>% mutate(formal_wave_F1U = ifelse(is.na(formal_wave_F1U), 0, 1)) %>% rename("From" = status_wave_YS, "to" = formal_wave_F1U, "sex" = sex_wave_F1U)

t2 <- df %>% select(status_wave_F1U, formal_wave_F2U, sex_wave_F2U) %>% mutate(formal_wave_F2U = ifelse(is.na(formal_wave_F2U), 0, 1)) %>% rename("From" = status_wave_F1U, "to" = formal_wave_F2U, "sex" = sex_wave_F2U)

t3 <- df %>% select(status_wave_F2U, formal_wave_F3U, sex_wave_F3U) %>% mutate(formal_wave_F3U = ifelse(is.na(formal_wave_F3U), 0, 1)) %>% rename("From" = status_wave_F2U, "to" = formal_wave_F3U, "sex" = sex_wave_F3U)

t4 <- df %>% select(status_wave_F3U, formal_wave_F4U, sex_wave_F4U) %>% mutate(formal_wave_F4U = ifelse(is.na(formal_wave_F4U), 0, 1)) %>% rename("From" = status_wave_F3U, "to" = formal_wave_F4U, "sex" = sex_wave_F4U)

ttot <- rbind(t1, t2, t3, t4)

x <- tabyl(ttot, From, to, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

y <- tabyl(ttot, From, to, sex, show_na = FALSE) %>% adorn_percentages("row") %>% adorn_totals(where = "row") %>% adorn_pct_formatting(digits = 2)

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
  set_caption("Transition Rates into Different Types of Work") %>% 
  add_footer_lines("Row % reported, but do not add up to 100% as activities or not exclusive.") %>% 
  fontsize(size = 9, part = 'all') %>% 
  width(width = .7) %>%
  width(j = 8, width = .4) %>% 
  bold(i = c(1,4,7,10,13), bold = TRUE)


tbl_stack(list(t1, t2, t3)) %>% 
  as_kable_extra(caption = "Summary Statistics - Employed Youth",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 1,
                         end_row = 10,
                         group_label = "All workers (wage and self-employed)") %>% 
  kableExtra::group_rows(start_row = 11,
                         end_row = 25,
                         group_label = "Wage employed") %>% 
  kableExtra::group_rows(start_row = 26,
                         end_row = 40,
                         group_label = "Self-employed") %>% 
  footnote(general = "\\\\tiny{Mean; \\\\%.}",
           number = c("Primary employer. Includes surveyed worker.", "Of past 12 months.", "Likert scale, 1 = Very dissatisfied, 5 = Very satisfied.", "Either registered with Benin Chamber of Commerce and Industry (CCIB), Register of Commerce and Personal Property Transaction (RCCM), National Social Security Fund (CNSS) or National Institute of Statistics and Economic Analysis (INSAE) or in possession of a professional card (carte professionnelle de commerçant, CPC) or a Unique Fiscal Identifier (IFU).", "Paying either Synthetic Professional Tax (Taxe Professionnelle Synthètique, TPS), taxes for public space usage (e.g. patente foraine), or any other local taxes.", "Not including surveyed business owner."),
           threeparttable = T,
           fixed_small_size = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(full_width = FALSE, font_size = 7)



df <- ys_panel_labels %>% select(IDYouth, wave, contains("occ"))

t1 <- df %>% select(occ13, occ14) %>% rename("From" = occ13, "to" = occ14)
t2 <- df %>% select(occ14, occ15) %>% rename("From" = occ14, "to" = occ15)
t3 <- df %>% select(occ15, occ16) %>% rename("From" = occ15, "to" = occ16)
t4 <- df %>% select(occ16, occ17) %>% rename("From" = occ16, "to" = occ17)
t5 <- df %>% select(occ17, occ18) %>% rename("From" = occ17, "to" = occ18)
t6 <- df %>% select(occ18, occ19) %>% rename("From" = occ18, "to" = occ19)
t7 <- df %>% select(occ19, occ20) %>% rename("From" = occ19, "to" = occ20)
t8 <- df %>% select(occ20, occ21) %>% rename("From" = occ20, "to" = occ21)
t9 <- df %>% select(occ21, occ22) %>% rename("From" = occ21, "to" = occ22)
t10 <- df %>% select(occ22, occ23) %>% rename("From" = occ22, "to" = occ23)
t11 <- df %>% select(occ23, occ24) %>% rename("From" = occ23, "to" = occ24)
t12 <- df %>% select(occ24, occ25) %>% rename("From" = occ24, "to" = occ25)
t13 <- df %>% select(occ25, occ26) %>% rename("From" = occ25, "to" = occ26)
t14 <- df %>% select(occ26, occ27) %>% rename("From" = occ26, "to" = occ27)
t15 <- df %>% select(occ27, occ28) %>% rename("From" = occ27, "to" = occ28)

ttot <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)

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

x <- data.matrix(tabyl(ttot, From, to, show_na = FALSE) %>% 
                 adorn_percentages("row"))[,-1]

lambda <- -diag(diag(x))
M <- sweep(-x, MARGIN=1, 1 / diag(x), `*`) + diag(5) # r_ij = -p_ij/p_ii
I <- diag(5)

lambda %*% (M-I)

solve(-lambda) # mean duration in state i



df <- ys_baseline %>% 
  mutate(housing = ifelse(YS6_1 %in% c(1,2,6,7), 1, 0),
         water = case_when(YS6_4_0 == 1 ~ 0,
                           YS6_4_1 == 1 ~ 0,
                           YS6_4_2 == 1 ~ 0,
                           YS6_4_3 == 1 ~ 1,
                           YS6_4_4 == 1 ~ 0),
         walls = case_when(YS6_7_1 == 1 ~ 0, # improved wall materials
                           YS6_7_2 == 1 ~ 0,
                           YS6_7_3 == 1 ~ 0,
                           YS6_7_4 == 1 ~ 0,
                           YS6_7_5 == 1 ~ 1,
                           YS6_7_6 == 1 ~ 1,
                           YS6_7_7 == 1 ~ 1),
         floor = case_when(YS6_8_1 == 1 ~ 0, # improved floor materials
                           YS6_8_2 == 1 ~ 0,
                           YS6_8_3 == 1 ~ 0,
                           YS6_8_4 == 1 ~ 0,
                           YS6_8_5 == 1 ~ 0,
                           YS6_8_6 == 1 ~ 1,
                           YS6_8_7 == 1 ~ 1,
                           YS6_8_8 == 1 ~ 0,
                           YS6_8_9 == 1 ~ 1,
                           YS6_8_11 == 1 ~ 1),
         roof = case_when(YS6_9_1 == 1 ~ 0, # improved floor materials
                           YS6_9_2 == 1 ~ 0,
                           YS6_9_3 == 1 ~ 0,
                           YS6_9_5 == 1 ~ 0,
                           YS6_9_6 == 1 ~ 0,
                           YS6_9_7 == 1 ~ 1,
                           YS6_9_8 == 1 ~ 1,
                           YS6_9_9 == 1 ~ 1)
  ) %>% 
  select(YS6_2, YS6_3, YS6_5, housing, water, walls, floor, roof, contains("YS6_11"))

prn<-psych::principal(df, rotate="varimax", nfactors=3,covar=T, scores=TRUE)
index<-prn$scores[,1]
nlab<-c(1,2,3,4,5)
ys_baseline <- cbind(ys_baseline, index) %>% 
  mutate(wealth_quintile = as.factor(cut(index, breaks = 5, labels = nlab)))

ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)


df <- ys_panel %>% filter(wave == 0) 
df$entry = relevel(factor(df$entry), ref=2)

m1 <- lm(first_employment_age ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus, data = df)

m2 <- lm(first_employment_age ~ entry + sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

ys_panel$first_employment_duration <- ys_panel$first_employment_duration+1

m3 <- lm(first_employment_duration ~ entry + sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus, data = df)

m4 <- lm(first_employment_duration ~ entry + sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

stargazer(m1, m2, m3, m4, df = FALSE, font.size= "scriptsize", column.sep.width = "6pt",
          no.space = TRUE, single.row = TRUE, digits = 2, header = F, table.placement = "H",
          notes.align = "r",
          notes.append = TRUE,
          covariate.labels = c("Entry: NEET (reference) \\\\ Entry: Employed",
                               "Entry: Self-Employed",
                               "Male (=1)",
                               "Years of Schooling",
                               "Completed apprenticeship (=1)",
                               "Primary school diploma: CEP (=1)",
                               "Junior high diploma: BEPC (=1)",
                               "Baccalauréat: BAC (=1)",
                               "Lower vocational certificate: CAP (=1)",
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
          omit.stat=c("aic", "bic", "adj.rsq", "ser"),
          dep.var.labels = c("Age", "Duration"),
          model.names = FALSE,
          dep.var.caption = "",
          label = "tab:tbl-firstempreg")


m1 <- lm(first_employment_age ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)
m2 <- lm(first_employment_duration ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)


df <- ys_panel %>% filter(wave == 0, entry %in% c("NEET", "Employed") ) 
df$entry = relevel(factor(df$entry), ref=1)

m3 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

df <- ys_panel %>% filter(wave == 0, entry %in% c("NEET", "Self-Employed") ) 
df$entry = relevel(factor(df$entry), ref=1)

m4 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

df <- ys_panel %>% filter(wave == 0, entry %in% c("Employed", "Self-Employed") ) 
df$entry = relevel(factor(df$entry), ref=2)

m5 <- multinom(entry ~ sex + yos + app + cep + bepc + bac + cap + licence + master + fathapp + fath_primary + fsecplus + mothapp + moth_primary + msecplus + married + beninese + fon + christian + city, data = df)

stargazer(m1, m2, m3, m4, m5, df = FALSE, font.size= "scriptsize", column.sep.width = "6pt",
          no.space = TRUE, single.row = TRUE, digits = 2, header = F, table.placement = "H",
          notes.align = "r",
          notes.append = TRUE,
          covariate.labels = c("Male (=1)",
                               "Years of Schooling",
                               "Completed apprenticeship (=1)",
                               "Primary school diploma: CEP (=1)",
                               "Junior high diploma: BEPC (=1)",
                               "Baccalauréat: BAC (=1)",
                               "Lower vocational certificate: CAP (=1)",
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
          omit.stat=c("aic", "bic", "adj.rsq", "ser"),
          dep.var.labels = c("Age", "Duration"),
          model.names = FALSE,
          dep.var.caption = "",
          label = "tab:tbl-firstempreg")
