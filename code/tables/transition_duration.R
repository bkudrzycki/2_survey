# Package names
packages <- c("tidyverse", "survey", "gtsummary")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)))
rm(installed_packages, packages)

## ---- load_panel data
setwd("/Users/Shared/Bart/Youth Employment/1_survey/")
load("data/ys_panel.rda")

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>%
  dplyr::select(IDYouth, starts_with("status"), starts_with("age"))

ys_panel <- left_join(ys_panel, df, by = "IDYouth")

transitions <- ys_panel %>% filter(wave == 0) %>% mutate(loop_buffer = NA) %>% select(IDYouth, c("act13":"act19"), loop_buffer, c("occ13":"occ28"), c("status_0", "status_1", "status_2", "status_3", "status_4"), c("age_0", "age_1", "age_2", "age_3", "age_4")) 

transitions <- transitions %>%
  mutate_at(vars(act13:occ28), ~ifelse(. %in% c(1:3), "In School",
                                       ifelse(. %in% c(4:5), "Apprentice",
                                              ifelse(. == 6, "Employed",
                                                     ifelse(. == 7, "Self-Employed",
                                                            ifelse(. %in% c(0,8), "NEET", NA))))))


transitions$transition_ready <- 0
transitions$graduation_age <- NA #last recorded year of schooling
transitions$transition_age <- NA  #age at which youth entered the final observed employment stint
transitions$first_employment_age <- NA
transitions$first_wage_age <- NA
transitions$first_self_age <- NA
transitions$first_act <- NA
transitions$first_job <- NA


# first, only consider retrospective data

for (k in 1:752) {
  for (i in 9:24) {
    if (!is.na(transitions[[i]][[k]])) {
      if (transitions[[i]][[k]] %in% c("In School", "Apprentice")) {
        transitions[["graduation_age"]][[k]] <- i+3
        transitions[["transition_ready"]][[k]] <- 1
      }
      if (!is.na(transitions[[i-1]][[k]]) && transitions[["transition_ready"]][[k]] == 1 && transitions[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions[[i]][[k]] %in% c("Employed", "Self-Employed")) {
        transitions[["transition_age"]][[k]] <- i+3
      }
      if (transitions[["transition_ready"]][[k]] == 1 && transitions[[i]][[k]] %in% c("NEET", "Employed", "Self-Employed") && is.na(transitions[["first_act"]][[k]])) {
        transitions[["first_act"]][[k]] <- transitions[[i]][[k]]
      }
      if (transitions[[i]][[k]] %in% c("Employed", "Self-Employed") && transitions[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions[["transition_ready"]][[k]] == 1 && is.na(transitions[["first_employment_age"]][[k]])) {
        transitions[["first_employment_age"]][[k]] <- i+3
        transitions[["first_job"]][[k]] <- transitions[[i]][[k]]
      }
      if (transitions[[i]][[k]] == "Employed" && transitions[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Self-Employed") && transitions[["transition_ready"]][[k]] == 1 && is.na(transitions[["first_wage_age"]][[k]])) {
        transitions[["first_wage_age"]][[k]] <- i+3
      }
      if (transitions[[i]][[k]] == "Self-Employed" && transitions[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Employed") && transitions[["transition_ready"]][[k]] == 1 && is.na(transitions[["first_self_age"]][[k]])) {
        transitions[["first_self_age"]][[k]] <- i+3
      }
      if (transitions[[i]][[k]] %in% c("NEET", "In School", "Apprentice")) {
        transitions[["transition_age"]][[k]] <- NA
      }
    }
  }
} 

# now, we consider all time periods
transitions2 <- transitions %>% add_column(status_19 = .$act19, .before = "status_0")

for (k in 1:752) {
  if (transitions[["act19"]][[k]] %in% c("In School", "Apprentice")) {
    transitions[["graduation_age"]][[k]] <- NA
  }
}


for (k in 1:752) {
  for (i in 27:31) {
    if (is.na(transitions2[[i]][[k]])) {
      transitions2[[i]][[k]] = transitions2[[i-1]][[k]] # impute NA data -> roll over from previous period
    }
    else {
      if (transitions2[[i]][[k]] %in% c("In School", "Apprentice")) {
        transitions2[["graduation_age"]][[k]] <- transitions2[[i+5]][[k]]
        transitions2[["transition_ready"]][[k]] <- 1
      }
      if (!is.na(transitions2[[i-1]][[k]]) && transitions2[["transition_ready"]][[k]] == 1 && transitions2[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions2[[i]][[k]] %in% c("Employed", "Self-Employed")) {
        transitions2[["transition_age"]][[k]] <- transitions2[[i+5]][[k]]
      }
      if (transitions2[["transition_ready"]][[k]] == 1 && transitions2[[i]][[k]] %in% c("NEET", "Employed", "Self-Employed") && is.na(transitions2[["first_act"]][[k]])) {
        transitions2[["first_act"]][[k]] <- transitions2[[i]][[k]]
      }
      if (transitions2[[i]][[k]] == "Employed" && transitions2[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Self-Employed") && transitions2[["transition_ready"]][[k]] == 1 && is.na(transitions2[["first_employment_age"]][[k]])) {
        transitions2[["first_wage_age"]][[k]] <- transitions2[[i+5]][[k]]
      }
      if (transitions2[[i]][[k]] == "Self-Employed" && transitions2[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Employed") && transitions2[["transition_ready"]][[k]] == 1 && is.na(transitions2[["first_employment_age"]][[k]])) {
        transitions2[["first_self_age"]][[k]] <- transitions2[[i+5]][[k]]
      }
      if (transitions2[[i]][[k]] %in% c("Employed", "Self-Employed") && transitions2[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions2[["transition_ready"]][[k]] == 1 && is.na(transitions2[["first_employment_age"]][[k]])) {
        transitions2[["first_employment_age"]][[k]] <- transitions2[[i+5]][[k]]
        transitions2[["first_job"]][[k]] <- transitions2[[i]][[k]]
      }
      if (transitions2[[i]][[k]] %in% c("NEET", "In School", "Apprentice")) {
        transitions2[["transition_age"]][[k]] <- NA
      }
    }
  }
  if (transitions2[["status_3"]][[k]] %in% c("In School", "Apprentice")) {
    transitions2[["graduation_age"]][[k]] <- NA
  }
}

# finally, calculate a third transition duration, where youth activity is determined by their occupation for the *majority* of the available survey waves

transitions3 <- transitions %>% mutate(Apprentice = 0, Employed = 0, `Self-Employed` = 0, `In School` = 0, NEET = 0)
for (k in 1:752) {
  for (i in 26:30) {
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] == "Apprentice") {transitions3[["Apprentice"]][[k]] = transitions3[["Apprentice"]][[k]] + 1}
    }
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] == "Employed") {transitions3[["Employed"]][[k]] = transitions3[["Employed"]][[k]] + 1}
    }
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] == "Self-Employed") {transitions3[["Self-Employed"]][[k]] = transitions3[["Self-Employed"]][[k]] + 1}
    }
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] == "In School") {transitions3[["In School"]][[k]] = transitions3[["In School"]][[k]] + 1}
    }
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] == "NEET") {transitions3[["NEET"]][[k]] = transitions3[["NEET"]][[k]] + 1}
    }
  }
}

x <- transitions3[44:48]

transitions3$act20 <- colnames(x)[max.col(x,ties.method="first")]

transitions3 <- transitions3 %>% select(IDYouth, c("act13":"act19"), act20, loop_buffer, c("occ13": "occ28")) %>% mutate(occ29 = NA)

# append latest occupation (up to age 29)
for (k in 1:752) {
  for (i in 27:17) {
    if (is.na(transitions3[[i]][[k]]) & !is.na(transitions3[[i-1]][[k]])) {
      transitions3[[i]][[k]] = transitions3[["act20"]][[k]]
    }
  }
}

transitions3$transition_ready <- 0
transitions3$graduation_age3 <- NA #last recorded year of schooling
transitions3$transition_age3 <- NA  #age at which youth entered the final observed employment stint
transitions3$first_employment_age3 <- NA
transitions3$first_wage_age3 <- NA
transitions3$first_self_age3 <- NA
transitions3$first_job3 <- NA

#calculate durations/ages once more
for (k in 1:752) {
  for (i in 10:27) {
    if (!is.na(transitions3[[i]][[k]])) {
      if (transitions3[[i]][[k]] %in% c("In School", "Apprentice")) {
        transitions3[["graduation_age3"]][[k]] <- i+2
        transitions3[["transition_ready"]][[k]] <- 1
      }
      if (!is.na(transitions3[[i-1]][[k]]) && transitions3[["transition_ready"]][[k]] == 1 && transitions3[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions3[[i]][[k]] %in% c("Employed", "Self-Employed")) {
        transitions3[["transition_age3"]][[k]] <- i+2
      }
      if (transitions3[[i]][[k]] %in% c("Employed") && transitions3[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Self-Employed") && transitions3[["transition_ready"]][[k]] == 1 && is.na(transitions3[["first_employment_age3"]][[k]])) {
        transitions3[["first_wage_age3"]][[k]] <- i+2
      }
      if (transitions3[[i]][[k]] %in% c("Self-Employed") && transitions3[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice", "Employed") && transitions3[["transition_ready"]][[k]] == 1 && is.na(transitions3[["first_employment_age3"]][[k]])) {
        transitions3[["first_self_age3"]][[k]] <- i+2
      }
      if (transitions3[[i]][[k]] %in% c("Employed", "Self-Employed") && transitions3[[i-1]][[k]] %in% c("NEET", "In School", "Apprentice") && transitions3[["transition_ready"]][[k]] == 1 && is.na(transitions3[["first_employment_age3"]][[k]])) {
        transitions3[["first_employment_age3"]][[k]] <- i+2
        transitions3[["first_job3"]][[k]] <- transitions3[[i]][[k]]
      }
      if (transitions3[[i]][[k]] %in% c("NEET", "In School", "Apprentice")) {
        transitions3[["transition_age3"]][[k]] <- NA
      }
    }
  }
} 

# transition duration is the difference between the age of entry into the last uninterrupted period of employment and the age of school leaving

transitions <- transitions %>% select(IDYouth, c("act13":"act19"), c("occ13":"occ28"), c("status_0", "status_1", "status_2", "status_3", "status_4"), graduation_age, first_employment_age, transition_age) %>% mutate(transition_duration = (transition_age-graduation_age)*12)

transitions2[c("status_0", "status_1", "status_2", "status_3", "status_4")] <- transitions[c("status_0", "status_1", "status_2", "status_3", "status_4")]

transitions2 <- transitions2 %>% select(IDYouth, c("act13":"act19"), c("occ13":"occ28"), c("status_0", "status_1", "status_2", "status_3", "status_4"), transition_ready, graduation_age2 = graduation_age, first_employment_age2 =  first_employment_age, first_wage_age2 =  first_wage_age, first_self_age2 =  first_self_age, first_act2 = first_act, first_job2 = first_job, transition_age2 = transition_age) 

transitions2 <- transitions2 %>% mutate(transition_age2 = as.numeric(transition_age2), graduation_age2 = as.numeric(graduation_age2)) %>% mutate(transition_duration2 = (transition_age2-graduation_age2)*12)

transitions3 <- transitions3 %>% select(IDYouth, c("act13":"act19"), act20, c("occ13":"occ29"), graduation_age3, first_employment_age3, first_wage_age3, first_self_age3, first_job3, transition_age3) %>% mutate(transition_duration3 = (transition_age3-graduation_age3)*12)

# append all to ys_panel
ys_panel <- ys_panel %>% left_join(transitions %>% select(IDYouth, graduation_age, first_employment_age, transition_age, transition_duration), by = "IDYouth") %>% 
  left_join(transitions2 %>% select(IDYouth, transition_ready, graduation_age2, first_act2, first_job2, first_employment_age2, first_wage_age2, first_self_age2, transition_age2, transition_duration2), by = "IDYouth") %>% 
  left_join(transitions3 %>% select(IDYouth, graduation_age3, first_employment_age3, first_wage_age3, first_self_age3, transition_age3, transition_duration3), by = "IDYouth")

#calculate weighted averages of transition age, transition duration, etc.

df <- ys_panel %>% filter(wave == 0) %>% haven::as_factor()

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)
options(survey.lonely.psu="adjust") # circumvent small strata problem for regions

#some descriptives

colSums(!is.na(df %>% select(first_employment_age, transition_age, graduation_age, transition_duration)))
colSums(!is.na(df %>% select(first_employment_age2, transition_age2, graduation_age2, transition_duration2)))
colSums(!is.na(df %>% select(first_employment_age3, transition_age3, graduation_age3, transition_duration3)))

mean(df$graduation_age, na.rm=TRUE) #raw mean
sd(df$graduation_age, na.rm=TRUE)
svymean(~graduation_age, sstrat, na.rm=TRUE)  #weighted mean

mean(df$first_employment_age, na.rm=TRUE) #raw mean
sd(df$first_employment_age, na.rm=TRUE)
svymean(~first_employment_age, sstrat, na.rm=TRUE)  #weighted mean

mean(df$transition_age, na.rm=TRUE) #raw mean
sd(df$transition_age, na.rm=TRUE)
svymean(~transition_age, sstrat, na.rm=TRUE) #weighted mean

mean(df$transition_duration, na.rm=TRUE) #raw mean
sd(df$transition_duration, na.rm=TRUE)
svymean(~transition_duration, sstrat, na.rm=TRUE) #weighted mean

mean(df$graduation_age2, na.rm=TRUE) #raw mean
sd(df$graduation_age2, na.rm=TRUE)
svymean(~graduation_age2, sstrat, na.rm=TRUE)  #weighted mean

mean(df$first_employment_age2, na.rm=TRUE) #raw mean
sd(df$first_employment_age2, na.rm=TRUE)
svymean(~first_employment_age2, sstrat, na.rm=TRUE)  #weighted mean

mean(df$transition_age2, na.rm=TRUE) #raw mean
sd(df$transition_age2, na.rm=TRUE)
svymean(~transition_age2, sstrat, na.rm=TRUE) #weighted mean

mean(df$transition_duration2, na.rm=TRUE) #raw mean
sd(df$transition_duration2, na.rm=TRUE)
svymean(~transition_duration2, sstrat, na.rm=TRUE) #weighted mean

mean(df$graduation_age3, na.rm=TRUE) #raw mean
sd(df$graduation_age3, na.rm=TRUE)
svymean(~graduation_age3, sstrat, na.rm=TRUE)  #weighted mean

mean(df$first_employment_age3, na.rm=TRUE) #raw mean
sd(df$first_employment_age3, na.rm=TRUE)
svymean(~first_employment_age3, sstrat, na.rm=TRUE)  #weighted mean

mean(df$transition_age3, na.rm=TRUE) #raw mean
sd(df$transition_age3, na.rm=TRUE)
svymean(~transition_age3, sstrat, na.rm=TRUE) #weighted mean

mean(df$transition_duration3, na.rm=TRUE) #raw mean
sd(df$transition_duration3, na.rm=TRUE)
svymean(~transition_duration3, sstrat, na.rm=TRUE) #weighted mean

sstrat %>% 
  tbl_svysummary(
    by = sex,
    # summarize a subset of the columns
    include = c(sex, graduation_age, graduation_age2, graduation_age3, first_employment_age, first_employment_age2, first_employment_age3, transition_age, transition_age2, transition_age3, transition_duration, transition_duration2, transition_duration3),
    label = list(graduation_age = "Event history 2013-2019",
                 graduation_age2 = "Combined data",
                 graduation_age3 = "Combined measure",
                 first_employment_age = "Event history 2013-2019",
                 first_employment_age2 = "Combined data", 
                 first_employment_age3 = "Combined measure", 
                 transition_age = "Event history 2013-2019", 
                 transition_age2 = "Combined data",
                 transition_age3 = "Combined measure",
                 transition_duration = "Event history 2013-2019",
                 transition_duration2 = "Combined data",
                 transition_duration3 = "Combined measure"),
    type = list(graduation_age2 ~ "continuous",
                transition_duration ~ "continuous",
                transition_duration2 ~ "continuous",
                transition_duration3 ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({N_nonmiss_unweighted})"),
    missing = "no") %>% 
  modify_header(list(stat_1 ~ "**Female**, N={n_unweighted}",
                     stat_2 ~ "**Male**, N={n_unweighted}",
                     label ~ "**Measure**")) %>% 
  add_overall(col_label = "**Overall**, N={N_unweighted}") %>% 
  modify_table_body(
    mutate,
    group_variable = case_when(variable %in% c("graduation_age", "graduation_age2", "graduation_age3") ~ "Graduation age (years)",
                               variable %in% c("first_employment_age", "first_employment_age2", "first_employment_age3") ~ "First Employment Age (years)",
                               variable %in% c("transition_age", "transition_age2", "transition_age3") ~ "Steady Employment Age (years)",
                               variable %in% c("transition_duration", "transition_duration2", "transition_duration3") ~ "Transition Duration (months)")) %>%
  modify_table_body(group_by, group_variable) %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()

rm(df, x, i, k, sstrat)
