# Package names
packages <- c("here", "haven", "gtsummary", "tidyverse", "survey", "ggplot2", "scales", "texreg")
#detach(package:here, unload=TRUE)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

#load baseline (not cleaned)
ys <- read_sav(here("data", "youth_survey", "youth_survey_merged.sav"))

#load panel
source(here("R", "source", "load_panel.R"))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i) # act#: # corresponds to year
  ys[[z]] <- coalesce(ys[[y]], ys[[x]])
}

ys_baseline <- ys_panel %>% filter(wave == 0)

# create empty occupations occupation at each age (possible ages: 13-29)
for (i in 13:28) { ## possible age of youth
  new <- rep(NA, nrow(ys_baseline))
  ys_baseline[ , ncol(ys_baseline) + 1] <- new
  colnames(ys_baseline)[ncol(ys_baseline)] <- paste0("occ", i)
}

for (k in 1:752){
  for (i in 20:29) { ## possible age of youth
    if (ys_baseline$baseline_age[k] == i) {
      for (j in 1:7) { ## 7 years of recall
        x <- paste0("act", 20-j) ## activity by year (stored in previous loop)
        y <- paste0("occ", i-j) ## age that year
        ys_baseline[[y]][[k]] <- ys_baseline[[x]][[k]]
      }
    }
  }
}

dflist <- ys_baseline %>% 
  dplyr::select(starts_with("occ")) %>% 
  names()

ys_baseline$transition_age <- NA
ys_baseline$first_employment_age <- NA
ys_baseline$transition_ready <- 0 # dummy indicating if the youth was in some sort of schooling or training at any point in the past 7 years

# the transition age is reset if youth revert to NEET or schooling status
for (k in 1:752){
  for (i in 1:15){
    x <- dflist[i+1]
    y <- dflist[i]
    if(ys_baseline[[y]][[k]] %in% c(1:5)) {
      ys_baseline[["transition_ready"]][[k]] <- 1
      if (ys_baseline[[x]][[k]] %in% c(6:7) & ys_baseline[[y]][[k]] %in% c(1:5,8:9) && ys_baseline[["transition_ready"]][[k]] == 1) {
        ys_baseline[["transition_age"]][[k]] <- i+13
      }
      if (ys_baseline[[x]][[k]] %in% c(6:7) & ys_baseline[[y]][[k]] %in% c(1:5,8:9) && ys_baseline[["transition_ready"]][[k]] == 1 & is.na(ys_baseline[["first_employment_age"]][[k]])) {
        ys_baseline[["first_employment_age"]][[k]] <- i+13
      }
      if (ys_baseline[[x]][[k]] %in% c(1,1:6,9)) {
        ys_baseline[["transition_age"]][[k]] <- NA
      }
    }
  }
}

df <- ys_baseline %>% select("IDYouth", starts_with("act1"))
ys_panel <- left_join(ys_panel, df, by = "IDYouth")

dflist <- ys_baseline %>% 
  dplyr::select(starts_with("act1")) %>% 
  names()

ys_panel$transition_ready <- 0
ys_panel$transitioned <- 0
ys_panel$yrs_school <- 0
ys_panel$yrs_self <- 0
ys_panel$yrs_emp <- 0
ys_panel$yrs_neet <- 0
ys_panel$num_transitions <- 0


# only those who haven't transitioned will be taken into account
for (k in 1:2439){
  for (i in 1:7){
    y <- dflist[i]
    if(ys_panel[[y]][[k]] %in% c(1:5)) {
      ys_panel[["transition_ready"]][[k]] <- 1
      ys_panel[["yrs_school"]][[k]] <- ys_panel[["yrs_school"]][[k]] + 1
    }
    if(ys_panel[[y]][[k]] %in% c(6:7)) {
      ys_panel[["transitioned"]][[k]] <- 1
    }
    if(!is.na(ys_panel[[y]][[k]]) && ys_panel[[y]][[k]] == 6) {
      ys_panel[["yrs_emp"]][[k]] <- ys_panel[["yrs_emp"]][[k]] + 1
    }
    if(!is.na(ys_panel[[y]][[k]]) && ys_panel[[y]][[k]] == 7) {
      ys_panel[["yrs_self"]][[k]] <- ys_panel[["yrs_self"]][[k]] + 1
    }
    if(ys_panel[[y]][[k]] %in% c(1,8)) {
      ys_panel[["yrs_neet"]][[k]] <- ys_panel[["yrs_neet"]][[k]] + 1
    }
    if(i > 1) {
      x <- dflist[i-1]
      ys_panel[["num_transitions"]][[k]] = ifelse(ys_panel[[y]][[k]] %in% c(1:3) && ys_panel[[x]][[k]] %in% c(1:3), ys_panel[["num_transitions"]][[k]], 
                                                  ifelse(ys_panel[[y]][[k]] %in% c(4:5) && ys_panel[[x]][[k]] %in% c(4:5), ys_panel[["num_transitions"]][[k]], 
                                                         ifelse(ys_panel[[y]][[k]] %in% c(6:7) && ys_panel[[x]][[k]] %in% c(6:7), ys_panel[["num_transitions"]][[k]], 
                                                                ifelse(ys_panel[[y]][[k]] %in% c(0,8,99) && ys_panel[[x]][[k]] %in% c(0,8,99), ys_panel[["num_transitions"]][[k]],
                                                                       ifelse(is.na(ys_panel[[y]][[k]]) && is.na(ys_panel[[x]][[k]]), ys_panel[["num_transitions"]][[k]], ys_panel[["num_transitions"]][[k]] + 1)))))
    }
  }
}

x <- ys_panel %>% filter(wave == 0) %>% select(IDYouth, transitioned, yrs_school, yrs_self, yrs_emp, yrs_neet, num_transitions)

ys_baseline <- left_join(ys_baseline, x, by = "IDYouth")

rm(x)

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(act20 = ifelse(status_0 %in% c("Employed", "Self-Employed") &
                          status_1 %in% c("Employed", "Self-Employed", NA) &
                          status_2 %in% c("Employed", "Self-Employed", NA) &
                          status_3 %in% c("Employed", "Self-Employed", NA), 1, 0)) %>% 
  dplyr::select(IDYouth, status_0, status_1, status_2, status_3, act20)

ys_baseline <- left_join(ys_baseline, df, by = "IDYouth")
rm(df)

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(act20_emp = ifelse(status_0 %in% c("Employed") &
                          status_1 %in% c("Employed", NA) &
                          status_2 %in% c("Employed", NA) &
                          status_3 %in% c("Employed", NA), 1, 0)) %>% 
  dplyr::select(IDYouth, act20_emp)

ys_baseline <- left_join(ys_baseline, df, by = "IDYouth")
rm(df)

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(act20_self = ifelse(status_0 %in% c("Self-Employed") &
                          status_1 %in% c("Self-Employed", NA) &
                          status_2 %in% c("Self-Employed", NA) &
                          status_3 %in% c("Self-Employed", NA), 1, 0)) %>% 
  dplyr::select(IDYouth, act20_self)

ys_baseline <- left_join(ys_baseline, df, by = "IDYouth")
rm(df)

rm(i, j, k, new, x, y, z)

## REGRESSION 1: ACT20 (TRANSITIONED) ON COVARIATES

ys_baseline <- ys_baseline %>% 
  mutate(beninese = ifelse(YS3_1 == 1, 1, 0),
         fathapp = YS3_9,
         mothapp = YS3_11,
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
         future_good = ifelse(YS6_19 > 4, 1, 0),
         shape_fate = ifelse(YS6_20 > 3, 1, 0),
         yos = ifelse(YS3_15 < 50, YS3_15, NA))

m1 <- glm(act20 ~ yrs_school + yrs_neet + num_transitions + baseline_age, ys_baseline, family = binomial)
m1 <- extract(m1)

m2 <- glm(act20 ~ yrs_school + yrs_neet + num_transitions + baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline, family = binomial)
m2 <- extract(m2)

m3 <- glm(act20_emp ~ yrs_school + yrs_neet + num_transitions + baseline_age, ys_baseline, family = binomial)
m3 <- extract(m3)

m4 <- glm(act20_emp ~ yrs_school + yrs_neet + num_transitions + baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline, family = binomial)
m4 <- extract(m4)

m5 <- glm(act20_self ~ yrs_school + yrs_neet + num_transitions +  baseline_age, ys_baseline, family = binomial)
m5 <- extract(m5)

m6 <- glm(act20_self ~ yrs_school + yrs_neet + num_transitions + baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline, family = binomial)
m6 <- extract(m6)

texreg(list(m1, m2, m3, m4, m5, m6),
       override.coef = list(exp(m1@coef),exp(m2@coef),exp(m3@coef),exp(m4@coef),exp(m5@coef),exp(m6@coef)),
       override.se = list(exp(m1@se),exp(m2@se),exp(m3@se),exp(m4@se),exp(m5@se),exp(m6@se)),
       omit.coef = c('(Intercept)|mu|sigma|beninese|city|fon|christian|siblings|fsecplus|fathapp|msecplus|mothapp|bac'),
       custom.coef.names = c("Years of school (in last 7)", "Years NEET (in last 7)", "No. transitions (in last 7)", "Age at baseline", "Male", "Total years of schooling", "Completed apprenticeship"),
       custom.model.names = c("Self or Wage", "Self or Wage", "Wage", "Wage", "Self", "Self"),
       custom.gof.rows = list("Additional covariates" = c("NO", "YES", "NO", "YES", "NO", "YES")),
       booktabs = TRUE,
       fontsize = "footnotesize",
       float.pos = "H",
       label = "tab:transition_reg",
       custom.note = paste("\\item %stars. Odds ratios reported."),
       single.row = TRUE,
       threeparttable = TRUE,
       caption = "Logistic regression of transition to work in 2019/2020")
       

## REGRESSION 2: TRANSITION AGE

ys_baseline$transition_ready <- 0 # dummy indicating if the youth was in some sort of schooling or training at any point in the past 7 years

# the transition age is reset if youth revert to NEET or schooling status
for (k in 1:752){
  for (i in 1:15){
    x <- dflist[i+1]
    y <- dflist[i]
    if(ys_baseline[[y]][[k]] %in% c(1:5)) {
      ys_baseline[["transition_ready"]][[k]] <- 1
      if (ys_baseline[[x]][[k]] %in% c(6:7) & ys_baseline[[y]][[k]] %in% c(1:5,8:9) && ys_baseline[["transition_ready"]][[k]] == 1) {
        ys_baseline[["transition_age"]][[k]] <- i+13
      }
      if (ys_baseline[[x]][[k]] %in% c(6:7) & ys_baseline[[y]][[k]] %in% c(1:5,8:9) && ys_baseline[["transition_ready"]][[k]] == 1 & is.na(ys_baseline[["first_employment_age"]][[k]])) {
        ys_baseline[["first_employment_age"]][[k]] <- i+13
      }
      if (ys_baseline[[x]][[k]] %in% c(1,1:6,9)) {
        ys_baseline[["transition_age"]][[k]] <- NA
      }
    }
  }
}

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(first_employment_age3 = ifelse(status_0 %in% c("Employed", "Self-Employed"), age_0,
                                        ifelse(status_1 %in% c("Employed", "Self-Employed"), age_1,
                                               ifelse(status_2 %in% c("Employed", "Self-Employed"), age_2,
                                                      ifelse(status_3 %in% c("Employed", "Self-Employed"), age_3, NA))))) %>% 
  dplyr::select(IDYouth, status_0, status_1, status_2, status_3, first_employment_age3)

ys_baseline <- left_join(ys_baseline, df, by = "IDYouth")


ys_baseline <- ys_baseline %>% 
  mutate(transition_age3 = ifelse(transition_ready == 1 & is.na(transition_age) & act20 == 1, first_employment_age3, 
                                  ifelse(!is.na(transition_age) & act20 == 0, NA, transition_age)),
         first_employment_age3 = ifelse(transition_ready == 1 & is.na(first_employment_age), first_employment_age3, first_employment_age))

m7 <- lm(first_employment_age3 ~ yrs_school + yrs_neet + baseline_age, ys_baseline, family = binomial)
m7 <- extract(m7)

m8 <- lm(first_employment_age3 ~ yrs_school + yrs_neet + baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline, family = binomial)
m8 <- extract(m8)

m9 <- lm(transition_age3 ~ yrs_school + yrs_neet + baseline_age, ys_baseline, family = binomial)
m9 <- extract(m9)

m10 <- lm(transition_age3 ~ yrs_school + yrs_neet + baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline, family = binomial)
m10 <- extract(m10)



texreg(list(m7, m8, m9, m10),
       omit.coef = c('(Intercept)|mu|sigma|beninese|city|fon|christian|siblings|fsecplus|fathapp|msecplus|mothapp|bac'),
       custom.coef.names = c("Years of school (in last 7)", "Years NEET (in last 7)", "Age", "Male", "Total Years of Schooling", "Completed Apprenticeship"),
       custom.model.names = c("First Employment Age", "First Employment Age", "Transition Age", "Transition Age"),
       custom.gof.rows = list("Covariates" = c("NO", "YES", "NO", "YES")),
       booktabs = TRUE,
       fontsize = "small",
       float.pos = "H",
       caption = "Determinants of age at first employment and transition")



## REGRESSION 3: TRANSITION DURATION
df <- ys_baseline %>% 
  dplyr::select(IDYouth, c("act13", "act14","act15","act16","act17","act18","act19", "act20")) %>% 
  mutate(act20 = ifelse(act20 == 1, 6, act20))

df$transition_duration3 <- NA
df$transition_ready <- 0

for (i in 1:752) {
  for (j in 2:9) {
    if (df[i,j] %in% c(1:5)) {
      df$transition_ready[i] <- 1
      df$transition_duration3[i] <- NA # reset transition duration to zero if youth goes back to school
    }
    if (df$transition_ready[i] == 1 && is.na(df$transition_duration3[i]) && df[i,j] %in% c(6,7)) {
      df$transition_duration3[i] <- 0 # immediate transition
    }
    if (df$transition_ready[i] == 1 && is.na(df$transition_duration3) && df[i,j] %in% c(0,8)) {
      df$transition_duration3[i] <- 1 # initiate transition duration if NEET or unemployed
    }
    if (df$transition_ready[i] == 1 && !is.na(df$transition_duration3) && df[i,j] %in% c(0,8)) {
      df$transition_duration3[i] <- df$transition_duration3[i] + 1 # initiate transition duration if NEET or unemployed
    }
  }
}


df <- df %>% 
  mutate(transition_duration3 = transition_duration3*12) %>% # duration in months
  select(IDYouth, transition_duration3)

ys_baseline <- left_join(ys_baseline, df, by = "IDYouth")

m3 <- lm(transition_duration3 ~ baseline_age + sex + yos + bac + formapp + beninese + city + fon + christian + siblings + fsecplus + fathapp + msecplus + mothapp, ys_baseline) %>% 
  tbl_regression(label = list(yos = "Years of schooling",
                                                   bac = "Baccalauréate",
                                                   beninese = "Benin nationality",
                                                   city = "From city",
                                                   fon = "Majority ethnicity (Fon)",
                                                   christian = "Christian",
                                                   fsecplus = "Father secondary school",
                                                   fathapp = "Father apprenticeship",
                                                   msecplus = "Mother secondary school",
                                                   mothapp = "Mother apprenticeship"))
tbl_merge(
  tbls = list(m1, m2, m3),
  tab_spanner = c("**Transitioned (Logit)**", "**Transition Age (OLS)**", "**Transition Duration (OLS)**")) %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()



