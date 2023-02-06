####################
## Data Cleaning  ##
####################

# Package names
packages <- c("haven", "tidyverse", "labelled")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

## Data
ys_panel <- read_sav("data/youth_survey_reshaped.sav", user_na = TRUE)

# filter by age and code status
ys_panel <- ys_panel %>% filter(baseline_age >=20 & baseline_age <= 29,
                                !is.na(YS1_1))

ys_panel <- ys_panel %>% 
  mutate(status = ifelse(YS4_1 == 1 | (YS1_2 == 1 & wave == 0), 5, 
                         ifelse(YS8_4 %in% c(1,2,3,5,6,8), 4,
                                ifelse(YS8_4 == 4, 3, NA)))) %>% 
  mutate(status = ifelse(is.na(status) & YS7_1 == 1, 1, status)) %>%
  mutate(status = ifelse(is.na(status) & F3U2_0a == 1 & YS8_4 %in% c(1,2,3,5,6,8), 4, status)) %>% 
  mutate(status = ifelse(is.na(status) & YE3_5 == 1 & YS8_4 %in% c(1,2,3,5,6,8), 4, status)) %>% 
  mutate(status = ifelse(is.na(status) & YE3_5 == 1 & YS8_4 == 4, 3, status)) %>% 
  mutate(status = ifelse(is.na(status), 2, status))

ys_panel$baseline_activity <- factor(ys_panel$baseline_activity, levels = c(1,2,3,4,5), labels = c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

# label status and generate lagged status

ys_panel$status <- factor(ys_panel$status, levels = c(1:5), labels=c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

df <- ys_panel %>% select(IDYouth, status, wave)

df = expand.grid(
  IDYouth = sort(unique(df$IDYouth)),
  wave   = seq(from = min(df$wave), to = max(df$wave))
) %>% 
  left_join(df, by = c("IDYouth", "wave")) %>% 
  arrange(IDYouth, wave) %>% 
  group_by(IDYouth) %>% 
  mutate(lagged_status = lag(status)) %>% 
  ungroup() %>% 
  select(-status)

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

# filter out CQP apprentices
ys_panel <- ys_panel %>% 
  filter(cqp == 0)

# replace missing values with 0s
ys_panel <- ys_panel %>% mutate_at(vars(starts_with("YS3_17")), ~ if_else(is.na(.), '0', '1'))

## generate weights

# replace census activity with baseline activity for single missing observation, region should be COTONOU
ys_panel <- ys_panel %>% mutate(activite = ifelse(IDYouth == "YS5613", "Ecolier, élève, ou étudiant", activite),
                                region = ifelse(IDYouth == "YS5613", "COTONOU", region),
                                ZD = ifelse(IDYouth == "YS5613", "8-1-53-12-4", ZD))

# generate multi-stage sampling information
x <- ys_panel %>% filter(wave == 0) %>% 
  group_by(activite) %>% 
  mutate(act_id = cur_group_id(), #strata id numbers (equivalent to dnum and snum in api example)
         act_nh = n()) %>% #number of observations per activity strata (nh)
  group_by(region) %>% 
  mutate(reg_id = cur_group_id(), #strata id numbers (equivalent to dnum and snum in api example)
         reg_nh = n_distinct(ZD)) %>%  #number of ZD per arrondissement
  ungroup() %>% 
  mutate(act_nh = ifelse(activite %in% c("Occupé, salarié occasionnel", "Occupé, salarié permanent"), 97, act_nh)) %>% 
  mutate(act_Nh = ifelse(activite == "Autre (cherche 1ère travail, ménagère, aide familiale, ou autre inactif)", 554,
                         ifelse(activite == "Ecolier, élève, ou étudiant", 1067,
                                ifelse(activite == "Occupé indépendant", 1125,
                                       ifelse(activite %in% c("Occupé, salarié occasionnel", "Occupé, salarié permanent"), 406, NA)))),
         reg_Nh = ifelse(region == "ABOMEY-CALAVI", 69,
                         ifelse(region == "AGBLANGANDAN", 62,
                                ifelse(region == "COTONOU", 618,
                                       ifelse(region == "GODOMEY", 167, NA)))),
         prob = (act_nh/act_Nh)*(reg_nh/reg_Nh)) %>% 
  select(IDYouth, act_id, act_nh, reg_id, reg_nh, act_Nh, reg_Nh, prob)

ys_panel <- left_join(ys_panel, x, by = "IDYouth")

# recode values

ys_panel <- ys_panel %>%
  mutate(yeduc = case_when(
    YS3_16 == 0 ~ "None",
    YS3_16 == 1 ~ "<Primary",
    YS3_16 == 2 ~ "<Primary",
    YS3_16 == 3 ~ "Primary",
    YS3_16 == 4 ~ "Collège",
    YS3_16 == 5 ~ "Lycée",
    YS3_16 == 6 ~ "Collège",
    YS3_16 == 7 ~ "Lycée",
    YS3_16 == 8 ~ "Tertiary",
    YS3_16 == 9 ~ "Tertiary",
    YS3_16 == 10 ~ "Other")) %>%
  mutate(feduc = case_when(
    YS3_10 == 0 ~ "None",
    YS3_10 == 1 ~ "<Primary",
    YS3_10 == 2 ~ "<Primary",
    YS3_10 == 3 ~ "Primary",
    YS3_10 == 4 ~ "Collège",
    YS3_10 == 5 ~ "Lycée",
    YS3_10 == 6 ~ "Collège",
    YS3_10 == 7 ~ "Lycée",
    YS3_10 == 8 ~ "Tertiary",
    YS3_10 == 9 ~ "Tertiary",
    YS3_10 == 10 ~ "Other")) %>%
  mutate(meduc = case_when(
    YS3_12 == 0 ~ "None",
    YS3_12 == 1 ~ "<Primary",
    YS3_12 == 2 ~ "<Primary",
    YS3_12 == 3 ~ "Primary",
    YS3_12 == 4 ~ "Collège",
    YS3_12 == 5 ~ "Lycée",
    YS3_12 == 6 ~ "Collège",
    YS3_12 == 7 ~ "Lycée",
    YS3_12 == 8 ~ "Tertiary",
    YS3_12 == 9 ~ "Tertiary",
    YS3_12 == 10 ~ "Other")) %>% 
  mutate(yos = ifelse(YS3_15 != 99, YS3_15, 0),
         secplus = ifelse(YS3_16 > 4 & YS3_16 != 10, 1, 0),
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
         siblings = as.numeric(YS3_7),
         beninese = YS3_1,
         fon = ifelse(!is.na(YS3_4_4), 1, 0),
         christian = ifelse(!is.na(YS3_5_1) | !is.na(YS3_5_2) | !is.na(YS3_5_3) | !is.na(YS3_5_4), 1, 0),
         city = ifelse(YS3_3 == 4, 1, 0), 
         age_cat2 = case_when(age %in% c(16:18) ~ "16-18",
                              age %in% c(19:21) ~ "19-21",
                              age %in% c(22:24) ~ "22-24",
                              age %in% c(25:27) ~ "25-27",
                              age %in% c(28:30) ~ "28-30"))

## WEALTH INDEX

x <- ys_panel %>% filter(wave == 0) %>% 
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
  select(IDYouth, YS6_2, YS6_3, YS6_5, housing, water, walls, floor, roof, contains("YS6_11"))

prn<-psych::principal(x[2:length(x)], rotate="varimax", nfactors=3,covar=T, scores=TRUE)
index<-prn$scores[,1]
nlab<-c(1,2,3,4,5)
x <- cbind(x, index) %>% 
  mutate(wealth_quintile = as.numeric(as.factor(cut(index, breaks = 5, labels = nlab)))) %>% 
  select(IDYouth, index, wealth_quintile)

ys_panel <- left_join(ys_panel, x, by = "IDYouth")

## PAST ACTIVITIES

# for each youth, we calculate the past activities by year (actxx) and age (occxx) 

df <- ys_panel %>% 
  filter(wave %in% c(0, 2)) %>% 
  pivot_wider(id_cols = c("IDYouth", "baseline_age"),
              names_from = wave,
              values_from = c("YS6_16_1", "YS6_16_2", "YS6_16_3", "YS6_16_4", "YS6_16_5", "YS6_16_6", "YS6_16_7"))

for (i in 1:7) {
  x <- paste0("YS6_16_", i, "_0")
  y <- paste0("YS6_16_", i, "_2")
  z <- paste0("act", 20-i) # act#: # corresponds to year
  df[[z]] <- coalesce(df[[y]], df[[x]])
}

df <- df %>% select(-starts_with("YS6_16_"))

# create empty occupations occupation at each age (possible ages: 13-28)
for (i in 13:28) { ## possible age of youth
  new <- rep(NA, nrow(df))
  df[ , ncol(df) + 1] <- new
  colnames(df)[ncol(df)] <- paste0("occ", i)
}

for (i in 20:28) { ## possible age of youth
  for (j in 7:1) { ## 7 years of recall
    x<-paste0("occ", i-j+1)
    df[[x]] = NA
  }
}

for (k in 1:752){
  for (i in 20:29) { ## possible age of youth
    if (df$baseline_age[k] == i) {
      for (j in 1:7) { ## 7 years of recall
        x <- paste0("act", 20-j) ## activity by year (stored in previous loop)
        y <- paste0("occ", i-j) ## age that year
        df[[y]][[k]] <- df[[x]][[k]]
      }
    }
  }
}

df <- df %>% select(-baseline_age)

ys_panel <- left_join(ys_panel, df, by = "IDYouth")

ys_baseline <- ys_panel %>% filter(wave == 0)

ys_baseline <- ys_baseline %>% 
  mutate(across(contains('act1'), ~ case_when(. == 1 | . == 2 | . == 3 ~ "In School",
                                              . == 0 | . == 8 | . == 99 ~ "NEET",
                                              . == 7 ~ "Self-Employed",
                                              . == 4 | . == 5 ~ "Apprentice",
                                              . == 6 ~ "Employed")),
         age13 = baseline_age - 6,
         age14 = baseline_age - 5,
         age15 = baseline_age - 4,
         age16 = baseline_age - 3,
         age17 = baseline_age - 2,
         age18 = baseline_age - 1,
         age19.1 = baseline_age)

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(act19.2 = status_0,
         age19.2 = age_0,
         act19.3 = status_1,
         age19.3 = age_1,
         act20.1 = status_2,
         age20.1 = age_2,
         act20.2 = status_3,
         age20.2 = age_3,
         act21 = status_4,
         age21 = age_4) %>% 
  select(-contains("age_"), -contains("status"))


## AGE AT FIRST EMPLOYMENT; DURATION BETWEEN GRADUATION AND FIRST EMPLOYMENT

ys_baseline <- left_join(ys_baseline, df, by = c("IDYouth"))

df <- ys_baseline %>% 
  select(IDYouth, contains("age2"), contains("age1"), contains("act1"), contains("act2"))

df <- df[ , order(names(df))]

df$graduation_age <- NA
df$first_employment_age <- NA
df$first_wage_age <- NA
df$first_self_age <- NA
df$right_censored <- 0
df$entry <- NA

actlist <- df %>% 
  dplyr::select(starts_with("act")) %>% 
  names()

actlist <- sort(actlist)

agelist <- df %>% 
  dplyr::select(starts_with("age")) %>% 
  names()

agelist <- sort(agelist)

# graduation age: only includes observed period (2013 and after)
# first employment age: erased if youth goes back to apprenticeship/schooling

for (k in 1:752){
  for (i in 1:11){
    x <- actlist[i]
    y <- actlist[i+1]
    z <- agelist[i]
    w <- agelist[i+1]
    if (df[[x]][[k]] %in% c("In School", "Apprentice")) { # reset if return to schooling
      df[["first_employment_age"]][[k]] <- NA
      df[["graduation_age"]][[k]] <- NA
    }
    if (df[[x]][[k]] %in% c("In School", "Apprentice") && df[[y]][[k]] %in% c("NEET", "Self-Employed", "Employed")) {
      df[["graduation_age"]][[k]] <- df[[z]][[k]]
    }
    if (df[[y]][[k]] %in% c("Self-Employed", "Employed") && is.na(df[["first_employment_age"]][[k]]) && !(is.na(df[["graduation_age"]][[k]]))) {
      df[["first_employment_age"]][[k]] <- df[[w]][[k]]
    }
    if (df[[y]][[k]] %in% c("Self-Employed", "Employed", "NEET") && is.na(df[["entry"]][[k]]) && !(is.na(df[["graduation_age"]][[k]]))) {
      df[["entry"]][[k]] <- as.character(df[[y]][[k]])
    }
    if (df[[x]][[k]] %in% "Employed" && is.na(df[["first_wage_age"]][[k]]) && !(is.na(df[["graduation_age"]][[k]]))) {
      df[["first_wage_age"]][[k]] <- df[[z]][[k]]
    }
    if (df[[x]][[k]] %in% "Self-Employed" && is.na(df[["first_self_age"]][[k]]) && !(is.na(df[["graduation_age"]][[k]]))) {
      df[["first_self_age"]][[k]] <- df[[z]][[k]]
    }
    if (df[[y]][[k]] %in% c("In School", "Apprentice")) {
      df[["right_censored"]][[k]] <- 1
    }
    if (df[[y]][[k]] %in% c("Self-Employed", "Employed") && !(is.na(df[["graduation_age"]][[k]]))) {
      df[["right_censored"]][[k]] <- 0
    }
  }
}

df$first_employment_duration <- df$first_employment_age - df$graduation_age

ys_panel <- left_join(ys_panel, (df %>% select(IDYouth, first_employment_duration, first_employment_age, graduation_age, right_censored, entry, act19.2, act19.3, contains(c("act2", "age1", "age2")))), by = "IDYouth")

# Transitioned/graduated in past seven years: for transition regression

ys_panel$transition_ready <- 0
ys_panel$transitioned <- 0

dflist <- ys_panel %>% 
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

## EMPLOYMENT TYPES

# "informal" -> wage employed with no contract, self-employed with less than 4 employees (firms < 5 workers), family workers
# "formal" -> wage employed on a regular basis with any contract (written or verbal), self-employed with more than 5 workers

ys_panel <- ys_panel %>% 
  mutate(formal = ifelse(YS8_4 == 9, NA, # no job at the moment -> NA
                         ifelse((YS8_4 == 1 & YS8_10 != 3) | (YS8_4 == 4 & YS9_15 > 4), 1, # single employer regular basis AND any contract OR self-employed w 5+ workers 
                                ifelse(!is.na(YS8_4), 0, NA))))

ys_panel <- ys_panel %>% mutate(informal = ifelse(formal == 0, 1, NA),
                                formal = ifelse(formal == 0, NA, formal))

# "underemp" -> wage- or self-employed who worked less than 35 hours in past week (Benin threshold) or did not work in the past week due to work shortages (strikes, lockouts, temporary layoffs, shortages, low demand, off-season)

# "full-time" -> wage- or self-employed who worked 35 hours or more in the past week, based on multiplying hours worked on last day of work times number of days worked in past 7

ys_panel <- ys_panel %>% 
  mutate(wagedays = coalesce(YS8_17, YS8_18),
         wagehours = wagedays*YS8_19,
         selfhours = ifelse(YS9_13<99, YS9_13*YS9_14, NA),
         hours = ifelse(status == "Employed" | status == "Self-Employed", coalesce(wagehours, selfhours), NA),
         underemp = case_when(hours < 35 ~ 1,
                              YS8_3 > 6 ~ 1,
                              hours >= 35 ~ 0),
         fulltime = ifelse(hours >= 35, 1, 0 )) %>% 
  select(-wagehours, -selfhours)

# "casual" -> one or more employers on *irregular* basis or single employer with irregular/task-based payment
# "regular work" -> single employer with regular wages

ys_panel <- ys_panel %>%
  mutate(casual = ifelse(YS8_4 %in% c(2,3) | (YS8_4 == 1 & YS8_14 %in% c(4:7)), 1, 
                         ifelse(YS8_4 == 1 & YS8_14 %in% c(1:3), 0, NA)))

ys_panel <- ys_panel %>% mutate(regular = ifelse(casual == 0, 1, NA),
                                casual = ifelse(casual == 0, NA, casual))

# "employer" -> self-employed with employees/apprentices
# "independent" -> self-employed, no employees or apprentices

ys_panel <- ys_panel %>%
  mutate(employer = ifelse(YS9_15 > 1 & status == "Self-Employed", 1, 0),
         independent = ifelse(YS9_15 == 1 & status == "Self-Employed", 1, 0))

# create labelled version of ys_panel
ys_panel_labels <- haven::as_factor(ys_panel)

rm(i,j,k,new,w, x,y,z,df, actlist, agelist, dflist, ys_baseline)

# save as .rda
save(ys_panel, file = "data/ys_panel.rda")
save(ys_panel_labels, file = "data/ys_panel_labels.rda")
