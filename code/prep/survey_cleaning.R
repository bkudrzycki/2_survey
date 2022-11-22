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

ys_panel <- ys_panel %>% filter(baseline_age >=20 & baseline_age <= 29,
       !is.na(YS1_1))

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

rm(x)

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
    YS3_12 == 10 ~ "Other"))

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

rm(i,j,k,new,x,y,z,df)

## GRADUATION AGE; FIRST EMPLOYMENT AGE; TRANSITION AGE; TRANSITION READY (dummy for having completed education or training)

ys_panel$transition_ready <- 0
ys_panel$graduation_age <- NA #last recorded year of schooling
ys_panel$transition_age <- NA  #age at which youth entered the final observed employment stint
ys_panel$first_employment_age <- NA
ys_panel$right_censored <- NA #whether transition duration is right censored

dflist <- ys_panel %>% 
  dplyr::select(starts_with("occ")) %>% 
  names()

for (k in 1:3021){
  for (i in 1:15){
    x <- dflist[i+1]
    y <- dflist[i]
    if (ys_panel[[y]][[k]] %in% c(1:5)) {
      ys_panel[["graduation_age"]][[k]] <- i+12
      ys_panel[["transition_ready"]][[k]] <- 1
    }
    if (ys_panel[[x]][[k]] %in% c(1:5)) {
      ys_panel[["first_employment_age"]][[k]] <- NA # if youth goes back to school, any first employment experience is nullified
    }
    if (ys_panel[[x]][[k]] %in% c(6:7) && ys_panel[[y]][[k]] %in% c(0,1:5,8) && ys_panel[["transition_ready"]][[k]] == 1) {
      ys_panel[["transition_age"]][[k]] <- i+13
    }
    if (ys_panel[[x]][[k]] %in% c(6:7) && ys_panel[[y]][[k]] %in% c(0,1:5,8) && ys_panel[["transition_ready"]][[k]] == 1 && is.na(ys_panel[["first_employment_age"]][[k]])) {
      ys_panel[["first_employment_age"]][[k]] <- i+13
    }
    if (ys_panel[[x]][[k]] %in% c(0,1:5,8)) {
      ys_panel[["transition_age"]][[k]] <- NA # if youth goes back to school or stops working, any first employment experience is nullified
    }
  }
  if(ys_panel[["act19"]][[k]] %in% c(1:5)) {
    ys_panel[["graduation_age"]][[k]] <- NA
  }
  if(!is.na(ys_panel[["graduation_age"]][[k]]) && is.na(ys_panel[["transition_age"]][[k]])) { #if school leaving age yes, steady employment no
    ys_panel[["right_censored"]][[k]] <- 1
  }
}

ys_panel <- ys_panel %>% dplyr::mutate(transition_duration = if_else(!is.na(transition_age), transition_age-graduation_age,
                                                                     if_else(!is.na(graduation_age), baseline_age-graduation_age-1, NULL)),
                                       first_transition_duration = if_else(!is.na(first_employment_age), first_employment_age-graduation_age,
                                                                           if_else(!is.na(graduation_age), baseline_age-graduation_age-1, NULL)),
                                       right_censored = ifelse(!is.na(right_censored), 1, 0))

ys_panel$status <- factor(ys_panel$status, levels = c(1:5), labels=c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))

## EMPLOYMENT TYPES

# "informal" -> wage employed with no contract, self-employed with less than 4 employees (firms < 5 workers), family workers
# "formal" -> wage employed on a regular basis with any contract (written or verbal), self-employed with more than 5 workers

ys_panel <- ys_panel %>% 
  mutate(formal = ifelse(YS8_4 == 9, NA, # no job at the moment -> NA
                         ifelse((YS8_4 == 1 & YS8_10 != 3) | (YS8_4 == 4 & YS9_15 > 4), 1, # single employer regular basis AND any contract OR self-employed w 5+ workers 
                                ifelse(!is.na(YS8_4), 0, NA))))

ys_panel <- ys_panel %>% mutate(informal = ifelse(formal == 0, 1, NA),
                                formal = ifelse(formal == 0, NA, formal))

# "underemp" -> wage- or self-employed who worked less than 35 hours in past week (Benin threshold) (90% of these wanted to work more hours in first two waves)

ys_panel <- ys_panel %>% 
  mutate(wagedays = coalesce(YS8_17, YS8_18),
         wagehours = wagedays*YS8_19,
         selfhours = ifelse(YS9_13<99, YS9_13*YS9_14, NA),
         hours = ifelse(status == "Employed" | status == "Self-Employed", coalesce(wagehours, selfhours), NA),
         underemp = ifelse(hours < 35, 1, 0)) %>% 
  select(-wagehours, -selfhours)

ys_panel <- ys_panel %>% mutate(fulltime = ifelse(underemp == 0, 1, NA),
                                underemp = ifelse(underemp == 0, NA, underemp))

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

# save as .rda
save(ys_panel, file = "data/ys_panel.rda")
save(ys_panel_labels, file = "data/ys_panel_labels.rda")
