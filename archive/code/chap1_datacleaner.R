library(nnet)
library(MASS)
library(xtable)
library(haven)
library(tidyverse)

## ---- dataload
ys <- read_sav("./data/youth_survey/youth_survey_merged.sav", user_na = TRUE)

ys <- ys %>% 
  filter(baseline_age >=20 & baseline_age <= 29,
         is.na(YS1_6) | (YS1_6 != 1 & YS1_6 != 3 & YS1_6 != 4 & YS1_6 != 6 & YS1_6 != 7 & YS1_6 != 8 & YS1_6 != 9 )) %>%
  mutate(f1u = ifelse(!is.na(F1U1_1), TRUE, FALSE),
         f2u = ifelse(!is.na(F2U1_1), TRUE, FALSE),
         f3u = ifelse(!is.na(F3U1_1), TRUE, FALSE)) %>% 
  mutate(status = ifelse(YS1_2 == 1 | YS4_1 == 1, "Apprentice",
                         ifelse(YS8_4 == 1 | YS8_4  == 2| YS8_4  == 3 | YS8_4  == 5 | YS8_4  == 6 | YS8_4  == 8, "Employed", ifelse(YS8_4 == 4, "Self-Employed", NA)))) %>% 
  mutate(status = ifelse(is.na(status) & YS7_1 == 1, "In School", status)) %>%
  mutate(status = ifelse(is.na(status), "NEET", status)) %>% 
  mutate(f1ustatus = ifelse(F1U2_1 == 1, "Apprentice",
                            ifelse((F1U1_14 == 1 | F1U1_15 == 1) & F1U4_1 %in% c(1,2,3,5,6,8), "Employed",
                                   ifelse((F1U1_14 == 1 | F1U1_15 == 1) & F1U4_1 == 4, "Self-Employed",
                                          ifelse(F1U6_1 == 1, "In School", NA)))),
         f1ustatus = ifelse(is.na(f1ustatus) & f1u, "NEET", f1ustatus)) %>% 
  mutate(f2ustatus = ifelse(F2U2_1 == 1, "Apprentice",
                            ifelse((F2U1_14 == 1 | F2U1_15 == 1) & F2U4_1 %in% c(1,2,3,5,8), "Employed",
                                   ifelse((F2U1_14 == 1 | F2U1_15 == 1) & F2U4_1 == 4, "Self-Employed",
                                          ifelse(F2U6_1 == 1, "In School", NA)))),
         f2ustatus = ifelse(is.na(f2ustatus) & f2u, "NEET", f2ustatus)) %>% 
  mutate(f3ustatus = ifelse(F3U2_0a == 1 & F3U4_1 %in% c(1,2,3,5,8), "Employed",
                            ifelse(F3U2_0a == 1 & F3U4_1 == 4, "Self-Employed",
                                   ifelse(F3U2_1a == 1, "Apprentice",
                                          ifelse((F3U1_14 == 1 | F3U1_15 == 1) & F3U4_1 %in% c(1,2,3,5,8), "Employed",
                                                 ifelse((F3U1_14 == 1 | F3U1_15 == 1) & F3U4_1 == 4, "Self-Employed",
                                                        ifelse(F3U6_1 == 1, "In School", NA)))))),
         f3ustatus = ifelse(is.na(f3ustatus) & f3u, "NEET", f3ustatus))

ys <- ys %>%
  mutate(grad = ifelse(status == "Apprentice" | status == "In School", "Graduate", "Educ. or Training"))

ys_labels <- haven::as_factor(ys) %>%
  filter(baseline_age >=20 & baseline_age <= 29,
         is.na(YS1_6) | (YS1_6 != "Abomey" & YS1_6 != "Athiémé" & YS1_6 != "Bohicon" & YS1_6 != "Djakatomé" & YS1_6 != "Houeyogbé" & YS1_6 != "Ouidah" & YS1_6 != "Porto-Novo" )) %>%
  mutate(status = ifelse(is.na(status) & YS7_1 == 1, "In School", status)) %>%
  mutate(status = ifelse(is.na(status), "NEET", status)) %>% 
  mutate(f1ustatus = ifelse(F1U2_1 == "Yes", "Apprentice",
                            ifelse((F1U1_14 == "Yes" | F1U1_15 == "Yes") & F1U4_1 %in% c("I work for ONE employer on a regular basis","I work for ONE employer on an irregular basis","I work for different employers on an irregular basis","I help out in the family business", "I work for another family (not my own)","Other (specify)"), "Employed",
                                   ifelse((F1U1_14 == "Yes" | F1U1_15 == "Yes") & F1U4_1 == "I am self-employed", "Self-Employed",
                                          ifelse(F1U6_1 == "Yes", "In School", NA)))),
         f1ustatus = ifelse(is.na(f1ustatus) & f1u, "NEET", f1ustatus)) %>% 
  mutate(f2ustatus = ifelse(F2U2_1 == "Yes", "Apprentice",
                            ifelse((F2U1_14 == "Yes" | F2U1_15 == "Yes") & F2U4_1 %in% c("I work for ONE employer on a regular basis","I work for ONE employer on an irregular basis","I work for different employers on an irregular basis","I help out in the family business", "I work for another family (not my own)","Other (specify)"), "Employed",
                                   ifelse((F2U1_14 == "Yes" | F2U1_15 == "Yes") & F2U4_1 == "I am self-employed", "Self-Employed",
                                          ifelse(F2U6_1 == "Yes", "In School", NA)))),
         f2ustatus = ifelse(is.na(f2ustatus) & f2u, "NEET", f2ustatus)) %>% 
  mutate(f3ustatus = ifelse(F3U2_0a == "Yes" & F3U4_1 %in% c("I work for ONE employer on a regular basis","I work for ONE employer on an irregular basis","I work for different employers on an irregular basis","I help out in the family business", "I work for another family (not my own)","Other (specify)"), "Employed",
                                        ifelse(F3U2_0a == "Yes" & F3U4_1 == "Self-Employed", "Self-Employed",
                                               ifelse(F3U2_1a == "Yes", "Apprentice",
                                                      ifelse((F3U1_14 == "Yes" | F3U1_15 == "Yes") & F3U4_1 %in% c("I work for ONE employer on a regular basis","I work for ONE employer on an irregular basis","I work for different employers on an irregular basis","I help out in the family business", "I work for another family (not my own)","Other (specify)"), "Employed",
                                                             ifelse((F3U1_14 == "Yes" | F3U1_15 == "Yes") & F3U4_1 == "I am self-employed", "Self-Employed",
                                                                    ifelse(F3U6_1 == "Yes", "In School", NA)))))),
         f3ustatus = ifelse(is.na(f3ustatus) & f3u, "NEET", f3ustatus))



ys_labels <- ys_labels %>%
  mutate(yeduc = recode(YS3_16,
                        "Aucun" = "None",
                        "Alphabétization" = "<Primary",
                        "Enseignement primaire partiel" = "<Primary",
                        "Enseignement primaire complet" = "Primary",
                        "Enseignement secondaire génèral, premier cycle" = "Collège",
                        "Enseignement secondaire génèral, second cycle" = "Lycée",
                        "Enseignement technique, premier cycle" = "Collège",
                        "Enseignement technique, second cycle" = "Lycée",
                        "Enseignement supérieur classique" = "Tertiary",
                        "Enseignement supèrieur technique" = "Tertiary",
                        "Autre (preciser)" = "None")) %>% 
  mutate(feduc = recode(YS3_10,
                        "Aucun" = "None",
                        "Alphabétization" = "<Primary",
                        "Enseignement primaire partiel" = "<Primary",
                        "Enseignement primaire complet" = "Primary",
                        "Enseignement secondaire génèral, premier cycle" = "Collège",
                        "Enseignement secondaire génèral, second cycle" = "Lycée",
                        "Enseignement technique, premier cycle" = "Collège",
                        "Enseignement technique, second cycle" = "Lycée",
                        "Enseignement supérieur classique" = "Tertiary",
                        "Enseignement supèrieur technique" = "Tertiary",
                        "Autre (preciser)" = "Other")) %>%
  mutate(meduc = recode(YS3_12,
                        "Aucun" = "None",
                        "Alphabétization" = "<Primary",
                        "Enseignement primaire partiel" = "<Primary",
                        "Enseignement primaire complet" = "Primary",
                        "Enseignement secondaire génèral, premier cycle" = "Collège",
                        "Enseignement secondaire génèral, second cycle" = "Lycée",
                        "Enseignement technique, premier cycle" = "Collège",
                        "Enseignement technique, second cycle" = "Lycée",
                        "Enseignement supérieur classique" = "Tertiary",
                        "Enseignement supèrieur technique" = "Tertiary",
                        "Autre (preciser)" = "Other")) %>%
  mutate(beninese = ifelse(YS3_1 == "Béninois", 1, 0),
         siblings = as.numeric(YS3_7),
         fathapp = YS3_9,
         mothapp = YS3_11,
         fon = ifelse(YS3_4_4 == "Fon et apparentés", 1, 0),
         fon = ifelse(is.na(fon), 0, 1),
         christian = ifelse(YS3_5_1 == "Catholic" | YS3_5_2 == "Evangelical" | YS3_5_3 == "Protestant" | YS3_5_4 == "Celestial Church of Christ", 1, 0),
         christian = ifelse(is.na(christian), 0, 1))

ys <- ys %>%
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
  mutate(hometown = case_when(
    YS3_3 == 1 ~ "Small village",
    YS3_3 == 2 ~ "Large village",
    YS3_3 == 3 ~ "Small city",
    YS3_3 == 4 ~ "Major city"))

ys <- ys %>%
  mutate(beninese = ifelse(YS3_1 == 1, 1, 0),
         siblings = as.numeric(ys$YS3_7),
         fathapp = YS3_9,
         mothapp = YS3_11,
         fon = ifelse(YS3_4_4 == 1, 1, 0),
         fon = ifelse(is.na(fon), 0, 1),
         christian = ifelse(YS3_5_1 == 1 | YS3_5_2 == 1 | YS3_5_3 == 1 | YS3_5_4 == 1, 1, 0),
         christian = ifelse(is.na(christian), 0, 1))

#ys$income_level <- coalesce(ys$YS8_20, ys$YS8_21, ys$YS8_22, ys$YS8_23, ys$YS8_24) %>% na_if(99)
ys$male <- ys$YS1_7
ys$past_app <- ys$YS3_13
ys$yrs_schooling <- ys$YS3_15 %>% 
  na_if(99)

ys$hometown <- relevel(as.factor(ys$hometown), ref = "Small village")

grads <- ys %>%
  filter(status != "In School" & status != "Apprentice")
grads_labels <- ys_labels %>%
  filter(status != "In School" & status != "Apprentice")

grads <- grads %>% 
  mutate(CEP = ifelse(YS3_17_2 == 1, 1, 0),
         CEP = ifelse(is.na(CEP), 0, 1),
         BEPC = ifelse(YS3_17_4 == 1, 1, 0),
         BEPC = ifelse(is.na(BEPC), 0, 1),
         BAC = ifelse(YS3_17_6 == 1, 1, 0),
         BAC = ifelse(is.na(BAC), 0, 1),
         CAP = ifelse(YS3_17_8 == 1, 1, 0),
         CAP = ifelse(is.na(CAP), 0, 1),
         "Licence" = ifelse(YS3_17_11 == 1, 1, 0),
         "License" = ifelse(is.na("License"), 0, 1))

beninswts12 <- read_dta("./data/swts/benin/benin_etwa12.dta") %>% 
  filter(urbrur == "Urbain",
         age >= 20 & age <= 29)

beninswts14 <- read_dta("./data/swts/benin/benin_etwa14.dta") %>% 
  filter(m00_type == 1,
         age >= 20 & age <= 29)

urban_census <- read_dta("./data/insae/urban_census.dta")

sample_frame <- read_dta("./data/insae/ZD_sample_frame.dta")
sample_frame <- sample_frame %>% 
  filter(age >= 20 & age <= 29) %>% 
  mutate(activity = case_when(
    activite ==  "Apprenti" ~ "Apprentice",
    activite == "Autre (cherche 1ère travail, ménagère, aide familiale, ou autre inactif)" ~ "Other",
    activite ==  "Ecolier, élève, ou étudiant" ~ "In School",
    activite ==  "Occupé indépendant" ~ "Self-Employed",
    activite ==  "Occupé, salarié occasionnel" ~ "Wage Employed",
    activite ==  "Occupé, salarié permanent" ~ "Wage Employed")) 

weights <- prop.table(table(sample_frame$activity))[c(1,5,2,3,4)]

## calculate transition age

df <- ys_labels %>% 
  filter(!is.na(baseline_age))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i) # act#: # corresponds to year
  df[[z]] <- coalesce(df[[y]], df[[x]])
}

# filter out non-respondents
df <- df %>% filter(!is.na(act13))

# create empty occupations occupation at each age (possible ages: 13-29)
for (i in 13:28) { ## possible age of youth
    new <- rep(NA, nrow(df))
    df[ , ncol(df) + 1] <- new
    colnames(df)[ncol(df)] <- paste0("occ", i)
}

for (k in 1:856){
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

df <- df %>% 
  dplyr::select(IDYouth, starts_with("occ"))

dflist <- df %>% 
  names()

ys_labels <- left_join(ys_labels, df, by = "IDYouth")

ys_labels$transition_age <- NA
ys_labels$transition_ready <- 0 # dummy indicating if the youth was in some sort of schooling or training at any point in the past 7 years

for (k in 1:941){
  for (i in 1:15){
    x <- dflist[i+1]
    y <- dflist[i]
    if(ys_labels[[y]][[k]] %in% c(2:6)) {
      ys_labels[["transition_ready"]][[k]] <- 1
      if (ys_labels[[x]][[k]] %in% c(7:8) & ys_labels[[y]][[k]] %in% c(1,2:6,9) && ys_labels[["transition_ready"]][[k]] == 1) {
      ys_labels[["transition_age"]][[k]] <- i+15
      }
      if (ys_labels[[x]][[k]] %in% c(1,2:6,9)) {
      ys_labels[["transition_age"]][[k]] <- NA
      }
    }
  }
}

## ____________________________________ NOTEBOOK _____________________________________________ ##
