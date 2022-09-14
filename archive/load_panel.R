# Package names
packages <- c("here", "haven", "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

## ---- load 

#ys <- read_sav(here("data", "youth_survey", "youth_survey_merged.sav", user_na = TRUE))
setwd("~/polybox/Youth Employment/Thesis")
ys_panel <- read_sav("/Users/kudrzycb/polybox/Youth Employment/Thesis/data/youth_survey/youth_survey_panel2.zsav", user_na = TRUE)

# filter by age and code status
ys_panel <- ys_panel %>% 
  mutate(status = ifelse(YS4_1 == 1 | (YS1_2 == 1 & wave == 0), "Apprentice", 
                         ifelse(YS8_4 %in% c(1,2,3,5,6,8), "Employed",
                                ifelse(YS8_4 == 4, "Self-Employed", NA)))) %>% 
  mutate(status = ifelse(is.na(status) & YS7_1 == 1, "In School", status)) %>%
  mutate(status = ifelse(is.na(status) & F3U2_0a == 1 & YS8_4 %in% c(1,2,3,5,6,8), "Employed", status)) %>% 
  mutate(status = ifelse(is.na(status) & YE3_5 == 1 & YS8_4 %in% c(1,2,3,5,6,8), "Employed", status)) %>% 
  mutate(status = ifelse(is.na(status) & F3U2_0a == 1 & YS8_4 == 4, "Self-Employed", status)) %>% 
  mutate(status = ifelse(is.na(status) & YE3_5 == 1 & YS8_4 == 4, "Self-Employed", status)) %>% 
  mutate(status = ifelse(is.na(status), "NEET", status))

ys_panel <- ys_panel %>% filter(baseline_age >=20 & baseline_age <= 29,
       !is.na(YS1_1))

ys_panel_labels <- haven::as_factor(ys_panel) %>%
  filter(baseline_age >=20 & baseline_age <= 29)

# filter out CQP apprentices
ys_panel <- ys_panel %>% 
  filter(cqp == 0)

ys_panel_labels <- ys_panel_labels %>% 
  filter(cqp == 0)

## generate weights

# replace census activity with baseline activity for single missing observation, region should be COTONOU
ys_panel <- ys_panel %>% mutate(activite = ifelse(IDYouth == "YS5613", "Ecolier, élève, ou étudiant", activite),
                                region = ifelse(IDYouth == "YS5613", "COTONOU", region),
                                ZD = ifelse(IDYouth == "YS5613", "8-1-53-12-4", ZD))

ys_panel_labels <- ys_panel_labels %>% mutate(activite = ifelse(IDYouth == "YS5613", "Ecolier, élève, ou étudiant", activite),
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
ys_panel_labels <- left_join(ys_panel_labels, x, by = "IDYouth")

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

ys_panel_labels <- ys_panel_labels %>%
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
                        "Autre (preciser)" = "Other"))


# for each youth, we calculate the past activities by year (actxx) and age (occxx) 

df <- ys_panel_labels %>% 
  filter(wave %in% c("YS", "F2U")) %>% 
  pivot_wider(id_cols = c("IDYouth", "baseline_age"),
              names_from = wave,
              values_from = c("YS6_16_1", "YS6_16_2", "YS6_16_3", "YS6_16_4", "YS6_16_5", "YS6_16_6", "YS6_16_7"))

for (i in 1:7) {
  x <- paste0("YS6_16_", i, "_YS")
  y <- paste0("YS6_16_", i, "_F2U")
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
ys_panel_labels <- left_join(ys_panel_labels, df, by = "IDYouth")

rm(i,j,k,new,x,y,z,df)


