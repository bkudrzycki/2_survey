####################
## Data Cleaning  ##
####################

# Package names
packages <- c("haven", "tidyverse", "labelled", "readxl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

# Set working directory
if(!exists("path")){
  setwd("~/polybox/Youth Employment/1a Youth Survey/Markdown")
}else{
  setwd(path)
}

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

#ys_panel$status <- factor(ys_panel$status, levels = c(1:5), labels=c("In School", "NEET", "Self-Employed", "Employed", "Apprentice"))


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

# create labelled version of ys_panel
ys_panel_labels <- haven::as_factor(ys_panel)

# save as .rda
save(ys_panel, file = "data/ys_panel.rda")
save(ys_panel_labels, file = "data/ys_panel_labels.rda")
