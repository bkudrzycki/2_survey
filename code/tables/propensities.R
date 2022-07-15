# Package names
packages <- c("tidyverse", "survey", "ggplot2", "gtsummary")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)))
rm(installed_packages, packages)

## ---- load_panel data
setwd("~/polybox/Youth Employment/Thesis")
load("data/youth_survey/ys_panel.rda")

dfbase <- ys_panel %>% 
  mutate(formal = ifelse(YS8_4 == 9, NA, # no job at the moment -> NA
                         ifelse((YS8_4 == 1 & YS8_10 != 3) | (YS8_4 == 4 & YS9_15 > 4), 1, # single employer regular basis AND any contract OR self-employed w 5+ workers 
                         ifelse(!is.na(YS8_4), 0, NA))))

# "underemp" -> wage- or self-employed who worked less than 35 hours in past week (Benin threshold) (90% of these wanted to work more hours in first two waves)
# "fully employed" -> wage- or self-employed who worked 35 hours or more in past week

dfbase <- dfbase %>% 
  mutate(wagedays = coalesce(YS8_17, YS8_18),
         wagehours = wagedays*YS8_19,
         selfhours = ifelse(dfbase$YS9_13<99, dfbase$YS9_13*dfbase$YS9_14, NA),
         hours = ifelse(status == "Employed" | status == "Self-Employed", coalesce(wagehours, selfhours), NA),
         underemp = ifelse(hours < 35, 1, 0)) %>% 
  select(-wagehours, -selfhours)

# underemp as % of all youth who reported work times (about 100 working youth did not report due to skip patterns)

# "casual" -> one or more employers on *irregular* basis or single employer with irregular/task-based payment
# "regular work" -> single employer with regular wages

dfbase <- dfbase %>%
  mutate(casual = ifelse(YS8_4 %in% c(2,3) | (YS8_4 == 1 & YS8_14 %in% c(4:7)), 1, 
                         ifelse(YS8_4 == 1 & YS8_14 %in% c(1:3), 0, NA)))

# "employer" -> self-employed with employees/apprentices
# "independent" -> self-employed, no employees or apprentices

dfbase <- dfbase %>%
  mutate(employer = ifelse(YS9_15 > 1, 1, 0))

# transitions from other states to different types of employment

df <- dfbase %>% 
  select(IDYouth, baseline_age, sex, wave, status, formal, underemp, casual, employer) %>% 
  pivot_wider(values_from = c("status", "formal", "underemp", "casual", "employer"), names_from = "wave", names_prefix = "wave") %>% 
  mutate(NEET_to_formal1 = NA,
         NEET_to_formal2 = NA,
         NEET_to_formal3 = NA,
         NEET_to_formal4 = NA)

# a transition is only possible if working status is observed for both waves

for (k in 1:752){
  df[["NEET_to_formal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                        ifelse(df[["formal_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_formal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                        ifelse(df[["formal_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_formal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                        ifelse(df[["formal_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_formal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                        ifelse(df[["formal_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  


df <- df %>% 
  mutate(NEET_to_informal1 = NA,
         NEET_to_informal2 = NA,
         NEET_to_informal3 = NA,
         NEET_to_informal4 = NA)

for (k in 1:752){
  df[["NEET_to_informal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                          ifelse(df[["formal_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_informal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                          ifelse(df[["formal_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_informal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                          ifelse(df[["formal_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_informal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                          ifelse(df[["formal_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_underemp1 = NA,
         NEET_to_underemp2 = NA,
         NEET_to_underemp3 = NA,
         NEET_to_underemp4 = NA)

for (k in 1:752){
  df[["NEET_to_underemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                          ifelse(df[["underemp_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_underemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                          ifelse(df[["underemp_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_underemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                          ifelse(df[["underemp_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_underemp4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                          ifelse(df[["underemp_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_fullemp1 = NA,
         NEET_to_fullemp2 = NA,
         NEET_to_fullemp3 = NA,
         NEET_to_fullemp4 = NA)

for (k in 1:752){
  df[["NEET_to_fullemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                          ifelse(df[["underemp_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_fullemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                          ifelse(df[["underemp_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_fullemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                          ifelse(df[["underemp_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_fullemp4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                         ifelse(df[["underemp_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_casual1 = NA,
         NEET_to_casual2 = NA,
         NEET_to_casual3 = NA,
         NEET_to_casual4 = NA)

for (k in 1:752){
  df[["NEET_to_casual1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["casual_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_casual2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["casual_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_casual3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["casual_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_casual4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                        ifelse(df[["casual_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_regular1 = NA,
         NEET_to_regular2 = NA,
         NEET_to_regular3 = NA,
         NEET_to_regular4 = NA)

for (k in 1:752){
  df[["NEET_to_regular1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                        ifelse(df[["casual_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_regular2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                        ifelse(df[["casual_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_regular3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                        ifelse(df[["casual_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_regular4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                         ifelse(df[["casual_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_employer1 = NA,
         NEET_to_employer2 = NA,
         NEET_to_employer3 = NA,
         NEET_to_employer4 = NA)

for (k in 1:752){
  df[["NEET_to_employer1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                        ifelse(df[["employer_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_employer2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                        ifelse(df[["employer_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_employer3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                        ifelse(df[["employer_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_employer4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                          ifelse(df[["employer_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  mutate(NEET_to_independent1 = NA,
         NEET_to_independent2 = NA,
         NEET_to_independent3 = NA,
         NEET_to_independent4 = NA)

for (k in 1:752){
  df[["NEET_to_independent1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["employer_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_independent2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["employer_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_independent3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["employer_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "NEET", 1, 0))
  df[["NEET_to_independent4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                             ifelse(df[["employer_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "NEET", 1, 0))
}  

df <- df %>% 
  pivot_longer(cols = c(NEET_to_casual1, NEET_to_casual2, NEET_to_casual3, NEET_to_casual4), names_to = "wave", values_to = "NEET_to_casual", names_pattern = "NEET_to_casual(.)") %>% 
  mutate(agecat = ifelse(baseline_age < 25, "20-24", "25-29"),
         wave = as.numeric(wave)) %>% 
  select(agecat, sex, wave, NEET_to_casual) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_regular1, NEET_to_regular2, NEET_to_regular3, NEET_to_regular4), names_to = "wave", values_to = "NEET_to_regular") %>% select(NEET_to_regular)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_underemp1, NEET_to_underemp2, NEET_to_underemp3, NEET_to_underemp4), names_to = "wave", values_to = "NEET_to_underemp") %>% select(NEET_to_underemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_fullemp1, NEET_to_fullemp2, NEET_to_fullemp3, NEET_to_fullemp4), names_to = "wave", values_to = "NEET_to_fullemp") %>% select(NEET_to_fullemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_informal1, NEET_to_informal2, NEET_to_informal3, NEET_to_informal4), names_to = "wave", values_to = "NEET_to_informal") %>% select(NEET_to_informal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_formal1, NEET_to_formal2, NEET_to_formal3, NEET_to_formal4), names_to = "wave", values_to = "NEET_to_formal") %>% select(NEET_to_formal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_employer1, NEET_to_employer2, NEET_to_employer3, NEET_to_employer4), names_to = "wave", values_to = "NEET_to_employer") %>% select(NEET_to_employer)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(NEET_to_independent1, NEET_to_independent2, NEET_to_independent3, NEET_to_independent4), names_to = "wave", values_to = "NEET_to_independent") %>% select(NEET_to_independent)) %>% 
  pivot_longer(cols = c(4:11), names_to = "transition type", values_to = "propensity")

## NEET Table

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("NEET_to_informal", "NEET_to_formal"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("NEET_to_informal", "NEET_to_formal"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From NEET (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

neet_alltab1 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("NEET_to_underemp", "NEET_to_fullemp"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("NEET_to_underemp", "NEET_to_fullemp"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From NEET (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

neet_alltab2 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('NEET_to_casual', 'NEET_to_regular'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('NEET_to_casual', 'NEET_to_regular'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From NEET (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>% 
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)


neet_emptab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)


df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('NEET_to_employer', 'NEET_to_independent'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('NEET_to_employer', 'NEET_to_independent'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From NEET (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

neet_selftab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

neettbl <- tbl_merge(list(neet_alltab1, neet_alltab2, neet_emptab, neet_selftab),
                     tab_spanner = c("All Workers", "All Workers", "Employed Workers", "Self-Employed Workers"))

## APPRENTICES

df <- dfbase %>% 
  select(IDYouth, baseline_age, sex, wave, status, formal, underemp, casual, employer) %>% 
  pivot_wider(values_from = c("status", "formal", "underemp", "casual", "employer"), names_from = "wave", names_prefix = "wave") %>% 
  mutate(app_to_formal1 = NA,
         app_to_formal2 = NA,
         app_to_formal3 = NA,
         app_to_formal4 = NA)


for (k in 1:752){
  df[["app_to_formal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                       ifelse(df[["formal_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_formal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                       ifelse(df[["formal_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_formal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                       ifelse(df[["formal_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_formal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                       ifelse(df[["formal_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_informal1 = NA,
         app_to_informal2 = NA,
         app_to_informal3 = NA,
         app_to_informal4 = NA)

for (k in 1:752){
  df[["app_to_informal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["formal_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_informal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["formal_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_informal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["formal_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_informal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                         ifelse(df[["formal_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_underemp1 = NA,
         app_to_underemp2 = NA,
         app_to_underemp3 = NA,
         app_to_underemp4 = NA)

for (k in 1:752){
  df[["app_to_underemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["underemp_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_underemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["underemp_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_underemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["underemp_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_underemp4"]][[k]] = ifelse(is.na(df[["status_wave4"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["underemp_wave3"]][[k]] == 1 & df[["status_wave4"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_fullemp1 = NA,
         app_to_fullemp2 = NA,
         app_to_fullemp3 = NA,
         app_to_fullemp4 = NA)

for (k in 1:752){
  df[["app_to_fullemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                        ifelse(df[["underemp_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_fullemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                        ifelse(df[["underemp_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_fullemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                        ifelse(df[["underemp_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_fullemp3"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                        ifelse(df[["underemp_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_casual1 = NA,
         app_to_casual2 = NA,
         app_to_casual3 = NA,
         app_to_casual4 = NA)

for (k in 1:752){
  df[["app_to_casual1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                       ifelse(df[["casual_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_casual2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                       ifelse(df[["casual_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_casual3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                       ifelse(df[["casual_wave2"]][[k]] == 1 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_casual4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                       ifelse(df[["casual_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_regular1 = NA,
         app_to_regular2 = NA,
         app_to_regular3 = NA,
         app_to_regular4 = NA)

for (k in 1:752){
  df[["app_to_regular1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                        ifelse(df[["casual_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_regular2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                        ifelse(df[["casual_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_regular3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                        ifelse(df[["casual_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_regular4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                        ifelse(df[["casual_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_employer1 = NA,
         app_to_employer2 = NA,
         app_to_employer3 = NA,
         app_to_employer4 = NA)

for (k in 1:752){
  df[["app_to_employer1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["employer_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_employer2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["employer_wave1"]][[k]] == 1 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_employer3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["employer_wave2"]][[k]] == 1 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_employer4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                         ifelse(df[["employer_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  mutate(app_to_independent1 = NA,
         app_to_independent2 = NA,
         app_to_independent3 = NA,
         app_to_independent4 = NA)

for (k in 1:752){
  df[["app_to_independent1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                            ifelse(df[["employer_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_independent2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                            ifelse(df[["employer_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_independent3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                            ifelse(df[["employer_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "Apprentice", 1, 0))
  df[["app_to_independent4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                            ifelse(df[["employer_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "Apprentice", 1, 0))
}  

df <- df %>% 
  pivot_longer(cols = c(app_to_casual1, app_to_casual2, app_to_casual3, app_to_casual4), names_to = "wave", values_to = "app_to_casual", names_pattern = "app_to_casual(.)") %>% 
  mutate(agecat = ifelse(baseline_age < 25, "20-24", "25-29"),
         wave = as.numeric(wave)) %>% 
  select(agecat, sex, wave, app_to_casual) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_regular1, app_to_regular2, app_to_regular3, app_to_regular4), names_to = "wave", values_to = "app_to_regular") %>% select(app_to_regular)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_underemp1, app_to_underemp2, app_to_underemp3, app_to_underemp4), names_to = "wave", values_to = "app_to_underemp") %>% select(app_to_underemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_fullemp1, app_to_fullemp2, app_to_fullemp3, app_to_fullemp4), names_to = "wave", values_to = "app_to_fullemp") %>% select(app_to_fullemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_informal1, app_to_informal2, app_to_informal3, app_to_informal4), names_to = "wave", values_to = "app_to_informal") %>% select(app_to_informal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_formal1, app_to_formal2, app_to_formal3, app_to_formal4), names_to = "wave", values_to = "app_to_formal") %>% select(app_to_formal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_employer1, app_to_employer2, app_to_employer3, app_to_employer4), names_to = "wave", values_to = "app_to_employer") %>% select(app_to_employer)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(app_to_independent1, app_to_independent2, app_to_independent3, app_to_independent4), names_to = "wave", values_to = "app_to_independent") %>% select(app_to_independent)) %>% 
  pivot_longer(cols = c(4:11), names_to = "transition type", values_to = "propensity")



df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("app_to_informal", "app_to_formal"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("app_to_informal", "app_to_formal"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From Apprenticeship (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

app_alltab1 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)


df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("app_to_underemp", "app_to_fullemp"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("app_to_underemp", "app_to_fullemp"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From Apprenticeship (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

app_alltab2 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('app_to_casual', 'app_to_regular'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('app_to_casual', 'app_to_regular'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From Apprenticeship (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>% 
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)


app_emptab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)


df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('app_to_employer', 'app_to_independent'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('app_to_employer', 'app_to_independent'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From Apprenticeship (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

app_selftab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

apptbl <- tbl_merge(list(app_alltab1, app_alltab2, app_emptab, app_selftab),
                    tab_spanner = c("All Workers", "All Workers", "Employed Workers", "Self-Employed Workers"))


## IN SCHOOL

df <- dfbase %>% 
  select(IDYouth, baseline_age, sex, wave, status, formal, underemp, casual, employer) %>% 
  pivot_wider(values_from = c("status", "formal", "underemp", "casual", "employer"), names_from = "wave", names_prefix = "wave") %>% 
  mutate(school_to_formal1 = NA,
         school_to_formal2 = NA,
         school_to_formal3 = NA,
         school_to_formal4 = NA)


for (k in 1:752){
  df[["school_to_formal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                          ifelse(df[["formal_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_formal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                          ifelse(df[["formal_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_formal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                          ifelse(df[["formal_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_formal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                          ifelse(df[["formal_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_informal1 = NA,
         school_to_informal2 = NA,
         school_to_informal3 = NA,
         school_to_informal4 = NA)

for (k in 1:752){
  df[["school_to_informal1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                            ifelse(df[["formal_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_informal2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                            ifelse(df[["formal_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_informal3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                            ifelse(df[["formal_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_informal4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                            ifelse(df[["formal_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_underemp1 = NA,
         school_to_underemp2 = NA,
         school_to_underemp3 = NA,
         school_to_underemp4 = NA)

for (k in 1:752){
  df[["school_to_underemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                            ifelse(df[["underemp_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_underemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                            ifelse(df[["underemp_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_underemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                            ifelse(df[["underemp_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_underemp4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                            ifelse(df[["underemp_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_fullemp1 = NA,
         school_to_fullemp2 = NA,
         school_to_fullemp3 = NA,
         school_to_fullemp4 = NA)

for (k in 1:752){
  df[["school_to_fullemp1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                           ifelse(df[["underemp_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_fullemp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                           ifelse(df[["underemp_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_fullemp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                           ifelse(df[["underemp_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_fullemp4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                           ifelse(df[["underemp_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_casual1 = NA,
         school_to_casual2 = NA,
         school_to_casual3 = NA,
         school_to_casual4 = NA)

for (k in 1:752){
  df[["school_to_casual1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                          ifelse(df[["casual_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_casual2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                          ifelse(df[["casual_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_casual3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                          ifelse(df[["casual_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_casual4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                          ifelse(df[["casual_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_regular1 = NA,
         school_to_regular2 = NA,
         school_to_regular3 = NA,
         school_to_regular4 = NA)

for (k in 1:752){
  df[["school_to_regular1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                           ifelse(df[["casual_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_regular2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                           ifelse(df[["casual_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_regular3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                           ifelse(df[["casual_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_regular4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                           ifelse(df[["casual_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_employer1 = NA,
         school_to_employer2 = NA,
         school_to_employer3 = NA,
         school_to_employer4 = NA)

for (k in 1:752){
  df[["school_to_employer1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                            ifelse(df[["employer_wave1"]][[k]] == 1 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_employer2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                            ifelse(df[["employer_wave2"]][[k]] == 1 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_employer3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                            ifelse(df[["employer_wave3"]][[k]] == 1 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_employer4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                            ifelse(df[["employer_wave4"]][[k]] == 1 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  mutate(school_to_independent1 = NA,
         school_to_independent2 = NA,
         school_to_independent3 = NA,
         school_to_independent4 = NA)

for (k in 1:752){
  df[["school_to_independent1"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]), NA,
                                               ifelse(df[["employer_wave1"]][[k]] == 0 & df[["status_wave0"]][[k]] == "In School", 1, 0))
  df[["school_to_independent2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                               ifelse(df[["employer_wave2"]][[k]] == 0 & df[["status_wave1"]][[k]] == "In School", 1, 0))
  df[["school_to_independent3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                               ifelse(df[["employer_wave3"]][[k]] == 0 & df[["status_wave2"]][[k]] == "In School", 1, 0))
  df[["school_to_independent4"]][[k]] = ifelse(is.na(df[["status_wave3"]][[k]]) | is.na(df[["status_wave4"]][[k]]), NA,
                                               ifelse(df[["employer_wave4"]][[k]] == 0 & df[["status_wave3"]][[k]] == "In School", 1, 0))
}  

df <- df %>% 
  pivot_longer(cols = c(school_to_casual1, school_to_casual2, school_to_casual3, school_to_casual4), names_to = "wave", values_to = "school_to_casual", names_pattern = "school_to_casual(.)") %>% 
  mutate(agecat = ifelse(baseline_age < 25, "20-24", "25-29"),
         wave = as.numeric(wave)) %>% 
  select(agecat, sex, wave, school_to_casual) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_regular1, school_to_regular2, school_to_regular3, school_to_regular4), names_to = "wave", values_to = "school_to_regular") %>% select(school_to_regular)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_underemp1, school_to_underemp2, school_to_underemp3, school_to_underemp4), names_to = "wave", values_to = "school_to_underemp") %>% select(school_to_underemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_fullemp1, school_to_fullemp2, school_to_fullemp3, school_to_fullemp4), names_to = "wave", values_to = "school_to_fullemp") %>% select(school_to_fullemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_informal1, school_to_informal2, school_to_informal3, school_to_informal4), names_to = "wave", values_to = "school_to_informal") %>% select(school_to_informal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_formal1, school_to_formal2, school_to_formal3, school_to_formal4), names_to = "wave", values_to = "school_to_formal") %>% select(school_to_formal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_employer1, school_to_employer2, school_to_employer3, school_to_employer4), names_to = "wave", values_to = "school_to_employer") %>% select(school_to_employer)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(school_to_independent1, school_to_independent2, school_to_independent3, school_to_independent4), names_to = "wave", values_to = "school_to_independent") %>% select(school_to_independent)) %>% 
  pivot_longer(cols = c(4:11), names_to = "transition type", values_to = "propensity")


df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("school_to_informal", "school_to_formal"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("school_to_informal", "school_to_formal"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From School (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

school_alltab1 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("school_to_underemp", "school_to_fullemp"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("school_to_underemp", "school_to_fullemp"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From School (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>%
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

school_alltab2 <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('school_to_casual', 'school_to_regular'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('school_to_casual', 'school_to_regular'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From School (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>% 
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  modify_header(list(
    stat_1 ~ "**To Casual**",
    stat_2 ~ "**To Employment**")) %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)


school_emptab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('school_to_employer', 'school_to_independent'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('school_to_employer', 'school_to_independent'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "From School (Overall)",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

y <- df2 %>% filter(sex == 1) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Male",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

z <- df2 %>% filter(sex == 0) %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Female",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

a <- df2 %>% filter(agecat == "20-24") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "20-24",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

b <- df2 %>% filter(agecat == "25-29") %>% 
  select(c("transition type", "propensity")) %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "25-29",
              missing = "no",
              digits = propensity ~ 3,
              type = "propensity" ~ 'continuous',
              statistic = "propensity" ~ "{mean}") %>% 
  add_p() %>%
  modify_footnote(update = everything() ~ NA)

school_selftab <- tbl_stack(tbls = list(x,y,z,a,b), quiet = TRUE)


schooltbl <- tbl_merge(list(school_alltab1, school_alltab2, school_emptab, school_selftab),
                       tab_spanner = c("All Workers", "All Workers", "Employed Workers", "Self-Employed Workers"))


tbl_stack(list(tbl_merge(list(neet_alltab1, neet_alltab2),
                         tab_spanner = c("Formality", "Working Hours")),
          tbl_merge(list(app_alltab1, app_alltab2)),
          tbl_merge(list(school_alltab1, school_alltab2))), quiet = TRUE) %>% 
  modify_header(list(
    stat_1_1 ~ "**Informal**",
    stat_2_1 ~ "**Formal**",
    stat_1_2 ~ "**Underemp.**",
    stat_2_2 ~ "**Full Emp.**")) %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()


tbl_stack(list(tbl_merge(list(neet_emptab, neet_selftab),
                         tab_spanner = c("Wage Employed", "Self-Employed")),
               tbl_merge(list(app_emptab, app_selftab)),
               tbl_merge(list(school_emptab, school_selftab))), quiet = TRUE) %>% 
  modify_header(list(
    stat_1_1 ~ "**Casual**",
    stat_2_1 ~ "**Regular**",
    stat_1_2 ~ "**Employer**",
    stat_2_2 ~ "**Independent**")) %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()

rm(df, df2, dfbase, neet_alltab1, neet_alltab2, neet_emptab, neet_selftab, neettbl, app_alltab1, app_alltab2, app_emptab, app_selftab, apptbl, school_alltab1, school_alltab2, school_emptab, school_selftab, schooltbl, a, b, k, x, y, z)


