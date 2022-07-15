# Package names
packages <- c("here", "tidyverse", "survey", "ggplot2", "gtsummary")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

## ---- load_panel data
setwd("~/polybox/Youth Employment/Thesis")
source(here("Youth Employment", "Thesis", "R", "source", "load_panel.R"))

dflist <- ys_panel %>% 
  dplyr::select(sort(starts_with("act1"), decreasing = TRUE)) %>% 
  names()

ys_panel$num_transitions <- 0

# calculate number of transitions in past seven years
for (k in 1:2439){
  for (i in 2:7){
    y <- dflist[i]
    x <- dflist[i-1]
    ys_panel[["num_transitions"]][[k]] = 
      ifelse(ys_panel[[y]][[k]] %in% c(1:3) && ys_panel[[x]][[k]] %in% c(1:3), ys_panel[["num_transitions"]][[k]],
             ifelse(ys_panel[[y]][[k]] %in% c(4:5) && ys_panel[[x]][[k]] %in% c(4:5), ys_panel[["num_transitions"]][[k]], 
                    ifelse(ys_panel[[y]][[k]] %in% c(6:7) && ys_panel[[x]][[k]] %in% c(6:7), ys_panel[["num_transitions"]][[k]], 
                           ifelse(ys_panel[[y]][[k]] %in% c(0,8,99) && ys_panel[[x]][[k]] %in% c(0,8,99), ys_panel[["num_transitions"]][[k]],
                                  ifelse(is.na(ys_panel[[y]][[k]]) && is.na(ys_panel[[x]][[k]]), 
                                         ys_panel[["num_transitions"]][[k]], 
                                         ys_panel[["num_transitions"]][[k]] + 1)))))
  }
}

# calculate number of years spent in each activity in past seven years
ys_panel$yrs_school <- 0
ys_panel$yrs_app <- 0
ys_panel$yrs_self <- 0
ys_panel$yrs_emp <- 0
ys_panel$yrs_neet <- 0

for (k in 1:2439){
  for (i in 1:7){
    y <- dflist[i]
    if(ys_panel[[y]][[k]] %in% c(1:3)) {
      ys_panel[["yrs_school"]][[k]] <- ys_panel[["yrs_school"]][[k]] + 1
    }
    if(ys_panel[[y]][[k]] %in% c(4:5)) {
      ys_panel[["yrs_app"]][[k]] <- ys_panel[["yrs_app"]][[k]] + 1
    }
    if(!is.na(ys_panel[[y]][[k]]) && ys_panel[[y]][[k]] == 6) {
      ys_panel[["yrs_emp"]][[k]] <- ys_panel[["yrs_emp"]][[k]] + 1
    }
    if(!is.na(ys_panel[[y]][[k]]) && ys_panel[[y]][[k]] == 7) {
      ys_panel[["yrs_self"]][[k]] <- ys_panel[["yrs_self"]][[k]] + 1
    }
    if(ys_panel[[y]][[k]] %in% c(0,8)) {
      ys_panel[["yrs_neet"]][[k]] <- ys_panel[["yrs_neet"]][[k]] + 1
    }
  }
}


## transition propensity

df <- ys_panel %>% 
  select(IDYouth, baseline_age, sex, wave, status) %>% 
  pivot_wider(values_from = "status", names_from = "wave", names_prefix = "wave") %>% 
  mutate(transition1 = 0,
         transition2 = 0,
         transition3 = 0)


for (k in 1:752){
  df[["transition1"]][[k]] = ifelse(df[["wave1"]][[k]] == df[["wave0"]][[k]], 0, 1)
  df[["transition2"]][[k]] = ifelse(df[["wave2"]][[k]] == df[["wave1"]][[k]], 0, 1)
  df[["transition3"]][[k]] = ifelse(df[["wave3"]][[k]] == df[["wave2"]][[k]], 0, 1)
}


## age categories

df <- df %>% mutate(agecat = ifelse(baseline_age < 25, "20-24", "25-29")) %>% 
  group_by(agecat, sex) %>% 
  summarise(propensity_precovid = mean(transition1, na.rm = T),
            propensity_post_covid = mean(transition3, na.rm = T))


x <- df %>% 
  pivot_wider(values_from = c(propensity_precovid, propensity_post_covid), names_from = agecat) %>% 
  select(-starts_with("propensity_post")) %>% 
  rename("20-24" = `propensity_precovid_20-24`,
         "25-29" = `propensity_precovid_25-29`) %>% 
  tbl_summary(by = sex,
              type = list(`20-24` ~ 'continuous',
                          `25-29` ~ 'continuous'),
              statistic = list(`20-24` ~ "{mean}",
                               `25-29` ~ "{mean}"),
              missing = "no") %>% 
  modify_header(list(
    label ~ "",
    stat_1 ~ "**Male**",
    stat_2 ~ "**Female**"
  )) %>%
  add_overall(col_label = "**Total**") %>% 
  modify_footnote(update = everything() ~ NA)

y <- df %>% 
  pivot_wider(values_from = c(propensity_precovid, propensity_post_covid), names_from = agecat) %>% 
  select(-starts_with("propensity_pre")) %>% 
  rename("20-24" = `propensity_post_covid_20-24`,
         "25-29" = `propensity_post_covid_25-29`) %>% 
  tbl_summary(by = sex,
              type = list(`20-24` ~ 'continuous',
                          `25-29` ~ 'continuous'),
              statistic = list(`20-24` ~ "{mean}",
                               `25-29` ~ "{mean}"),
              missing = "no") %>% 
  modify_header(list(
    label ~ "",
    stat_1 ~ "**Male**",
    stat_2 ~ "**Female**"
  )) %>%
  add_overall(col_label = "**Total**") %>% 
  modify_footnote(update = everything() ~ NA)

  
tbl_merge(list(x, y), tab_spanner = c("Pre-Covid", "Post-Covid"))

## propensity to move between different categories of work

# "informal" -> wage employed with no contract, self-employed with less than 4 employees (firms < 5 workers), family workers
# "formal" -> wage employed on a regular basis with any contract (written or verbal), self-employed with more than 5 workers

df <- ys_panel %>% 
  mutate(formal = ifelse(YS8_4 == 9, NA, # no job at the moment -> NA
                         ifelse((YS8_4 == 1 & YS8_10 != 3) | (YS8_4 == 4 & YS9_15 > 4), 1, # single employer regular basis AND any contract OR self-employed w 5+ workers 
                                ifelse(!is.na(YS8_4), 0, NA))))
         
# "underemp" -> wage- or self-employed who worked less than 35 hours in past week (Benin threshold) (90% of these wanted to work more hours in first two waves)
# "fully employed" -> wage- or self-employed who worked 35 hours or more in past week

df <- df %>% 
  mutate(wagedays = coalesce(YS8_17, YS8_18),
         wagehours = wagedays*YS8_19,
         selfhours = ifelse(df$YS9_13<99, df$YS9_13*df$YS9_14, NA),
         hours = ifelse(status == "Employed" | status == "Self-Employed", coalesce(wagehours, selfhours), NA),
         underemp = ifelse(hours < 35, 1, 0)) %>% 
  select(-wagehours, -selfhours)

# underemp as % of all youth who reported work times (about 100 working youth did not report due to skip patterns)
         
# "casual" -> one or more employers on *irregular* basis or single employer with irregular/task-based payment
# "regular work" -> single employer with regular wages

df <- df %>%
  mutate(casual = ifelse(YS8_4 %in% c(2,3) | (YS8_4 == 1 & YS8_14 %in% c(4:7)), 1, 
                         ifelse(YS8_4 == 1 & YS8_14 %in% c(1:3), 0, NA)))
                          
# "employer" -> self-employed with employees/apprentices
# "independent" -> self-employed, no employees or apprentices

df <- df %>%
  mutate(employer = ifelse(YS9_15 > 1, 1, 0))

df %>% 
  dplyr::select(sex, formal, underemp, casual, employer) %>% 
  tbl_summary(by = sex,
              missing = "no") %>% 
  modify_header(list(
    stat_1 ~ "**Male**, N = {n}",
    stat_2 ~ "**Female**, N = {n}"
  )) %>% 
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA)


df <- dfbase %>% 
  select(IDYouth, baseline_age, sex, wave, status, formal, underemp, casual, employer) %>% 
  pivot_wider(values_from = c("status", "formal", "underemp", "casual", "employer"), names_from = "wave", names_prefix = "wave") %>% 
  mutate("to_emp1" = NA,
         "to_emp2" = NA,
         "to_emp3" = NA)

# equals 1 if transition from self-employment to employment in a given wave
  
for (k in 1:752){
  df[["to_emp1"]][[k]] = ifelse(is.na(df[["status_wave0"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                   ifelse(df[["status_wave1"]][[k]] == "Self-Employed" & df[["status_wave0"]][[k]] == "Employed", 1, 0))
  df[["to_emp2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                   ifelse(df[["status_wave2"]][[k]] == "Self-Employed" & df[["status_wave1"]][[k]] == "Employed", 1, 0))
  df[["to_emp3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                   ifelse(df[["status_wave3"]][[k]] == "Self-Employed" & df[["status_wave2"]][[k]] == "Employed", 1, 0))
}  

df <- df %>% 
  mutate("to_self1" = NA,
         "to_self2" = NA,
         "to_self3" = NA)

# equals 1 if transition from employment to self-employment

for (k in 1:752){
  df[["to_self1"]][[k]] = ifelse(is.na(df[["status_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                ifelse(df[["status_wave1"]][[k]] == "Employed" & df[["status_wave0"]][[k]] == "Self-Employed", 1, 0))
  df[["to_self2"]][[k]] = ifelse(is.na(df[["status_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                ifelse(df[["status_wave2"]][[k]] == "Employed" & df[["status_wave1"]][[k]] == "Self-Employed", 1, 0))
  df[["to_self3"]][[k]] = ifelse(is.na(df[["status_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                ifelse(df[["status_wave3"]][[k]] == "Employed" & df[["status_wave2"]][[k]] == "Self-Employed", 1, 0))
}  

df <- df %>% 
  mutate("to_formal1" = NA,
         "to_formal2" = NA,
         "to_formal3" = NA)

# equals 1 if transition from informal to formal work in a given wave

for (k in 1:752){
  df[["to_formal1"]][[k]] = ifelse(is.na(df[["formal_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                         ifelse(df[["formal_wave1"]][[k]] == 1 & df[["formal_wave0"]][[k]] == 0, 1, 0))
  df[["to_formal2"]][[k]] = ifelse(is.na(df[["formal_wave1"]][[k]])| is.na(df[["status_wave2"]][[k]]), NA,
                                         ifelse(df[["formal_wave2"]][[k]] == 1 & df[["formal_wave1"]][[k]] == 0, 1, 0))
  df[["to_formal3"]][[k]] = ifelse(is.na(df[["formal_wave2"]][[k]])| is.na(df[["status_wave3"]][[k]]), NA,
                                         ifelse(df[["formal_wave3"]][[k]] == 1 & df[["formal_wave2"]][[k]] == 0, 1, 0))
}

## NOTE: to_formal only takes into account youth that were working in both periods in consideration (i.e. transition to school is "NA", transition from school is also "NA")

df <- df %>% 
  mutate("to_informal1" = NA,
         "to_informal2" = NA,
         "to_informal3" = NA)

# equals 1 if transition from formal to informal work

for (k in 1:752){
  df[["to_informal1"]][[k]] = ifelse(is.na(df[["formal_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                   ifelse(df[["formal_wave1"]][[k]] == 0 & df[["formal_wave0"]][[k]] == 1, 1, 0))
  df[["to_informal2"]][[k]] = ifelse(is.na(df[["formal_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                   ifelse(df[["formal_wave2"]][[k]] == 0 & df[["formal_wave1"]][[k]] == 1, 1, 0))
  df[["to_informal3"]][[k]] = ifelse(is.na(df[["formal_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                   ifelse(df[["formal_wave3"]][[k]] == 0 & df[["formal_wave2"]][[k]] == 1, 1, 0))
  }

# equals 1 if transition from underemployment to full employment

df <- df %>% 
  mutate("to_underemp1" = NA,
         "to_underemp2" = NA,
         "to_underemp3" = NA)

for (k in 1:752){
  df[["to_underemp1"]][[k]] = ifelse(is.na(df[["underemp_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                     ifelse(df[["underemp_wave1"]][[k]] == 0 & df[["underemp_wave0"]][[k]] == 1, 1, 0))
  df[["to_underemp2"]][[k]] = ifelse(is.na(df[["underemp_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                     ifelse(df[["underemp_wave2"]][[k]] == 0 & df[["underemp_wave1"]][[k]] == 1, 1, 0))
  df[["to_underemp3"]][[k]] = ifelse(is.na(df[["underemp_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                     ifelse(df[["underemp_wave3"]][[k]] == 0 & df[["underemp_wave2"]][[k]] == 1, 1, 0))
}

df <- df %>% 
  mutate("to_fullemp1" = NA,
         "to_fullemp2" = NA,
         "to_fullemp3" = NA)

for (k in 1:752){
  df[["to_fullemp1"]][[k]] = ifelse(is.na(df[["underemp_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                     ifelse(df[["underemp_wave1"]][[k]] == 1 & df[["underemp_wave0"]][[k]] == 0, 1, 0))
  df[["to_fullemp2"]][[k]] = ifelse(is.na(df[["underemp_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                     ifelse(df[["underemp_wave2"]][[k]] == 1 & df[["underemp_wave1"]][[k]] == 0, 1, 0))
  df[["to_fullemp3"]][[k]] = ifelse(is.na(df[["underemp_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                     ifelse(df[["underemp_wave3"]][[k]] == 1 & df[["underemp_wave2"]][[k]] == 0, 1, 0))
}

# equals 1 if transition from casual to regular work

df <- df %>% 
  mutate(to_regular1 = NA,
         to_regular2 = NA,
         to_regular3 = NA)

for (k in 1:752){
  df[["to_regular1"]][[k]] = ifelse(is.na(df[["casual_wave0"]][[k]])| is.na(df[["status_wave1"]][[k]]), NA,
                                   ifelse(df[["casual_wave1"]][[k]] == 1 & df[["casual_wave0"]][[k]] == 0, 1, 0))
  df[["to_regular2"]][[k]] = ifelse(is.na(df[["casual_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                   ifelse(df[["casual_wave2"]][[k]] == 1 & df[["casual_wave1"]][[k]] == 0, 1, 0))
  df[["to_regular3"]][[k]] = ifelse(is.na(df[["casual_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                   ifelse(df[["casual_wave3"]][[k]] == 1 & df[["casual_wave2"]][[k]] == 0, 1, 0))
}

df <- df %>% 
  mutate(to_casual1 = NA,
         to_casual2 = NA,
         to_casual3 = NA)

# equals 1 if transition from regular to casual work

for (k in 1:752){
  df[["to_casual1"]][[k]] = ifelse(is.na(df[["casual_wave0"]][[k]]) | is.na(df[["status_wave1"]][[k]]), NA,
                                   ifelse(df[["casual_wave1"]][[k]] == 0 & df[["casual_wave0"]][[k]] == 1, 1, 0))
  df[["to_casual2"]][[k]] = ifelse(is.na(df[["casual_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                   ifelse(df[["casual_wave2"]][[k]] == 0 & df[["casual_wave1"]][[k]] == 1, 1, 0))
  df[["to_casual3"]][[k]] = ifelse(is.na(df[["casual_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                   ifelse(df[["casual_wave3"]][[k]] == 0 & df[["casual_wave2"]][[k]] == 1, 1, 0))
}

# equals 1 if transition from employer to independent

df <- df %>% 
  mutate(to_employer1 = NA,
         to_employer2 = NA,
         to_employer3 = NA)

for (k in 1:752){
  df[["to_employer1"]][[k]] = ifelse(is.na(df[["employer_wave0"]][[k]])| is.na(df[["status_wave1"]][[k]]), NA,
                                     ifelse(df[["employer_wave1"]][[k]] == 1 & df[["employer_wave0"]][[k]] == 0, 1, 0))
  df[["to_employer2"]][[k]] = ifelse(is.na(df[["employer_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                     ifelse(df[["employer_wave2"]][[k]] == 1 & df[["employer_wave1"]][[k]] == 0, 1, 0))
  df[["to_employer3"]][[k]] = ifelse(is.na(df[["employer_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                     ifelse(df[["employer_wave3"]][[k]] == 1 & df[["employer_wave2"]][[k]] == 0, 1, 0))
}


df <- df %>% 
  mutate(to_independent1 = NA,
         to_independent2 = NA,
         to_independent3 = NA)


for (k in 1:752){
  df[["to_independent1"]][[k]] = ifelse(is.na(df[["employer_wave0"]][[k]])| is.na(df[["status_wave1"]][[k]]), NA,
                                    ifelse(df[["employer_wave1"]][[k]] == 0 & df[["employer_wave0"]][[k]] == 1, 1, 0))
  df[["to_independent2"]][[k]] = ifelse(is.na(df[["employer_wave1"]][[k]]) | is.na(df[["status_wave2"]][[k]]), NA,
                                    ifelse(df[["employer_wave2"]][[k]] == 0 & df[["employer_wave1"]][[k]] == 1, 1, 0))
  df[["to_independent3"]][[k]] = ifelse(is.na(df[["employer_wave2"]][[k]]) | is.na(df[["status_wave3"]][[k]]), NA,
                                    ifelse(df[["employer_wave3"]][[k]] == 0 & df[["employer_wave2"]][[k]] == 1, 1, 0))
}



df <- df %>% 
  pivot_longer(cols = c(to_self1, to_self2, to_self3), names_to = "wave", values_to = "to_self", names_pattern = "to_self(.)") %>% 
  mutate(agecat = ifelse(baseline_age < 25, "20-24", "25-29"),
         wave = as.numeric(wave)) %>% 
  select(agecat, sex, wave, to_self) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_emp1, to_emp2, to_emp3), names_to = "wave", values_to = "to_emp") %>% select(to_emp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_casual1, to_casual2, to_casual3), names_to = "wave", values_to = "to_casual") %>% select(to_casual)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_regular1, to_regular2, to_regular3), names_to = "wave", values_to = "to_regular") %>% select(to_regular)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_underemp1, to_underemp2, to_underemp3), names_to = "wave", values_to = "to_underemp") %>% select(to_underemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_fullemp1, to_fullemp2, to_fullemp3), names_to = "wave", values_to = "to_fullemp") %>% select(to_fullemp)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_informal1, to_informal2, to_informal3), names_to = "wave", values_to = "to_informal") %>% select(to_informal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_formal1, to_formal2, to_formal3), names_to = "wave", values_to = "to_formal") %>% select(to_formal)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_employer1, to_employer2, to_employer3), names_to = "wave", values_to = "to_employer") %>% select(to_employer)) %>% 
  cbind(df %>% 
          pivot_longer(cols = c(to_independent1, to_independent2, to_independent3), names_to = "wave", values_to = "to_independent") %>% select(to_independent)) %>% 
  pivot_longer(cols = c(4:13), names_to = "transition type", values_to = "propensity")

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("to_self", "to_emp"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("to_self", "to_emp"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Overall Propensity",
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

overalltab <- tbl_stack(tbls = list(x,y,z,a,b))

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("to_informal", "to_formal"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("to_informal", "to_formal"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Overall Propensity",
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

alltab1 <- tbl_stack(tbls = list(x,y,z,a,b))

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c("to_underemp", "to_fullemp"))

df2$`transition type` <- factor(df2$`transition type`, levels = c("to_underemp", "to_fullemp"))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Overall Propensity",
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

alltab2 <- tbl_stack(tbls = list(x,y,z,a,b))

df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('to_casual', 'to_regular'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('to_casual', 'to_regular'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Overall Propensity",
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


emptab <- tbl_stack(tbls = list(x,y,z,a,b))


df2 <- df %>% select(c("transition type", "propensity", "sex", "agecat")) %>% 
  filter(`transition type` %in% c('to_employer', 'to_independent'))

df2$`transition type` <- factor(df2$`transition type`, levels = c('to_employer', 'to_independent'))

x <- df2 %>% select(-"sex", -"agecat") %>% 
  tbl_summary(by="transition type",
              label = propensity ~ "Overall Propensity",
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

selftab <- tbl_stack(tbls = list(x,y,z,a,b))

tbl_merge(list(overalltab, alltab1, alltab2, emptab, selftab),
          tab_spanner = c("Overall", "All Workers", "All Workers", "Employed", "Self-Employed"))


x <- tbl_merge(list(alltab1, alltab2))
y <- tbl_merge(list(emptab, selftab))

tbl_stack(list(overalltab, x, y))


