# Package names
packages <- c("here", "gtsummary", "tidyverse", "texreg")
#detach(package:here, unload=TRUE)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

#load panel
source(here("R", "source", "load_panel.R"))

#load survey
ys <- read_sav(here("data", "youth_survey", "youth_survey_merged.sav"))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i) # act#: # corresponds to year
  ys[[z]] <- coalesce(ys[[y]], ys[[x]])
}

ys_baseline <- ys_panel %>% filter(wave == 0)

x <- ys %>% select(IDYouth, starts_with("act1"))

ys_baseline <- left_join(ys_baseline, x, by = "IDYouth")
rm(ys, x)

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

# youth are "transitioned" when baseline and all subsequent periods are in employment, "in school" if in education or training at baseline
df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>%
  dplyr::select(IDYouth, starts_with("status"))

ys_panel <- left_join(ys_panel, df, by = "IDYouth")

df <- ys_panel %>% 
  filter(wave == 0) %>% 
  mutate(trans_status = ifelse(status_0 %in% c("Employed", "Self-Employed") & status_1 %in% c("Employed", "Self-Employed", NA) & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA), "Transitioned",
                               ifelse(status_0 %in% c("In School", "Apprentice"), "In School or Training", "In Transition")))

rm(dflist, i, j, k, new, x, y, z)

# We are interested in the differences between youth who are in transition (no first work experience, school in past seven years) with others

#convert to numeric for svysummary
df <- df %>% 
  mutate_at(c('YS3_8', 'YS6_6', 'YS6_2', 'YS6_11_1', 'YS6_11_2', 'YS6_11_5', 'YS6_11_8', 'YS3_15', 'YS3_17_6', 'YS3_13', 'YS6_17_1', 'YS6_17_2', 'YS6_17_3', 'YS6_17_4', 'YS6_17_5', 'YS6_17_6', 'YS6_18_1', 'YS6_18_2', 'YS6_18_3', 'YS6_18_4', 'YS6_18_5', 'YS6_18_6', 'YS6_18_7', 'YS6_18_8', 'YS6_18_9', 'YS6_18_10', 'YS6_19', 'YS6_20', 'YS6_21'), as.numeric) %>% 
  mutate(YS6_18_2 = ifelse(YS6_18_2 == 99, NA, YS6_18_2),
         YS6_18_5 = ifelse(YS6_18_5 == 99, NA, YS6_18_5),
         YS6_18_6 = ifelse(YS6_18_6 == 99, NA, YS6_18_6),
         YS6_18_7 = ifelse(YS6_18_7 == 99, NA, YS6_18_7),
         YS6_18_8 = ifelse(YS6_18_8 == 99, NA, YS6_18_8),
         YS6_18_9 = ifelse(YS6_18_9 == 99, NA, YS6_18_9),
         YS6_18_10 = ifelse(YS6_18_10 == 99, NA, YS6_18_10)) %>% 
  mutate_at(vars(starts_with("YS3_17")), ~ if_else(is.na(.), '0', '1'))

sstrat <- survey::svydesign(id = ~reg_id + act_id, strata = ~region + activite, prob = ~prob, data = df, fpc = ~reg_Nh + act_Nh, nest= TRUE)

options(gtsummary.tbl_summary.percent_fun = function(x) style_number(x, digits = 3)) #two digits for percentages
options(survey.lonely.psu="adjust") # circumvent small strata problem for regions

sstrat %>% 
  tbl_svysummary(
    by=trans_status, 
    # summarize a subset of the columns
    include = c(sex, trans_status, baseline_age, YS3_8, YS6_6, YS6_2, YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8, YS3_15, YS3_17_6, YS3_13,   YS6_17_1 ,  YS6_17_2 ,  YS6_17_3 ,  YS6_17_4 ,  YS6_17_5 ,  YS6_17_6 ,  YS6_18_1 ,  YS6_18_2 ,  YS6_18_3 ,  YS6_18_4 ,  YS6_18_5 ,  YS6_18_6 ,  YS6_18_7 ,  YS6_18_8 ,  YS6_18_9 ,  YS6_18_10 ,  YS6_19 ,  YS6_20 ,  YS6_21, yrs_school, yrs_self, yrs_emp, yrs_neet, num_transitions),
    # adding labels to table
    label = list(sex = "Male",
                 baseline_age = "Age",
                 YS3_8 = "Children",
                 YS6_6  = "People in household",
                 YS6_2 = "Home electrified",
                 YS6_11_1 = "Cell Phone",
                 YS6_11_2 = "Smartphone",
                 YS6_11_5 = "Motorcycle",
                 YS6_11_8 = "Television",
                 YS3_15 = "Years of schooling",
                 YS3_17_6 = "Baccalauréate diploma",
                 YS3_13 = "Completed apprenticeship",
                 YS6_17_1 = "Enjoyed Life",  
                 YS6_17_2  = "Restless Sleep",
                 YS6_17_3  = "Couldn't Focus",
                 YS6_17_4  = "Felt Unmotivated",
                 YS6_17_5 = "Felt Disliked",
                 YS6_17_6  = "Lacked Appetite",
                 YS6_18_1  = "Read Newspaper",
                 YS6_18_2  = "Read Book",
                 YS6_18_3  = "Listened to Radio",
                 YS6_18_4  = "Watched TV",
                 YS6_18_5  = "Internet: Info",
                 YS6_18_6  = "Internet: Shop",
                 YS6_18_7  = "Internet: Job",
                 YS6_18_8  = "Facebook",
                 YS6_18_9  = "Whatsapp",
                 YS6_18_10  = "Twitter", 
                 YS6_19  = "Optimism About Future",
                 YS6_20  = "Feeling of Agency",
                 YS6_21  = "Life Satisfaction",
                 yrs_school = "In School",
                 yrs_self = "Self-Employment",
                 yrs_emp = "Wage Employment", 
                 yrs_neet = "NEET", 
                 num_transitions = "No. Transitions"),
    type = list(c(YS3_8, YS6_6, YS6_2, YS6_17_1, YS6_17_2, YS6_17_3, YS6_17_4, YS6_17_5, YS6_17_6, YS6_19, YS6_20, YS6_21, yrs_school, yrs_self, yrs_emp, yrs_neet, num_transitions) ~ "continuous",
                c(YS3_17_6, YS3_13, YS6_18_1 ,  YS6_18_2 ,  YS6_18_3 ,  YS6_18_4 ,  YS6_18_5 ,  YS6_18_6 ,  YS6_18_7 ,  YS6_18_8 ,  YS6_18_9 ,  YS6_18_10 ) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}",
                     all_continuous() ~ "{mean}"),
    missing = "no") %>% 
  modify_header(stat_by = "**{level}** N={n_unweighted} ({style_percent(p_unweighted)}%)", label ~ "") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Status**") %>% 
  modify_table_body(
    mutate,
    group_variable = case_when(variable %in% c("sex", "baseline_age", "YS3_8", "YS6_6", "YS6_2") ~ "Characteristics",
                               variable %in% c("YS6_17_1",  "YS6_17_2",  "YS6_17_3",  "YS6_17_4",  "YS6_17_5",  "YS6_17_6", "YS6_19", "YS6_20", "YS6_21") ~ "Psychological Wellbeing",
                               variable %in% c( "YS6_18_1",  "YS6_18_2",  "YS6_18_3",  "YS6_18_4",  "YS6_18_5",  "YS6_18_6",  "YS6_18_7", "YS6_18_8",  "YS6_18_9", "YS6_18_10") ~ "Time Usage",
                               variable %in% c("YS6_11_1", "YS6_11_2", "YS6_11_5", "YS6_11_8") ~ "Asset ownership",
                               variable %in% c("YS3_15", "YS3_17_6", "YS3_13") ~ "Education",
                               variable %in% c("yrs_school", "yrs_self", "yrs_emp", "yrs_neet", "num_transitions") ~ "Of Past 7 Years")) %>%
  modify_table_body(group_by, group_variable) %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  italicize_labels() %>% 
  as_gt() %>% 
  gt::as_latex() %>% 
  as.character() %>%
  cat()


x <- df %>% select(IDYouth, trans_status, YS6_17_1,  YS6_17_2,  YS6_17_3,  YS6_17_4,  YS6_17_5,  YS6_17_6)

x %>%
  rename("Enjoyed Life" = YS6_17_1,  
         "Restless Sleep" = YS6_17_2,
         "Couldn't Focus" = YS6_17_3,
         "Felt Unmotivated" = YS6_17_4,
         "Felt Disliked" = YS6_17_5,
         "Lacked Appetite" = YS6_17_6) %>% 
  pivot_longer("Enjoyed Life":"Lacked Appetite", names_to = "question", values_to = "response") %>%
  group_by(question, trans_status, response) %>%
  filter(!is.na(response)) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n, na.rm = TRUE)) %>% 
  ggplot(aes(x = response, y = freq, fill = trans_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (1 (Never) to 5 (All the time))", y = "Percent", fill = "") +
  scale_fill_manual(name = "", values = alpha(c("black", "dark gray", "light gray"), .8)) +
  theme_minimal() +
  ggsave("figures/chap01/psych_grid.png",
         width = 20, height = 12, units = "cm")
  

x <- df %>% select(IDYouth, trans_status, YS6_19,  YS6_20,  YS6_21)

x %>%
  rename( "Optimism About Future" = YS6_19,  
          "Feeling of Agency" = YS6_20,
          "Life Satisfaction" = YS6_21) %>% 
  pivot_longer("Optimism About Future":"Life Satisfaction", names_to = "question", values_to = "response") %>%
  group_by(question, trans_status, response) %>%
  filter(!is.na(response)) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n, na.rm = TRUE)) %>% 
  ggplot(aes(x = response, y = freq, fill = trans_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (1 (Low) to 5 (High))", y = "Percent") +
  scale_fill_manual(name = "", values = alpha(c("black", "dark gray", "light gray"), .8)) +
  theme_minimal() +
  ggsave("figures/chap01/psych_grid2.png",
         width = 20, height = 8, units = "cm")


x <- df %>% select(IDYouth, trans_status, sex)

# Proportion transitioning  
x %>% 
  mutate(sex = ifelse(sex == 0, "Female", "Male")) %>% 
  group_by(sex, trans_status) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n, na.rm = TRUE)) %>% 
  rbind(x %>% mutate(sex = "Overall") %>% group_by(sex, trans_status) %>% summarise(n = n()) %>% mutate(freq = n / sum(n, na.rm = TRUE))) %>%
  ggplot(aes(x = trans_status, y = freq, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = freq,
                label = paste0(round(freq* 100, 1),"%")),
            vjust = -0.5,
            size = 3,
            stat="identity",
            position = position_dodge(width=0.9)) +
  theme_minimal()+
  scale_fill_manual(name = "Gender:", values = alpha(c("black", "dark gray", "light gray"), .8)) +
  labs(x="",y="Proportion") +
  ggsave("figures/chap01/transitions.png",
         width = 20, height = 12, units = "cm")



