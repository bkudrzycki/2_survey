library(here)
library(tidyverse)
library(gtsummary)

# load panel data
source(here("R", "source", "load_panel.R"))

## all responses

df <- ys_panel_labels %>% 
  select(IDYouth, YS8_33, YS9_34, wave) %>% 
  mutate(sat_emp = as.numeric(YS8_33),
         sat_self = as.numeric(YS9_34))

df %>% 
  dplyr::select(sat_emp, sat_self, YS8_33, YS9_34, wave) %>% 
  tbl_summary(by = wave,
              label = list(sat_emp ~ "Mean Satisfaction (Employed)",
                           YS8_33 ~ "Employed Levels",
                           sat_self ~ "Mean Satisfaction (Self-Employed)",
                           YS9_34 ~ "Self-Emplyed Levels"),
              type = list(sat_emp ~ 'continuous',
                          sat_self ~ 'continuous'),
              statistic = list(sat_emp ~ "{mean} ({sd})",
                               sat_self ~ "{mean} ({sd})"),
              missing = "no")

## only those that were employed or self-employed for all periods

#only employed

df <- ys_panel_labels %>% 
  select(IDYouth, status, wave, YS8_33) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, YS8_33)) %>% 
  filter(status_YS == "Employed" & status_F1U %in% c("Employed", NA) & status_F2U %in% c("Employed", NA) & status_F3U %in% c("Employed", NA)) %>% 
  pivot_longer(starts_with("YS8_33"), names_to = "wave", values_to = "YS8_33", names_prefix = "YS8_33_") %>% 
  mutate(sat = as.numeric(YS8_33))
  
  
df$wave <- factor(df$wave, levels = c("YS", "F1U", "F2U", "F3U"))

df %>%
  dplyr::select(sat, YS8_33, wave) %>% 
  tbl_summary(by = wave,
            label = list(sat ~ "Mean Satisfaction (Employed or NA in all periods)",
                         YS8_33 ~ "Employed Levels"),
            type = list(sat ~ 'continuous'),
            statistic = list(sat ~ "{mean} ({sd})"))

#only self-employed

df <- ys_panel_labels %>% 
  select(IDYouth, status, wave, YS9_34) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, YS9_34)) %>% 
  filter(status_YS == "Self-Employed" & status_F1U %in% c("Self-Employed", NA) & status_F2U %in% c("Self-Employed", NA) & status_F3U %in% c("Self-Employed", NA)) %>% 
  pivot_longer(starts_with("YS9_34"), names_to = "wave", values_to = "YS9_34", names_prefix = "YS9_34_") %>% 
  mutate(sat = as.numeric(YS9_34))

df$wave <- factor(df$wave, levels = c("YS", "F1U", "F2U", "F3U"))

df %>%
  dplyr::select(sat, wave) %>% 
  tbl_summary(by = wave,
              label = list(sat ~ "Mean Satisfaction (Self-employed or NA in all periods)"),
              type = list(sat ~ 'continuous'),
              statistic = list(sat ~ "{mean} ({sd})"))




