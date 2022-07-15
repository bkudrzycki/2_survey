# Package names
packages <- c("here", "tidyverse", "ggplot2")

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
source(here("R", "source", "load_panel.R"))

# youth are "transitioned" when baseline and all subsequent periods are in employment, "in school" if in education or training at baseline
df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>%
  dplyr::select(IDYouth, starts_with("status"), starts_with("age"))

ys_panel <- left_join(ys_panel, df, by = "IDYouth")

x <- ys_panel %>%
  filter(wave == 0) %>% 
  mutate(trans_status = ifelse(status_0 %in% c("Employed", "Self-Employed") & status_1 %in% c("Employed", "Self-Employed", NA) & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA), "Transitioned",
                               ifelse(status_0 %in% c("In School", "Apprentice"), "In School or Training", "In Transition"))) %>% 
  select(IDYouth, trans_status, sex) %>%
  mutate(sex = "Overall") %>% 
  group_by(sex, trans_status) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n, na.rm = TRUE))

ys_panel %>% 
  filter(wave == 0) %>% 
  mutate(trans_status = ifelse(status_0 %in% c("Employed", "Self-Employed") & status_1 %in% c("Employed", "Self-Employed", NA) & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA), "Transitioned",
                               ifelse(status_0 %in% c("In School", "Apprentice"), "In School or Training", "In Transition"))) %>% 
  select(IDYouth, trans_status, sex) %>% 
  mutate(sex = ifelse(sex == 0, "Female", "Male")) %>% 
  group_by(sex, trans_status) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n, na.rm = TRUE)) %>% 
  rbind(x) %>%
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
  labs(x="",y="Proportion")