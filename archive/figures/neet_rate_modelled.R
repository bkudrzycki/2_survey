library(tidyverse)

rm(list = ls())

# development of NEET rates over time for selected countries
source('./R/source/countryList.R')
neetrate <- read.csv("./data/ilostat/neetrate_sex_modelled.csv") %>% 
  mutate(real_value = ifelse(obs_status.label == "Real value", obs_value, NA))

ssa <- country_lists[[4]]
neet <- read.csv("./data/ilostat/neet_sex_modelled.csv")
pop <- read.csv("./data/ilostat/pop_sex_age.csv") %>% 
  filter(classif1.label == "Age (Aggregate bands): 15-24",
         time >= 2005)

neet <- right_join(neet, pop, by = c("ref_area.label", "time", "sex.label"))

ssaneet <- neet %>% 
  filter(ref_area.label %in% ssa$ref_area.label) %>% 
  group_by(time, sex.label) %>%
  summarize(total_neet = sum(obs_value.x),
            total_pop = sum(obs_value.y)) %>% 
  mutate(obs_value = total_neet/total_pop*100) %>% 
  add_column("ref_area.label" = as.factor("SSA Average"))

## ---- neetr1
neetrate %>% 
  filter(sex.label == "Sex: Total") %>% 
  filter(ref_area.label %in% c("Benin", "Congo, Democratic Republic of the", "Liberia", "Madagascar", "Malawi","Tanzania, United Republic of", "Togo", "Uganda", "Zambia")) %>% 
  ggplot(., aes(x = time)) +
  geom_line(aes(y = obs_value)) +
  facet_wrap(~ ref_area.label, scales = "free_y") +
  geom_point(aes(y = real_value), color="red", size = 4) +
  labs(y = "Youth NEET rate", x = "")

## ---- neetr2
ssaneet %>% 
  filter(sex.label == "Sex: Total") %>% 
  ggplot(., aes(time, obs_value)) +
  geom_line(color = "red") +
  labs(y = "Youth NEET rate", x = "", title = "SSA Average")

## ---- neetr3
source('./R/source/countryList.R')
neetrate <- read.csv("./data/ilostat/neetrate_sex_modelled.csv") %>% 
  mutate(real_value = ifelse(obs_status.label == "Real value", obs_value, NA))

ssa <- country_lists[[4]]
neet <- read.csv("./data/ilostat/neet_sex_modelled.csv")
pop <- read.csv("./data/ilostat/pop_sex_age.csv") %>% 
  filter(classif1.label == "Age (Aggregate bands): 15-24",
         time >= 2005)

neet <- right_join(neet, pop, by = c("ref_area.label", "time", "sex.label"))

ssaneet <- neet %>% 
  filter(ref_area.label %in% ssa$ref_area.label) %>% 
  group_by(time, sex.label) %>%
  summarize(total_neet = sum(obs_value.x),
            total_pop = sum(obs_value.y)) %>% 
  mutate(obs_value = total_neet/total_pop*100) %>% 
  add_column("ref_area.label" = as.factor("SSA Average"))

neetrate %>% 
  bind_rows(neetrate, ssaneet) %>% 
  filter(ref_area.label %in% ssa$ref_area.label | ref_area.label == "SSA Average",
         sex.label == "Sex: Total") %>%
  mutate(ssadummy = ifelse(ref_area.label == "SSA Average", 1, 0)) %>% 
  ggplot(., aes(x = time)) +
  geom_line(aes(y=obs_value, color = as.factor(ssadummy)), show.legend = FALSE) +
  scale_color_manual(values=c('black','red')) +
  geom_point(aes(y = real_value), color="red", size = 4) +
  facet_wrap(~ ref_area.label, scales = "free_y") +
  labs(y = "Youth NEET rate", x = "") +
  theme(axis.text.x = element_text(angle = 45))

## ---- neetr4
neetrate %>% 
  bind_rows(neetrate, ssaneet) %>%
  mutate(region = ifelse(ref_area.label %in% unlist(country_lists[[5]][1]), "East Africa",
                         ifelse(ref_area.label %in% unlist(country_lists[[6]][1]), "Central Africa",
                                ifelse(ref_area.label %in% unlist(country_lists[[7]][1]), "West Africa",
                                       ifelse(ref_area.label %in% unlist(country_lists[[8]][1]), "Southern Africa", "SSA Average"))))) %>%  
  filter(ref_area.label %in% ssa$ref_area.label | ref_area.label == "SSA Average",
         sex.label == "Sex: Total") %>%
  mutate(ssadummy = ifelse(ref_area.label == "SSA Average", 1, 0)) %>%  
  ggplot(., aes(x = time, y=obs_value, group = ref_area.label)) +
  geom_line(aes(y=obs_value, color = as.factor(region), size = as.factor(ssadummy)), alpha = 0.85) +
  scale_color_manual(values=c("#377eb8", "#4daf4a", "#984ea3", "#e41a1c", "#ff7f00")) +
  scale_size_manual(values=c(.5, 2)) +
  guides(size = FALSE, color=guide_legend(title="")) +
  labs(y = "Youth NEET rate", x = "", title = "") +
  theme_minimal()

## ---- neetr5

neetrate %>% 
  right_join(., pop, by = c("ref_area.label", "time", "sex.label"))  %>% 
  rename("obs_value" = obs_value.x,
         "total_pop" = obs_value.y) %>% 
  bind_rows(., ssaneet) %>%
  mutate(Region = ifelse(ref_area.label %in% unlist(country_lists[[5]][1]), "East Africa",
                         ifelse(ref_area.label %in% unlist(country_lists[[6]][1]), "Central Africa",
                                ifelse(ref_area.label %in% unlist(country_lists[[7]][1]), "West Africa",
                                       ifelse(ref_area.label %in% unlist(country_lists[[8]][1]), "Southern Africa", "SSA Average"))))) %>%  
  filter(ref_area.label %in% ssa$ref_area.label | ref_area.label == "SSA Average",
         sex.label == "Sex: Total") %>%
  group_by(Region, time) %>% 
  summarise(obs_value = weighted.mean(obs_value, total_pop)) %>%
  mutate(ssadummy = ifelse(Region == "SSA Average", 1, 0)) %>%  
  ggplot(., aes(x = time, y=obs_value, group = Region)) +
  geom_line(aes(y=obs_value, color = as.factor(Region), size = as.factor(ssadummy))) +
  scale_color_manual(values=c("#377eb8", "#4daf4a", "#984ea3", "#e41a1c", "#ff7f00")) +
  scale_size_manual(values=c(.8, 2)) +
  guides(size = FALSE, color=guide_legend(title="")) +
  labs(y = "Youth NEET rate, weighted average", x = "", title = "") +
  theme_minimal()


  


