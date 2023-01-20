# Package names
packages <- c("here", "tidyverse", "survey", "ggplot2")

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

## ---- youth activity by age figure

df <- ys_panel_labels %>%
  filter(wave == "YS") %>% 
  pivot_longer(cols = starts_with("occ"),
               names_to = "actage",
               names_prefix = "occ",
               values_to = "activity") %>% 
  mutate(actage = as.numeric(actage))

df <- df %>% 
  filter(activity != 99) %>% ## no apprentices, drop "don't want to say"
  mutate(activity = dplyr::recode(activity, `5` = 4),## merge formal and traditional apprenticeship
         activity = dplyr::recode(activity, `0` = 8)) %>% 
  group_by(actage, activity) %>%
  summarise(n = sum(activity, na.rm = TRUE)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!is.na(activity)) %>% 
  ungroup() %>% 
  rbind(c(13,3,0,0),
        c(14,7,0,0),
        c(15,6,0,0),
        c(28,2,0,0))

ggplot(df, aes(x=actage, y=prop, fill=factor(activity))) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_x_continuous(breaks=c(13:28)) +
  xlab("age") +
  theme_minimal() +
  scale_fill_brewer(palette="Greys", name = "Activity", labels = c("Primary School", "Secondary School", "University", "Apprenticeship", "Wage Employment", "Self-employment", "NEET")) 
