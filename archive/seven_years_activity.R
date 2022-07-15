# Package names
packages <- c("here", "haven", "tidyverse")
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

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i) # act#: # corresponds to year
  ys[[z]] <- coalesce(ys[[y]], ys[[x]])
}

ys_baseline <- ys_panel %>% filter(wave == 0)

x <- ys_baseline %>% select(IDYouth, starts_with("act1"), sex) %>% 
  pivot_longer(cols = starts_with("act"),
               names_to = "year",
               names_prefix = "act",
               values_to = "activity")

write.csv(x, "/Users/kudrzycb/polybox/Youth Employment/1a Youth Survey/Paper/Analysis/Data/past_activities.csv")

  
table(x$sex)

table(ys_panel$sexe, ys_panel$sex)

