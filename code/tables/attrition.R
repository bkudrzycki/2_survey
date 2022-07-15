# Package names
packages <- c("haven", "gtsummary", "kableExtra")
#detach(package:here, unload=TRUE)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Packages loading
invisible(suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)))
rm(packages, installed_packages)

setwd("~/polybox/Youth Employment/Thesis")

load("data/youth_survey/ys_panel_labels.rda")

ys_panel_labels <- ys_panel_labels %>% 
  mutate(YS3_15 = as.numeric(YS3_15),
         yos = ifelse(YS3_15 < 50, YS3_15, NA))

ys_panel_labels$baseline_activity <- factor(ys_panel_labels$baseline_activity, levels=c("Apprentice", "In School", "Employed", "Self-Employed", "NEET"))

ys_panel_labels$wave <- factor(ys_panel_labels$wave, labels=c("Baseline", "Remote 1", "Remote 2", "Remote 3", "Endline"))

ys_panel_labels %>% 
  dplyr::select(wave, status, baseline_activity, sex, baseline_age, yos) %>% 
  tbl_summary(by = wave,
              label = list(status ~ "Activity",
                           baseline_activity ~ "Baseline activity",
                           sex ~ "Male",
                           baseline_age ~ "Age",
                           yos ~ "Years of Schooling"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)",
                               sex ~ "{p}%")) %>% 
  add_p(list(status ~ NULL,
             baseline_activity ~ "kruskal.test",
             sex ~ "chisq.test",
             baseline_age ~ "kruskal.test",
             yos ~ "kruskal.test"),
        pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  as_kable_extra(format = "latex", booktabs = T, linesep = "") %>% 
  save_kable("tables/panel/R/attrition.tex")
