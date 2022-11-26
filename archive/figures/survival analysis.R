# Package names
packages <- c("tidyverse", "survival", "survminer")

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

ys_panel$transition_ready <- 0
ys_panel$graduation_age <- NA #last recorded year of schooling
ys_panel$transition_age <- NA  #age at which youth entered the final observed employment stint
ys_panel$first_employment_age <- NA
ys_panel$right_censored <- NA #whether transition duration is right censored

dflist <- ys_panel %>% 
  dplyr::select(starts_with("occ")) %>% 
  names()

for (k in 1:3021){
  for (i in 1:15){
    x <- dflist[i+1]
    y <- dflist[i]
    if (ys_panel[[y]][[k]] %in% c(1:5)) {
      ys_panel[["graduation_age"]][[k]] <- i+12
      ys_panel[["transition_ready"]][[k]] <- 1
    }
    if (ys_panel[[x]][[k]] %in% c(1:5)) {
      ys_panel[["first_employment_age"]][[k]] <- NA # if youth goes back to school, any first employment experience is nullified
    }
    if (ys_panel[[x]][[k]] %in% c(6:7) && ys_panel[[y]][[k]] %in% c(0,1:5,8) && ys_panel[["transition_ready"]][[k]] == 1) {
      ys_panel[["transition_age"]][[k]] <- i+13
    }
    if (ys_panel[[x]][[k]] %in% c(6:7) && ys_panel[[y]][[k]] %in% c(0,1:5,8) && ys_panel[["transition_ready"]][[k]] == 1 && is.na(ys_panel[["first_employment_age"]][[k]])) {
      ys_panel[["first_employment_age"]][[k]] <- i+13
    }
    if (ys_panel[[x]][[k]] %in% c(0,1:5,8)) {
      ys_panel[["transition_age"]][[k]] <- NA # if youth goes back to school or stops working, any first employment experience is nullified
    }
  }
  if(ys_panel[["act19"]][[k]] %in% c(1:5)) {
    ys_panel[["graduation_age"]][[k]] <- NA
  }
  if(!is.na(ys_panel[["graduation_age"]][[k]]) && is.na(ys_panel[["transition_age"]][[k]])) { #if school leaving age yes, steady employment no
    ys_panel[["right_censored"]][[k]] <- 1
  }
}

ys_panel <- ys_panel %>% dplyr::mutate(transition_duration = if_else(!is.na(transition_age), transition_age-graduation_age,
                                                                     if_else(!is.na(graduation_age), baseline_age-graduation_age-1, NULL)),
                                       first_transition_duration = if_else(!is.na(first_employment_age), first_employment_age-graduation_age,
                                                                           if_else(!is.na(graduation_age), baseline_age-graduation_age-1, NULL)),
                                       right_censored = ifelse(!is.na(right_censored), 1, 0))

df <- ys_panel %>% 
  filter(wave == 0) %>% 
  mutate(id = row_number(), 
         weights = 1/prob,
         right_censored = 1-right_censored) %>%  #by convention, 0 = alive (i.e. censored) and 1 = dead (not censored)
  select(id, sex, baseline_age, starts_with("occ"), graduation_age, first_employment_age, transition_age, transition_duration, first_transition_duration, right_censored, prob) %>% 
  mutate(sex = recode(sex, `1` = "Male", `0` = "Female"))

rm(dflist, i, k, x, y)

## survival analysis, first pass

df <- df %>% mutate(weights = 1/prob)

surv_object <- Surv(time = df$transition_duration, event = df$right_censored)

fit1 <- survfit(surv_object ~ sex, data = df)
fit2 <- survfit(surv_object ~ sex, data = df, weights = weights)

png("figures/chap01/survplot_transduration.png", width = 900, height = 450, units = "px")
ggsurvplot(fit1, data = df, pval = FALSE, risk.table = "abs_pct", risk.table.title = "Probability of needing an additional year before transitioning", font.y =15, palette = "aaas", conf.int = TRUE, tables.theme = theme_survminer(font.main = 14), legend.labs=c("Female" ,"Male"), fontsize = 5.5) + guides(colour = "none", fill = "none") + labs(x = "Duration in Years")
dev.off()


mean(ys_panel$transition_duration, na.rm = T) # raw mean = 1.48
print(fit1, print.rmean=T) # unweighted restricted mean from survival analysis = 1.92
print(fit2, print.rmean=T) # weighted restricted mean from survival analysis = 1.82

# first employment age transition duration

surv_object2 <- Surv(time = df$first_transition_duration, event = df$right_censored)

fit3 <- survfit(surv_object2 ~ sex, data = df)
fit4 <- survfit(surv_object2 ~ sex, data = df, weights = weights)

png("figures/chap01/survplot_first_emp.png", width = 900, height = 450, units = "px")
ggsurvplot(fit3, data = df, pval = FALSE, risk.table = "abs_pct", risk.table.title = "Transition Probability", font.y =15, palette = "aaas", tables.theme = theme_survminer(font.main = 14), legend.labs=c("Female" ,"Male"), fontsize = 5.5) + guides(colour = "none", fill = "none") + labs(x = "Duration in Years")
dev.off()


print(fit3, print.rmean=T) # unweighted restricted mean from survival analysis = 1.5675
print(fit4, print.rmean=T) # weighted restricted mean from survival analysis = 1.51
