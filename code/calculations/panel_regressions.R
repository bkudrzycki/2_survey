# Package names
packages <- c("here", "haven", "gtsummary", "tidyverse", "plm", "pglm", "texreg")
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
source(here("Youth Employment", "Thesis", "R", "source", "load_panel.R"))

#load survey
ys <- read_sav(here("Youth Employment", "Thesis", "data", "youth_survey", "youth_survey_merged.sav"))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i) # act#: # corresponds to year
  ys[[z]] <- coalesce(ys[[y]], ys[[x]])
}

ys_baseline <- ys_panel %>% filter(wave == 0)

dflist <- ys_baseline %>% 
  dplyr::select(starts_with("act1")) %>% 
  names()

ys_panel$transition_ready <- 0
ys_panel$transitioned <- 0

#only those who have been in school can transition; only those who haven't transitioned will be taken into account
for (k in 1:3021){
  for (i in 1:7){
    y <- dflist[i]
    if(ys_panel[[y]][[k]] %in% c(1:5)) {
      ys_panel[["transition_ready"]][[k]] <- 1
    }
    if(ys_panel[[y]][[k]] %in% c(6:7)) {
      ys_panel[["transitioned"]][[k]] <- 1
    }
  }
}

# for those who did not transition in past seven years, transitioned when all subsequent periods are in employment:
df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(transitioned_0 = ifelse(status_0 %in% c("Employed", "Self-Employed") & status_1 %in% c("Employed", "Self-Employed", NA) & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0), 
         transitioned_1 = ifelse(status_1 %in% c("Employed", "Self-Employed") & status_2 %in% c("Employed", "Self-Employed", NA) & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_2 = ifelse(status_2 %in% c("Employed", "Self-Employed") & status_3 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA) & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_3 =ifelse(status_3 %in% c("Employed", "Self-Employed") & status_4 %in% c("Employed", "Self-Employed", NA), 1, 0),
         transitioned_4 =ifelse(status_4 %in% c("Employed", "Self-Employed"), 1, 0)) %>%
  pivot_longer(cols = starts_with("trans"),
               names_to = "wave",
               names_prefix = "transitioned_",
               values_to = "transitioned2") %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, transitioned2)
         
ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

rm(df, i, x, y, z)

# additionally, create dummy for following transition types: self->wage (sw), wage->self (ws), neet->work (nw), work->neet (wn)

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(sw_0 = ifelse(act19 == 7 & status_0 == "Employed",1,0),
         sw_1 = ifelse(status_0 == "Self-Employed" & status_1 == "Employed",1,0),
         sw_2 = ifelse(status_1 == "Self-Employed" & status_2 == "Employed",1,0),
         sw_3 = ifelse(status_2 == "Self-Employed" & status_3 == "Employed",1,0),
         sw_4 = ifelse(status_3 == "Self-Employed" & status_4 == "Employed",1,0)) %>% 
  pivot_longer(cols = starts_with("sw"),
             names_to = "wave",
             names_prefix = "sw_",
             values_to = "sw") %>% 
  mutate(sw = replace_na(sw, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, sw)

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(ws_0 = ifelse(act19 == 7 & status_0 == "Self-Employed",1,0),
         ws_1 = ifelse(status_0 == "Employed" & status_1 == "Self-Employed",1,0),
         ws_2 = ifelse(status_1 == "Employed" & status_2 == "Self-Employed",1,0),
         ws_3 = ifelse(status_2 == "Employed" & status_3 == "Self-Employed",1,0),
         ws_4 = ifelse(status_3 == "Employed" & status_4 == "Self-Employed",1,0)) %>% 
  pivot_longer(cols = starts_with("ws"),
               names_to = "wave",
               names_prefix = "ws_",
               values_to = "ws") %>% 
  mutate(ws = replace_na(ws, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, ws)

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))

df <- ys_panel %>% 
  dplyr::select(IDYouth, wave, status, age, act19) %>% 
  pivot_wider(names_from = wave,
              values_from = c(status, age)) %>% 
  mutate(nw_0 = ifelse(act19 %in% c(0,8) & status_0 %in% c("Self-Employed","Employed"),1,0),
         nw_1 = ifelse(status_0 == "NEET" & status_1 %in% c("Self-Employed","Employed"),1,0),
         nw_2 = ifelse(status_1 == "NEET" & status_2 %in% c("Self-Employed","Employed"),1,0),
         nw_3 = ifelse(status_2 == "NEET" & status_3 %in% c("Self-Employed","Employed"),1,0),
         nw_4 = ifelse(status_3 == "NEET" & status_4 %in% c("Self-Employed","Employed"),1,0)) %>% 
  pivot_longer(cols = starts_with("nw"),
               names_to = "wave",
               names_prefix = "nw_",
               values_to = "nw") %>% 
  mutate(nw = replace_na(nw, 0)) %>% 
  mutate(wave = as.double(wave)) %>% 
  dplyr::select(IDYouth, wave, nw)

ys_panel <- left_join(ys_panel, df, by = c("IDYouth", "wave"))


ys_panel <- ys_panel %>% 
  mutate(beninese = ifelse(YS3_1 == 1, 1, 0),
         fathapp = YS3_9,
         mothapp = YS3_11,
         siblings = as.numeric(YS3_7),
         fon = ifelse(YS3_4_4 == 1, 1, 0),
         fon = ifelse(is.na(fon), 0, 1),
         christian = ifelse(YS3_5_1 == 1 | YS3_5_2 == 1 | YS3_5_3 == 1 | YS3_5_4 == 1, 1, 0),
         christian = ifelse(is.na(christian), 0, 1),
         secplus = ifelse(YS3_16 > 4 & YS3_16 != 10, 1, 0),
         bac = ifelse(is.na(YS3_17_6), 0, 1),
         fsecplus = ifelse(YS3_10 > 4 & YS3_10 != 10, 1, 0),
         msecplus = ifelse(YS3_12 > 4 & YS3_12 != 10, 1, 0),
         city = ifelse(YS3_3 == 4, 1, 0),
         yos = ifelse(YS3_15 != 99, YS3_15, 0),
         yos = ifelse(YS3_15 < 50, YS3_15, NA))

ys_panel$siblings <- ifelse(is.na(ys_panel$siblings), 0, ys_panel$siblings)

for_plm <- ys_panel %>% filter(!is.na(YS6_21)) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, sw, ws, nw, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm2 <- ys_panel %>% filter(!is.na(YS6_21),
                                transitioned == 0) %>%
  select(IDYouth, wave, age, sex, YS6_19, YS6_21, transitioned2, beninese, fathapp, mothapp, siblings, fon, christian, secplus, bac, fsecplus, msecplus, city, yos)

for_plm$YS6_21 <- as.double(for_plm$YS6_21)
for_plm2$YS6_21 <- as.double(for_plm2$YS6_21)

extract.pglm <- function (model, include.nobs = TRUE, include.loglik = TRUE, ...) {
  s <- summary(model, ...)
  coefficient.names <- rownames(s$estimate)
  coefficients <- s$estimate[, 1]
  standard.errors <- s$estimate[, 2]
  significance <- s$estimate[, 4]
  loglik.value <- s$loglik
  n <- nrow(model$model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    gof <- c(gof, loglik.value)
    gof.names <- c(gof.names, "Log-Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                     se = standard.errors, pvalues = significance, gof.names = gof.names, 
                     gof = gof, gof.decimal = gof.decimal)
  return(tr)
}

setMethod("extract", signature = className("maxLik", "maxLik"), 
          definition = extract.pglm)

# new tables
op1 <- pglm(YS6_21 ~ transitioned2 + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op1 <- YS6_21 ~ transitioned2 + age + sex + yos
phtest(form_op1, data = for_plm)
m1 <- extract.pglm(op1)

op2 <- pglm(YS6_21 ~ sw + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op2 <- YS6_21 ~ sw + age + sex + yos
phtest(form_op2, data = for_plm)
m2 <- extract.pglm(op2)

op3 <- pglm(YS6_21 ~ ws + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = c("IDYouth", "wave"), model = "random", na.action=na.omit)
form_op3 <- YS6_21 ~ ws + age + sex + yos
phtest(form_op3, data = for_plm)
m3 <- extract.pglm(op3)

op4 <- pglm(YS6_21 ~ nw + age + sex + yos, data = for_plm, family = ordinal('probit'), effect=("individual"), method = 'bfgs', index = "IDYouth", model = "random", na.action=na.omit)
form_op4 <- YS6_21 ~ nw + age + sex + yos
phtest(form_op4, data = for_plm)
m4 <- extract.pglm(op4)

texreg(list(m1, m2, m3, m4),
       override.coef = list(exp(m1@coef),exp(m2@coef),exp(m3@coef),exp(m4@coef)),
       override.se = list(exp(m1@se),exp(m2@se),exp(m3@se),exp(m4@se)),
       custom.coef.names = c("Intercept", "Transitioned", "Age at Baseline", "Male", "Years of Schooling", "Self->Wage", "Wage->Self", "NEET->Work"),
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
       #omit.coef = c('mu|sigma|beninese|city|fon|christian|siblings|fsecplus|fathapp|msecplus|mothapp|bac'),
       #custom.gof.rows = list("Covariates" = c("YES", "YES", "YES", "YES")),
       booktabs = TRUE,
       fontsize = "small",
       label = "tab:reg_table",
       custom.note = paste("\\item %stars. Odds ratios reported."),
       float.pos = "H",
       caption = "Random effects probit regression of life satisfaction on successful transition")

