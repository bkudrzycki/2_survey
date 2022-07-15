## ---- reservation wage
beninswts12 <- beninswts12 %>%
  mutate(d60x = d60x/4) %>% 
  mutate(reswage = ifelse(d60x <  7000, "Less than 7.000 FCFA",
                          ifelse(d60x >= 7000 & d60x < 10000, "7.000-9.999 FCFA",
                                 ifelse(d60x >= 10000 & d60x < 15000, "10.000-14.999 FCFA",
                                        ifelse(d60x >= 15000 & d60x < 25000, "15.000-24.999 FCFA",
                                               ifelse(d60x >= 25000 & d60x < 50000, "25.000-49.999 FCFA","more than 50.000 FCFA")))))) %>% 
  mutate(id = "swts12")



beninswts14 <- beninswts14 %>%
  mutate(E46X = E46X/4) %>%
  mutate(reswage = ifelse(E46X < 7, "Less than 7.000 FCFA",
                          ifelse(E46X >= 7 & E46X < 10, "7.000-9.999 FCFA",
                                 ifelse(E46X >= 10 & E46X < 15, "10.000-14.999 FCFA",
                                        ifelse(E46X >= 15 & E46X < 25, "15.000-24.999 FCFA",
                                               ifelse(E46X >= 25 & E46X < 50, "25.000-49.999 FCFA","more than 50.000 FCFA")))))) %>% 
  mutate(id = "swts14")


ys_labels$reswage <- ys_labels$YS10_19 %>% 
  recode("Moins de 1.500 FCFA" = "Less than 7.000 FCFA",
         "1.500-2.499 FCFA" = "Less than 7.000 FCFA",
         "2.500-3.499 FCFA" = "Less than 7.000 FCFA",
         "5.000-6.999 FCFA" = "Less than 7.000 FCFA",
         "50.000 FCFA ou plus" = "more than 50.000 FCFA")
ys_labels <- ys_labels %>% 
  mutate(id = "ys19")

reswage <- rbind(subset(beninswts12, select = c(reswage, id)),
                 subset(beninswts14, select = c(reswage, id)),
                 subset(ys_labels, select = c(reswage, id)))

round(prop.table(table(reswage$reswage, reswage$id),2),2)
cat("Apprentices have lower reservation wages")
round(prop.table(table(ys_labels$reswage, ys_labels$YS3_13),2),2)

reswage %>% 
  drop_na(reswage) %>% 
  ggplot(aes(reswage, fill = id, group  = id)) +
  geom_bar(aes(y=..prop..), position = "dodge") +
  scale_x_discrete(limits=c("Less than 7.000 FCFA",
                            "7.000-9.999 FCFA", "10.000-14.999 FCFA",
                            "15.000-24.999 FCFA", "25.000-49.999 FCFA",
                            "more than 50.000 FCFA")) +
  guides(fill=guide_legend(title="")) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_grey(start=0.8, end=0.2)

## ---- selectivity index1

beninswts12 <- beninswts12 %>%
  mutate(age = age) %>% 
  mutate(sex = 3-sex) %>% # 1 = male
  mutate(educ = m01_17n) %>%
  mutate(feduc = b24) %>% 
  mutate(meduc = b26) %>%
  mutate(id = 1)

beninswts14 <- beninswts14 %>%
  mutate(educ = m01_17n) %>% 
  mutate(feduc = b21) %>% 
  mutate(meduc = b23) %>% 
  mutate(id = 2)

ys <- ys %>% 
  mutate(sex = 2 - YS1_7) %>%
  mutate(educ = case_when(
    YS3_16 == 0 ~ 1, #none
    YS3_16 == 1 ~ 1, #less than prim
    YS3_16 == 2 ~ 1, #less than prim
    YS3_16 == 3 ~ 2, #prim
    YS3_16 == 4 ~ 4, #secondary, general
    YS3_16 == 5 ~ 4, #secondary, general
    YS3_16 == 6 ~ 3, #secondary, prof
    YS3_16 == 7 ~ 3, #secondary, prof
    YS3_16 == 8 ~ 6, #university (grad)
    YS3_16 == 9 ~ 5, #post-secondary professional
    YS3_16 == 10 ~ 99)) %>% #other
  mutate(feduc = case_when(
    YS3_10 == 0 ~ 1,
    YS3_10 == 1 ~ 1,
    YS3_10 == 2 ~ 1,
    YS3_10 == 3 ~ 2,
    YS3_10 == 4 ~ 4,
    YS3_10 == 5 ~ 4,
    YS3_10 == 6 ~ 3,
    YS3_10 == 7 ~ 3,
    YS3_10 == 8 ~ 6,
    YS3_10 == 9 ~ 5,
    YS3_10 == 10 ~ 99)) %>%
  mutate(meduc = case_when(
    YS3_12 == 0 ~ 1,
    YS3_12 == 1 ~ 1,
    YS3_12 == 2 ~ 1,
    YS3_12 == 3 ~ 2,
    YS3_12 == 4 ~ 4,
    YS3_12 == 5 ~ 4,
    YS3_12 == 6 ~ 3,
    YS3_12 == 7 ~ 3,
    YS3_12 == 8 ~ 6,
    YS3_12 == 9 ~ 5,
    YS3_12 == 10 ~ 99)) %>% 
  mutate(id = 3)

df <- ys %>% 
  rename(age = baseline_age)

educ <- rbind(subset(df, select = c(age, sex, educ, feduc, meduc)))

educ$educ <- labelled(educ$educ, c(None = 1, Prim = 2, SecProf = 3, SecGen = 4, PostProf = 5, PostGen = 6, PostGrad = 7, IDK = 8, Other = 99))
educ$feduc <- labelled(educ$feduc, c(None = 1, Prim = 2, SecProf = 3, SecGen = 4, PostProf = 5, PostGen = 6, PostGrad = 7, IDK = 8, Other = 99))
educ$meduc <- labelled(educ$meduc, c(None = 1, Prim = 2, SecProf = 3, SecGen = 4, PostProf = 5, PostGen = 6, PostGrad = 7, IDK = 8, Other = 99))

## secondary school selectivity index

ss <- educ %>% 
  filter(educ %in% c(3:6), ## youth who completed secondary school
         feduc < 8,
         meduc < 8) %>% 
  mutate(feduc1 = case_when( ##new levels: none: 1, prim: 2, sec: 3, uni: 4
    feduc == 1 ~ 1,
    feduc == 2 ~ 2,
    feduc == 4 ~ 3,
    feduc == 5 ~ 4,
    feduc == 6 ~ 4,
    feduc == 7 ~ 4)) %>% 
  mutate(meduc1 = case_when( ##new levels: none: 1, prim: 2, sec: 3, uni: 4
    meduc == 1 ~ 1,
    meduc == 2 ~ 2,
    meduc == 4 ~ 3,
    meduc == 5 ~ 4,
    meduc == 6 ~ 4,
    meduc == 7 ~ 4))

ts <- educ %>% 
  filter(educ %in% c(5:6), ## youth who completed university
         feduc < 8,
         meduc < 8) %>% 
  mutate(feduc2 = case_when( ##new levels: none: 1, prim: 2, sec: 3, uni: 4
    feduc == 1 ~ 1,
    feduc == 2 ~ 2,
    feduc == 4 ~ 3,
    feduc == 5 ~ 4,
    feduc == 6 ~ 4,
    feduc == 7 ~ 4)) %>% 
  mutate(meduc2 = case_when( ##new levels: none: 1, prim: 2, sec: 3, uni: 4
    meduc == 1 ~ 1,
    meduc == 2 ~ 2,
    meduc == 4 ~ 3,
    meduc == 5 ~ 4,
    meduc == 6 ~ 4,
    meduc == 7 ~ 4))

sIndex <- prop.table(table(ss$feduc1)) %>% 
  cbind(ss_meduc = prop.table(table(ss$meduc1))) %>% 
  cbind(ts_feduc = prop.table(table(ts$feduc2))) %>% 
  cbind(ts_meduc = prop.table(table(ts$meduc2))) %>% 
  as.data.frame() %>% 
  rename(ss_feduc = ".")
  

## append rates of educational attainment in general adult population
urban_census <- urban_census %>% 
  mutate(educ = case_when(
    P19 < 12 ~ 1,
    P19 >= 12 & P19 < 17 ~ 2,
    P19 >= 21 & P19 < 27 ~ 2,
    P19 == 27 ~ 3,
    P19 == 17 ~ 3,
    P19 >= 31 & P19 < 33 ~ 3,
    P19 >= 33 & P19 < 39 ~ 4))

feduc_rates <- urban_census %>% 
  filter(P19 < 90,
         P06 > 18, #adults
         P04 == 1) #fathers

feduc_rates <- as.data.frame(prop.table(table(feduc_rates$educ))) %>% 
  rename(., educ = Var1,
         feduc_rates = Freq)

meduc_rates <- urban_census %>% 
  filter(P19 < 90,
         P06 > 18, #adults
         P04 == 2) #mothers

meduc_rates <- as.data.frame(prop.table(table(meduc_rates$educ))) %>% 
  rename(., educ = Var1,
         meduc_rates = Freq)

sIndex <- sIndex %>% 
  cbind(feduc_rates[2], meduc_rates[2]) %>% 
  mutate("Father/secondary: Ghana 1960" = c(0.4,1.5,6.9,13.0),
         "Father/secondary: Benin 2019" = round(ss_feduc / feduc_rates,2),
         "Mother/secondary" = round(ss_meduc / meduc_rates,2),
         "Father/tertiary" = round(ts_feduc / feduc_rates,2),
         "Mother/tertiary" = round(ts_meduc / meduc_rates,2))

row.names(sIndex) <- c("None", "Primary", "Secondary", "Tertiary")

addtorow <- list(
  pos=list(-1), 
  command=c("\\\\[-1.8ex]\\hline")
)

mdat <- matrix(c(rep(1,4), 0, rep(2, (4*5))),
               nrow = 5, ncol=5, byrow=TRUE)

print(xtable(t(sIndex[,7:11]), digits=mdat), floating=F, size="\\small", hline.after = c(-1,-1,0,5,5), file = "./tables/chap01/sIndex.tex")


## ---- probability of schooling

cond_prob <- matrix(nrow = 19, ncol =20)
yos <- matrix(nrow = nrow(ys), ncol = 20)
males <- ys %>% 
  filter(YS3_15<90) %>% 
  filter(YS1_7 == 1) %>% 
  filter(status != "Apprentice")
females <- ys %>% 
  filter(YS3_15<90) %>% 
  filter(YS1_7 == 0) %>% 
  filter(status != "Apprentice")


for (j in 0:20) {
  for(i in 1:nrow(ys)) {
    yos[i,j] <- ifelse(males$YS3_15[i] >= j, 1 ,0)
  }
}

for (j in 1:19) {
  cond_prob[j,] <- as.data.frame(yos) %>% 
    filter(yos[,j] == 1) %>% 
    colMeans(yos)
}

males <- as.data.frame(cbind(diag(cond_prob[,-1]), c(1:19))) %>% 
  mutate(Gender = "Male")

for (j in 0:20) {
  for(i in 1:nrow(ys)) {
    yos[i,j] <- ifelse(females$YS3_15[i] >= j, 1 ,0)
  }
}

for (j in 1:19) {
  cond_prob[j,] <- as.data.frame(yos) %>% 
    filter(yos[,j] == 1) %>% 
    colMeans(yos)
}

females <- as.data.frame(cbind(diag(cond_prob[,-1]), c(1:19))) %>% 
  mutate(Gender = "Female")

diag <- rbind(males, females)

ggplot(diag, aes(x=V2, y = V1, group = Gender)) +
  geom_line(aes(color = Gender)) +
  theme_minimal() +
  labs(x = "Years of schooling completed", 
       y = "Prob. of additional year")

## ---- 3-phase transitions

df <- ys_labels %>% 
  filter(!is.na(baseline_age))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i)
  df[[z]] <- coalesce(df[[y]], df[[x]])
}

for (i in 20:29) { ## possible age of youth
  for (j in 7:1) { ## 7 years of recall
    x<-paste0("occ", i-j+1)
    df[[x]] = NA
  }
}

for (k in 1:943){
  for (i in 20:29) { ## possible age of youth
    if (df$baseline_age[k] == i) {
      for (j in 1:7) { ## 7 years of recall
        x <- paste0("act", 20-j) ## activity by year (stored in previous loop)
        y <- paste0("occ", i-j+1) ## age that year
        df[[y]][[k]] <- df[[x]][[k]] 
      }
    }
  }
}

df <- df %>% 
  pivot_longer(cols = starts_with("occ"),
               names_to = "actage",
               names_prefix = "occ",
               values_to = "activity")

df2 <- df %>% 
  mutate(actage = as.numeric(actage)) %>%
  filter(YS1_2 == "No",
         activity != 10) %>% ## no apprentices, drop "don't want to say"
  mutate(activity = case_when(
    activity == 0 ~ 3L, # none (stay at home) -> NEET
    activity == 2 ~ 1L, # school & apprenticeship
    activity == 3 ~ 1L,
    activity == 4 ~ 1L,
    activity == 5 ~ 1L,
    activity == 6 ~ 1L,
    activity == 7 ~ 2L, # employed and self-employed
    activity == 8 ~ 2L,
    activity == 9 ~ 3L,)) %>%  # unemployed -> NEET
  group_by(actage, activity) %>%
  summarise(n = sum(activity, na.rm = TRUE)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!is.na(activity))

ggplot(df2, aes(x=actage, y = prop, group = activity)) +
  geom_line(aes(color = as.factor(activity))) +
  scale_color_discrete(name = "Transition status", labels = c("School or training", "Employed", "NEET")) +
  theme_minimal() +
  scale_x_continuous(breaks=c(14:29)) +
  labs(x = "Age", 
       y = "Proportion")


## ---- 7-year transitions

df <- ys_labels %>% 
  filter(!is.na(baseline_age))

for (i in 1:7) {
  x <- paste0("YS6_16_", i)
  y <- paste0("F2U3_0a_", i)
  z <- paste0("act", 20-i)
  df[[z]] <- coalesce(df[[y]], df[[x]])
}

for (i in 20:29) { ## possible age of youth
  for (j in 7:1) { ## 7 years of recall
    x<-paste0("occ", i-j+1)
    df[[x]] = NA
  }
}

for (k in 1:943){
  for (i in 20:29) { ## possible age of youth
    if (df$baseline_age[k] == i) {
      for (j in 1:7) { ## 7 years of recall
        x <- paste0("act", 20-j) ## activity by year (stored in previous loop)
        y <- paste0("occ", i-j+1) ## age that year
        df[[y]][[k]] <- df[[x]][[k]] 
      }
    }
  }
}

df <- df %>% 
  pivot_longer(cols = starts_with("occ"),
               names_to = "actage",
               names_prefix = "occ",
               values_to = "activity")

df <- df %>% 
  mutate(actage = as.numeric(actage)) %>%
  filter(YS1_2 == "No",
         activity != 10) %>% ## no apprentices, drop "don't want to say"
  mutate(activity = recode(activity, `6` = 5L),## merge formal and traditional apprenticeship
         activity = recode(activity, `1` = 9L)) %>% 
  group_by(actage, activity) %>%
  summarise(n = sum(activity, na.rm = TRUE)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(!is.na(activity)) %>% 
  ungroup() %>% 
  rbind(c(14,4,0,0),
        c(14,8,0,0),
        c(15,8,0,0),
        c(28,3,0,0),
        c(29,3,0,0))

ggplot(df, aes(x=actage, y=prop, fill=factor(activity))) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_discrete(name = "Activity", labels = c("Primary School", "Secondary School", "University", "Apprenticeship", "Wage Employment", "Self-employment", "NEET")) +
  scale_x_continuous(breaks=c(14:29)) +
  xlab("age") +
  theme_minimal()

## ---- 


ys_labels %>%
  drop_na(YS2_10_3_1) %>%
  mutate(age = 2019 - as.numeric(as.character(YS2_10_3_1))) %>%
  filter(age > 19,
         age < 30) %>%
  filter(YS1_6 == "Abomey-Calavi" | YS1_6 == "Cotonou" | YS1_2 == "No") %>%
  filter(status == "Apprentice") %>% 
  mutate(YS4_15 = fct_recode(YS4_15, "<10,000" = 'Moins de 10.000 FCFA',
                             "10,000-19,999" = '10.000-19.999 FCFA',
                             "20,000-34,999" = '20.000-34.999 FCFA',
                             "35,000-54,999" = '35.000-54.999 FCFA',
                             "55,000-84,999" = '55.000-84.999 FCFA',
                             "85,000-124,999" = '85.000-124.999 FCFA',
                             "125,000-174,999" = '125.000-174.999 FCFA',
                             "175,000-249,999" = '175.000-249.999 FCFA',
                             "250,000-349,999" = '250.000-349.999 FCFA',
                             ">350,000" = '350.000 FCFA ou plus')) %>%
  mutate(YS4_21 = fct_recode(YS4_21, "<10,000" = 'Moins de 10.000 FCFA',
                             "10,000-19,999" = '10.000-19.999 FCFA',
                             "20,000-34,999" = '20.000-34.999 FCFA',
                             "35,000-54,999" = '35.000-54.999 FCFA',
                             "55,000-84,999" = '55.000-84.999 FCFA',
                             "85,000-124,999" = '85.000-124.999 FCFA',
                             "125,000-174,999" = '125.000-174.999 FCFA',
                             "175,000-249,999" = '175.000-249.999 FCFA',
                             "250,000-349,999" = '250.000-349.999 FCFA',
                             ">350,000" = '350.000 FCFA ou plus')) %>%
  mutate(YS4_24 = fct_recode(YS4_24, "<10,000" = 'Moins de 10.000 FCFA',
                             "10,000-19,999" = '10.000-19.999 FCFA',
                             "20,000-34,999" = '20.000-34.999 FCFA',
                             "35,000-54,999" = '35.000-54.999 FCFA',
                             "55,000-84,999" = '55.000-84.999 FCFA',
                             "85,000-124,999" = '85.000-124.999 FCFA',
                             "125,000-174,999" = '125.000-174.999 FCFA',
                             "175,000-249,999" = '175.000-249.999 FCFA',
                             "250,000-349,999" = '250.000-349.999 FCFA',
                             ">350,000" = '350.000 FCFA ou plus')) %>%
  select("Entry Fee" = YS4_15, "Training Fee" = YS4_21, "Liberation Fee" = YS4_24) %>%
  gather(type, value) %>%
  ggplot(aes(x = value, fill = type)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "FCFA",
       y = "",
       title = "",
       fill= "") +
  theme_minimal() +
  scale_x_discrete(limits=c("<10,000", "10,000-19,999",
                            "20,000-34,999", "35,000-54,999",
                            "55,000-84,999", "85,000-124,999",
                            "125,000-174,999", "175,000-249,999",
                            "250,000-349,999", ">350,000")) +
  scale_fill_manual(values = alpha(c("black", "light blue", "dark gray"), .8)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("figures/chap01/appfees.png",
       width = 20, height = 12, units = "cm")

# Apprentice satisfaction with workshop conditions

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_1) %>% 
  table()

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_2) %>% 
  table() %>% 
  cbind("physical safety" = df, "knowledge of master" = .)

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_3) %>% 
  table() %>% 
  cbind(df, "treatment by master" = .)

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_5) %>% 
  table() %>% 
  cbind(df, "working hours" = .)

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_6) %>% 
  table() %>% 
  cbind(df, "quality of training" = .)

df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_7) %>% 
  table() %>% 
  cbind(df, "equipment and machinery" = .)


df <- ys_labels %>% 
  filter(status == "Apprentice") %>% 
  dplyr::select(YS4_40_8) %>% 
  table() %>% 
  cbind(df, "relationship with coworkers" = .) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "question") %>% 
  gather(key = score, value = value, -question) %>%
  group_by(question) %>%
  mutate(Percentage = ifelse(value > 0, value/sum(value), 0))

df %>%
  ggplot(aes(x=question,y=Percentage,fill=forcats::fct_rev(score))) +
  geom_bar(stat = "identity", position="fill", color="black", width=.9) + 
  theme_minimal() +
  labs(y="Percent", x = "", fill="") + ylab("") + ggtitle("Apprentices") + theme(plot.title = element_text(size = 12)) + coord_flip() + scale_y_continuous(labels=scales::percent) +
  scale_fill_grey(
    start = 0.2,
    end = 1,
    na.value = "red",
    aesthetics = "fill"
  ) +
  ggsave("figures/chap01/apprankings.png",
         width = 20, height = 8, units = "cm")

## Wage worker satisfaction with employer

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_1) %>% 
  table()

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_4) %>% 
  table() %>% 
  cbind("physical safety" = df, "salary" = .)

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_5) %>% 
  table() %>% 
  cbind(df, "working hours" = .)

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_6) %>% 
  table() %>% 
  cbind(df, "quality of training" = .)

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_8) %>% 
  table() %>% 
  cbind(df, "coworkers" = .)

df <- ys_labels %>% 
  filter(status == "Employed") %>% 
  dplyr::select(YS8_32_9) %>% 
  table() %>% 
  cbind(df, "job security" = .) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "question") %>% 
  gather(key = score, value = value, -question) %>%
  group_by(question) %>%
  mutate(Percentage = ifelse(value > 0, value/sum(value), 0))

df %>%
  ggplot(aes(x=question,y=Percentage,fill=forcats::fct_rev(score))) +
  geom_bar(stat = "identity", position="fill", color="black", width=.9) + 
  theme_minimal() +
  labs(y="Percent", x = "", fill="") + ylab("") + ggtitle("") + theme(plot.title = element_text(size = 12)) + coord_flip() + scale_y_continuous(labels=scales::percent) +
  scale_fill_grey(
    labels = c("Very good", "", "", "", "Very poor"),
    start = 0.2,
    end = 1,
    na.value = "red",
    aesthetics = "fill"
  ) +
  ggsave("figures/chap01/wagerankings.png",
         width = 20, height = 10, units = "cm")


## Self-employed obstacles

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_1) %>% 
  table()

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_2) %>% 
  table() %>% 
  cbind("no access to electricity" = df, "competition" = .)

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_3) %>% 
  table() %>% 
  cbind(df, "no access to credit" = .)

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_4) %>% 
  table() %>% 
  cbind(df, "no qualified workers" = .)

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_5) %>% 
  table() %>% 
  cbind(df, "no apprentices" = .)

df <- ys %>% 
  filter(status == "Self-Employed") %>% 
  dplyr::select(YS9_29_6) %>% 
  table() %>% 
  cbind(df, "bureaucracy" = .) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "question") %>% 
  gather(key = score, value = value, -question) %>%
  group_by(question) %>%
  mutate(Percentage = ifelse(value > 0, value/sum(value), 0))

df %>%
  ggplot(aes(x=question,y=Percentage,fill=forcats::fct_rev(score))) +
  geom_bar(stat = "identity", position="fill", color="black", width=.9) + 
  theme_minimal() +
  labs(y="Percent", x = "", fill="") + ylab("") + ggtitle("") + theme(plot.title = element_text(size = 12)) + coord_flip() + scale_y_continuous(labels=scales::percent) +
  scale_fill_grey(
    labels = c("Least pressing", "", "", "", "", "Most pressing"),
    start = .2,
    end = 1,
    na.value = "red",
    aesthetics = "fill"
  ) + ggsave("figures/chap01/serankings.png",
       width = 20, height = 10, units = "cm")




