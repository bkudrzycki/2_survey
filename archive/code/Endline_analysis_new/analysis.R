# Package names
packages <- c("haven", "tidyverse", "readstata13")
#detach(package:here, unload=TRUE)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

#load survey
endline <- read_sav("/Users/kudrzycb/Desktop/Endline_analysis_new/endline_merged.sav", user_na = TRUE) %>% 
  filter(YS1_2 == 1)
endline_labels <- haven::as_factor(endline) %>% filter(YS1_2=="Yes")

#apprenticeship completion rate
df <- endline %>% 
  select(YS3_13, YE3_3) %>% 
  pivot_longer(cols = YS3_13:YE3_3, names_to = "Year")

df$Year <- recode(df$Year, "YS3_13" = "2018", "YE3_3" = "2021")
df$value <- haven::as_factor(df$value)
df$value <- recode(df$value, "0" = "No", "1" = "Yes")
  
df %>% 
  ggplot(aes(x = value, fill = Year)) +
  geom_bar(aes(y = (2*..count..)/sum(..count..)), position = "dodge") +
  theme_minimal() +
  xlab("Completed Apprenticeship") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))
    
##skills questions

#YS5_2
df <- endline %>% 
  select(YE4_2, YS5_2, SELECTED) %>% 
  pivot_longer(cols = YE4_2:YS5_2, names_to = "Year")

df$Year <- recode(df$Year, "YS5_2" = "2018", "YE4_2" = "2021")
df$correct <- ifelse(df$value == 1, 1, 0)

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Identify a Folding Machine") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Identify a Folding Machine") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1))

#YS5_4
df <- endline %>% 
  select(YE4_4, YS5_4, SELECTED) %>% 
  pivot_longer(cols = YE4_4:YS5_4, names_to = "Year")

df$Year <- recode(df$Year, "YS5_4" = "2018", "YE4_4" = "2021")
df$correct <- ifelse(df$value == 2, "Correct", "Incorrect")

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Define a Right Angle") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Define a Right Angle") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1))

#YS5_9
df <- endline %>% 
  select(YE4_9, YS5_9, SELECTED) %>% 
  pivot_longer(cols = YE4_9:YS5_9, names_to = "Year")

df$Year <- recode(df$Year, "YS5_9" = "2018", "YE4_9" = "2021")
df$correct <- ifelse(df$value == 4, "Correct", "Incorrect")

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Define a Manchon") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Define a Manchon") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1))

#YS5_15
df <- endline %>% 
  select(YE4_15, YS5_15, SELECTED) %>% 
  pivot_longer(cols = YE4_15:YS5_15, names_to = "Year")

df$Year <- recode(df$Year, "YS5_15" = "2018", "YE4_15" = "2021")
df$correct <- ifelse(df$value == 1, "Correct", "Incorrect")

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("What is a Plumb Line Used For?") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("What is a Plumb Line Used For?") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1))

#YS5_18
df <- endline %>% 
  select(YE4_18, YS5_18, SELECTED) %>% 
  pivot_longer(cols = YE4_18:YS5_18, names_to = "Year")

df$Year <- recode(df$Year, "YS5_18" = "2018", "YE4_18" = "2021")
df$correct <- ifelse(df$value == 2, "Correct", "Incorrect")

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Plaster Mix Proportions") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Plaster Mix Proportions") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1))

#YS5_20
df <- endline %>% 
  select(YE4_20, YS5_20, SELECTED) %>% 
  pivot_longer(cols = YE4_20:YS5_20, names_to = "Year")

df$Year <- recode(df$Year, "YS5_20" = "2018", "YE4_20" = "2021")
df$correct <- ifelse(df$value == 3, "Correct", "Incorrect")

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct) %>%
  count() %>%
  group_by(Year) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = correct, y = percentage, fill = Year)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("What is a Voltmeter used for?") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 1))

df %>% 
  drop_na(correct) %>% 
  group_by(Year, correct, SELECTED) %>%
  count() %>%
  group_by(Year, SELECTED) %>% 
  mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(x = Year, y = percentage, fill = SELECTED)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  xlab("Plaster Mix Proportions") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_discrete("", labels=c("CQP", "Non-CQP"))


