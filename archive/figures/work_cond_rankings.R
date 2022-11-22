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

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>% 
  dplyr::select(YS4_40_1) %>% 
  table()

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>% 
  dplyr::select(YS4_40_2) %>% 
  table() %>% 
  cbind("physical safety" = df, "knowledge of master" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>% 
  dplyr::select(YS4_40_3) %>% 
  table() %>% 
  cbind(df, "treatment by master" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>%  
  dplyr::select(YS4_40_5) %>% 
  table() %>% 
  cbind(df, "working hours" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>% 
  dplyr::select(YS4_40_6) %>% 
  table() %>% 
  cbind(df, "quality of training" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>%  
  dplyr::select(YS4_40_7) %>% 
  table() %>% 
  cbind(df, "equipment and machinery" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Apprentice") %>% 
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

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
  dplyr::select(YS8_32_1) %>% 
  table()

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
  dplyr::select(YS8_32_4) %>% 
  table() %>% 
  cbind("physical safety" = df, "salary" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
  dplyr::select(YS8_32_5) %>% 
  table() %>% 
  cbind(df, "working hours" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
  dplyr::select(YS8_32_6) %>% 
  table() %>% 
  cbind(df, "quality of training" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
  dplyr::select(YS8_32_8) %>% 
  table() %>% 
  cbind(df, "coworkers" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Employed") %>% 
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


df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
  dplyr::select(YS9_29_1) %>% 
  table()

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
  dplyr::select(YS9_29_2) %>% 
  table() %>% 
  cbind("no access to electricity" = df, "competition" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
  dplyr::select(YS9_29_3) %>% 
  table() %>% 
  cbind(df, "no access to credit" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
  dplyr::select(YS9_29_4) %>% 
  table() %>% 
  cbind(df, "no qualified workers" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
  dplyr::select(YS9_29_5) %>% 
  table() %>% 
  cbind(df, "no apprentices" = .)

df <- ys_panel_labels %>% 
  filter(wave == "YS",
         status == "Self-Employed") %>% 
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



