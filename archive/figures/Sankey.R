# Package names
packages <- c("haven", "tidyverse", "tidygraph", "networkD3", "webshot", "htmlwidgets")
#detach(package:here, unload=TRUE)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

mydata <- read_sav("./data/youth_survey/youth_survey_merged.sav", user_na = TRUE) %>% 
  mutate(status = ifelse(YS1_2 == 1 | YS4_1 == 1, "Apprentice",
                         ifelse((YS8_1 == 1 | YS8_2 == 1) & YS8_4 != 4, "Employed",
                                ifelse((YS8_1 == 1 | YS8_2 == 1) & YS8_4 == 4, "Self-Employed",
                                       ifelse(YS7_1 == 1, "In School",
                                              ifelse(YS4_1 == 0 & YS7_1 != 1 & YS8_2 == 0, "NEET",NA)))))) %>% 
  filter(baseline_age >=20 & baseline_age <= 29,
         is.na(YS1_6) | (YS1_6 != 1 & YS1_6 != 3 & YS1_6 != 4 & YS1_6 != 6 & YS1_6 != 7 & YS1_6 != 8 & YS1_6 != 9 ),
         cqp == 0)
mydata_labels <- haven::as_factor(mydata)

## replace missing data with observations from follow-up 2
mydata_labels$YS6_16_1 %>% 
  coalesce(mydata_labels$YS6_16_1, mydata_labels$F2U3_0a_1)
mydata_labels$YS6_16_2 %>% 
  coalesce(mydata_labels$YS6_16_2, mydata_labels$F2U3_0a_2)
mydata_labels$YS6_16_3%>% 
  coalesce(mydata_labels$YS6_16_3, mydata_labels$F2U3_0a_3)
mydata_labels$YS6_16_4 %>% 
  coalesce(mydata_labels$YS6_16_4, mydata_labels$F2U3_0a_4)
mydata_labels$YS6_16_5 %>% 
  coalesce(mydata_labels$YS6_16_5, mydata_labels$F2U3_0a_5)
mydata_labels$YS6_16_6 %>% 
  coalesce(mydata_labels$YS6_16_6, mydata_labels$F2U3_0a_6)
mydata_labels$YS6_16_7 %>% 
  coalesce(mydata_labels$YS6_16_7, mydata_labels$F2U3_0a_7)

## create activities dataframe, clean labels

sankeyFunc <- function(mydata_labels) {
  
  activities <- mydata_labels %>% 
    tbl_df() %>%
    dplyr::select(starts_with("YS6_16")) %>%
    filter_all(all_vars(. != "Don't want to say")) %>% 
    mutate_all(., funs(recode(., "Apprenticeship (formal)" = "Apprenticeship",
                              "Apprenticeship (traditional)" = "Apprenticeship",
                              "None (stay at home)" = "NEET")))
  
  activities$YS6_16_1 = paste0(activities$YS6_16_1, ' (2019)')
  activities$YS6_16_2 = paste0(activities$YS6_16_2, ' (2018)')
  activities$YS6_16_3 = paste0(activities$YS6_16_3, ' (2017)')
  activities$YS6_16_4 = paste0(activities$YS6_16_4, ' (2016)')
  activities$YS6_16_5 = paste0(activities$YS6_16_5, ' (2015)')
  activities$YS6_16_6 = paste0(activities$YS6_16_6, ' (2014)')
  activities$YS6_16_7 = paste0(activities$YS6_16_7, ' (2013)')
  
  vec14 <- activities[6:7] %>% 
    rename("to" = YS6_16_6, 
           "from" = YS6_16_7) %>% 
    mutate(year = 2014)
  
  names(vec14) <- make.names(names(vec14))
  vec14 <- vec14 %>%
    group_by_(.dots=names(vec14)) %>%
    summarise(Weight = n())
  
  vec15 <- activities[5:6] %>% 
    rename("to" = YS6_16_5, 
           "from" = YS6_16_6) %>% 
    mutate(year = 2015)
  
  names(vec15) <- make.names(names(vec15))
  vec15 <- vec15 %>%
    group_by_(.dots=names(vec15)) %>%
    summarise(Weight = n())
  
  vec16 <- activities[4:5] %>% 
    rename("to" = YS6_16_4, 
           "from" = YS6_16_5) %>% 
    mutate(year = 2016)
  
  names(vec16) <- make.names(names(vec16))
  vec16 <- vec16 %>%
    group_by_(.dots=names(vec16)) %>%
    summarise(Weight = n())
  
  vec17 <- activities[3:4] %>% 
    rename("to" = YS6_16_3, 
           "from" = YS6_16_4) %>% 
    mutate(year = 2017)
  
  names(vec17) <- make.names(names(vec17))
  vec17 <- vec17 %>%
    group_by_(.dots=names(vec17)) %>%
    summarise(Weight = n())
  
  vec18 <- activities[2:3] %>% 
    rename("to" = YS6_16_2, 
           "from" = YS6_16_3) %>% 
    mutate(year = 2018)
  
  names(vec18) <- make.names(names(vec18))
  vec18 <- vec18 %>%
    group_by_(.dots=names(vec18)) %>%
    summarise(Weight = n())
  
  vec19 <- activities[1:2] %>% 
    rename("to" = YS6_16_1, 
           "from" = YS6_16_2) %>% 
    mutate(year = 2019)
  
  names(vec19) <- make.names(names(vec19))
  vec19 <- vec19 %>%
    group_by_(.dots=names(vec19)) %>%
    summarise(Weight = n())
  
  vec_full <- full_join(vec14, vec15) %>% 
    full_join(., vec16) %>% 
    full_join(., vec17) %>% 
    full_join(., vec18) %>% 
    full_join(., vec19)
  ##mutate(colour = to)
  
  to <- vec_full[1]
  from <- vec_full[2]
  weight <- vec_full[3]
  year <- vec_full[4]
  
  # create tidygraph from data frame
  ig <- tidygraph::as_tbl_graph(vec_full)
  
  # extract node data and reduce ids by 1 (required for D3 plotting)
  nodes <- as_tibble(ig) %>% 
    rowid_to_column("id") %>% 
    mutate(id = id -1) %>% 
    as.data.frame
  
  # do the same for the edges
  edges <- ig %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    mutate(from = from - 1, to = to - 1) %>% 
    as.data.frame
  
#  my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'
  
  
  
  sn <- sankeyNetwork(Links = edges, Nodes = nodes, Source = "from", Target = "to", 
                      NodeID = "name", Value = "Weight", fontSize = 8, fontFamily = "sans-serif", width = "100%",
                      height = 700, sinksRight = FALSE)
  onRender(
    sn,
    '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 20);
  }
  '
  )
  saveNetwork(sn, "Sankey.html", selfcontained = TRUE)
}

sankeyNetwork(Links = edges, Nodes = nodes, Source = "from", Target = "to", 
                    NodeID = "name", Value = "Weight", fontSize = 12, fontFamily = "sans-serif", width = "80%",
                    height = 700, sinksRight = FALSE)

