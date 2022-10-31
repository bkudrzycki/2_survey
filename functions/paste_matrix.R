paste_matrix <- function(wave_from, wave_to) {
  df <- ys_panel %>% select(IDYouth, wave, status) %>% zap_labels() %>% 
    mutate(status = recode(status,
                           `1` = "School",
                           `2` = "NEET",
                           `3` = "Self-\nEmployed",
                           `4` = "Employed",
                           `5` = "Apprentice")) %>% 
    pivot_wider(id_cols = IDYouth, names_from = wave, names_prefix = "wave_", values_from = status) %>% 
    rename("Baseline" = wave_0, "Follow-up 1" = wave_1, "Follow-up 2" = wave_2, "Follow-up 3" = wave_3, "Endline" = wave_4)
  
  x <- df %>% 
    tbl_cross(row = {{wave_from}}, col = {{wave_to}}, label = NULL, statistic = "{n}\n({p}%)", percent = "row", missing = "no") %>% 
    modify_spanning_header(all_stat_cols() ~ NA)
  y <- 
    df %>% tbl_cross(row = {{wave_from}}, col = {{wave_to}}, label = NULL, statistic = "[{p}%]", percent = "column", missing = "no") %>% 
    modify_spanning_header(all_stat_cols() ~ NA)
  z1 <- x$table_body %>% as.matrix()
  z2 <- y$table_body %>% as.matrix()
  
  x$table_body[2:7,6:11] <- matrix(paste0(z1[2:7,6:11], "\n", z2[2:7,6:11]), 6)
  x$table_body <- x$table_body[2:7,]
  
  tbl <- x %>% as_flex_table() %>% 
    flextable::font(fontname = "Times New Roman", part = "all") %>% 
    flextable::width(width = .8) %>%
    add_header_row(
      values = c("Baseline", "Follow-up 1"),
      colwidths = c(1, 6),
    )
  
  return(tbl)
}


