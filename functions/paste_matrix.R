paste_matrix <- function(df, wave_from, wave_to) {
  x <- df %>% tbl_cross(row = {{wave_from}}, col = {{wave_to}}, statistic = "{n}\n({p}%)", percent = "row", missing = "no")
  y <- df %>% tbl_cross(row = {{wave_from}}, col = {{wave_to}}, statistic = "[{p}%]", percent = "column", missing = "no")
  z1 <- x$table_body %>% as.matrix()
  z2 <- y$table_body %>% as.matrix()
  x$table_body[2:7,6:11] <- matrix(paste0(z1[2:7,6:11], " \n", z2[2:7,6:11]), 6)
  return(x)
}