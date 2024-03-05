diag_venn_maker = function(df, var) {

  vals = na.omit(unique(unlist(str_split(df[, var], "\\| "))))

  df$id <- seq_len(nrow(df))

  kk = df[, c("id", var)] %>%

    separate_rows(var, sep = "\\| ") %>%

    table %>%

    data.frame %>%

    filter(Freq > 0)

  x_names <- paste0("id_", tolower(vals))
  x <- vector("list", length = length(vals))

  for (i in seq_along(vals)) {
    x[[i]] <- as.numeric(kk[kk[, var] == vals[i], "id"])
  }

  g = ggVennDiagram(
    x,
    set_siz = 2,
    label_size = 2,
    category.names = vals
  )

  return(g)

}
