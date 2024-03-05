library(tidyr)
library(dplyr)
library(ComplexUpset)

upstplot_maker <- function(df, vars, sep) {

  tmp_df <- df[, c("id", vars)] %>%
    separate_rows(vars, sep = sep) %>%
    table

  tmp_df <-

    as.data.frame.matrix(tmp_df) %>%

    mutate_all(function(x) as.logical(as.numeric(x)))


  imge <- upset(tmp_df, names(tmp_df), width_ratio = 0.1)

  return(imge)
}
