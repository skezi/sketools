data_engine <- function(df, vars) {

    if (length(vars) == 1) {

    data <- df %>%
      pull(vars) %>%
      as.factor() %>%
      fct_count() %>%
      dplyr::rename(group = f, value = n) %>%
      mutate(freq = value / sum(value) * 100) %>%
      na.omit()

    } else {

    df_tmp <- organize_multi_responses(df, vars)

    data <- df_tmp %>%
      select(contains(vars)) %>%
      gather(key = "key", value = "value") %>%
      group_by(key,value) %>%
      plyr::count() %>%
      ungroup() %>%
      group_by(key) %>%
      mutate(n = freq / sum(freq) * 100)
  }

  return(data)

}
