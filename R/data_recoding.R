data_recoding = function(df,df_bbl, sep = ','){

  tmp_bbl = df_bbl %>%
    dplyr::mutate(id = 1:n()) %>%
    separate_rows(Choices, sep = " \\| ") %>%
    mutate(new_choices = str_split_fixed(Choices,sep, 2))


  tmp_bbl$Number = trimws(tmp_bbl$new_choices[,1])
  tmp_bbl$Value  = tmp_bbl$new_choices[,2]


  tmp_bbl = tmp_bbl %>% filter(!is.na(Choices))

  tmp_bbl = tmp_bbl %>%

    group_by(id) %>%

    dplyr::summarise(Variable...Field.name = unique(Variable...Field.name), across(c(Number, Value), list))


  for (i in tmp_bbl$Variable...Field.name[1:length(tmp_bbl$Variable...Field.name)]){

    before = unlist(tmp_bbl[tmp_bbl$Variable...Field.name == i,'Number'])
    after = unlist(tmp_bbl[tmp_bbl$Variable...Field.name == i,'Value'])

    d = setNames(after, before)

    f = function(x) str_split(x, ", ", simplify = TRUE) %>% {d[.]} %>% paste0(collapse = " | ")

    a = df %>%
      pull(i)

    tmp = map_chr(a, f)

    df[,i] = tmp

  }

  df = as.data.frame(apply(df, 2, function(x) gsub("\\|NA|NA", NA, x)))

  tmp = df %>%

    type.convert(as.is = TRUE) %>%

    select(where(is.numeric))

  df[,names(tmp)] = tmp

  return(df)

}
