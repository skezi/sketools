survey_table <- function(df, df_bbl) {
  num_participant <- "Nombre de participants"
  num_pages <- "Nombre de pages"
  num_quest <- "Nombre de questions"
  num_mand_quest <- "Dont obligatoires"
  num_include_part <- "Respectant l'inclusion"

  survey_df <- tibble(
    parameter = c(num_participant, num_include_part, num_pages, num_quest, num_mand_quest),
    value = c(nrow(df), NA, length(unique(df_bbl$Form.name)), nrow(df_bbl), nrow(df_bbl[df_bbl$Required.field == "yes", ]))
  )

  return(survey_df)
}
