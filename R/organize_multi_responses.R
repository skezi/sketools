organize_multi_responses <- function(data_frame, variables, separator = ", ") {

  result_data <- as.data.frame(
    sapply(data_frame[variables], function(column) {
      sapply(strsplit(as.character(column), separator, fixed = TRUE), function(response) {
        sorted_unique_response <- sort(unique(response))
        paste(sorted_unique_response, collapse = ", ")
      })
    })
  )

  result_data[result_data == ""] <- NA

  return(result_data)
}
