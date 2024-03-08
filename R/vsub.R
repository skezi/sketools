vsub <- function(df, vars, before, after) {
  
  for (i in colnames(df[, vars])){
    
    df[, i] <- stri_replace_all_regex(df[, i],
                                      pattern = before,
                                      replacement = after,
                                      vectorize = FALSE)
  }
  
  
  return(df)

}