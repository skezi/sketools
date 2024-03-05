\name{data_engine2}
\alias{data_engine2}
\title{
  Data Engineering Function
}
\description{
  The \code{data_engine2} function performs data engineering tasks on a dataframe. When provided with a single variable (\code{vars}), it computes frequencies and percentages for each unique value. When given multiple variables, it organizes the data using \code{multi\_response\_organizer} and computes frequencies and percentages for each combination of values.
}
\usage{
  data_engine2(df, vars)
}
\arguments{
  \item{df}{
    The input dataframe.
  }
  \item{vars}{
    A vector of variable names for analysis.
  }
}
\details{
  When a single variable is provided, the function computes frequencies and percentages for each unique value. For multiple variables, it organizes the data and calculates frequencies and percentages for each combination of values.
}
\value{
  Returns a dataframe with frequencies, percentages, and organized values.
}
\examples{
  # Example usage:
  df_single_var <- data.frame(var = c("A", "B", "A", "C", "B"))
  data_engine2(df_single_var, "var")

  df_multi_var <- data.frame(var1 = c("A, B", "B, C", "A, C"), var2 = c("X, Y", "Y, Z", "X, Z"))
  data_engine2(df_multi_var, c("var1", "var2"))
}
