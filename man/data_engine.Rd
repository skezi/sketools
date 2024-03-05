\name{data_engine}
\alias{data_engine}
\title{
  Data Engine Function
}
\description{
  A function to perform basic data engineering tasks on a dataframe. It can either calculate frequencies for a single categorical variable or handle multiple response variables and organize the data for further analysis.
}
\usage{
  data_engine(df, vars)
}
\arguments{
  \item{df}{
    The dataframe containing the variables for analysis.
  }
  \item{vars}{
    Either the name of a single categorical variable or a vector of multiple response variables.
  }
}
\details{
  The function calculates frequencies and percentages for either a single categorical variable or multiple response variables in the provided dataframe.
}
\value{
  Returns a dataframe with calculated frequencies and percentages.
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  [Your Name]
}
\note{
  %%  ~~further notes~~
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{dplyr}}, \code{\link{forcats}}
}
\examples{
  # Example usage for a single variable:
  df_single <- data.frame(category = c("A", "B", "A", "C", "B"))
  data_engine(df_single, "category")

  # Example usage for multiple response variables:
  df_multi <- data.frame(var1 = c("A", "B", "A", "C", "B"),
                          var2 = c("X", "Y", "Z", "X", "Y"))
  data_engine(df_multi, c("var1", "var2"))
}
