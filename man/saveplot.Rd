\name{saveplot}
\alias{saveplot}
\title{
  Save Plot Function
}
\description{
  A function to generate and save a plot based on the variable type in a dataframe. It utilizes the `decision_maker2` function to create a plot and then saves it as a PNG file.
}
\usage{
  saveplot(df, df_bbl, var)
}
\arguments{
  \item{df}{
    The main dataframe containing the variables for analysis.
  }
  \item{df_bbl}{
    A dataframe containing metadata about the variables, including their types and characteristics.
  }
  \item{var}{
    The name of the variable for which to generate and save the plot.
  }
}
\details{
  The function generates a plot using `decision_maker2`, and based on the variable type, it saves the plot as a PNG file in the "plot" directory.
}
\value{
  Saves a plot as a PNG file.
}
\author{
  [Your Name]
}
\note{
  Ensure that the necessary packages (e.g., `ggplot2`, `ggsave`) are installed and loaded before using this function.
}
\examples{
  # Example usage:
  df <- data.frame(var1 = c("A", "B", "A", "C", "B"),
                   var2 = c("X", "Y", "Z", "X", "Y"))
  df_bbl <- data.frame(Field.name = c("var1", "var2"),
                       Field.type = c("dropdown", "checkbox"))
  saveplot(df, df_bbl, "var1")
}
