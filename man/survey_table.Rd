\name{survey_table}
\alias{survey_table}
\title{
  Survey Table Function
}
\description{
  A function to generate a summary table for survey data. It provides information on the number of participants, pages, questions, and mandatory questions, as well as those respecting inclusion criteria.
}
\usage{
  survey_table(df, df_bbl)
}
\arguments{
  \item{df}{
    The main dataframe containing survey participant data.
  }
  \item{df_bbl}{
    A dataframe containing metadata about the survey, including form names, required fields, etc.
  }
}
\details{
  The function creates a tibble summarizing various aspects of the survey, such as the number of participants, pages, questions, mandatory questions, and those respecting inclusion criteria.
}
\value{
  Returns a tibble containing survey summary information.
}
\author{
  [Your Name]
}
\note{
  Ensure that the necessary packages (e.g., `tibble`) are installed and loaded before using this function.
}
\examples{
  # Example usage:
  survey_table(df, df_bbl)
}
