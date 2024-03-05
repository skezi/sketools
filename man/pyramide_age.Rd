\name{pyramid_age_maker}
\alias{pyramid_age_maker}
\title{
  Pyramid Age Maker Function
}
\description{
  A function to create a pyramid plot illustrating the age distribution between two sexes in a dataset. It takes a dataframe containing age and sex information and produces a pyramid plot using ggplot2.
}
\usage{
  pyramid_age_maker(df, var, axis_col = "black", axis_size = 12, switch_col = FALSE, title = "Pyramide d'âge")
}
\arguments{
  \item{df}{
    The dataframe containing age and sex information.
  }
  \item{var}{
    The variable representing birth years.
  }
  \item{axis_col}{
    Color of the axis numbers.
  }
  \item{axis_size}{
    Size of the axis legends.
  }
  \item{switch_col}{
    Logical, switch the color of the second barplot if TRUE.
  }
  \item{title}{
    Title for the pyramid plot.
  }
}
\details{
  The function calculates age groups and creates a pyramid plot using ggplot2, illustrating the age distribution between the two most represented sexes in the dataset.
}
\value{
  Returns a ggplot object representing the pyramid plot.
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
  \code{\link{ggplot2}}, \code{\link{dplyr}}, \code{\link{forcats}}
}
\examples{
  # Example usage:
  df <- data.frame(age = c(25, 35, 45, 55, 65, 75),
                   inf_sexe = c("Male", "Female", "Male", "Female", "Male", "Female"))

  pyramid_age_maker(df, "age")
}
