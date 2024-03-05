\name{piechart_maker}
\alias{piechart_maker}
\title{
  Piechart Maker Function
}
\description{
  A function to create a pie chart using ggplot2. It takes a dataframe and a categorical variable, then generates a pie chart with the option to display either counts or percentages.
}
\usage{
  piechart_maker(df, var, pourcentage = TRUE, palette_skezi = p_skezi,
                 xlab_title = NULL, ylab_title = NULL, title = NULL)
}
\arguments{
  \item{df}{
    The dataframe containing the categorical variable.
  }
  \item{var}{
    The name of the categorical variable.
  }
  \item{pourcentage}{
    Logical. If TRUE, the pie chart displays percentages; if FALSE, it displays counts.
  }
  \item{palette_skezi}{
    Color palette for the pie chart. Provide a vector of color values.
  }
  \item{xlab_title, ylab_title}{
    Optional. Titles for the x-axis and y-axis.
  }
  \item{title}{
    Optional. Title for the pie chart.
  }
}
\details{
  The function generates a pie chart with either counts or percentages for the specified categorical variable using ggplot2.
}
\value{
  Returns a ggplot object representing the pie chart.
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
  \code{\link{ggplot2}}
}
\examples{
  # Example usage:
  df_piechart <- data.frame(category = c("A", "B", "A", "C", "B"))
  piechart_maker(df_piechart, "category", pourcentage = TRUE)
}
