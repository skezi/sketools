\name{upstplot_maker}
\alias{upstplot_maker}
\title{
  Upset Plot Maker Function
}
\description{
  A function to create an upset plot using the \code{ComplexUpset} package. It takes a dataframe (\code{df}) and a vector of variable names (\code{vars}), then generates an upset plot to visualize the intersections of set elements.
}
\usage{
  upstplot_maker(df, vars)
}
\arguments{
  \item{df}{
    The dataframe (\code{df}) containing the set variables.
  }
  \item{vars}{
    A vector of variable names (\code{vars}) representing the sets for the upset plot.
  }
}
\details{
  The function generates an upset plot to visualize the intersections of set elements for the specified variables using the \code{ComplexUpset} package.
}
\value{
  Returns an upset plot using the \code{ComplexUpset} package.
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
  \code{\link{ComplexUpset}}, \code{\link{tidyr}}, \code{\link{dplyr}}
}
\examples{
  # Example usage:
  df_upset <- data.frame(id = 1:5, sets = c("A, B", "B, C", "A, C", "A, B, C", "B, C"))
  upstplot_maker(df_upset, "sets")
}
