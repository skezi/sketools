\name{diag_venn_maker}
\alias{diag_venn_maker}
\title{
  Venn Diagram Maker Function
}
\description{
  A function to create a Venn diagram using the \code{ggVennDiagram} package. It takes a dataframe and a variable containing comma-separated values, then generates a Venn diagram illustrating the intersections of unique values.
}
\usage{
  diag_venn_maker(df, var)
}
\arguments{
  \item{df}{
    The dataframe containing the data.
  }
  \item{var}{
    The name of the variable containing comma-separated values. Values should be separated by "|".
  }
}
\details{
  The function extracts unique values from the specified variable and creates a Venn diagram illustrating the intersections of these values. Values in the variable should be separated by "|".
}
\value{
  Returns a Venn diagram using the \code{ggVennDiagram} package.
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
  \code{\link{ggVennDiagram}}, \code{\link{stringr}}
}
\examples{
  \dontrun{
    # Example usage:
    df_venn <- data.frame(id = 1:5, categories = c("A | B", "B | C", "A | C", "A | B | C", "B | C"))
    diag_venn_maker(df_venn, "categories")
  }
}
