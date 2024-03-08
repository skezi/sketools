\name{decision_maker2}
\alias{decision_maker2}
\title{
  Decision Maker Function
}
\description{
  A versatile function for creating different types of plots based on the variable type in a dataframe. It dynamically selects the appropriate plot type for a given variable, considering its type and characteristics.
}
\usage{
  decision_maker2(df, df_bbl, var)
}
\arguments{
  \item{df}{
    The main dataframe containing the variables for analysis.
  }
  \item{df_bbl}{
    A dataframe containing metadata about the variables, including their types and characteristics.
  }
  \item{var}{
    The name of the variable for which to generate the plot.
  }
}
\details{
  The function dynamically selects a plot type based on the variable's type and characteristics, creating a variety of plots, including pie charts, bar plots, histograms, and word clouds.
}
\value{
  Returns a plot object based on the type of the variable.
}
\author{
  [Your Name]
}
\note{
  Ensure that the necessary plotting functions (e.g., `piechart_maker`, `barplot_maker`, etc.) are available before using this function.
}
\seealso{
  \code{\link{piechart_maker}}, \code{\link{barplot_maker}}, \code{\link{histogramme_maker}}, \code{\link{wordcloud_maker}}, \code{\link{barplot_maker2}}, \code{\link{compute_interval_labels}}, \code{\link{upstPlot_maker}}, \code{\link{plot_theme}}, \code{\link{diag_venn_maker}}
}
\examples{
  # Example usage:
  decision_maker2(df, df_bbl, "variable_name")
}
