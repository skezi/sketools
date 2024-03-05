\name{histogramme_maker}
\alias{histogramme_maker}
\title{How to make a histogram with SKEZI}

\description{
  Create a histogram with customizable features using the SKEZI theme.
}

\usage{
  histogramme_maker(
    data,
    var,
    x_lab = NULL,
    y_lab = NULL,
    clr = "#000000",
    clr_fill = p_skezi[1],
    palette = p_skezi,
    ltype = "solid",
    density_test,
    mean_test,
    title_lab = NULL
  )
}

\arguments{
  \item{data}{
    A data.frame containing the dataset.
  }
  \item{var}{
    The variable for which the histogram is to be plotted.
  }
  \item{x_lab}{
    Text for the x-axis label.
  }
  \item{y_lab}{
    Text for the y-axis label.
  }
  \item{clr}{
    Color to personalize the graph.
  }
  \item{clr_fill}{
    Entirely colorize the graph.
  }
  \item{palette}{
    A vector of colors for plot elements.
  }
  \item{ltype}{
    Type of line for the histogram border.
  }
  \item{density_test}{
    Draws an estimate of the density of values.
  }
  \item{mean_test}{
    Logical. If TRUE, adds a dashed line indicating the mean of the variable.
  }
  \item{title_lab}{
    Text for the title.
  }
}

\details{
  This function creates a histogram with customizable aesthetics and features. Users can specify the dataset, variable of interest, axis labels, color options, line types, and more. Additionally, options for adding a density plot and a mean indicator provide enhanced insights into the distribution.
}

\author{
  Clementine LE RIDOU
}

\examples{
  # Example usage
  data <- data.frame(variable = rnorm(100))
  histogramme_maker(data, "variable", "X-axis Label", "Y-axis Label", "#333333", "#66CC66", c("#66CC66", "#FF9933"), "dashed", TRUE, TRUE, "Your Plot Title")
}

\seealso{
  \code{\link[ggplot2]{geom_histogram}},
  \code{\link[ggplot2]{geom_density}},
  \code{\link[ggplot2]{theme}},
  \code{\link[ggplot2]{aes_string}}
}
