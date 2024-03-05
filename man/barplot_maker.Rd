\name{barplot_maker}
\alias{barplot_maker}
\title{Create Bar Plots with Text Labels}
\description{
  A function for creating bar plots with text labels based on input data. The function uses ggplot2 to generate visually appealing bar plots with customizable features.
}
\usage{
  barplot_maker(
    df,
    var,
    Pourcentage = FALSE,
    flip = TRUE,
    xlab_title = NULL,
    ylab_title = NULL,
    title = NULL,
    palette_skezi = p_skezi
  )
}
\arguments{
  \item{df}{
    The dataframe containing the data for the bar plot.
  }
  \item{var}{
    A character string specifying the variable in the dataframe to be used for the bar plot.
  }
  \item{Pourcentage}{
    A logical value indicating whether to display the values as percentages. Default is \code{FALSE}.
  }
  \item{flip}{
    A logical value indicating whether to flip the orientation of the bar plot. Default is \code{TRUE}.
  }
  \item{xlab_title}{
    Label for the x-axis. Default is \code{NULL}.
  }
  \item{ylab_title}{
    Label for the y-axis. Default is \code{NULL}.
  }
  \item{title}{
    Title for the plot. Default is \code{NULL}.
  }
  \item{palette_skezi}{
    Color palette for the bars. Default is \code{p_skezi}.
  }
}
\details{
  The function takes a dataframe (\code{df}) and a variable (\code{var}) within the dataframe to create a bar plot. Text labels are added to each bar, and the orientation of the plot can be flipped if desired.
}
\value{
  Returns a ggplot object representing the bar plot with text labels.
}
\examples{
  \dontrun{
    # Example usage:
    barplot_maker(
      df = your_dataframe,
      var = "your_variable",
      Pourcentage = TRUE,
      flip = FALSE,
      xlab_title = "Label for X-axis",
      ylab_title = "Label for Y-axis",
      title = "Bar Plot with Text Labels"
    )
  }
}
\seealso{
  Other functions for creating visualizations in ggplot2.
}
\author{
  [Your Name]
}
\note{
  Additional notes or important information about the function.
}
