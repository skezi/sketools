\name{wordcloud_maker}
\alias{wordcloud_maker}
\title{
  Wordcloud Maker Function
}
\description{
  A function to generate a wordcloud from a text variable in a dataframe. It preprocesses the text, creates a term-document matrix, and generates a wordcloud using the specified parameters.
}
\usage{
  wordcloud_maker(df, var)
}
\arguments{
  \item{df}{
    The dataframe containing the text variable for creating the wordcloud.
  }
  \item{var}{
    The name of the text variable in the dataframe.
  }
}
\details{
  The function preprocesses the text, creates a term-document matrix, and generates a wordcloud with specified parameters, saving the result as a PNG file in the "plot" directory.
}
\value{
  Returns the wordcloud plot.
}
\author{
  [Your Name]
}
\note{
  Ensure that the necessary packages (e.g., `tm`, `wordcloud`, `slam`) are installed and loaded before using this function.
}
\examples{
  # Example usage:
  df <- data.frame(text = c("This is a sample sentence.", "Another example here."))
  wordcloud_maker(df, "text")
}
