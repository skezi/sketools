\name{vsub}
\alias{vsub}
\title{
  Variable Substitution Function
}
\description{
  A function to perform variable-wise substring replacement in a dataframe. It replaces specified substrings in the selected variables with a specified replacement string.
}
\usage{
  vsub(df, vars, before, after)
}
\arguments{
  \item{df}{
    The dataframe containing the variables for substring replacement.
  }
  \item{vars}{
    The names of variables (columns) in the dataframe where substring replacement will be applied.
  }
  \item{before}{
    The substring to be replaced.
  }
  \item{after}{
    The replacement string.
  }
}
\details{
  The function performs variable-wise substring replacement for the specified variables in the dataframe.
}
\value{
  Returns the modified dataframe with substring replacements.
}
\author{
  [Your Name]
}
\note{
  Ensure that the necessary packages (e.g., `stringi`) are installed and loaded before using this function.
}
\examples{
  # Example usage:
  
  d <- data.frame(var1 = c("apple", "banana", "orange"),
                   var2 = c("red apple", "yellow banana", "orange fruit"))
                   
  vsub(d, c("var1", "var2"), c("orange","apple"), c("fruit","color"))
}
