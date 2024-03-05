\name{organize_multi_responses}
\alias{organize_multi_responses}
\title{
  Organize Multiple Responses Function
}
\description{
  The \code{organize_multi_responses} function efficiently organizes and sorts multiple response variables within a dataframe. It takes a dataframe (\code{data_frame}) and a vector of variable names (\code{variables}) as input. Each specified variable may contain responses separated by a specified separator (default is ", "). The function processes each specified variable by extracting unique values from comma-separated responses, sorts them, and then combines them into a single, organized response. The output is a new dataframe with the organized and sorted multiple response variables.
}
\usage{
  organize_multi_responses(data_frame, variables, separator = ", ")
}
\arguments{
  \item{data\_frame}{
    The input dataframe containing the multiple response variables.
  }
  \item{variables}{
    A vector of variable names with multiple response values.
  }
  \item{separator}{
    The separator used to split the response values (default is ", ").
  }
}
\details{
  The function internally processes each specified variable in the dataframe (\code{data\_frame}) to extract unique values from comma-separated responses, sorts them, and combines them into a single, organized response. The result is a new dataframe with the organized and sorted multiple response variables.
}
\value{
  Returns a dataframe with organized and sorted multiple response variables. Empty responses are replaced with \code{NA}.
}
\examples{
  # Example usage:
  df_multi_response <- data.frame(
    var1 = c("A, B", "B, C", "A, C"),
    var2 = c("X, Y", "Y, Z", "X, Z")
  )
  organize_multi_responses(df_multi_response, c("var1", "var2"))
}
