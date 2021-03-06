% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaling.R
\name{standardize}
\alias{standardize}
\title{Standardize}
\usage{
standardize(x, margin = 1L)
}
\arguments{
\item{x}{numeric, matrix or dataframe input vector.}

\item{margin}{1 - standardize by rows and 2 - standardize by columns. Default is 1.}
}
\value{
A standardize numeric or data.frame.
}
\description{
Currently implemented for numeric vectors, numeric matrices and data.frame standardization.
}
\examples{
df <- data.frame("V1" = 1:3, "V2" = 3:5, "V3" = 5:7)
standardize(df, margin = 1L)
standardize(df, margin = 2L)

m <- matrix(c(1,2,3,3,4,5,5,6,7), ncol = 3, byrow = FALSE)
standardize(m, margin = 1L)
standardize(m, margin = 2L)

v <- c(1, 3, 5)
standardize(v)


}
