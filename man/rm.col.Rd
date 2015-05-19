\name{rm.col}
\alias{rm.col}
\title{
Delete specified columns for dataframe or matrix
}
\description{
Delete specified columns for dataframe or matrix.
}
\usage{
rm.col(mat, to.rm = NULL)
}

\arguments{
  \item{mat}{
a dataframe or matrix, must have column names.
}
  \item{to.rm}{
a character vector representing the column names that to be removed.
}
}

\value{
A dataframe or matrix (according to the input data)with certain column deleted.
}
\references{
None
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

x <- rnorm(600)
dim(x) <- c(20,30)
colnames(x) <- paste("col",1:30,sep = "")
rm.col(x, to.rm = c("col1", "col3", "col8", "col22", "col30"))

}

