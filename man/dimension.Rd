\name{dimension}
\alias{dimension}
\title{
Get the dimension of the matrix defined by "XnYm" labels
}
\description{
This is an internal function for handling "XnYm" labels, users do not need to call this function directly.
}
\usage{
dimension(x, unique = FALSE, sort = FALSE)
}

\arguments{
  \item{x}{
A vector contain the "XnYm" labels
}
  \item{unique}{
Logical, remove the duplicated value
}
  \item{sort}{
Logical, whether to sort the value in the result
}
}


\value{
A list with the rows or columns
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{imagevect}}
}
\examples{

dimension(c("X1Y4","X33Y56"))

}


