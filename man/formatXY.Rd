\name{formatXY}
\alias{formatXY}

\title{
Formating XY label
}
\description{
Add "0" in characters where should be added, inorder to make the character labels sortable.
}
\usage{
formatXY(x)
}

\arguments{
  \item{x}{
A character vector containing the labels in "XnYm" format
}
}
\details{
This function will add "0" before the number 'n' if it has fewer characters than the number that has more characters in the same position. Only then the elements "XnYm" in the label vector could be sorted. 
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{imagevect}}
}
\examples{
formatXY(c("X1Y1","X33Y567"))
}
