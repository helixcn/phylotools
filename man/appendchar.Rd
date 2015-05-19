\name{appendchar}
\alias{appendchar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Paste character according to the longest character.
}
\description{
Paste character for each elements at the second column which is shorter than the longest one.
}
\usage{
appendchar(mat, pattern = "?")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
data frame with two columns
}
  \item{pattern}{
Character to be added to the end of elements
}
}
\details{
None.
}
\value{
A data frame with specified character appended to the end of each shorter elements. 
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{

Jinlong Zhang \email{jinlongzhang01@gmail.com}

}
\note{
None
}

\seealso{
See Also \code{\link{framsub}}
}
\examples{

##appendchar  example ########
aa <- c("1","2","3")
bb <- c("abc","abcd","abcdefg")
rrr <- data.frame(aa, bb)
rrr
appendchar(rrr)

}
\keyword{ character }
