\name{fmatch}
\alias{fmatch}

\title{
Find the matching index
}
\description{
Find the matching index
}
\usage{
fmatch(dna, pattern = "")
}

\arguments{
  \item{dna}{
A character string including the DNA sequence.
}
  \item{pattern}{
A character to be used in matching the DNA sequence.
}
}
\details{
None.
}
\value{
The max number the input character could be matched from left to right.
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
See Also \code{\link{edgesub}} for a general application of this function.
}
\examples{

### fmatch  example ########
dna = "---ATTGCCTTAS--TTTAAAAAACCGTTC-----"
fmatch(dna, "-")

}

\keyword{ match }
