\name{edgesub}
\alias{edgesub}
\title{
Substitute character for a sequence from both sides.
}
\description{
Substitute character for a sequence from both sides without changing the internal matched characters.
}
\usage{
edgesub(x, pattern = "-", replacement = "?")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The input DNA sequence.
}
  \item{pattern}{
The character that to be substitute.
}
  \item{replacement}{
The character that will replace the pattern.
}
}
\details{
None.
}
\value{
This function will return to the substituted pattern of the input DNA sequence.
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
See Also \code{\link{framsub}}, for substitution of a dataframe, which is a general application of this function.
}
\examples{

## edgesub example ########
#### Substitute each terminal of the sequence
dna = "---ATTGCCTTAS--TTTAAAAAACCGTTC-----"
edgesub(dna)

}

\keyword{ substitution }

