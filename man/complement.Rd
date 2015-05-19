\name{complement}
\alias{complement}

\title{
Get the complement sequences
}
\description{
This function could be used to convert the sequences to its complement sequence. The input data must be fasta format.
}
\usage{
complement(fas)
}

\arguments{
  \item{fas}{
Fasta format object
}
}
\details{
This function could be used to obtain the complement sequence given a fasta file. For example, the complement sequence for \code{"5-TTGAACC-3"} is \code{"5-GGTTCAA-3"}.This may be used for converting the consensus sequence to its complement, when the user find that the sequence can not be aligned with other sequences. 
}
\value{
The complement sequences in fasta format.
}
\references{
None
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}


\seealso{
\code{\link{revComp}} for more information.
}
\examples{

cat(
">No305",
"NTTCGAAAAACACACCCACTACTAAAANTTATCAGTCACT",
file = "dna1.fas", sep = "\n")

sequence <- read.fasta("dna1.fas")
complement(sequence)
unlink("dna1.fas")

}

\keyword{ fasta }

