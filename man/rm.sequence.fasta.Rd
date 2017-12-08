\name{rm.sequence.fasta}
\alias{rm.sequence.fasta}
\title{
Delete sequences from fasta file
}
\description{
Delete sequences from fasta file
}
\usage{
rm.sequence.fasta(infile, outfile = "sequence.removed.fasta", to.rm = NULL)
}

\arguments{
  \item{infile}{
Character string representing the name of the fasta file.
}
  \item{outfile}{
Character string representing the name of the output fasta file.
}
  \item{to.rm}{
Vector of character string containing the names of sequences to be deleted.
}
}
\details{
Delete sequences from a fasta file.
}
\value{
This is a subroutine without return value.
}
\references{
\url{http://www.genomatix.de/online_help/help/sequence_formats.html}
}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}


\seealso{
\code{\link{read.fasta}}, \code{\link{dat2fasta}}
}
\examples{

cat(
">seq_1",  "---TCCGCCCCCCTACTCTA",
">seq_3",  "CTCTCCGCCCCTCTACTCTA",
">seq_5",  "---TCCGCCC-TTTACTCTA",
">seq_6",  "---TCCGCCCCTCTACTCTA",
">seq_9",  "---TCCGCCC-TCTACTCTA",
">seq_12", "CTCTCCGCCC-TCTACTCTA",
file = "trn2.fasta", sep = "\n")

rm.sequence.fasta(infile = "trn2.fasta", to.rm = c("seq_1","seq_12"))

}

\keyword{ fasta }
