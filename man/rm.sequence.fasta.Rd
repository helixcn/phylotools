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

fasta.file <- system.file("/extdata/aligned.fasta", package = "phylotools")
sequence.names <- c("Mortierella elongata(n21_10)]",
    "Other fungsu(N8_7)]",
    "uncultured fungus,or olpidium brassicae(N11_7)]",
    "B1_22]",
    "Olpidium brassicae(B1_9)]",
    "Ambispora gerdemannii(n8_9)]"
)
rm.sequence.fasta(infile = fasta.file, to.rm = sequence.names )
}

\keyword{ fasta }

