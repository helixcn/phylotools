\name{dat2fasta}
\alias{dat2fasta}
\title{
Convert and Save sequence data frame to fasta file
}
\description{
Convert and Save sequence data frame to fasta file.
}
\usage{
dat2fasta(dat, outfile = "out.fasta")
}

\arguments{
  \item{dat}{
data frame by \code{\link{read.phylip}} or \code{\link{read.fasta}}
}
  \item{outfile}{
a character string, representing the name of the fasta file to be generated
}
}
\details{
The column of the data frame must be: 1. seq.name, 2. seq.text, represent the name of the sequences, the content of the sequence, eg. ATCGGGAAC.

}
\value{
This is a routine without return value.
}
\references{
    \url{http://www.genomatix.de/online_help/help/sequence_formats.html}
}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}


\seealso{
\code{\link{read.fasta}},\code{\link{read.phylip}}
}
\examples{

cat(
">seq_2", "GTCTTATAAGAAAGAATAAGAAAG--AAATACAAA-------AAAAAAGA",
">seq_3", "GTCTTATAAGAAAGAAATAGAAAAGTAAAAAAAAA-------AAAAAAAG",
">seq_5", "GACATAAGACATAAAATAGAATACTCAATCAGAAACCAACCCATAAAAAC",
">seq_8", "ATTCCAAAATAAAATACAAAAAGAAAAAACTAGAAAGTTTTTTTTCTTTG",
">seq_9", "ATTCTTTGTTCTTTTTTTTCTTTAATCTTTAAATAAACCTTTTTTTTTTA",
file = "trn1.fasta", sep = "\n")

res <- read.fasta("trn1.fasta")
dat2fasta(res)
unlink("trn1.fasta")
unlink("out.fasta")
}

\keyword{ fasta }
