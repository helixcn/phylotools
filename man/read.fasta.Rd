\name{read.fasta}
\alias{read.fasta}
\title{
Read FASTA file
}
\description{
Read and convert the fasta file to data frame
}
\usage{
read.fasta(file = NULL, clean_name = FALSE)
}

\arguments{
  \item{file}{
character string representing the name of the fasta file.
}
  \item{clean_name}{
logical, representing cleaning of the names will be performed. Punctuation characters and white space be replaced by "_" . See \code{\link{regex}} for more details.

}
}
\details{
In this function, names of the sequences are identified by ">", and all the lines before next ">" will be concatenated.

}
\value{
a data frame with two columns:
(1) seq.name, the names for all the sequences.
(2) seq.text, the raw sequence data.
}
\references{
\url{http://www.genomatix.de/online_help/help/sequence_formats.html}
}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}
\note{
Punctuation characters and white space in the names of the sequences will be replaced by "_".
}

\seealso{
\code{\link{read.phylip}},\code{\link{dat2fasta}},\code{\link{dat2phylip}},\code{\link{split_dat}}
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
unlink("trn1.fasta")
}
\keyword{ fasta }
