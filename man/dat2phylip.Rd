\name{dat2phylip}
\alias{dat2phylip}

\title{
Conver the data frame to sequential PHYLIP format file
}
\description{
Convert and save a data frame to sequential PHYLIP file.
}
\usage{
dat2phylip(dat, outfile = "out.phy")
}

\arguments{
  \item{dat}{
the data frame returned by  \code{\link{read.phylip}},  \code{\link{read.fasta}}.
}
  \item{outfile}{
character string represents the phylip file to be generated.
}
}
\details{
The output will be in sequential PHYLIP format.
}
\value{
This is a subroutine, there is no return value.
}
\references{

\url{http://www.genomatix.de/online_help/help/sequence_formats.html}

}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}
\note{
The names of the sequences should not contain white space or Punctuation characters. See \code{\link{regex}} for more details.
}

\seealso{
\code{\link{dat2fasta}}, \code{\link{read.fasta}}, \code{\link{read.phylip}}
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
dat2phylip(res)

}
\keyword{ phylip }
