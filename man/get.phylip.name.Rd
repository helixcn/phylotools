\name{get.phylip.name}
\alias{get.phylip.name}
\title{
get the names of sequences from a PHYLIP file
}
\description{
get the names of sequences from a PHYLIP file.
}
\usage{
get.phylip.name(infile, clean_name = FALSE)
}

\arguments{
  \item{infile}{
      character representing the name or path of the phylip file.
  }
  \item{clean_name}{
     logical, representing cleaning of the names will be performed.  }
}

\details{

Punctuation characters and white space be replaced by "_". Definition of Punctuation characters can be found at \code{\link{regex}}.

}

\value{
a character vector of the names of the sequences
}

\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}

\seealso{
\code{\link{read.phylip}}, \code{\link{regex}}
}

\examples{

  cat("6 22",
  "seq_1    --TTACAAATTGACTTATTATA",
  "seq_2    GATTACAAATTGACTTATTATA",
  "seq_3    GATTACAAATTGACTTATTATA",
  "seq_5    GATTACAAATTGACTTATTATA",
  "seq_8    GATTACAAATTGACTTATTATA",
  "seq_10   ---TACAAATTGAATTATTATA",
  file = "matk.phy", sep = "\n")
get.phylip.name("matk.phy")

}

\keyword{ phylip }
