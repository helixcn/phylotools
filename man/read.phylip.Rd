\name{read.phylip}
\alias{read.phylip}
\title{
read phylip file
}
\description{
read the phylip file, and store the sequences and their names in a dataframe.
}
\usage{
read.phylip(infile, clean_name = TRUE, seq.name.length = NA)
}

\arguments{
  \item{infile}{
character string for the name of the phylip file.
}
  \item{clean_name}{
logical, representing cleaning of the names will be performed.
}
  \item{seq.name.length}{
Number of characters for the sequence names, if not specified, the function will use the first white space as the identifier. All the character before the first white space will be treated as sequence names, and the rest will be treated as sequences.
}
}
\details{
read.phylip accepts both interleaved and sequential phylip, the number of sequences is identified by parsing the first line of the file. Sequences and their names will be stored in a data frame.

If clean_name is TRUE, punctuation characters and white space be replaced by "_". Definition of punctuation characters can be found at \code{\link{regex}}.

}
\value{
a data frame with two columns:
(1) seq.name, the names for all the sequences;
(2) seq.text, the raw sequence data.
}

\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}
\note{
the Punctuation characters and white space in the names of the sequences will be replaced by "_".
}

\seealso{
\code{\link{read.fasta}}
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

res <- read.phylip(infile = "matk.phy")
unlink("matk.phy")
}

\keyword{ phylip }
