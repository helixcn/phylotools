\name{get.fasta.name}
\alias{get.fasta.name}
\title{
get the names of all the sequences of fasta file
}

\description{
get the names of all the sequences of a fasta file, and perform cleaning of the names of the sequences
}

\usage{
get.fasta.name(infile, clean_name = FALSE)
}

\arguments{

  \item{infile}{ 
character string representing the name of the fasta file.
}
  \item{clean_name}{
logical, representing cleaning of the names will be performed. 
}

}

\value{
a character vector containing the names of the sequences
}

\references{
\url{http://www.genomatix.de/online_help/help/sequence_formats.html}
}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}
\note{
Punctuation characters and white space be replaced by "_".  Definition of Punctuation characters can be found at \code{\link{regex}}.
}

\seealso{
\code{\link{read.fasta}}, \code{\link{regex}}
}
\examples{

fasta.file <- system.file("/extdata/aligned.fasta", package = "phylotools")
get.fasta.name(fasta.file)

}

\keyword{ fasta }

