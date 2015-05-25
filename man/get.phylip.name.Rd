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

\references{

\url{http://bioperl.org/wiki/PHYLIP_multiple_alignment_format}

}

\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}


\seealso{
\code{\link{read.phylip}}, \code{\link{regex}}
}

\examples{

phylip.file <- system.file("/extdata/matK.phy", package = "phylotools")
get.phylip.name(phylip.file)

}

\keyword{ phylip }

