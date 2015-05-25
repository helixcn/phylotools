\name{read.phylip}
\alias{read.phylip}
\title{
read phylip file
}
\description{
read the phylip file, and store the sequences and their names in data frame. 
}
\usage{
read.phylip(infile, clean_name = TRUE)
}

\arguments{
  \item{infile}{
character string for the name of the phylip file.
}
  \item{clean_name}{
logical, representing cleaning of the names will be performed. }
}
\details{
read.phylip accepts both interleaved and sequential phylip, the number of sequences is identified by parsing the first line of the file. Sequences and their names will be stored in a data frame. 

If clean_name is true, punctuation characters and white space be replaced by "_". Definition of punctuation characters can be found at \code{\link{regex}}.

}
\value{
a data frame with two columns: 
(1) seq.name, the names for all the sequences; 
(2) seq.text, the raw sequence data. 
}
\references{
\url{http://bioperl.org/wiki/PHYLIP_multiple_alignment_format}
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

phylip.file <- system.file("/extdata/test.phy", package = "phylotools")
read.phylip(infile = phylip.file)

}

\keyword{ phylip }

