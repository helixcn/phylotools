\name{read.fasta}
\alias{read.fasta}
\title{
Read FASTA file
}
\description{
Read and convert the fasta file to data frame
}
\usage{
read.fasta(file = NULL, clean_name = TRUE)
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

fasta.file <- system.file("/extdata/aligned.fasta", package = "phylotools")
res <- read.fasta(fasta.file)

}
\keyword{ fasta }
