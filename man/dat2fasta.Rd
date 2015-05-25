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

fasta.file <- system.file("extdata/aligned.fasta", package = "phylotools")
res <- read.fasta(fasta.file)
dat2fasta(res)

}

\keyword{ fasta }
