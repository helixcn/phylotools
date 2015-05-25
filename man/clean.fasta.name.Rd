\name{clean.fasta.name}
\alias{clean.fasta.name}
\title{
Clean the name of a fasta file
}

\description{
Cleaning the sequences names of a fasta file. The punctuation characters and the white space will be replaced with "_".
}

\usage{
clean.fasta.name(infile = NULL, outfile = "name_cleaned.fasta")
}

\arguments{

  \item{infile}{
   character string representing the name of the fasta file.
}
\item{outfile}{
   Character string representing the file name to be generated. 
}

}
\details{
Punctuation characters and white space will be replaced by "_". More information can be found at \code{\link{regex}}.

}
\value{
This is a subroutine without a return value. A fasta file with all the names of sequences renamed will be saved to the working directory.
}
\references{
\url{http://www.genomatix.de/online_help/help/sequence_formats.html}
}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}

\seealso{
     \code{\link{read.fasta}}
}

\examples{

fasta.file <- system.file("/extdata/aligned.fasta", package = "phylotools")
clean.fasta.name(infile = fasta.file)

}

\keyword{ fasta }
