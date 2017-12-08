\name{clean.fasta.name}
\alias{clean.fasta.name}
\title{
Clean the name of a fasta file
}

\description{
Cleaning the names of sequences for a fasta file. The punctuation characters and the white space will be replaced with "_".
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

  cat(
    ">seq_1*66",  "--TTACAAATTGACTTATTATA",
    ">seq_2()r",  "GATTACAAATTGACTTATTATA",
    ">seq_3:test",  "GATTACAAATTGACTTATTATA",
    ">seq_588",  "GATTACAAATTGACTTATTATA",
    ">seq_8$$yu",  "GATTACAAATTGACTTATTATA",
    ">seq_10", "---TACAAATTGAATTATTATA",
    file = "matk.fasta", sep = "\n")
    
  clean.fasta.name(infile = "matk.fasta")
  get.fasta.name("name_cleaned.fasta")

  # Delete file
  unlink("matk.fasta")

}

\keyword{ fasta }
