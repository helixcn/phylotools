\name{phylocom.comdist}
\alias{phylocom.comdist}

\title{
Calculating phylogenetic comdist using Phylocom
}
\description{
This is a wrapper function for phylocom.
}
\usage{
phylocom.comdist(sample = "sample", phylo = "phylo", method = "comdist", aw = TRUE, file = "comdist.txt")
}

\arguments{
  \item{sample}{
Character string, the name of the sample file used in Phylocom
}
  \item{phylo}{
Character string, the name of the "phylo" file used in Phylocom
}
  \item{method}{
character, only "comdist" or "comdistn"
}
  \item{aw}{
Logical, if TRUE, the comdist will be computed in accounting of abundance. Otherwise, for presence - absence.
}
  \item{file}{
Result File that to be saved to .
}
}

\references{
Campbell Webb, David Ackerly, Steve Kembel (2010) Phylocom software for the analysis of phylogenetic community structure and character evolution (with phylomatic and ecovolve) User S Manual Version 4.1 
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{phylocom.pd}},\code{\link{phylocom.comstruct}}
}
\examples{
### Assume the file sample1 and phylo1 have been saved 
### to the R working directory. The phylocom executable 
### file has been copied to the same working directory
### Or could be invoked in console by command lines.
### in order to run phylocom in R, simply copy the 
### following command:
#   i = 1
#   phylocom.comdist(sample = paste("sample",i, sep = ""),  
#                  phylo = paste("phylo",i, sep = ""), 
#                  method = "comdist", aw = TRUE,  
#                  file =  paste("comdist.aw",  i,
#                  ".txt", sep = ""))
#   
}

\keyword{ phylocom }

