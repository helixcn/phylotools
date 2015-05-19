\name{phylocom.pd}
\alias{phylocom.pd}

\title{
Calculate phylogenetic diversity using Phylocom
}
\description{
This is a wrapper function of phylocom for Calculate phylogenetic diversity using Phylocom
}
\usage{
phylocom.pd(sample = "sample", phylo = "phylo")
}

\arguments{
  \item{sample}{
Character string, the name of the sample file used in Phylocom
}
  \item{phylo}{
Character string, the name of the "phylo" file used in Phylocom
}

}

\value{
PD data frame
}
\references{
Campbell Webb, David Ackerly, Steve Kembel (2010) Phylocom software for the analysis of phylogenetic community structure and character evolution (with phylomatic and ecovolve) User S Manual Version 4.1 
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{phylocom.comstruct}},\code{\link{phylocom.comdist}}
}
\examples{
### Assume the file sample1 and phylo1 have been saved to the R working directory.
### The phylocom executable file has been copied to the same working directory
### Or could be invoked in console by command lines.
### inorder to run phylocom in R, simply copy the following command:
#   i = 1
#   PD.res <- phylocom.pd(sample = paste("sample",i, sep = ""), 
#          phylo = paste("phylo",i, sep = ""), 
#          )
#   PD.res
#            
}

\keyword{ phylocom }

