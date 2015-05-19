\name{framsub}
\alias{framsub}
\title{
Pattern of substitutions of a dataframe.
}
\description{
This function will replace the specified character in the egde of each element to the replacement character of the second column of the input dataframe.
}
\usage{
framsub(x, pattern = "-", replacement = "?")
}

\arguments{
  \item{x}{
The input dataframe.
}
  \item{pattern}{
A character to be matched.
}
  \item{replacement}{
A character to be replaced.
}
}
\details{
This is a general application of the function \code{\link{edgesub}}.The only the matched characters at the egde of the input DNA string will be replaced.
}
\value{
A dataframe with the matched characters at the two edges replaced.
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
See Also  \code{\link{edgesub}}
}
\examples{

##framsub example ########
dir <- system.file("extdata", package = "phylotools")
setwd(dir)
testphy <- read.phylip("rbcla.phy")
testdat <- phy2dat(testphy)
framsub(testdat)

}

\keyword{ dataframe }
\keyword{ substitution }
