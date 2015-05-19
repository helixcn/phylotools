\name{dat2phy}
\alias{dat2phy}
\title{
Convert dataframe to phylip format
}
\description{
Convert dataframe to phylip format
}
\usage{
dat2phy(input, write = TRUE)
}

\arguments{
  \item{input}{
dataframe with the first column the names of the sequence's and the second column the sequences 
}
  \item{write}{
Write the file to disk.
}
}
\details{
Convert dataframe to phylip format, whose first column are the names of the sequence's and the second column the sequences 
}
\value{
A vector of phylip format.
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\note{
None
}

\seealso{
See Also \code{\link{phy2dat}}
}
\examples{

##dat2phy example ########
##Convert dataframe to phylip object
dir <- system.file("extdata", package = "phylotools")
setwd(dir)
test <- read.phylip("matK.phy")
test2 <- phy2dat(test)
dat2phy(test2)

}

\keyword{ dataframe }
\keyword{ phylip }
