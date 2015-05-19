\name{write.mat}
\alias{write.mat}
\title{
Save supermatrix
}
\description{
Save supermatrix to a phylip file
}
\usage{
write.mat(supermat, file = NULL)
}

\arguments{
  \item{supermat}{
Supermatrix as build by function \code{supermat}
}
  \item{file}{
File name to be specified
}
}
\details{
None
}
\value{
Save supermatrix to phylip file.
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{supermat}}
}
\examples{

dir <- system.file("extdata", package = "phylotools")
file.copy(from = dir, to = getwd())
## Supermatrix with "rbcla","matk","trnH-psbA"
supermat1 <- supermat(rbcl = "rbcla.phy", matk = "matK.phy", 
           trn = c("trn1.phy", "trn2.phy","trn3.phy","trn4.phy"))	   
write.mat(supermat1, "result1.phy")

}

\keyword{ supermat }
