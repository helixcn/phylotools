\name{add.mat}
\alias{add.mat}
\title{
Add matrix to current matrix
}
\description{
Concatenates second columns of matrix.
}
\usage{
add.mat(mat1, mat2)
}
\arguments{
  \item{mat1}{
A dataframe with 2 columns.
}
  \item{mat2}{
A dataframe with 2 columns.
}
}
\details{
This function could be used to concatenate the second column of the second matrix with the first matrix's second column according to each matrix's first column (as the names of the sequences ). The "?" will be added automatically.
}
\value{
A matrix with first column the names of the sequences, the second column the concatenated sequences.
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
See Also \code{\link{supermat}}
}

\examples{

## add.mat example ########
dir <- system.file("extdata", package = "phylotools")
setwd(dir)
rbcla <- read.phylip("rbcla.phy")
matk <- read.phylip("matK.phy" )
rbdat <- phy2dat(rbcla)
matdat <- phy2dat(matk)
add.mat(rbdat, matdat)

}

\keyword{ matrix }

