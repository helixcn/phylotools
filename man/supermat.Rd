\name{supermat}
\alias{supermat}
\title{
Build PHYLIP supermatrix and RAxML partition file using aligned FASTA or PHYLIP files.
}
\description{
Build PHYLIP supermatrix and create RAxML partition file using aligned fasta or phylip files.
}
\usage{
supermat(infiles, outfile = "supermat.out.phy", 
         partition.file = "gene_partition.txt")
}

\arguments{
  \item{infiles}{
a character string vector for phylip or aligned fasta file. 
}
  \item{outfile}{
the name of the PHYLIP supermatrix
}
  \item{partition.file}{
partition data summary describing the genes.
}
}
\details{
Supermatrix here means a phylip file with combined aligned sequences. The missing sequences should be replaced with either "?" or "-".  
}
\value{
A list containing: 
(1)supermat.dat:a list containing all the data frames read by read.phylip or read.fasta
(2)res.super.dat: a data frame containing the sequences and the names
(3)partition.dat: summary for all the fasta or phylip files
(4)partition.dat.vector: character string vector for the partition file for RAxML 
}
\references{

Kress, W. J., Erickson, D. L., Jones, F. A., Swenson, N. G., Perez, R., Sanjur, O., & Bermingham, E. (2009). Plant DNA barcodes and a community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences, 106(44), 18621-18626.

de Queiroz, A.and Gatesy, J. (2007). The supermatrix approach to systematics. Trends in Ecology & Evolution, 22(1), 34-41.

\url{https://github.com/stamatak/standard-RAxML}

}
\author{
Jinlong Zhang <jinlongzhang01@gmail.com>
}
\note{
Punctuation characters and white space in the names of the sequences will be replaced by "_". More information can be found at \code{\link{regex}}.
Type of the sequence in the RAxML partition file should be changed manually according to the manual of RAxML.
}

\seealso{
\code{\link{read.fasta}},\code{\link{read.phylip}},\code{\link{dat2phylip}},
}
\examples{

matK.phy     <- system.file("/extdata/matK.phy", package = "phylotools")
rbcla.phy    <- system.file("/extdata/rbcla.phy", package = "phylotools")
test.phy     <- system.file("/extdata/test.phy", package = "phylotools")
trn1.phy     <- system.file("/extdata/trn1.phy", package = "phylotools")
trn2.phy     <- system.file("/extdata/trn2.phy", package = "phylotools")
trn3.phy     <- system.file("/extdata/trn3.phy", package = "phylotools")
trn3.phy     <- system.file("/extdata/trn3.phy", package = "phylotools")
trn4.phy     <- system.file("/extdata/trn4.phy", package = "phylotools")
trnh4.fasta  <- system.file("/extdata/trnh4.fasta", package = "phylotools")
supermat(infiles = c(matK.phy, rbcla.phy, test.phy,trn1.phy,trn2.phy,trn3.phy,trn4.phy,trnh4.fasta))

}

\keyword{ supermatrix }
\keyword{ fasta }
\keyword{ phylip }
\keyword{ partition}
\keyword{ RAxML}
