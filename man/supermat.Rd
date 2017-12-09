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

  cat("6 22",
  "seq_1    --TTACAAATTGACTTATTATA",
  "seq_2    GATTACAAATTGACTTATTATA",
  "seq_3    GATTACAAATTGACTTATTATA",
  "seq_5    GATTACAAATTGACTTATTATA",
  "seq_8    GATTACAAATTGACTTATTATA",
  "seq_10   ---TACAAATTGAATTATTATA",
  file = "matk.phy", sep = "\n")

  cat("5 15",
  "seq_1     GATTACAAATTGACT",
  "seq_3     GATTACAAATTGACT",
  "seq_4     GATTACAAATTGACT",
  "seq_5     GATTACAAATTGACT",
  "seq_8     GATTACAAATTGACT",
  file = "rbcla.phy", sep = "\n")

  cat("5 50",
  "seq_2          GTCTTATAAGAAAGAATAAGAAAG--AAATACAAA-------AAAAAAGA",
  "seq_3          GTCTTATAAGAAAGAAATAGAAAAGTAAAAAAAAA-------AAAAAAAG",
  "seq_5          GACATAAGACATAAAATAGAATACTCAATCAGAAACCAACCCATAAAAAC",
  "seq_8          ATTCCAAAATAAAATACAAAAAGAAAAAACTAGAAAGTTTTTTTTCTTTG",
  "seq_9          ATTCTTTGTTCTTTTTTTTCTTTAATCTTTAAATAAACCTTTTTTTTTTA",
  file = "trn1.phy", sep = "\n")

supermat(infiles = c("matk.phy", "rbcla.phy", "trn1.phy"))
unlink(c("matk.phy", "rbcla.phy", "trn1.phy"))
unlink(c("supermat.out.phy","gene_partition.txt"))

cat(
  ">seq_1",  "--TTACAAATTGACTTATTATA",
  ">seq_2",  "GATTACAAATTGACTTATTATA",
  ">seq_3",  "GATTACAAATTGACTTATTATA",
  ">seq_5",  "GATTACAAATTGACTTATTATA",
  ">seq_8",  "GATTACAAATTGACTTATTATA",
  ">seq_10", "---TACAAATTGAATTATTATA",
  file = "matk.fasta", sep = "\n")

cat(
  ">seq_1", "GATTACAAATTGACT",
  ">seq_3", "GATTACAAATTGACT",
  ">seq_4", "GATTACAAATTGACT",
  ">seq_5", "GATTACAAATTGACT",
  ">seq_8", "GATTACAAATTGACT",
  file = "rbcla.fasta", sep = "\n")

cat(
  ">seq_2", "GTCTTATAAGAAAGAATAAGAAAG--AAATACAAA-------AAAAAAGA",
  ">seq_3", "GTCTTATAAGAAAGAAATAGAAAAGTAAAAAAAAA-------AAAAAAAG",
  ">seq_5", "GACATAAGACATAAAATAGAATACTCAATCAGAAACCAACCCATAAAAAC",
  ">seq_8", "ATTCCAAAATAAAATACAAAAAGAAAAAACTAGAAAGTTTTTTTTCTTTG",
  ">seq_9", "ATTCTTTGTTCTTTTTTTTCTTTAATCTTTAAATAAACCTTTTTTTTTTA",
  file = "trn1.fasta", sep = "\n")

supermat(infiles = c("matk.fasta", "rbcla.fasta", "trn1.fasta"))
unlink(c("matk.fasta", "rbcla.fasta", "trn1.fasta"))

unlink(c("supermat.out.phy","gene_partition.txt"))

}

\keyword{ supermatrix }
\keyword{ fasta }
\keyword{ phylip }
\keyword{ partition}
\keyword{ RAxML}
