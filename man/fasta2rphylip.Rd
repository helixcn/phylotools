\name{fasta2rphylip}
\alias{fasta2rphylip}
\title{
Converting the aligned fasta file to relaxed phylip files
}
\description{
Converting the aligned sequences in fasta format to relaxed phylip format. relaxed phylip format allows name of sequences longer than 10 characters. The relaxed phylip is suitable for RAxML for phylogenetic inferences. 
}
\usage{
fasta2rphylip(infile, outfile = "out.phy")
}

\arguments{
  \item{infile}{
Character string represents the path of the fasta file. 
}
  \item{outfile}{
The relaxed phylip file to be generated. 
}
}
\details{
To be added. 
}
\value{
Instead of giving some return value, the function generate the relaxed phy files. 
}
\references{
http://www.phylo.org/index.php/help/relaxed_phylip
}
\author{
Jinlong Zhang
}

\seealso{
\code{\link{supermat}}
}
\examples{
## Coming Soon.
}
\keyword{ fasta }
\keyword{ phylip }
\keyword{ relaxed phylip }
