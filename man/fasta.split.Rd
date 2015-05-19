\name{fasta.split}
\alias{fasta.split}

\title{
Split the fasta object to fasta objects
}
\description{
Split the fasta object to fasta objects according to the groups given
}
\usage{
fasta.split(fasta, ref, save2disk = FALSE)
}

\arguments{
  \item{fasta}{
The fasta object
}
  \item{ref}{
Data frame with first column of the names of the sequences , the second column which group it belongs to.
}
  \item{save2disk}{
Whether to save the groups of fasta files to separate files
}
}
\details{
This function may be used to split the trnH-psbA sequences, given the order ( usually APGIII ) it belongs to.
}
\value{
Returns a list, indicate the groups for the sequences
}
\references{
Non.
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
\code{\link{rename.fasta}}
}
\examples{

library(seqRFLP)
## loading data
data(fil.fas)
## Get the names of the sequences
col1 <- gnames.fas(fil.fas)
## Generating new names
col2 <- paste("seq", 1:length(col1), sep = "")
reftable.rename <- data.frame(col1, col2)
renamed <- rename.fasta(fil.fas, reftable.rename)

##Generate split factor levels.
index <- rep(NA, length(col2))
level1 <- seq(1, length(col2), by = 2)
index[level1] <- 1
index[-level1] <- 2

## Reference table
reftable.split <- data.frame(col2, index)

## split the fasta object
fasta.split(renamed, reftable.split)

}

\keyword{ fasta }
