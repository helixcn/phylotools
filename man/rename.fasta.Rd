\name{rename.fasta}
\alias{rename.fasta}

\title{
Renaming a fasta object given a reference table
}
\description{
this function could be used to rename the sequences in the fasta object, given a reference dataframe.
}
\usage{
rename.fasta(fas, ref, fil = NULL, prefix = NULL)
}

\arguments{
  \item{fas}{
the fasta object.
}
  \item{ref}{
Dataframe with first column of the names of the sequences , the second column which name to change into.
}
  \item{fil}{
A string indicating the file name, the result written to.
}
  \item{prefix}{
prefix of which the names of the sequences.
}
}
\details{
Prefix providing the flexibility of modifying the input names of sequences, for example, if the sequences names begin with "P_seq1", and unfortunately the names in reference table are lack of "P_". You may set the prefix as "P_", and without changing the reference table. 

}
\value{
fasta object
}
\references{
None
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
\code{\link{gnames.fas}}
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
## 
rename.fasta(fil.fas, reftable.rename)

}

\keyword{ fasta }

