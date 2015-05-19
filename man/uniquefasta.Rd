\name{uniquefasta}
\alias{uniquefasta}

\title{
Deleting duplicated sequences in fasta object
}
\description{
This function may be used to deleting the duplicated sequences in the fasta object. The sequences with its name same to any sequences will be ignored. Only the first sequence will be retained.
}
\usage{

uniquefasta(fasta)
}

\arguments{
  \item{fasta}{
The fasta object
}
}

\value{
This function returns to the fasta object with unique sequence. 
}
\references{
None
}
\author{
Jinlong Zhang \email{jinlongzhang01@mail.com}
}
\note{
This function will remove the duplicated sequences and retain the first sequence only, all according to the names of the sequences. 
}

\seealso{
\code{\link{unique}}
}
\examples{


library(seqRFLP)
## loading data
data(fil.fas)
## Get the names of the sequences
fasta.names <- gnames.fas(fil.fas)
new.names0 <- substring(fasta.names,1,2 )
new.names <- paste(new.names0, sep = "")
reftable.rename <- data.frame(fasta.names, new.names)
renamed <- rename.fasta(fil.fas, reftable.rename)
uniquefasta(renamed)

}


