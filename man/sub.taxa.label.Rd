\name{sub.taxa.label}
\alias{sub.taxa.label}

\title{
Substitute the tip labels of a phylogenetic tree
}
\description{
Substitute the tip labels of a phylogenetic tree according to a reference data table.
}
\usage{
sub.taxa.label(tree, dat)
}

\arguments{
  \item{tree}{
  Phylogenetic tree
}
  \item{dat}{
A dataframe with the first column the tip labels and the second column the new names.
}
}

\value{
A Phylogenetic tree with the tip labels substituted
}

\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
\code{\link{read.tree}}
}

\examples{

library(ape)
data(bird.families)
tips <- bird.families$tip.label
abr <- paste("fam",1:length(tips), sep = "")
dat <- data.frame(tips, abr)
ntree <- sub.taxa.label(bird.families, dat)

}

