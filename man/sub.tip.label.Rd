\name{sub.tip.label}
\alias{sub.tip.label}

\title{
Substitute the tip labels of a phylogenetic tree
}
\description{
This function may be used in changing the tip labels of a phylogenetic tree according to a reference data table.
}
\usage{
sub.tip.label(tree, dat)
}

\arguments{
  \item{tree}{
  Phylogenetic tree
}
  \item{dat}{
A dataframe with the first the tip labels and the second the substitutions.
}
}
\details{
To be added
}
\value{
A Phylogenetic tree with the tip labels substituted
}
\references{
None
}

\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\examples{

library(ape)
data(bird.families)
tips <- bird.families$tip.label
abr <- paste("fam",1:length(tips), sep = "")
dat <- data.frame(tips, abr)
ntree <- sub.tip.label(bird.families, dat)
}

