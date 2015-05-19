\name{phyloshuffle}
\alias{phyloshuffle}

\title{
Tip label shuffling of a phylogenetic tree
}
\description{
Internal function used in phylotools.
}
\usage{
phyloshuffle(tree)
}

\arguments{
  \item{tree}{
Phylogenetic tree
}
}
\details{
Shuffle the phylogenetic tips
}
\value{
Randomized phylogenetic tree
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

data(bird.orders)
par(mfrow = c(1,2))
plot(bird.orders)
rand <- phyloshuffle(bird.orders)
plot(rand)

}
