\name{RMPD}
\alias{RMPD}

\title{
Ratio of mean phylogenetic distance of two phylogenetic trees
}
\description{
Ratio of the mean phylogenetic distance between the tips in subtree to the mean phylogenetic distance between the tips in whole tree.
}
\usage{
RMPD(subtree, tree)
}

\arguments{
  \item{subtree}{
A subset of phylogenetic tree
}
  \item{tree}{
A phylogenetic tree
}
}
\details{
RMPD > 1 indicate the tips in the "subtree" are less phylogenetic related than in "tree". This ratio could be used as a representative of phylogenetic relationships of species that occurred in subtree in comparison with in "tree". RMPD < 1 indicate the tips in "subtree" are more close related
}

\references{
 To be added
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}


\seealso{
\code{\link{resid.tree}}
}

\examples{
data(bird.orders)
del <- del.tree.tip(bird.orders,3)
del.tree <- resid.tree(bird.orders, del)
RMPD(subtree = del.tree, tree = bird.orders)
}

