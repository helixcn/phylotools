\name{del.tree.tip}
\alias{del.tree.tip}

\title{
Randomly Delete tree tip labels
}
\description{
This function will randomly deleting tree tip labels, this is an internal function in phylotools
}
\usage{
del.tree.tip(tree, n)
}

\arguments{
  \item{tree}{
The phylogenetic tree
}
  \item{n}{
Number of tips to be deleted
}
}

\value{
The phylogenetic tree in class 'phylo'
}
\references{
To be added
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{help}}
}

\examples{
data(bird.orders)
del.tree.tip(bird.orders, 4)
}

