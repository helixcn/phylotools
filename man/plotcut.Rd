\name{plotcut}
\alias{plotcut}

\title{
Cutting the phylogenetic cutting pattern by a given time or times 
}
\description{
Add a line to the plot phylogenetic tree
}
\usage{
plotcut(tree, n)
}

\arguments{
  \item{tree}{
The phylogenetic tree in class 'phylo' defined in ape
}
  \item{n}{
Number of randomly placed lines to add to.
}
}
\details{
This function add lines to a phylogenetic tree randomly. There for it is only a demonstration for cutting patters of the phylogenetic tree. This cutting pattern could be used to evaluate the inequality of clade insufficient sampling as implemented in \code{\link{meangini}}
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{meangini}}
}
\examples{
data(bird.orders)
plotcut(bird.orders, 10)
}

\keyword{ ~kwd1 }

