\name{rh}
\alias{rh}

\title{
Generating random time between the oldest node and the latest node
}
\description{
Random time between the oldest node and the latest node. This function only handle the ultrametric tree. 
}
\usage{
rh(tree, times = 1)
}

\arguments{
  \item{tree}{
The phylogenetic tree
}
  \item{times}{
Number of random time values to be generated
}
}

\value{
A vector representing the time, that randomly generated
}
\references{
To be added
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}
\note{
This function only handle the ultrametric tree. 
}

\examples{
data(bird.orders)
rh(bird.orders, 10)
}

