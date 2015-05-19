\name{meangini}
\alias{meangini}

\title{
Mean Gini coefficient
}
\description{
Calculating the inequality of clade insufficient sampling Gini coefficient  using Monte Carlo simulations 
}
\usage{
meangini(tree, subtree, times = 1000, plot = FALSE)
}

\arguments{
  \item{tree}{
A phylogenetic tree in class 'phylo' as in package 'ape'
}
  \item{subtree}{
A phylogenetic tree in class 'phylo', but have fewer tips than the input 'tree'.
}
  \item{times}{
Times for calculating the Gini coefficient
}
  \item{plot}{
Whether to plot the cutting pattern and the histogram
}
}
\details{
This function could be used to calculating the inequality of clade insufficient sampling. The inequality can be measured by Gini coefficient, which is frequently used in economy. The Gini coefficient is based on Lorenz's curve, and the range of this coefficient is between 0 and 1. The more lower the value, the more equality the distribution is. 
A random time between the oldest node and the latest node in the phylogenetic tree was generated, and the phylogenetic tree was divided into two to n clades according to the time. For each clade in each "cutting" pattern, number of tips that can not be found in the perfect phylogenetic tree was recorded as number of "missing" tips. The number of missing tips was further divided by the number of expectation in the clade in perfect sampling tree. This ratio was calculated for each clade.The Gini coefficient was calculated for these values to represent the inequality of species that not included among clades.
This procedure will be replicated for 1000 times in default and require intensively computationally power, this may last for minutes, even in handling small phylogenetic trees. Therefore, please be patient on the time consuming task. 
}
\value{
returns a list with the first the mean for all value, and the second a vector representing the all Gini coefficient for each randomly cutting pattern. 
}
\references{
Jinlong Zhang, Nathan Swenson, Xiangcheng Mi, Jihong Huang, Keping Ma, The imperfect sampling and the accuracy of phylogenetic patterns in communities.
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}
\note{
This function require intensive computationally power, and can be very slow during handling large phylogenies. 
}

\seealso{
\code{\link{gini}}
}
\examples{

data(bird.orders)
to.drop <- c("Craciformes", "Galliformes", "Gruiformes")
dropped <- drop.tip(bird.orders, to.drop)
meangini(tree = bird.orders, subtree = dropped, times = 10, plot = TRUE)
meangini(tree = bird.orders, subtree = dropped, times = 50, plot = TRUE)
meangini(tree = bird.orders, subtree = dropped, times = 100, plot = TRUE)
meangini(tree = bird.orders, subtree = dropped, times = 200, plot = TRUE)

}

\keyword{ equality }

