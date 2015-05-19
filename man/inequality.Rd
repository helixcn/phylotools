\name{inequality}
\alias{inequality}
\title{
The inequality of a cutting time
}
\description{
This function could be applied to calculate the inequality of a given time.
}
\usage{
inequality(tree, subtree, h, detail = FALSE)
}

\arguments{
  \item{tree}{
The phylogenetic tree, in class 'phylo' as defined in ape
}
  \item{subtree}{
The phylogenetic tree, usually the original 'tree' with some of the tips removed.
}
  \item{h}{
Numeric: The given time
}
  \item{detail}{
Logical,the time
}
}
\details{
The inequality of the insufficient sampling is evaluated by Gini coefficient with a range of c(0, 1). If the value equals to zero, a ideally equality observed; if the values equals to 1, a extremely inequality observed.
}

\references{
To be added
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

data(bird.orders)
rtr1 <- del.tree.tip(bird.orders,3)
inequality(bird.orders, rtr1[[1]], h = 25.25103)

}

\keyword{ ~kwd1 }

