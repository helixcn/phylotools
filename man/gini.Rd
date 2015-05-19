\name{gini}
\alias{gini}

\title{
Gini coefficient of inequality
}
\description{
The Gini coefficient of inequality
}
\usage{
gini(x)
}

\arguments{
  \item{x}{
A numeric vector
}
}
\details{
Gini coefficient is a measure of inequality. Gini coefficient is based on Lorenz's curve, and the range of this coefficient is between 0 and 1. The more lower the value, the more equality the distribution is. The algorithm of this function is directly adopted from package "ineq" by Achim Zeileis. 
}

\references{
F A Cowell: Measurement of Inequality, 2000, in A B Atkinson / F Bourguignon (Eds): Handbook of Income Distribution, Amsterdam
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\seealso{
\code{\link{meangini}}
}
\examples{
### 0.3076923
gini(c(1,3,2,1,1,1,4))

}

\keyword{ inequality }

