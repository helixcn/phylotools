\name{plotscale}
\alias{plotscale}

\title{
Rescaling the individuals occurred in a Forestry Dynamic Plot
}
\description{
Rescaling the individuals occurred in a Forestry Dynamic Plot. It will add a column called "gridnames". This function could make the analysis based on different scales easier.
}
\usage{
plotscale(inputdata, len = NULL, wid = NULL, scale = NULL)
}

\arguments{
  \item{inputdata}{
A data matrix inputdata that has "tag", "x" and "y". 
}
  \item{len}{
Length of the FDP
}
  \item{wid}{
Width of the FDP
}
  \item{scale}{
Size of the quadrat
}
}
\details{
The input data must he following columns, "Tag" : the tag number for each individual in the FDP.
"x", "y" are the coordinats of the individuals in the FDP.
}
\value{
A column will be added to the input matrix , to represent the position for each individual. For example, "X5Y20" means the corresponding individual lies at cell X = 5,y = 20. The labels could be further used to represent the species matrix.
}
\references{

Nathan J. B. Kraft, Renato Valencia, David D. Ackerly (2008) Functional Traits and Niche-Based Tree Community Assembly in an Amazonian Forest Science 322, 580 ;

}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

## 20m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500,
##           scale = 20)
## 50m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500, 
##           scale = 50)
## 100m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500, 
##          scale = 100)


}


