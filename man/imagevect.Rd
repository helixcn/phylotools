\name{imagevect}
\alias{imagevect}

\title{
Image plot of a numeric vector
}
\description{
An image plot of a vector given the labels in "XnYn" in format.
}
\usage{
imagevect(x, labels, contour = FALSE, gridsize = 20, axes = TRUE, nlabx = 5, nlaby = 5, ...) 
}

\arguments{
  \item{x}{
A numeric vector representing the value for each quadrat. 
}
  \item{labels}{
A character vector representing the row and column for each value in x. Should be in format "X10Y5", which is account for column 10 (X = 10), and row 5(Y = 5).  
}
  \item{contour}{
Whether to add contour lines to the existing image plot.
}
 \item{gridsize}{
Size for each quadrat or grid
}
\item{axes}{
Whether to draw axes for the plot
}
\item{nlabx}{
Number of labels to be added to the x axis
}
\item{nlaby}{
Number of labels to be added to the y axis
}
\item{\dots}{
Further arguments to passed on for plotting
}
}

\details{
This function will automatically sort the values in vector according to the information of rows and columns that extracted from labels, and subsequently build a matrix for invoking the \code{image.plot} function in "fields" package. The labels must be in "XnYn" format, and must be in the same length with vec. 
}

\value{
Image plot for the vector.
}
\references{
None
}
\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

x <- rnorm(600)
labmat <- expand.grid(paste("X",1:30, sep = ""), paste("Y", 1:20, sep = ""))
lab <- paste(labmat[,1], labmat[,2], sep = "")
imagevect(x, labels = lab, col = cm.colors(10))

}

\keyword{ plot }

