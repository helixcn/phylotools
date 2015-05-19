\name{phylotools-package}
\alias{phylotools-package}
\alias{phylotools}
\docType{package}
\title{
Phylogenetic tools for ecologists
}
\description{
This package currently consists of a few functions for handling DNA-barcoding sequences to build supermatrix for further analysis with RAxML etc. Much more functions for conducting phylogenetic analysis would be added in the future, especially for community phylogentic analysis.
}
\details{
\tabular{ll}{
Package: \tab phylotools\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2010-8-18\cr
License: \tab GLP-2\cr
LazyLoad: \tab yes\cr
}

}
\author{

Jinlong Zhang, Nancai Pei, Xiangcheng Mi

Maintainer: Jinlong Zhang <jinlongzhang01@gmail.com>

}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}

\keyword{ package }
\examples{


###############################################
## Example Part I###Building Supermatrix########
###############################################

### Build super matrix
dir <- system.file("extdata", package = "phylotools")
setwd(dir)

## Supermatrix with "rbcla","matk","trnH-psbA"
supermat <- supermat(rbcl = "rbcla.phy", matk = "matK.phy", 
          trn = c("trn1.phy", "trn2.phy","trn3.phy","trn4.phy"))	   
## Save to file
write.mat(supermat, "result.phy")

## Delete file
unlink("result.phy")



###############################################
## Example Part II###Create a image plot#######
###############################################

## Create an Image with legend from named numerical vectors
x <- rnorm(600)
labmat <- expand.grid(paste("X",1:30, sep = ""), 
                  paste("Y", 1:20, sep = ""))
                  
lab <- paste(labmat[,1], labmat[,2], sep = "")
imagevect(x, labels = lab, col = cm.colors(10))



###############################################
## Example Part III###Handling fasta project###
###############################################

## Handling Fasta files
library(seqRFLP)
## loading data
data(fil.fas)
## Get the names of the sequences
col1 <- gnames.fas(fil.fas)
## Generating new names
col2 <- paste("seq", 1:length(col1), sep = "")
reftable.rename <- data.frame(col1, col2)
renamed <- rename.fasta(fil.fas, reftable.rename)

##Generate split factor levels.
index <- rep(NA, length(col2))
level1 <- seq(1, length(col2), by = 2)
index[level1] <- 1
index[-level1] <- 2

## Reference table
reftable.split <- data.frame(col2, index)

## split the fasta object
fasta.split(renamed, reftable.split)



#################################################
## Example Part IV###Phylosor for very large data
#################################################
## Make sure the PhyloCom can be invoked by command line
## res <- phylocom.phylosor(sample.file = "sample",
##                                phylo = "phylo")



#################################################
## Example Part V###Rescale the FDP data #######
## to different scales ##########################
#################################################
## 20m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500,
##           scale = 20)
## 50m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500, 
##           scale = 50)
## 100m                                
## plotscale(inputdata = BCI, len = 1000, wid = 500, 
##          scale = 100)



##################################################
## Example Part VI### 
## Lineages inequality and 
## Mean Gini Coefficient amonge 
## lineages caused by imperfect sampling 
##################################################
data(bird.orders)
rtr1 <- del.tree.tip(bird.orders,3)
inequality(bird.orders, rtr1[[1]], h = 25.25103)


data(bird.orders)
to.drop <- c("Craciformes", "Galliformes", "Gruiformes")
droped <- drop.tip(bird.orders, to.drop)
meangini(tree = bird.orders, subtree = droped, 
         times = 10, plot = TRUE)
meangini(tree = bird.orders, subtree = droped, 
         times = 50, plot = TRUE)
meangini(tree = bird.orders, subtree = droped, 
          times = 100, plot = TRUE)
meangini(tree = bird.orders, subtree = droped, 
          times = 200, plot = TRUE)
          
}
