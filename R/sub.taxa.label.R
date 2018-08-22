#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015
#### Modified: 22 AUG 2018

#### Function sub.tip.label as part of R package phylotools
#### By Jinlong Zhang  <Jinlongzhang01@gmail.com>
#### Institute of Botany, the Chinese Academy of Sciences, Beijing ,China
#### Nov- 01-2010

sub.taxa.label <- function(tree, dat){
    
    if(!inherits(tree,"phylo")){
       stop("the input tree is not a \"phylo\" object.")
    }

    if(!is.data.frame(dat)){
       stop("the input dat is not a \'data.frame\'.")
    }

    tree2 <- tree
    nnn <- tree$tip.label

    if(!nrow(dat) == length(nnn)){
       warning("Number of tip labels in the phylogenetic\n tree differs from the number of rows in the reference table.\n")
    }

    xxx1 <- as.character(dat[,1])
    xxx2 <- as.character(dat[,2])

    if(!all(xxx1 %in% nnn)){
       unsub.dat <- xxx1[!xxx1 %in% nnn]
       cat("The following names in the reference data.frame \n can not be found in the phylogeny:\n", unsub.dat, "\n")
    }

    if(!all(nnn %in% xxx1)){
        unsub.tree <- nnn[nnn %in% xxx1]
       cat("The following tip labels in the phylogenetic tree \ncan not be found in the reference data.frame:\n", unsub.tree, "\n")
    }

    label <- c()
    for(i in 1:length(dat[,1])){
        for(j in 1:length(nnn)){
            if(nnn[j] == xxx1[i]){
                label[j] <- xxx2[i]
            }
        }
    }

    tree2$tip.label <- label
    return(tree2)
}
