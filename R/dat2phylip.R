#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL： http://github.com/helixcn/phylotools
#### date: 26 MAY 2015


dat2phylip <- function(dat, outfile = "out.phy"){
    row1.1 <- nrow(dat)
    row1.2 <- unique(nchar(as.character(dat[, 2])))
    if(length(row1.2) > 1){
       stop("Not a aligned fasta file, conversion stopped.")
    }
    row1   <- paste(" ", row1.1, "  ", row1.2,  collapse = "", sep = "") ### header of the phylip file
    
    ## leave at least one space between the species names and sequence
    space  <- max(nchar(as.character(dat[, 1]))) + 1 - nchar(as.character(dat[, 1]))  
    res    <- c()
    for (i in 1:length(space)) {
        res[i] <- paste(dat[i, 1], paste(rep(" ", space[i]), collapse = ""), dat[i, 2], sep = "")
    }
    res <- c(row1, res)
    writeLines(res, outfile)
    cat(paste(outfile, "has been saved to ", getwd(), "\n"))
}

