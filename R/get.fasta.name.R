#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015

get.fasta.name <- function(infile, clean_name = FALSE){
    dat <- read.fasta(infile, clean_name = FALSE)
    return(dat[,1])
}
