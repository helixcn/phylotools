#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015


get.phylip.name <- function(infile, clean_name = FALSE){
    dat <- read.phylip(infile, clean_name = clean_name)
    return(dat[,1])
}
