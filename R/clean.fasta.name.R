#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL： http://github.com/helixcn/phylotools
#### date: 26 MAY 2015

clean.fasta.name <- function (infile = NULL, outfile= "name_cleaned.fasta") {
    res <- read.fasta(infile, clean_name = TRUE)
    dat2fasta(res, outfile = outfile)
    #### return(res)
    cat(paste(outfile, "has been saved to ", getwd(), "\n"))
}

