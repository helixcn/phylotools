#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015


dat2fasta <- function(dat, outfile = "out.fasta") {
  writeLines(paste(">", as.character(dat$seq.name), "\n", as.character(dat$seq.text), sep = ""), outfile)
  cat(paste(outfile, "has been saved to ", getwd(), "\n"))
}
