rm.sequence.fasta <- function(infile, outfile = "sequence.removed.fasta",to.rm = NULL){
    dat <- read.fasta(infile)
    sequence.to.keep.index <- !as.character(dat[,1]) %in% to.rm
    dat <- dat[sequence.to.keep.index,] 
    dat2fasta(dat, outfile = outfile)
}
