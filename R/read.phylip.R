#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL： http://github.com/helixcn/phylotools
#### date: 26 MAY 2015


read.phylip <- function(infile, clean_name = TRUE){
    dat <- readLines(infile)
    
    ### Obtain the number of sequences
    seq.info <- strsplit(gsub("[[:space:]]", "_",dat[1]), "_")[[1]]
    seq.info.numeric <- as.numeric(seq.info)
    seq.info.numeric <- seq.info.numeric[!is.na(seq.info.numeric)]
    nseq <- seq.info.numeric[1]
    
    
    if(length(dat) > (nseq + 1)){ ### interleaved
        seqs <- dat[-1]           ### omit the first line
        
        seq.text <- c()
        for (i in 1:nseq){        ### obtain the corresponding lines for each sequence
            seq.index   <- seq(from = 0, to = length(dat)-1, by = nseq + 1) + i
            seq.text[i] <- paste(seqs[seq.index], collapse = "")
       }
    } else {                      
        seq.text <- dat[2:length(dat)]  ### sequential, can be read directly
    }
    seq.name <- substr(seq.text, start = 1,  stop = regexpr(" ", seq.text) - 1)
    if(clean_name){
        seq.name <- gsub("^[_]+|[_]+$| ","",gsub("^[[:space:]]+|[[:space:]]+$", "", gsub("[[:punct:]]","_", seq.name)))
    }
    seq.text <- gsub("[[:space:]]", "", substr(seq.text, start = regexpr(" ", seq.text), stop = nchar(seq.text)))
    res <- data.frame(seq.name, seq.text)
    return(res)
}







