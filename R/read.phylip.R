#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015
#### date: 2018-7-26
#### Acknowledgements: I would like to thank Zhang Jia for pointing out an important bug of this function.

read.phylip <- function(infile, clean_name = TRUE, seq.name.length = NA) {
  dat <- readLines(infile)
  dat <- gsub("[[:space:]]", " ", dat) # Replace all multiple whitespace with one white space.
  dat <- dat[dat != " "] # In case the interleaved sequences are spaced by whitespace...
  dat <- dat[dat != ""] # Delete empty lines, this is essential!!!

  ### Obtain the number of sequences
  seq.info <- strsplit(gsub("[[:space:]]", "_", dat[1]), "_")[[1]]
  seq.info.numeric <- as.numeric(seq.info)
  seq.info.numeric <- seq.info.numeric[!is.na(seq.info.numeric)]
  nseq <- seq.info.numeric[1]


  if (length(dat) > (nseq + 1)) { ### interleaved
    seqs <- dat[-1] ### omit the first line

    seq.text <- c()
    for (i in 1:nseq) { ### obtain the corresponding lines for each sequence
      seq.index <- seq(from = 0, to = length(seqs), by = nseq) + i ## Since all the white space between each blockes have been removed
      seq.text[i] <- paste(seqs[seq.index], collapse = "")
    }
  } else {
    seq.text <- dat[2:length(dat)] ### sequential, read directly
  }
  if (is.na(seq.name.length)) {
    seq.name <- substr(seq.text, start = 1, stop = regexpr(" ", seq.text) - 1) # There must be a whitespace between the sequence name and the sequence !
    seq.text <- substring(seq.text, first = regexpr(" ", seq.text) + 1)
  } else {
    seq.name <- substr(seq.text, start = 1, stop = seq.name.length)
    # In case there is no whitespace between the sequence names and the sequences.  Usually, seq.name.length should be 10, means the first 10 characters of the text are the sequence names.
    seq.text <- substring(seq.text, first = seq.name.length + 1)
  }
  if (clean_name) {
    seq.name <- gsub("^[_]+|[_]+$| ", "", gsub("^[[:space:]]+|[[:space:]]+$", "", gsub("[[:punct:]]", "_", seq.name)))
  }
  seq.text <- gsub("[[:space:]]", "", substr(seq.text, start = regexpr(" ", seq.text), stop = nchar(seq.text)))
  res <- data.frame(seq.name, seq.text)
  return(res)
}
