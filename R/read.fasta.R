#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015

read.fasta <- function(file = NULL, clean_name = FALSE) {
  fasta.content <- readLines(file)

  ### find and process the names of the sequence.
  ### all the punctuated symbols will be replaced with _
  seq.name.index <- grep(">", fasta.content)
  seq.name <- gsub(">", "", fasta.content[seq.name.index])
  if (clean_name) {
    seq.name <- gsub("^[_]+|[_]+$| ", "", gsub(
      "^[[:space:]]+|[[:space:]]+$",
      "", gsub("[[:punct:]]", "_", seq.name)
    ))
  }

  ### obtain the sequence
  seq.start.index <- seq.name.index + 1
  seq.end.index <- c(seq.name.index, length(fasta.content) + 1)[-1] - 1

  seq.text <- rep(NA, length(seq.name.index))

  ### find the starting and ending of each sequence, and concantented the lines.
  for (i in 1:length(seq.name.index)) {
    seq.start <- seq.start.index[i]
    seq.end <- seq.end.index[i]
    seq.text[i] <- gsub(
      "[[:space:]]", "",
      paste(fasta.content[seq.start:seq.end],
        collapse = ""
      )
    )
  }

  res <- data.frame(seq.name, seq.text)
  return(res)
}
