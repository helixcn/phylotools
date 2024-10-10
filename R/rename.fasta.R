#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015
#### Revised: 9 OCT 2024

rename.fasta <- function(infile = NULL, ref_table,
                         outfile = "renamed.fasta") {
  fasta <- read.fasta(infile)
  ## Convert the input ref to dataframe
  ## usually the file was obtained by
  ## save the result of get.names.fasta to a csv file.
  ## edit the csv file, and provide the name to be replaced.

  colnames(ref_table) <- c("old_name", "new_name")
  
  names_fasta <- fasta[,1]
  
  names_to_add <- names_fasta[!names_fasta %in% ref_table$old_name]
  
  old_name <- append(ref_table$old_name, names_to_add)
  new_name <- append(ref_table$new_name, names_to_add)
  
  ref_table <- data.frame(old_name, new_name)
  
  res <- merge(
    x = fasta, y = ref_table, by.x = "seq.name",
    by.y = "old_name", all.x = TRUE
  ) ### Order of the sequence will change because of merge

  writeLines(paste(">", as.character(res$new_name), "\n", as.character(res$seq.text), sep = ""), outfile)
  cat(paste(outfile, "has been saved to ", getwd(), "\n"))
}
