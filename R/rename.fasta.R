#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015

rename.fasta <- function(infile = NULL, ref_table,
                         outfile = "renamed.fasta"){
    fasta <- read.fasta(infile)
    ## Convert the input ref to dataframe
    ## usually the file was obtained by
    ## save the result of get.names.fasta to a csv file.
    ## edit the csv file, and provide the name to be replaced.

    colnames(ref_table)  <- c("old_name", "new_name")
    res <- merge(x = fasta, y = ref_table, by.x = "seq.name",
                 by.y = "old_name", all.x = TRUE) ### Order of the sequence will change because of merge
    rename <- rep(NA, nrow(res))

    ### if the sequence was not found in the ref_table,
    ### keep the old name

    for(i in 1:nrow(res)){
       rename[i] <- ifelse(is.na(res$new_name[i]),
                           paste("old_name", "_",as.character(res$old_name[i]), sep = ""),
                           as.character(res$new_name[i]))
    }
    writeLines(paste(">", rename, "\n", as.character(res$seq.text), sep = ""), outfile )
    cat(paste(outfile, "has been saved to ", getwd(), "\n" ))
}
