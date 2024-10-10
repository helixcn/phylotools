#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL: http://github.com/helixcn/phylotools
#### date: 26 MAY 2015
#### modified: 8 DEC 2017

### Create fasta files based on the groups specified
split_dat <- function(dat, ref_table) {
  colnames(ref_table) <- c("seq.name", "group")
  dat.merged <- merge(dat, ref_table, by = "seq.name", all.x = TRUE)

  ### save the ungrouped sequences first
  group.dat_i <- dat.merged[is.na(dat.merged$group), ][, -3]
  dat2fasta(group.dat_i, outfile = "ungrouped.fasta")

  ### deleted ungrouped sequences from the merged
  dat.merged <- na.omit(dat.merged)
  group.name <- as.character(unique(dat.merged$group))

  ### generate fasta files according to the groups
  for (i in 1:length(group.name)) {
    group.name_i <- group.name[i]
    group.dat_i <- dat.merged[as.character(dat.merged$group) == group.name_i, ][, -3]
    dat2fasta(group.dat_i, outfile = paste(group.name_i, ".fasta", sep = ""))
  }
  cat(paste("splitted fasta files have been saved to: \n", getwd(), "\n"))
}
