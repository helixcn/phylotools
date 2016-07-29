#### author: Jinlong Zhang <jinlongzhang01@gmail.com>
#### institution: Kadoorie Farm and Botanic Garden, Hong Kong
#### package: phylotools
#### URL： http://github.com/helixcn/phylotools
#### date: 26 MAY 2015
#### revised on: 29 July 2016

supermat <- function(infiles, 
             outfile = "supermat.out.phy", 
             partition.file = "gene_partition.txt"){

    infiles.type <- rep(NA, length(infiles))     
    seq.names <- c()                            
    dat <- list()                               
    
    for(i in 1:length(infiles)){                
        temp <- readLines(infiles[i])            
        if(any(grepl(">", temp))){              ### find if a ">" symbol exists, it is fasta file. 
            infiles.type[i] <- "fasta"
            dat.temp <- read.fasta(infiles[i])  
        } else {
            infiles.type[i] <- "phylip"         ### if no ">" symbol exists, it is a phylip file
            dat.temp <- read.phylip(infiles[i]) 
        }
        ### rename the column to avoid duplicate column names for the merge function
        colnames(dat.temp) <- c("seq.name", paste(infiles[i], ".seq.text", sep = "")) 
        dat[[i]] <- dat.temp                    
        seq.names <- c(seq.names, as.character(dat[[i]][,1]))  ### get all the names for sequences
    }
    
    ### Obtain the names of the genes
    gene.name <- gsub(".fas|.fasta|phylip|.phy", "", basename(infiles) )
    
    
    supermat.dat <- data.frame(seq.name = sort(unique(seq.names)))
    
    ### merge all the data.frame the supermat.dat
    ### since some of the sequences were not availble for some taxa, there are NA in the merged dataset
    ### these NAs must be replaced by the repeated "-" according to the length of the sequence.
    for(i in 1:length(dat)){
        supermat.dat <- merge(supermat.dat, dat[[i]], by = "seq.name", all.x = TRUE)
    }
    
    supermat.sequences <- rep("", nrow(supermat.dat))
    nsites <- c() 
    
    for(i in 2:ncol(supermat.dat)){  
        tem.sequences      <- as.character(supermat.dat[,i])  
        nsites[i]          <- max(na.omit(nchar(tem.sequences)))      
        ### if the sequence is NA, it will be replaced by the repeated "-". 
        ### times of repeat was determined by the the length of the longest squence of the same gene. 
        supermat.sequences <- paste(supermat.sequences,       
                                    ifelse(is.na(tem.sequences), 
                                           paste(rep("-", nsites[i]), collapse = ""), 
                                           as.character(tem.sequences)), sep = "")
    }
    
    nsites <- na.omit(nsites)
    
    ### create a RAxML partition file
    type = rep("DNA", length(infiles))
    site.start <- c(0, cumsum(nsites))
    site.start <- site.start[-length(site.start)] + 1
    site.end   <- c(cumsum(nsites))
    partition.dat <- data.frame(infiles, gene.name, nsites, type, site.start, site.end)
    partition.dat.vector <- paste(partition.dat$type, ", ",gene.name, " = ", 
                                   partition.dat$site.start, " - ",
                                   partition.dat$site.end, sep = "")
    writeLines(partition.dat.vector, partition.file )
    
    ### Create the super matrix
    res.super.dat <- data.frame(supermat.dat[,1], supermat.sequences)
    dat2phylip(res.super.dat, outfile)
    
    invisible(list(supermat.dat = supermat.dat, 
                   res.super.dat = res.super.dat, 
                   partition.dat = partition.dat, 
                   partition.dat.vector = partition.dat.vector))
    cat(paste("Supermatrix \"",outfile, "\" and RAxML partition file \"", 
        partition.file,"\" have been saved to: \n", getwd(), "\n", sep = ""))
}
