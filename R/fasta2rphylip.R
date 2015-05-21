fasta2rphylip <- function(infile, outfile = "out.phy"){
   read.fasta <- function (file = NULL) {
       if (is.null(file)) {
           stop("Please specify the input fasta file.")
       }
       result <- readLines(file)
       nameline <- result[grepl("[>]", result)]
       test <- regexpr(">", nameline) > 1
       if (any(test)) {
           warning(paste("\">\" in line(s)", which(test), 
           "\n appeared not at the beginning. 
            \n Please remove any character(s) before \">\"."))
       }
       result = result[grepl("[A-Za-z0-9]", result)]
       result <- ConvFas(result, "fas")
       if (any(regexpr(">", result[seq(1, length(result), by = 2)]) <
           0)) {
           xx <- 2 * which(regexpr(">", result[seq(1, length(result),
               by = 2)]) < 0)
           if (length(xx) > 10) {
               xx <- xx[1:10]
               stop(paste("readfasta could not find \">\" in row: \n",
                   paste(xx, collapse = ", "), "... \n", "Make sure the file ",
                   file, " is in fasta format.\n"))
           }
       }
       class(result) <- "fasta"
       return(result)
   }
   gseq.fas <- function (x = NULL)
   {
       if (!inherits(x, "fasta")) {
           stop("Make sure the data is a fasta object.")
       }
       if (is.null(x)) {
           stop("You have to specify the input data.")
       }
       result = x[!grepl(">", x)]
       result = gsub(">", "", result)
       return(result)
   }
   
   gnames.fas <- function (x = NULL)
   {
       if (!inherits(x, "fasta")) {
           stop("Make sure the data is a fasta object.")
       }
       if (is.null(x)) {
           stop("You have to specify the input data.")
       }
       result = x[grepl(">", x)]
       result = gsub(">", "", result)
       return(result)
   }
   
    ConvFas <-
    function (fil = NULL, type = c("fas", "nxs", "phy"))
    {
        match.arg(type)
        dna = fil
        if (type == "fas") {
            seqNamPos = grep("^>", dna)
            pos = c(seqNamPos, length(dna) + 1)
            seqNam = dna[seqNamPos]
        }
        if (type == "nxs") {
            dna = dna[(grep("matrix", dna, ignore.case = TRUE) +
                1):length(dna)]
            dna = dna[-which(dna == "" | dna == "end;" | dna == ";")]
            seqNam = unique(substr(dna, 1, regexpr(" ", dna) - 1))
        }
        if (type == "phy") {
            dna = dna[regexpr("[ATGC-]", dna) > 0]
            seqNam = substr(dna, 1, regexpr(" ", dna) - 1)
            seqNam = seqNam[-which(seqNam == "")]
        }
        nSeq = length(seqNam)
        for (i in 1:nSeq) {
            if (type == "fas") {
                st = pos[i] + 1
                ed = pos[i + 1] - 1
                stri = gsub(" ", "", paste(dna[st:ed], collapse = ""))
            }
            if (type == "nxs" | type == "phy") {
                nBlock = length(dna)/length(seqNam)
                rNam = ((1:nBlock) - 1) * nSeq + i
                stri = gsub("[ -]", "", gsub(seqNam[i], "", paste(dna[rNam],
                    collapse = "")))
                stri = toupper(stri)
            }
            Nam = paste(">", seqNam[i], sep = "")
            if (i == 1) {
                DNA = c(Nam, stri)
            }
            if (i > 1) {
                DNA = c(DNA, Nam, stri)
            }
        }
        DNA = gsub(">>", ">", DNA)
        class(DNA) <- "fasta"
        return(DNA)
    }
   
   dat2phy2 <-
   function (input, write = TRUE)
   {  
       input = dat.fasta
       row1.1 <- nrow(input)
       row1.2 <- nchar(as.character(input[1, 2]))
       row1 <- paste(row1.1, row1.2)
       space <- max(nchar(as.character(input[, 1]))) + 5 - nchar(as.character(input[, 1]))
       res <- c()
       for (i in 1:length(space)) {
           res[i] <- paste(input[i, 1], paste(rep(" ", space[i]),
               collapse = ""), input[i, 2])
       }
       res <- c(row1, res)
       return(res)
   }
   
   dat <- read.fasta(infile)
   dat.names <- gnames.fas(dat)
   #### Replace illegal characters
   dat.names2 <- gsub(" Concatenation of 2 sequences", "", dat.names)
   dat.names3 <- gsub("\\.", "_", dat.names2)
   dat.names3 <- gsub("\'", "", dat.names3)
   
   dat.seq <- gseq.fas(dat)
   dat.fasta <- data.frame(as.character(dat.names3), as.character(dat.seq))
   res <- dat2phy2(dat.fasta)
   writeLines(res, outfile)
}
