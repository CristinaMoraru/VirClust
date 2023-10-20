library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)

input0 <- as.character(commandArgs(trailingOnly = TRUE))

#manual run
#input0 <- ""
#input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P438___CLM_test___Anne_Ben_ph___20211111130848"


in1 <- str_which(input0, "^projdir=")

projdir<- str_remove(input0[in1], "^projdir=")
rm(in1)
step0_d <- paste0(projdir, "/00/00_in")

out_d <- paste0(projdir, "/00/00_out/")
dir.create(out_d, recursive = TRUE)


fasta_p <- list.files(step0_d, full.names = TRUE)
if(length(fasta_p)==1)
{
  mfasta <- read.fasta(file = fasta_p, seqtype = "DNA", as.string = TRUE)
  
  df <- data.frame(stringsAsFactors = FALSE,
                   names = getAnnot(object = mfasta) %>%
                     unlist(),
                   seqs = getSequence(mfasta, as.string = TRUE) %>%
                     unlist()) %>%
    mutate(genomeID = str_remove(names, "^>")) %>%
    mutate(genomeID = str_replace_all(genomeID, " ", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "\t", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, ":", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "\\(", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "\\)", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "\\/", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "-", "_")) %>%
    mutate(genomeID = str_replace_all(genomeID, "\\|", "_")) %>%
    mutate(exp_path = paste0(out_d, genomeID, ".fasta"))
  
 todel <-  mapply(write.fasta, sequences = df$seqs, names = df$genomeID, file.out = df$exp_path, open = "w", as.string = TRUE)
 rm(todel)
}

