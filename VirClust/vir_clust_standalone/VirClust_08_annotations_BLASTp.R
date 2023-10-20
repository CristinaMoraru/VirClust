## ----Librarries------------------------------------------------
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
library(purrr, warn.conflicts = FALSE, quietly = FALSE)
library(readr, warn.conflicts = FALSE, quietly = FALSE)

## ----paths and other parameters---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P286_CLM/"
# input0[2] <- "prot_type=core_PCs"

###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)

#paths and params ----
projdir <- get_params_simple_fun(input0, param_name = "projdir")
prot_type <- get_params_simple_fun(input0, param_name = "prot_type")
cpu <- get_params_simple_fun(input0, param_name = "cpu", type = "Int")
virclust_path <- get_params_simple_fun(input0, param_name = "virclust_path")
#sing <- get_params_simple_fun(input0, param_name="sing")
blastdb <- get_params_simple_fun(input0, param_name="annot_path")
rm(input0)

P_D <- ret_annots_dir_fun(prot_type, projdir)
dir.create(P_D)

#load inputs ----
prot_p <- multiFasta_AnnotProt_path_fun(prot_type, projdir)
prot_DF <- protDFAnnot_fun(prot_type, projdir)

#BLASTP_NR ----

print("Starting BLASTP searches in NR database.")
blast_NR_D <- paste0(P_D, "/BlastP_NR")
reset_dir_fun(blast_NR_D)
blast_out_p <- paste0(blast_NR_D, "/blastp_NR_out.txt")

###do BLASTP
# if(sing %in% c("sing", "rhea"))
# {
#   NR_DB <- "/opt/blastdb/nr"
# }else
# {if(sing == "conda")
# {NR_DB <- blastdb}}
NR_DB <- blastdb

outfmt <- "6 qseqid sacc sallacc stitle salltitles qlen qstart qend slen sstart send length evalue bitscore pident mismatch staxid staxids qcovs"

##if its not working, try this path: /opt/ncbi-blast/bin/blastp  

# blastP_NR_cmd <- paste0("blastp -db ", NR_DB, " -query ", prot_p, " -task 'blastp' -evalue 0.0001 ", outfmt, 
#                         " -num_threads ", cpu, " -max_target_seqs 1000 -out ", blast_out_p)

blastP_NR_st <- sys::exec_wait(cmd = "blastp",
                               args = c("-db", NR_DB,
                                        "-query", prot_p,
                                        "-task", "blastp",
                                        "-evalue", "0.0001",
                                        "-outfmt", outfmt,
                                        "-num_threads", cpu,
                                        "-max_target_seqs", "1000",
                                        "-out", blast_out_p
                               ),
                               std_out = paste0(blast_NR_D, "/blastp_stdout.txt"),
                               std_err = paste0(blast_NR_D, "/blastp_stderr.txt"))

check_exec_st_fun(st = blastP_NR_st, who= "BLASTp")
rm(NR_DB, blastP_NR_st, outfmt)

###bring BLASTP results
if(file.size(blast_out_p) > 0)
{
  blast_NR_DF <- read.csv(blast_out_p, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  colnames(blast_NR_DF) <- c("qseqid", "sacc", "sallacc", "stitle", "salltitles", "qlen", "qstart", "qend", "slen", "sstart", "send", "length", "evalue", "bitscore", "pident", "mismatch", "staxid", "staxids", "qcovs")
  
  split_stitle <- str_split(blast_NR_DF[,"stitle"], "\\[")
  blast_NR_DF[,"product"] <- unlist(lapply(split_stitle, "[[", 1))
  rm(split_stitle)
  
  blast_NR_DF <- blast_NR_DF %>%
    filter(!str_detect(product, pattern = "[Hh]ypothetical")) %>%
    mutate(calc_qcov = (length*100)/qlen) %>%
    mutate(calc_scov = (length*100)/slen) %>%
    filter(calc_qcov>40) %>%
    filter(calc_scov>40) %>%
    filter(pident>30) %>%
    filter(bitscore>50)
  
  if(nrow(blast_NR_DF) > 0)
  {
    blast_NR_queries <- levels(as.factor(blast_NR_DF[,"qseqid"]))
    blast_TB <- tibble(protein_ID = blast_NR_queries,
                       blastp_NR_annot = "",
                       blastp_NR_evalue = "",
                       blastp_NR_bitscore = "")
    
    blastp_annots_fun <-  function(queries, DF)
    {
      sel_query_DF <-  DF %>%
        filter(qseqid == queries)
      row_max_bitscore <- which.max(sel_query_DF[,"bitscore"]) ## this function returns only one min value, even if there are multiple min values
      
      annot <- ""
      annot[1] <- sel_query_DF[row_max_bitscore,"product"]
      annot[2] <- sel_query_DF[row_max_bitscore,"evalue"]
      annot[3] <- sel_query_DF[row_max_bitscore,"bitscore"]
      
      rm(sel_query_DF, row_max_bitscore)
      
      return(annot)
    }
    
    res_ls <- map(.x = blast_NR_queries, .f = blastp_annots_fun, DF = blast_NR_DF) 
    blast_TB$blastp_NR_annot <- lapply(res_ls, "[[", 1) %>%
      unlist()
    
    blast_TB$blastp_NR_evalue <- lapply(res_ls, "[[", 2) %>%
      unlist()
    
    blast_TB$blastp_NR_bitscore <- lapply(res_ls, "[[", 3) %>%
      unlist()
    
    rm(blast_NR_DF, blast_NR_queries, res_ls)
    
    blast_TB_j <- left_join(blast_TB, prot_DF)
    
    #save TB
    saveRDS(blast_TB, paste0(blast_NR_D, "/blastp_TB.RDS"))
    saveRDS(blast_TB_j, paste0(blast_NR_D, "/blastp_TB_joined.RDS"))
    
    #save tsv
    write.table(x = blast_TB, file =  paste0(blast_NR_D, "/blastp_TB.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = blast_TB_j, file =  paste0(blast_NR_D, "/blastp_TB_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  #rm(blast_NR_D, blast_TB, blast_NR, blast_out_p)
}
print("Ending BLASTP searches in NR database.")

