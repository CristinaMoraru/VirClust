## ----Librarries------------------------------------------------
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
library(purrr, warn.conflicts = FALSE, quietly = FALSE)

## ----paths---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P370___CLMoraru___Zobelviridae_test_identity___20210928151301/"
# input0[2] <- "cpu=20"
# input0[3] <- "clust_PC=evalue"
# input0[4] <- "eval_PC=0,00001"
# input0[5] <- "bitsc_PC=50"
# input0[6] <- "cov_PC=100"
# input0[7] <- "pident_PC=100"

###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)

## Paths and params-----------------------------------------------
projdir <- get_params_simple_fun(input0, param_name = "projdir")

cpu <- get_params_simple_fun(input0, param_name = "cpu", type = "Int")
clust_PC <- get_params_simple_fun(input0, param_name = "clust_PC")
eval_PC <- get_params_simple_fun(input0, param_name = "eval_PC") %>%
  str_replace(",", ".") %>%
  as.numeric()

bitsc_PC <- get_params_simple_fun(input0, param_name = "bitsc_PC", type = "Int")
cov_PC <- get_params_simple_fun(input0, param_name = "cov_PC", type = "Int")
pident_PC <- get_params_simple_fun(input0, param_name = "pident_PC", type = "Int")

rm(input0)


P_D <- paste0(projdir, "/02")
if(dir.exists(P_D)){unlink(P_D, recursive = TRUE)}
dir.create(P_D)

ALL_prot_path <- paste0(projdir, "/01/01_all_proteins.faa")
protDF_p <- paste0(projdir, "/01/01_genome_protDF_PCs.RDS")
protDF <- readRDS(protDF_p)

## ----makeblastDB---------------------------------------------------------
blastDB_dir <- paste0(P_D, "/02_02_BlastDB")
if(dir.exists(blastDB_dir)==TRUE){unlink(blastDB_dir, recursive = TRUE)}
dir.create(blastDB_dir)

prot_blastDB_path <- paste0(blastDB_dir, "/All_prot_BlastDB")

makeBlast_st <- sys::exec_wait(cmd = "makeblastdb", 
                               args = c("-in", ALL_prot_path,
                                        "-input_type", "fasta", "-dbtype", "prot",
                                        "-out", prot_blastDB_path,
                                        "-title", prot_blastDB_path), 
                               std_out = paste0(blastDB_dir, "/makeBlastDB_log.txt"), 
                               std_err = paste0(blastDB_dir, "/makeBlastDB_err.txt"))
check_exec_st_fun(st = makeBlast_st, who= "MakeBlastDB")

rm(blastDB_dir, makeBlast_st)

## ----blastp--------------------------------------------------------------
blastp_dir <- paste0(P_D, "/02_03_Blastp_out")
if(dir.exists(blastp_dir)==TRUE){unlink(blastp_dir, recursive = TRUE)}
dir.create(blastp_dir)

blastp_out_path <- paste0(blastp_dir, "/ALL_prot_blastp_out.txt")
outfmt<- "6 qseqid sseqid evalue bitscore qstart qend sstart send qlen slen pident"

blastp_st <- sys::exec_wait(cmd = "blastp",
                                  args = c("-db", prot_blastDB_path,
                                           "-query",  ALL_prot_path,
                                           "-out", blastp_out_path,
                                           "-outfmt", outfmt,
                                           "-evalue", eval_PC,
                                           "-max_target_seqs", (nrow(protDF)*2),
                                           "-word_size", "2",
                                           "-num_threads", cpu),
                                  std_out = paste0(blastp_dir, "/blastp_log.txt"),
                                  std_err = paste0(blastp_dir, "/blastp_err.txt"))

# blastp_st <- system2(command = "blastp", 
#                                   args = paste0(" -db ", prot_blastDB_path,
#                                            " -query ",  ALL_prot_path, 
#                                            " -out ", blastp_out_path,
#                                            " -outfmt ", outfmt, 
#                                            " -evalue ", eval_PC,
#                                            " -max_target_seqs ", (nrow(protDF)*2),
#                                            " -word_size", " 2",
#                                            " -num_threads ", cpu), 
#                                   stdout = paste0(blastp_dir, "/blastp_log.txt"), 
#                                   stderr = paste0(blastp_dir, "/blastp_err.txt"))

print(paste0("Blast_st is ", blastp_st))
check_exec_st_fun(st = blastp_st, who= "BLASTp")
rm(outfmt, prot_blastDB_path, ALL_prot_path, blastp_st)

## ----convert to mcl input------------------------------------------------

blastp_DF <- read.csv2(blastp_out_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE) %>%
  mutate(qcov = ((V6-V5+1)*100)/V9,
         scov = ((V8-V7+1)*100)/V10,
         bitscore = as.numeric(V4),
         evalue = V3,
         evalue_log10 = -log10(as.numeric(V3)),
         pident = as.numeric(V11)) %>%
  filter(qcov >= cov_PC, scov >= cov_PC) %>%
  filter(bitscore >= bitsc_PC) %>%
  filter(pident >= pident_PC) %>%
  mutate(evalue_log10_capped = if_else(condition = {evalue_log10 > 200}, 
                                       true = 200,
                                       false = evalue_log10))



if(clust_PC == "evalue")
{
  for_mcl_DF <-  blastp_DF %>%
    select(V1, V2, evalue)
}

if(clust_PC == "evalue_log")
{
  for_mcl_DF <-  blastp_DF %>%
    select(V1, V2, evalue_log10_capped)
}

if(clust_PC == "bitscore")
{
  for_mcl_DF <-  blastp_DF %>%
    select(V1, V2, bitscore)
}

if(clust_PC == "norm_bitscore")
{
  blastp_DF$norm_bitscore <- map2(.x = blastp_DF$V1, .y = blastp_DF$V2, .f = norm_bitscore_fun, DF = blastp_DF) %>%
    unlist()
  
  for_mcl_DF <-  blastp_DF %>%
    select(V1, V2, norm_bitscore)
}


for_mcl_path <- paste0(blastp_dir, "/for_mcl.abc")
write.table(for_mcl_DF, for_mcl_path, sep = "\t", col.names = FALSE, row.names = FALSE)

rm(blastp_DF, blastp_out_path, for_mcl_DF)


## ----MCL----------------------------------------------------------------
mcl_dir <- paste0(P_D, "/02_04_mcl")
if(dir.exists(mcl_dir)==TRUE){unlink(mcl_dir, recursive = TRUE)}
dir.create(mcl_dir)
mcl_clust_path <- paste0(mcl_dir, "/mcl_out.txt")

mcl_st <- sys::exec_wait(cmd = "mcl", 
                         args = c(for_mcl_path,
                                  "-I", "2", "--abc", 
                                  "-o", mcl_clust_path), 
                         std_out = paste0(mcl_dir, "/stdout_mcl_clusters_.txt"), 
                         std_err = paste0(mcl_dir, "/stderr_mcl_clusters.txt"))
check_exec_st_fun(st = mcl_st, who= "MCL for PCs")
rm(mcl_dir, mcl_st, for_mcl_path)

## ----assigning_clusters_in_DFs-------------------------------------------
mcl_clust_DF <- read.csv(mcl_clust_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

for(i in 1:nrow(mcl_clust_DF))
{
  prot_ls <- mcl_clust_DF[i,] %>%
    as.list() %>%
    unname() %>%
    unlist()
  prot_ls <- prot_ls[prot_ls!=""]
  
  protDF[protDF[,"protein_ID"] %in% prot_ls, "PC_ID"] <- i
  rm(prot_ls)
}
rm(i)

rm(mcl_clust_path, mcl_clust_DF)

## save protDF
protDF_p <- paste0(P_D, "/02_04_genome_protDF_PCs.RDS")
if(file.exists(protDF_p)){file.remove(protDF_p)}
saveRDS(protDF, protDF_p)
rm(protDF_p)

protTSV_p <- paste0(P_D, "/02_04_genome_protDF_PCs.tsv")
write.table(x = protDF, file = protTSV_p, sep = "\t", row.names = FALSE, col.names = TRUE)
rm(protTSV_p)

print("Step 2, proteins to PCs has finished successfully.")