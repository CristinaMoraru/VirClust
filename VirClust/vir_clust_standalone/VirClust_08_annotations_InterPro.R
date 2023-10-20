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
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/virclust_buttons/dataButtons/P18___minimum_6_cha___minimum_6_char___20221226195036/"
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
interproscan <- get_params_simple_fun(input0, param_name="annot_path")
rm(input0)

P_D <- ret_annots_dir_fun(prot_type, projdir)
dir.create(P_D)

#load inputs ----
prot_p <- multiFasta_AnnotProt_path_fun(prot_type, projdir)
prot_DF <- protDFAnnot_fun(prot_type, projdir)


#InterProScan ----

print("Starting InterProScan searches in InterPro database.")
interpro_D <- paste0(P_D, "/interpro")
reset_dir_fun(interpro_D)

int_out <- paste0(interpro_D, "/interproscan_out")

interpro_st <- sys::exec_wait(cmd = paste0(interproscan, "/interproscan.sh"), #assumes interpro path param is given, works only for conda and rhea
                              # complete path Rhea: "/opt/InterProScan/interproscan.sh"
                              ##complete path singularity: /virclust/bin/InterProScan/interproscan.sh
                              args = c("-i", prot_p,
                                       "-b", int_out,
                                       "-iprlookup", "-pa",
                                       "-cpu", cpu
                              ), 
                              std_out = paste0(interpro_D, "/stdout.txt"), 
                              std_err = paste0(interpro_D, "/stderr.txt"))


#interproscan_cmd <- paste0(InterProScan, " -i ", prot_p, " -b ", ips_base_p," -iprlookup -pa -cpu ", cpu, " > ", errs_p)
check_exec_st_fun(interpro_st, "InterProScan")
rm(interpro_st)


#bring results
if(file.size(paste0(int_out, ".tsv")) > 0)
{
  interpro_DF <- read.csv(file = paste0(int_out, ".tsv"), header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  colnames(interpro_DF) <- c("protein_ID", "IP_MD5", "IP_seq_length", "IP_analysis", "IP_signature_accession", "IP_signature_description", "IP_start", 
                             "IP_end", "IP_eval", "IP_status", "IP_Date", "InterPro annotations_accession", "InterPro annotations_description", "IP_comments")
  
  
  interpro_DF <- interpro_DF %>%
    filter(IP_signature_description != "") %>%
    select(-IP_MD5, -IP_seq_length, -IP_status, -IP_Date) %>%
    filter(IP_analysis != "MobiDBLite") %>%
    filter(str_detect(IP_signature_description, "Domain of unknown function") != TRUE)
  
  if(nrow(interpro_DF) > 0)
  {
  interpro_DF_j <- left_join(interpro_DF, prot_DF)
  
  #save RDS
  saveRDS(interpro_DF, paste0(interpro_D, "/interpro_TB.RDS"))
  saveRDS(interpro_DF_j, paste0(interpro_D, "/interpro_TB_joined.RDS"))
  
  #save tsv
  write.table(x = interpro_DF, file = paste0(interpro_D, "/interpro_TB.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(x = interpro_DF, file = paste0(interpro_D, "/interpro_TB_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  }
}
print("Ending InterProScan searches in InterPro database.")

