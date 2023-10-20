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
dbs <- get_params_simple_fun(input0, param_name="annot_path")
rm(input0)

P_D <- ret_annots_dir_fun(prot_type, projdir)
dir.create(P_D)

#### DBs folder
# if(dir.exists(paste0(virclust_path, "/dbs")))
# {
#   dbs_path <- paste0(virclust_path,"/dbs")
# }else
# {
#   dbs_path <- "/opt/dbs"
# }
#if(sing == "conda")
#{
  dbs_path <- dbs
#}

#load inputs ----
prot_p <- dir_AnnotProt_path_fun(prot_type, projdir)
prot_DF <- protDFAnnot_fun(prot_type, projdir) %>%
  mutate(prot_file_p = paste0(prot_p, "/", protein_ID, ".faa"))


#hhsearch_pVOGs ----

print("Starting hhsearch searches in pVOGs database.")
pvogs_d <- paste0(P_D, "/hhsearch_pVOGs/")
reset_dir_fun(pvogs_d)

pvogs_out_d <- paste0(pvogs_d, "/outputs")
dir.create(pvogs_out_d)

pVOGs_DF <- prot_DF %>%
  select(protein_ID, prot_file_p) %>%
  mutate(hrr_p = paste0(pvogs_out_d, "/", protein_ID, ".hrr")) %>%
  mutate(hhsearch_tsv_p = paste0(pvogs_out_d, "/", protein_ID, ".tsv")) %>%
  mutate(hhsearch_log_p = paste0(pvogs_out_d, "/", protein_ID, ".log")) %>%
  mutate(hhsearch_err_p = paste0(pvogs_out_d, "/", protein_ID, ".err"))
rm(pvogs_out_d)

##do hhsearch
pVOGs_hhsuite_DB <- paste0(dbs_path, "/pVOGs/pVOGs-hhsuite_db/pVOGs_align_msa")
todel <- pmap(.l=pVOGs_DF[2:6], .f = hhsearch_fun, hhsuite_DB=pVOGs_hhsuite_DB)
rm(todel, pVOGs_hhsuite_DB)

##bring results
pVOGs_hhsuite_DB_Table_p <- paste0(dbs_path, "/pVOGs/VOGTable_pVOGs.txt")
pVOGs_hhsuite_DB_Table <- read.csv(file = pVOGs_hhsuite_DB_Table_p, header = FALSE, sep = "\t", stringsAsFactors = FALSE) %>%
  rename(hhsearch_pVOGs_ID=V1)
rm(pVOGs_hhsuite_DB_Table_p)

res_hhsearch_pVOGs<- map(.x = pVOGs_DF$hhsearch_tsv_p, .f = import_hhsuite_VOGDB_res_fun) 

#pVOGs_DF <- pVOGs_DF[1:20,]
pVOGs_DF$hhsearch_pVOGs_ID  <- lapply(res_hhsearch_pVOGs, "[[", 1) %>%  unlist()
#pVOGs_DF$hhsearch_pVOGs_probability  <- lapply(res_hhsearch_pVOGs, "[[", 2) %>%  unlist()
pVOGs_DF$hhsearch_pVOGs_score  <- lapply(res_hhsearch_pVOGs, "[[", 2) %>%  unlist()
pVOGs_DF$hhsearch_pVOGs_log_eval  <- lapply(res_hhsearch_pVOGs, "[[", 3) %>%  unlist()
rm(res_hhsearch_pVOGs)

pVOGs_DF <- left_join(pVOGs_DF, pVOGs_hhsuite_DB_Table) %>%
  select(-hrr_p, -hhsearch_tsv_p, -hhsearch_log_p, -hhsearch_err_p, -prot_file_p, -V3, -V4, -V5, -V6, -V8) %>%
  rename(pVOGs_host_domain = V2, pVOGs_annots = V7) %>%
  replace_na(replace = list(pVOGs_host_domain = "no_hit", pVOGs_annots = "no_hit"))
rm(pVOGs_hhsuite_DB_Table)

pVOGs_DF_j <- left_join(pVOGs_DF, prot_DF)

#save RDS
saveRDS(object = pVOGs_DF, file = paste0(pvogs_d, "/hhsearch_pVOGs_TB.RDS"))
saveRDS(object = pVOGs_DF_j, file = paste0(pvogs_d, "/hhsearch_pVOGs_TB_joined.RDS"))

#save tsv
write.table(x = pVOGs_DF, file = paste0(pvogs_d, "/hhsearch_pVOGs_TB.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(x = pVOGs_DF_j, file = paste0(pvogs_d, "/hhsearch_pVOGs_TB_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)

rm(pVOGs_DF, pvogs_d)
print("Ending hhsearch searches in pVOGs database.")


