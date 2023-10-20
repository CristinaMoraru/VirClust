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


#hhsearch_VOGDB ----
print("Starting hhsearch searches in VOGDB database.")
hhsearch_VOGDB_D <- paste0(P_D, "/hhsearch_VOGDB")
reset_dir_fun(hhsearch_VOGDB_D)

VOGDB_out_d <- paste0(hhsearch_VOGDB_D, "/outputs")
dir.create(VOGDB_out_d)

VOGDB_DF <- prot_DF %>%
  select(protein_ID, prot_file_p) %>%
  mutate(hrr_p = paste0(VOGDB_out_d, "/", protein_ID, ".hrr")) %>%
  mutate(hhsearch_tsv_p = paste0(VOGDB_out_d, "/", protein_ID, ".tsv")) %>%
  mutate(hhsearch_log_p = paste0(VOGDB_out_d, "/", protein_ID, ".log")) %>%
  mutate(hhsearch_err_p = paste0(VOGDB_out_d, "/", protein_ID, ".err"))

rm(VOGDB_out_d)

##do hhsearch
VOGDB_hhsuite_DB <- paste0(dbs_path, "/VOGDB/VOGDB_hhsuite_DB/VOGDB_align__msa")
todel <- pmap(.l=VOGDB_DF[2:6], .f = hhsearch_fun, hhsuite_DB=VOGDB_hhsuite_DB)
rm(todel, VOGDB_hhsuite_DB)

##import results from hhsearch out files
res_hhsearch_VOGDB<- map(.x = VOGDB_DF$hhsearch_tsv_p, .f = import_hhsuite_VOGDB_res_fun) 

VOGDB_DF$VOGDB_ID  <- lapply(res_hhsearch_VOGDB, "[[", 1) %>%  unlist()
VOGDB_DF$hhsearch_VOGDB_score  <- lapply(res_hhsearch_VOGDB, "[[", 2) %>%  unlist()
VOGDB_DF$hhsearch_VOGDB_eval  <- lapply(res_hhsearch_VOGDB, "[[", 3) %>%  unlist()
rm(res_hhsearch_VOGDB)

#asign VOGDB annotations

VOGDB_hhsuite_DB_Table_p <- paste0(dbs_path, "/VOGDB/VOGDB_annotations.tsv")
VOGDB_hhsuite_DB_Table <- read.csv(file = VOGDB_hhsuite_DB_Table_p, header = TRUE, sep = "\t", stringsAsFactors = FALSE) %>%
  rename(hhsearch_VOGDB_ID = X.GroupName)
rm(VOGDB_hhsuite_DB_Table_p)

VOGDB_DF <- left_join(VOGDB_DF, VOGDB_hhsuite_DB_Table, by=c("VOGDB_ID" = "hhsearch_VOGDB_ID")) %>%
  select(-hrr_p, -hhsearch_tsv_p, -prot_file_p, -hhsearch_log_p, -hhsearch_err_p, -ProteinCount, -SpeciesCount) %>%
  rename(VOGDB_functional_category = FunctionalCategory, VOGDB_annot = ConsensusFunctionalDescription) %>%
  replace_na(replace = list(VOGDB_functional_category = "no_hit", VOGDB_annot = "no_hit")) 
rm(VOGDB_hhsuite_DB_Table)

VOGDB_DF_j <- left_join(VOGDB_DF, prot_DF)

#save RDS
saveRDS(object = VOGDB_DF, file = paste0(hhsearch_VOGDB_D, "/hhsearch_VOGDB_TB.RDS"))
saveRDS(object = VOGDB_DF_j, file = paste0(hhsearch_VOGDB_D, "/hhsearch_VOGDB_TB_joined.RDS"))

#save tsv
write.table(x = VOGDB_DF, file = paste0(hhsearch_VOGDB_D, "/hhsearch_VOGDB_TB.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(x = VOGDB_DF_j, file = paste0(hhsearch_VOGDB_D, "/hhsearch_VOGDB_TB_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)

rm(VOGDB_DF, hhsearch_VOGDB_D)
print("Ending hhsearch searches in VOGDB database.")

