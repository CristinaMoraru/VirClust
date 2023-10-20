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

# hhsearch_PHROGS ----

print("Starting searches in PHROGS database.")
hhsearch_PHROGS_D <- paste0(P_D, "/hhsearch_PHROGS")
reset_dir_fun(hhsearch_PHROGS_D)

PHROGS_out_d <- paste0(hhsearch_PHROGS_D, "/outputs")
dir.create(PHROGS_out_d)

PHROGS_DF <- prot_DF %>%
  select(protein_ID, prot_file_p) %>%
  mutate(hrr_p = paste0(PHROGS_out_d, "/", protein_ID, ".hrr")) %>%
  mutate(hhsearch_tsv_p = paste0(PHROGS_out_d, "/", protein_ID, ".tsv")) %>%
  mutate(hhsearch_log_p = paste0(PHROGS_out_d, "/", protein_ID, ".log")) %>%
  mutate(hhsearch_err_p = paste0(PHROGS_out_d, "/", protein_ID, ".err"))

rm(PHROGS_out_d)

##do search
PHROGS_hhsuite_DB <- paste0(dbs_path, "/PHROGS/PHROGS_DB/PHROGS_align__msa")
todel <- pmap(.l=PHROGS_DF[2:6], .f = hhsearch_fun, hhsuite_DB=PHROGS_hhsuite_DB)
rm(todel, PHROGS_hhsuite_DB)


res_hhsearch_PHROGS <- map(.x = PHROGS_DF$hhsearch_tsv_p, .f = import_hhsuite_VOGDB_res_fun) 

PHROGS_DF$PHROGS_ID <- lapply(res_hhsearch_PHROGS, "[[", 1) %>% unlist()
PHROGS_DF$hhsearch_PHROGS_score <- lapply(res_hhsearch_PHROGS, "[[", 2) %>% unlist()
PHROGS_DF$hhsearch_PHROGS_eval <- lapply(res_hhsearch_PHROGS, "[[", 3) %>% unlist()
rm(res_hhsearch_PHROGS)

#assign PHROGS annots
PHROGS_annots_TB_p <- paste0(dbs_path, "/PHROGS/phrog_annot_v2.tsv")
PHROGS_annots_TB <- read.csv(file = PHROGS_annots_TB_p, header = TRUE, sep = "\t", stringsAsFactors = FALSE) %>%
  mutate(phrog = paste0("phrog_", phrog))
rm(PHROGS_annots_TB_p)

PHROGS_DF <- left_join(PHROGS_DF, PHROGS_annots_TB, by = c("PHROGS_ID" = "phrog")) %>%
  select(-prot_file_p, -hrr_p, -hhsearch_tsv_p, -hhsearch_log_p, -hhsearch_err_p, -color) %>%
  rename(PHROGS_functional_category = category, PHROGS_annot = annot) %>%
  replace_na(replace = list(PHROGS_functional_category = "no_hit", PHROGS_annot = "no_hit"))
rm(PHROGS_annots_TB)

PHROGS_DF_j <- left_join(PHROGS_DF, prot_DF)

#save RDS
saveRDS(object = PHROGS_DF, file = paste0(hhsearch_PHROGS_D, "/hhsearch_PHROGS_TB.RDS"))
saveRDS(object = PHROGS_DF_j, file = paste0(hhsearch_PHROGS_D, "/hhsearch_PHROGS_TB_joined.RDS"))

#save tsv
write.table(x = PHROGS_DF, file = paste0(hhsearch_PHROGS_D, "/hhsearch_PHROGS_TB.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(x = PHROGS_DF_j, file = paste0(hhsearch_PHROGS_D, "/hhsearch_PHROGS_TB_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)

rm(PHROGS_DF, hhsearch_PHROGS_D) 
print("Ending hhsearch searches in PHROGS database.")

