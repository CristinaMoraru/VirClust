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
prot_p <- multiFasta_AnnotProt_path_fun(prot_type, projdir)
prot_DF <- protDFAnnot_fun(prot_type, projdir)


#hmmscan_Efam_XC ----
hmmscan_Efam_XC_d <- paste0(P_D, "/hmmscan_Efam_XC")
reset_dir_fun(hmmscan_Efam_XC_d)

hmmscan_out <- paste0(hmmscan_Efam_XC_d, "/hmmscan_out_table.txt")

efam_xc_st <- sys::exec_wait(cmd = "hmmscan", 
                             args = c("--cpu", cpu,
                                      "-E", "0.01", 
                                      "--tblout", hmmscan_out,
                                      paste0(dbs_path, "/Efam_XC/xfam_xc"),
                                      prot_p
                             ), 
                             std_out = paste0(hmmscan_Efam_XC_d, "/hmmscan_out_log.txt"), 
                             std_err = paste0(hmmscan_Efam_XC_d, "/hmmscan_out_sdterr.txt"))

#hmmscan_cmd <-  paste0("hmmscan --cpu ", cpu, " -E 0.01 --tblout ", hmmscan_Efam_XC_out_p, " ", Efam_XC_db_p, " ", prot_p, " > ", hmmscan_Efam_XC_log_p)
check_exec_st_fun(efam_xc_st, "Efam-XC")


##brings annots
annots_p <- paste0(dbs_path, "/Efam_XC/Final_Super_Condensed_Annotations-updated_xfam_XC.tsv")
annots_DF <- read.csv(file = annots_p, header = TRUE, sep = "\t", skip = 0, stringsAsFactors = FALSE)
rm(annots_p)

#load results
if(file.size(hmmscan_out) > 0)
{
  efam__XC_DF <- read.csv(file = hmmscan_out, header = FALSE, sep = "", comment.char = "#",  stringsAsFactors = FALSE) %>%
    select(V1, V3, V5, V6, V7, V11) %>%
    rename(Efam_XC_ID = V1, protein_ID = V3, Efam_XC_eval = V5, Efam_XC_score = V6, Efam_XC_bias = V7, Efam_XC_exp = V11) %>%
    mutate(Efam_XC_ID = str_remove(Efam_XC_ID, "_")) %>%
    filter(Efam_XC_score > 40) %>%
    group_by(protein_ID) %>%
    nest()
  
  
  sel_Efam_XC_res_fun <- function(DF)
  {
    DF_temp <- DF %>%
      filter(Efam_XC_score == max(Efam_XC_score))
    return(DF_temp)
  }
  
  if(nrow(efam__XC_DF) > 0)
  {
    efam__XC_DF$data <- map(.x = efam__XC_DF$data, .f = sel_Efam_XC_res_fun)
    efam__XC_DF <-  efam__XC_DF%>%
      unnest(data) %>%
      left_join(annots_DF, by = c("Efam_XC_ID" = "Cluster")) %>%
      select(-Number.of.Proteins, -RANK.s.., -Proteins) %>%
      rename(Efam_XC_annotations = Annotation.s....Hit.s..)
    
    rm(annots_DF)
    
    efam__XC_DF_j <- left_join(efam__XC_DF, prot_DF)
    
    #save RDS
    saveRDS(object = efam__XC_DF, file = paste0(hmmscan_Efam_XC_d, "/hmmscan_Efam_XC.RDS"))
    saveRDS(object = efam__XC_DF_j, file = paste0(hmmscan_Efam_XC_d, "/hmmscan_Efam_XC_joined.RDS"))
    
    #save tsv
    write.table(x = efam__XC_DF, file = paste0(hmmscan_Efam_XC_d, "/hmmscan_Efam_XC.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
    write.table(x = efam__XC_DF_j, file = paste0(hmmscan_Efam_XC_d, "/hmmscan_Efam_XC_joined.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  #rm(efam__XC_DF, hmmscan_Efam_XC_d)
}
print("Ending hmmscan searches in Efam_XC database.")

