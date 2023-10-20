## ----Librarries------------------------------------------------
library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
library(zip, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)

## ----paths and other parameters---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/virclust_buttons/dataButtons/P2___minimum_6_chara___minimum_6_charac___20221214185649/"
# input0[2] <- "prot_type=all_PCs"

###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)

#paths and params ----
projdir <- get_params_simple_fun(input0, param_name = "projdir")
prot_type <- get_params_simple_fun(input0, param_name = "prot_type")
rm(input0)

P_D <- ret_annots_dir_fun(prot_type, projdir)
#dir.create(P_D)

outputs_d <- paste0(P_D, "/all_outputs")
if(dir.exists(outputs_d)){unlink(outputs_d, recursive = TRUE)}
dir.create(outputs_d)

#load input protDFs ----
protDF <- protDFAnnot_fun(prot_type, projdir)

#rm(prot_type)

#Annots TBs paths:
p_TB <- tibble(DB_paths = c("/BlastP_NR/blastp_TB.RDS",
                            "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS",
                            "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS",
                            "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS",
                            "/hmmscan_Efam/hmmscan_Efam.RDS",
                            "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS")) %>%
  mutate(DB_paths = paste0(P_D, "/", DB_paths))

p_TB$exist <- lapply(p_TB$DB_paths, FUN = file.exists) %>%
  unlist()
p_TB <- p_TB %>%
  filter(exist == TRUE)

if(nrow(p_TB) > 0)
{
  all_annots_ls <- lapply(X = p_TB$DB_paths, readRDS)
  prot_annots_DF <- protDF
  
  if(length(all_annots_ls) > 0)
  {
    
    for (i in 1:length(all_annots_ls))
    {
      prot_annots_DF <- left_join(prot_annots_DF, all_annots_ls[[i]])
    }
    rm(i, all_annots_ls)
    
    
    ### prepare final outputs ----
    saveRDS(object = prot_annots_DF, file = paste0(outputs_d, "/annots_TB_", prot_type, ".RDS"))
    write.table(x = prot_annots_DF, file = paste0(outputs_d, "/annots_TB_", prot_type, ".tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  }
  
  ##interpro results separately
  interpro_RDS_p <- paste0(P_D, "/interpro/interpro_TB.RDS")
  
  if(file.exists(interpro_RDS_p))
  {
    interPro_TB <- readRDS(interpro_RDS_p)
    interpro_annots_DF <- left_join(prot_annots_DF, interPro_TB)
    rm(interPro_TB, interpro_RDS_p)
    
    saveRDS(object = interpro_annots_DF, file = paste0(outputs_d, "/annots_and_interpro_TB_", prot_type, ".RDS"))
    write.table(x = interpro_annots_DF, file = paste0(outputs_d, "/annots_and_interpro_TB_", prot_type, ".tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
    rm(interPro_TB)
    }
  
  rm(interpro_RDS_p)
}else
{
  ##interpro results separately
  interpro_RDS_p <- paste0(P_D, "/interpro/interpro_TB.RDS")
  
  if(file.exists(interpro_RDS_p))
  {
    interPro_TB <- readRDS(interpro_RDS_p)
    interpro_annots_DF <- left_join(protDF, interPro_TB)
    rm(interPro_TB, interpro_RDS_p)
    
    saveRDS(object = interpro_annots_DF, file = paste0(outputs_d, "/annots_and_interpro_TB_", prot_type, ".RDS"))
    write.table(x = interpro_annots_DF, file = paste0(outputs_d, "/annots_and_interpro_TB_", prot_type, ".tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
    rm(interPro_TB)
  }
  
  rm(interpro_RDS_p)
}

#prepare zip for download
tozip <- outputs_d
outzip <- paste0(outputs_d, "/all_outputs.zip")
zipr(zipfile = outzip, files = tozip)

print("Merge annotations step has finished.")