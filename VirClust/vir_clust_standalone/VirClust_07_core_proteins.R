## ----Librarries------------------------------------------------
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)

## ----paths and other parameters---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/virclust_buttons/dataButtons/P1___minimum_6_chara___minimum_6_char___20221209131722"
# input0[2] <- "pc_type=PC"


#projdir ----
projdir_c <- str_which(input0, "^projdir=")
projdir <- str_remove(input0[projdir_c], "^projdir=")
rm(projdir_c)

#pc_type ----
pc_type_c <- str_which(input0, "^pc_type=")
pc_type <- str_remove(input0[pc_type_c], "^pc_type=")   
rm(pc_type_c, input0)

#load input DFs ----

if(pc_type == "PC")
{
  in_protDF_p <- paste0(projdir, "/02/02_04_genome_protDF_PCs.RDS")
  in_virDF_p <- paste0(projdir, "/04a-06a_genome_clustering_PC/05/virDF.RDS")
  P_D <- paste0(projdir, "/07/core_a")
}

if(pc_type == "PSC")
{
  in_protDF_p <- paste0(projdir, "/03/03_09_genome_protDF_PCs_PSCs.RDS")
  in_virDF_p <- paste0(projdir, "/04b-06b_genome_clustering_PSC/05/virDF.RDS")
  P_D <- paste0(projdir, "/07/core_b")
}

if(pc_type == "PSSC")
{
  in_protDF_p <- paste0(projdir, "/03C/03C_09_genome_protDF_PSCs_PSSCs.RDS")
  in_virDF_p <- paste0(projdir, "/04c-06c_genome_clustering_PSSC/05/virDF.RDS")
  P_D <- paste0(projdir, "/07/core_c")
}

#dir.create(P_D, recursive = TRUE)
dir_indiv_protsAnnots <- paste0(P_D, "/all_proteins_indiv")
dir.create(dir_indiv_protsAnnots, recursive = TRUE)

protDF <- readRDS(in_protDF_p)
virDF <- readRDS(in_virDF_p) %>%
  select(genome_name, genome_cluster_ID)

rm(in_protDF_p, in_virDF_p)

if(pc_type == "PC")
{
  protDF <- protDF %>%
    mutate(protein_cluster_ID = PC_ID)
}
if(pc_type == "PSC")
{
  protDF <- protDF %>%
    mutate(protein_cluster_ID = PSC_ID)
}
if(pc_type == "PSSC")
{
  protDF <- protDF %>%
    mutate(protein_cluster_ID = PSSC_ID)
}


#merge DFs
protDF_treeclust <- left_join(protDF, virDF) %>%
  mutate(tree_clust_ID = genome_cluster_ID) %>%
  group_by(tree_clust_ID) %>%
  nest()

rm(protDF, virDF)


## ----finding core genes -----------------------------------------------------

find_core_prot_fun <- function(inDF, genome_cluster_ID, path)
{
  
  core_prots_p <- paste0(path, "/genome-cluster-", genome_cluster_ID, "_core-proteins.faa")
  core_protsAnnots_p <- paste0(path, "/genome-cluster-", genome_cluster_ID, "_core-proteins_for_annots.faa")
  core_prots_DF_p <- paste0(path, "/genome-cluster-", genome_cluster_ID, "_table-core-proteins.tsv")
  core_prots_RDS_p <- paste0(path, "/genome-cluster-", genome_cluster_ID, "_table-core-proteins.RDS")
  
  phage_names <- inDF$genome_name %>%
    as.factor() %>%
    levels()
  
  if(length(phage_names) > 1)
  {     
    stats_presenceinALL <- inDF %>%
      dplyr::count(protein_cluster_ID, genome_name) %>%
      select(-n)%>%
      dplyr::count(protein_cluster_ID) %>%
      filter(n == length(phage_names))
    
    
    if(nrow(stats_presenceinALL) > 0)
    {
      inDF[inDF$protein_cluster_ID %in% stats_presenceinALL$protein_cluster_ID, "core_protein"] <- "yes"
      inDF[!inDF$protein_cluster_ID %in% stats_presenceinALL$protein_cluster_ID, "core_protein"] <- "no"
      
      coreDF <- inDF %>%
        filter(core_protein == "yes") %>%
        mutate(clusterID_prot_name =  paste0(pc_type, "-", protein_cluster_ID, "__", protein_name)) %>%
        arrange(protein_cluster_ID) %>%
        select(-protein_cluster_ID) %>%
        mutate(export_path = paste0(dir_indiv_protsAnnots, "/", protein_ID, ".faa"))
      
      write.table(x = coreDF, file = core_prots_DF_p, sep = "\t", row.names = FALSE, col.names = TRUE)
      saveRDS(object = coreDF, file = core_prots_RDS_p)
      
      todel <- mapply(write.fasta, sequences = coreDF$protein_seq, names = coreDF$clusterID_prot_name, file.out = core_prots_p, open = "a", as.string = TRUE)
      todel <- mapply(write.fasta, sequences = coreDF$protein_seq, names = coreDF$protein_ID, file.out = core_protsAnnots_p, open = "a", as.string = TRUE)
      todel <- mapply(write.fasta, sequences = coreDF$protein_seq, names = coreDF$protein_ID, file.out = coreDF$export_path, open = "w", as.string = TRUE)
  
      rm(coreDF, todel)
    }
    rm(stats_presenceinALL)
  }
  rm(core_prots_p, core_prots_DF_p, phage_names)
  
  return(core_protsAnnots_p)
}

prot_paths <- mapply(find_core_prot_fun, inDF=protDF_treeclust$data, genome_cluster_ID=protDF_treeclust$tree_clust_ID, path=P_D)

#make one file with all core prots, for annots
system(paste0("cat ", paste(prot_paths, collapse = " "), " > ", paste0(P_D, "/core_prots_for_annots_all.faa")))

rm(prot_paths)



