library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(stats, warn.conflicts = FALSE, quietly = FALSE)
library(ape, warn.conflicts = FALSE, quietly = FALSE)
library(pvclust, warn.conflicts = FALSE, quietly = FALSE)
library(purrr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(zip, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
#library(furrr)
#library(future)


## ---PATHS and OPTIONS---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P867_Roseo_round4_CLM"
# input0[2] <- "pc_type=PSC"
# input0[3] <- "boot_pv=no"
# input0[4] <- "clust_dist=0.95"
# input0[5] <- "max_cols_HT=3000"
# input0[6] <- "aglom=complete"

###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)

#paths and params
P_D <-  get_params_simple_fun(input0, param_name = "projdir")
boot_pv <-  get_params_simple_fun(input0, param_name = "boot_pv")
pc_type <-  get_params_simple_fun(input0, param_name = "pc_type")
aglom <-  get_params_simple_fun(input0, param_name = "aglom")
clust_dist <- get_params_simple_fun(input0, param_name = "clust_dist", type = "numeric")
max_cols_HT <- get_params_simple_fun(input0, param_name = "max_cols_HT", type = "Int")


abort_p <- paste0(P_D, "/script5_", pc_type, ".txt")
if(file.exists(abort_p)){file.remove(abort_p)}

#### New paths and load DFs
if(pc_type == "PC")
{
  in_rds <- paste0(P_D, "/02/02_04_genome_protDF_PCs.RDS")
  heatmap_dir <- paste0(P_D, "/04a-06a_genome_clustering_PC")
}

if(pc_type == "PSC")
{
  in_rds <- paste0(P_D, "/03/03_09_genome_protDF_PCs_PSCs.RDS")
  heatmap_dir <- paste0(P_D, "/04b-06b_genome_clustering_PSC")
}

if(pc_type == "PSSC")
{
  in_rds <- paste0(P_D, "/03C/03C_09_genome_protDF_PSCs_PSSCs.RDS")
  heatmap_dir <- paste0(P_D, "/04c-06c_genome_clustering_PSSC")
}


out_dir <- paste0(heatmap_dir, "/05")
reset_dir_fun(out_dir)

protDF <- readRDS(in_rds)
rm(in_rds)

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

in_dir <- paste0(heatmap_dir, "/04")

hDF_p <- paste0(in_dir, "/unord_hDF.RDS")
hDF <- readRDS(hDF_p)
rm(hDF_p)

statsPC_p <- paste0(in_dir, "/statsPCs.RDS")   #from statsPCs the singlestons have already been removed in step 4
stats_PCs <- readRDS(statsPC_p)
rm(statsPC_p)
PCs_keep <- stats_PCs$protein_cluster_ID

if(boot_pv == "yes")
{
  pv_p <- paste0(in_dir, "/pv.RDS")
  pv <- readRDS(pv_p)
  hc <- pv$hclust
  rm(pv_p, pv)
}else
{
  hc_p <- paste0(in_dir, "/hc.RDS")
  hc <- readRDS(hc_p)
  rm(hc_p)
}


##remove singletons from hDF

hDF <- hDF %>%
  select(PCs_keep[1:length(PCs_keep)])
rm(PCs_keep)

if(ncol(hDF) == 1)   ##if there is only one PC in common between multiple genomes, the statistics can't proceed or the split cannot proceed?
{
  print(paste0("Only one ", pc_type, "was found in all genomes. The remaining proteins are singletons. Aborting this step"))
  write(x = "aborted", file = abort_p)
  quit()
}

## ---- make VirDF - input LENGTH ---
virDF <- data.frame(stringsAsFactors = FALSE,
                    genome_name = row.names(hDF))

name_ls <- list.files(path = paste0(P_D, "/00/00_out/"), full.names = FALSE)
name_DF <- data.frame(stringsAsFactors = FALSE, full_name = name_ls) %>%
  mutate(name = str_remove(full_name, "\\.f[an][sa]t*a*$")) %>%
  mutate(path = paste0(P_D, "/00/00_out/", full_name)) %>%
  select(name, path)

virDF <- left_join(virDF, name_DF, by = c("genome_name"="name"))
rm(name_ls, name_DF)

virDF$length <- lapply(virDF$path, read.fasta, seqtype = "DNA") %>%
  getLength()


## ---- calculate ALL/single genes/PCs ----
protDF_clust <- protDF %>%
  mutate(protein_cluster_ID = paste0(pc_type, "_", protein_cluster_ID))

all_PCs <- protDF_clust$protein_cluster_ID %>%
  as.factor() %>%
  levels()

singPCs <- setdiff(all_PCs, stats_PCs$protein_cluster_ID)
rm(all_PCs, stats_PCs)

protDF_clust <- protDF_clust%>%
  group_by(genome_name) %>%
  nest() #%>%
#mutate(genome_name = str_remove(genome_name, "\\.fna$"))

virDF_join <- left_join(virDF, protDF_clust)
rm(protDF_clust, virDF)

virDF_join$gene_count <- lapply(virDF_join$data, nrow) %>%
  unlist()

count_singPCs_fun <- function(DF, singPCs)
{
  DF_t <- DF %>%
    filter(protein_cluster_ID %in% singPCs) %>%
    filter(protein_cluster_ID != paste0(pc_type, "_"))  ##this is to remove any proteins not assigned to PCs / PSCs. Can there be prots not assigned to PCs? Because there should be none not assigned to PSCs, I have solved that problem.
  
  if(nrow(DF_t) > 0)
  {
    return(nrow(DF_t))
  }else
  {return(0)}
}

virDF_join$Single_Proteins <- lapply(virDF_join$data, count_singPCs_fun, singPCs = singPCs) %>%
  unlist()
virDF_join <- virDF_join %>%
  select(-data)

rm(singPCs)

## Calc PCs per genome in each GC ------
clust_DF <- cutree(hc, h=clust_dist) %>%
  as.data.frame(stringsAsFactors=FALSE)
colnames(clust_DF) <- "genome_cluster_ID"
clust_DF$genome_name <- row.names(clust_DF)


hDF$genome_name <- row.names(hDF)
hDF_GC <- left_join(hDF, clust_DF) %>%             #this hDF is grouped on GCs
  group_by(genome_cluster_ID) %>%
  nest()

hDF_GC_disp <- hDF_GC

hDF_temp <- hDF %>%                            #this DF here has 1 whenever a PC is found in a genome, regardless of how many times is found
  select(-genome_name)
hDF_temp[hDF_temp > 0] <- 1
hDF_temp$genome_name <- row.names(hDF_temp)

PCs_inclust_pergenome_fun <- function(genome, DF2, DF3, hDF_temp2)
{
  ## for the current genome, calculate if there are replicated PCs in the same genome and if they have hits in the same tree cluster
  DF3_sel <- DF3 %>%
    filter(genome_name == genome) %>%
    select(-genome_name)
  col_rn <- which(DF3_sel[1,] > 1)
  
  if(length(col_rn) > 0)
  {
    multiPCs <- colnames(DF3_sel)[col_rn]
    
    if(nrow(DF2) > 1)
    {
      extra_pcs <- 0    ## this will tell the number of extraPC which are replicates and have hits in this tree cluster (one count of the replicate is ignored)
      
      for(i in 1:length(multiPCs))
      {
        pcs <- DF3[, multiPCs[i]] %>%
          sum()
        if(DF3_sel[1, multiPCs[i]] < pcs)
        {
          extra_pcs <- extra_pcs + DF3_sel[1, multiPCs[i]] -1
        }
        rm(pcs)
      }
      rm(i)
    }
  }
  
  ###removes all PCs that don't belong to the current genome
  DF2_sel <- DF2 %>%
    select(which(DF2[genome, ] > 0))
  
  if(length(DF2_sel) > 0)
  {
    if(nrow(DF2) > 1)
    {
      ## for the current genome, calculate the number of PCs with hits in the same tree cluster
      DF2_sum <- DF2_sel %>%
        summarise(across(.cols=everything(), .fns = sum))
      Pcs_sum <- DF2_sum[DF2_sum > 1] %>%   ### remove all PCs that don't have hits with any other genomes in the current cluster
        length()
      if(length(col_rn > 0))
      {
        Pcs_sum <- Pcs_sum + extra_pcs
      }
      
      
      
      ##for the current genome, how many PCs hit ONLY outside the same tree cluster
      outPCs_DF <- DF2_sum[,DF2_sum == 1]
      
      outPCs <- length(outPCs_DF)
      if(outPCs > 0 & length(col_rn) > 0)
      {
        names_outPcs <- colnames(outPCs_DF)
        rep_outPcs <- intersect(multiPCs, names_outPcs)
        if(length(rep_outPcs) > 0)
        {
          for(i in 1:length(rep_outPcs))
          {
            extra_outPcs <- DF3_sel[1, rep_outPcs[i]] - 1
            outPCs <- outPCs + extra_outPcs
            rm(extra_outPcs)
          }
          rm(i)
        }
        rm(names_outPcs, rep_outPcs)
      }
      rm(outPCs_DF)
      
      ## for the current genome, how many PCs hit in this tree cluster and also outside this tree cluster (includes the ONLY outside this tree cluster proteins)
      cols_in_this_cluster <- colnames(DF2_sel)
      hDF_temp3 <- hDF_temp2 %>%
        select(cols_in_this_cluster[1:length(cols_in_this_cluster)])
      #hDF_temp3_sum <- summarize_each(hDF_temp3, funs(sum))
      hDF_temp3_sum <- hDF_temp3 %>%
        summarise(across(.cols = everything(), .fns = sum))
      checkSum <- hDF_temp3_sum - DF2_sum
      checkSum <- checkSum[, checkSum>0]
      Proteins_shared_also_Out_Own_GC <- length(checkSum)
      
      if(Proteins_shared_also_Out_Own_GC > 0  & length(col_rn) > 0)
      {
        names_Proteins_shared_also_Out_Own_GC <- colnames(checkSum)
        rep_Proteins_shared_also_Out_Own_GC <- intersect(names_Proteins_shared_also_Out_Own_GC, multiPCs)
        
        if(length(rep_Proteins_shared_also_Out_Own_GC) > 0)
        {
          for(i in 1:length(rep_Proteins_shared_also_Out_Own_GC))
          {
            extra_outPcs <- DF3_sel[1, rep_Proteins_shared_also_Out_Own_GC[i]] - 1
            Proteins_shared_also_Out_Own_GC <- Proteins_shared_also_Out_Own_GC + extra_outPcs
            rm(extra_outPcs)
          }
          rm(i)
        }
        rm(names_Proteins_shared_also_Out_Own_GC, rep_Proteins_shared_also_Out_Own_GC)
      }
      
      rm(cols_in_this_cluster, hDF_temp3, hDF_temp3_sum, checkSum)
      
      
      ## return DF
      returnDF <- data.frame(stringsAsFactors = FALSE, genome_name =genome,
                             Proteins_shared_inOwn_GC = Pcs_sum, Proteins_shared_only_Out_Own_GC = outPCs,
                             Proteins_shared_also_Out_Own_GC = Proteins_shared_also_Out_Own_GC, Proteins_shared_only_In_Own_GC = (Pcs_sum + outPCs - Proteins_shared_also_Out_Own_GC))
    }else
    {
      Proteins_shared_inOwn_GC <- sum(DF3_sel)
      
      ## how many PCs hit also outside this tree cluster
      cols_in_this_cluster <- colnames(DF2_sel)
      hDF_temp3 <- hDF_temp2 %>%
        select(cols_in_this_cluster[1:length(cols_in_this_cluster)])
      hDF_temp3_sum <- hDF_temp3 %>%
        summarise(across(.cols = everything(), .fns = sum))
      
      Proteins_shared_also_Out_Own_GC <- hDF_temp3_sum[,hDF_temp3_sum > 1] %>%
        length()
      
      if(Proteins_shared_also_Out_Own_GC > 0 & length(col_rn) > 0)
      {
        names_Proteins_shared_also_Out_Own_GC <- colnames(hDF_temp3_sum)
        rep_Proteins_shared_also_Out_Own_GC <- intersect(names_Proteins_shared_also_Out_Own_GC, multiPCs)
        
        if(length(rep_Proteins_shared_also_Out_Own_GC) > 0)
        {
          for(i in 1:length(rep_Proteins_shared_also_Out_Own_GC))
          {
            extra_outPcs <- DF3_sel[1, rep_Proteins_shared_also_Out_Own_GC[i]] - 1
            Proteins_shared_also_Out_Own_GC <- Proteins_shared_also_Out_Own_GC + extra_outPcs
            rm(extra_outPcs)
          }
          rm(i)
        }
        rm(names_Proteins_shared_also_Out_Own_GC, rep_Proteins_shared_also_Out_Own_GC)
      }
      
      ## return DF
      returnDF <- data.frame(stringsAsFactors = FALSE, genome_name =genome,
                             Proteins_shared_inOwn_GC = Proteins_shared_inOwn_GC, Proteins_shared_only_Out_Own_GC = 0,
                             Proteins_shared_also_Out_Own_GC = Proteins_shared_also_Out_Own_GC, Proteins_shared_only_In_Own_GC = 0)
      
    }
  }else
  {
    ## return DF
    returnDF <- data.frame(stringsAsFactors = FALSE, genome_name =genome,
                           Proteins_shared_inOwn_GC = NA, Proteins_shared_only_Out_Own_GC = 0,
                           Proteins_shared_also_Out_Own_GC = 0, Proteins_shared_only_In_Own_GC = NA)
  }
  
  return(returnDF)
}

PCs_inclust_fun <- function(DF, hDF_temp)
{
  DF_temp <- DF %>%
    as.data.frame(stringsAsFactors = FALSE)
  rownames(DF_temp) <- DF_temp$genome_name
  DF_temp2 <- DF_temp %>%
    select(-genome_name)
  DF_temp2[DF_temp2 > 0] <- 1
  
  PCs_stats_DF <- lapply(row.names(DF_temp2), PCs_inclust_pergenome_fun, DF2 = DF_temp2, DF3 = DF_temp, hDF_temp2 = hDF_temp) %>%
    bind_rows()
  
  return(PCs_stats_DF)
}

hDF_GC$data2 <- map(hDF_GC$data, PCs_inclust_fun, hDF_temp = hDF_temp)    #here, new stats are calculated for each GC
rm(hDF_temp)
PCs_inclust_DF <- bind_rows(hDF_GC$data2)           #here I have a df with all the stats calc above
rm(hDF_GC)

virDF_join <- left_join(virDF_join, PCs_inclust_DF) %>%
  left_join(clust_DF)
rm(PCs_inclust_DF, clust_DF)

virDF_join[is.na(virDF_join[,"Proteins_shared_inOwn_GC"]) == TRUE, "Proteins_shared_inOwn_GC"] <- 0 #virDF_join[is.na(virDF_join[,"Proteins_shared_inOwn_GC"]) == TRUE, "gene_count"]
virDF_join[is.na(virDF_join[,"Proteins_shared_only_In_Own_GC"]) == TRUE, "Proteins_shared_only_In_Own_GC"] <- 0# virDF_join[is.na(virDF_join[,"Proteins_shared_only_In_Own_GC"]) == TRUE, "gene_count"]  - virDF_join[is.na(virDF_join[,"Proteins_shared_only_In_Own_GC"]) == TRUE, "Proteins_shared_also_Out_Own_GC"]

virDF_join <- virDF_join %>%
  select(-path) %>%
  mutate(Proteins_shared = gene_count - Single_Proteins) %>%
  select(genome_cluster_ID, genome_name, length, gene_count, Proteins_shared, Single_Proteins, Proteins_shared_inOwn_GC, Proteins_shared_only_In_Own_GC, Proteins_shared_also_Out_Own_GC,
         Proteins_shared_only_Out_Own_GC)

#calculate silhouette stats ----

dist_MA_p <- paste0(in_dir, "/MyDistPCs_MA.RDS")
dist_MA <- readRDS(dist_MA_p)
rm(dist_MA_p)

sil <- cluster::silhouette(x = cutree(hc, h=clust_dist), dist = dist_MA)

if(length(sil) > 1)
{
  sil_TB <- tibble(
    genome_name = hc$labels,
    genome_cluster_ID =sil[,1],
    silhouette_neighbour = sil[,2],
    silhouette_width = round(sil[,3], digits = 2)
  )
  
  virDF_join <- left_join(virDF_join, sil_TB)
  
  rm(sil_TB)
}
rm(dist_MA, sil)

###save VirDF

virDF_p <- paste0(out_dir, "/virDF.RDS")
saveRDS(virDF_join, virDF_p)

virDF_tsv_p <- paste0(out_dir, "/virDF.tsv")
write.table(x = virDF_join, file = virDF_tsv_p, sep = "\t", row.names = FALSE, col.names = TRUE)

rm(protDF, virDF_tsv_p, virDF_p)

###find common PCs/PSCs in Genome Cluster -------
##to plot only them  in heatmap, if the number of P(S)Cs is too big
sum_fun <- function(DF)
{
  DF_t <- DF %>%
    column_to_rownames(var = "genome_name")
  
  DF_t[DF_t>1] <- 1
  sum_DF <- summarise(.data=DF_t, across(.cols = starts_with(pc_type), sum))
  
  th <- (nrow(DF_t)*75/100)
  
  PC_names <- colnames(sum_DF[,which(sum_DF >= th)])
  
  return(PC_names)
}

hDF <- hDF %>%
  select(-genome_name)

sumALL_DF <- map(.x = hDF_GC_disp$data, .f = sum_fun)

display_PCs <- flatten(sumALL_DF) %>%
  unlist() %>%
  unique()

hDF_reduced <- hDF[, display_PCs]

if((ncol(hDF) -1) > max_cols_HT)
{
  hDF_HT <- hDF_reduced
}else
{
  hDF_HT <- hDF
}
rm(sumALL_DF, hDF_reduced, display_PCs, hDF_GC_disp)

#cluster PCs (columns)  - heatmap hDF ######
matT <- hDF_HT %>%
  t()
hcT <- dist(matT, method = "binary") %>%
  hclust(method = aglom)
col_order <- row.names(matT)
col_order2 <- col_order[hcT$order]
hDF_HT <- hDF_HT[, col_order2]

DF_p <- paste0(out_dir, "/heatmapDF.RDS")
saveRDS(hDF_HT, DF_p)

rm(matT, DF_p, hcT, col_order, col_order2, hDF_HT)


#cluster PCs (columns) and order full hDF ######

matT <- hDF %>%
  t()
hcT <- dist(matT, method = "binary") %>%
  hclust(method = aglom)
col_order <- row.names(matT)
col_order2 <- col_order[hcT$order]
hDF <- hDF[hc$order, col_order2]   #both genomes and P(S)Cs ordered

DF_p <- paste0(out_dir, "/ord_hDF.RDS")
saveRDS(hDF, DF_p)

tsv_p <- paste0(out_dir, "/ord_hDF.tsv")
write.table(x = hDF%>%rownames_to_column("genome_name"), file = tsv_p, sep = "\t", row.names = FALSE, col.names = TRUE)

rm(matT, DF_p, hcT, col_order, col_order2, hc, tsv_p)



#create distance value DFs for each genome cluster
dist_dir <- paste0(heatmap_dir, "/05/VGCs_dist")
dir.create(dist_dir, recursive = TRUE)

dist_df <- readRDS(file = paste0(heatmap_dir, "/04/MyDistPCs_MA.RDS")) %>%
  rownames_to_column("genome_name")

VGC_df <- virDF_join %>%
  group_by(genome_cluster_ID) %>%
  nest()

for(i in 1:nrow(VGC_df))
{
  df <- VGC_df$data[[i]]%>%
    column_to_rownames("genome_name")
  
  dist_df_VGC <- dist_df[, c("genome_name", rownames(df))] %>%
    filter(genome_name %in% rownames(df)) %>%
    column_to_rownames("genome_name")
  
  if(nrow(dist_df_VGC) > 1)
  {
    dist_df_VGC <- dist_df_VGC[rownames(df),]
    
    hc_VGC <- dist_df_VGC  %>%
      as.data.frame() %>%
      as.dist() %>%
      hclust(method = aglom)
    
    dist_df_VGC <- dist_df_VGC[hc_VGC$order, hc_VGC$order]
    df <- df[rownames(dist_df_VGC),]
    rm(hc_VGC)
  }
  
  #save dist DF for each VGC
  saveRDS(object = dist_df_VGC, file = paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i], "_dist.RDS"))
  write.table(x = dist_df_VGC, file = paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i], "_dist.tsv"), sep = "\t", row.names = TRUE, col.names = TRUE)
  
  #save genome length df for each VGC
  saveRDS(object = df, file = paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i], "_stats.RDS"))
  write.table(x = df%>%rownames_to_column("genome_name"), file = paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i], "_stats.tsv"), sep = "\t", row.names = FALSE, col.names = TRUE)
  rm(df)
  
  #save fasta file for each VGC
  VGC_dir <- paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i])
  reset_dir_fun(VGC_dir)
  
  save_tb <- tibble(genome_name = row.names(dist_df_VGC),
                    fna_from_p = paste0(P_D, "/00/00_out/", genome_name, ".fasta"),
                    fna_to_p = paste0(VGC_dir, "/", genome_name, ".fna"))
  
  
  todel <- mapply(FUN = file.copy, from= save_tb$fna_from_p, to =save_tb$fna_to_p)
  
  all_files <- paste0(save_tb$fna_from_p, collapse = " ")
  
  all_p <- paste0(dist_dir, "/VGC_", VGC_df$genome_cluster_ID[i], "_all_genomes.fna")
  if(file.exists(all_p))file.remove(all_p)
  
  cat_cmd <- paste0("cat ", all_files, " > ", all_p)
  system(cat_cmd)
  
  rm(all_files, all_p, cat_cmd, dist_df_VGC, save_tb, todel)
}
rm(i)


#prepare zip for download
tozip <- dist_dir
outzip <- paste0(out_dir, "/VGCs.zip")
zipr(zipfile = outzip, files = tozip)

print("Step split genome tree and calculate stats has finished.")