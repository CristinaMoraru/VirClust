library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(stats, warn.conflicts = FALSE, quietly = FALSE)
library(ape, warn.conflicts = FALSE, quietly = FALSE)
library(pvclust, warn.conflicts = FALSE, quietly = FALSE)
library(zip, warn.conflicts = FALSE, quietly = FALSE)
#library(purrr)
#library(furrr)
#library(future)


## ----paths---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)

#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P228___CLMoraru___Test_Zob_PSC_PSSC_opt___20210603063814"
# input0[2] <- "boot_pv=no"
# input0[3] <- "pc_type=PC"
# input0[4] <- "bootstrap_no=100"
# input0[5] <- "cpu=20"
# input0[6] <- "aglom=complete"


#parameters
in1 <- str_which(input0, "projdir=")
P_D <- str_remove(input0[in1], "projdir=")


boot_pv_c <- str_which(input0, "^boot_pv=")
boot_pv <- str_remove(input0[boot_pv_c], "^boot_pv=")
rm(boot_pv_c)

bootstrap_no_c <- str_which(input0, "^bootstrap_no=")
bootstrap_no <- str_remove(input0[bootstrap_no_c], "^bootstrap_no=") %>%
  as.numeric()
rm(bootstrap_no_c)

pc_type_c <- str_which(input0, "^pc_type=")
pc_type <- str_remove(input0[pc_type_c], "^pc_type=")   
rm(pc_type_c)

aglom_n <- str_which(input0, "^aglom=")
aglom <- str_remove(input0[aglom_n], "^aglom=")
rm(aglom_n)

cpu_n <- str_which(input0, "^cpu=")
cpu <- str_remove(input0[cpu_n], "^cpu=") %>%
  as.integer()
rm(cpu_n)

rm(input0, in1)


## ---- load protDF -------------------------------------------------------

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
## ----  output folder

if(dir.exists(heatmap_dir)){unlink(heatmap_dir, recursive = TRUE)}
dir.create(heatmap_dir)
out_dir <- paste0(heatmap_dir, "/04")
if(dir.exists(out_dir)){unlink(out_dir, recursive = TRUE)}
dir.create(out_dir)


#### Make bipartite heatmap DF ####
hDF <- protDF %>%
  select(genome_name, protein_cluster_ID) %>%
  mutate(genome_name = str_remove(genome_name,"\\.fna$")) %>%
  dplyr::count(genome_name, protein_cluster_ID) %>%
  mutate(protein_cluster_ID = paste0(pc_type, "_", as.numeric(protein_cluster_ID))) %>%
  rename(counts_ingenome = n)

rm(protDF)

#stats_PCs will keep only PCs found in more than 1 genome, necessary for VirClust_05
stats_PCs <- hDF %>%
  dplyr::count(protein_cluster_ID) %>%
  filter(n > 1) %>%
  arrange(n)

statsPC_p <- paste0(out_dir, "/statsPCs.RDS")
saveRDS(stats_PCs, statsPC_p)
rm(statsPC_p, stats_PCs)

hDF <- hDF %>%
  spread(protein_cluster_ID, counts_ingenome) %>%
  replace(is.na(.), 0) %>%
  arrange(genome_name) %>%
  as.data.frame(stringsAsFactors = FALSE)

####here, dereplicate hDF by ---> NEW IDEAS!!!!
# hDF_derep <- group_by(hDF, across(starts_with("P"))) %>%
#   nest() %>%
#   mutate(genome_name =)
# 
# 
# extract_replicated_genomes_fun <- function(df)
# {
#   "I need to make lists of the names of each group of viral genomes which have the same PCs, and thus, are identical"
#   pivot_wider() instead of spread() !!!!!!!!!!!!!!!!!!!!1
#   #find a dplyr function to glue all genome names from a group in a list???? / string separated with ; or what? samething to save on the HDD
#   return a name for each group: if it is only a single genome, than return its name; if the group is from multiple genomes, then return the name of the first genome
#   and save the list on the HDD for the use
#   
#   keep only the dereplicated hDF
#   }




rownames(hDF) <- hDF$genome_name
hDF <- hDF %>%
  select(-genome_name)

hDF_p <- paste0(out_dir, "/unord_hDF.RDS")
saveRDS(hDF, hDF_p)
rm(hDF_p)

###### Functions: My distance between phages based on protein clusters ######
myPC_distMA_fun <- function(data)
{
  
  myPC_pairwise_dist_fun <- function(genome1, genome2, data)
  {
    phage_ls <- c(genome1, genome2)
    
    tempDF <- data[,phage_ls] 
    tempDF <- tempDF[tempDF[,1] != 0 | tempDF[,2] != 0,]                      # keep only PCs present at least in one genome
    
    
    if(nrow(tempDF) > 0)
    {
      tempDF[tempDF>=1] <- 1                                                    # if more than one PC copy per genome, set score to 1
      
      tempDF$sum <- tempDF[,1] + tempDF[,2]
      tempDF$sum[tempDF$sum <2] <- 0
      tempDF$sum[tempDF$sum ==2] <- 1
      
      PC_A <- sum(tempDF[,1]) # here can be speed optimized, by calculating only onces for all genomes, and saving data in a DF
      PC_B <- sum(tempDF[,2]) # here can be speed optimized, by calculating only onces for all genomes, and saving data in a DF
      PC_com <- sum(tempDF[,3])
      
      pairwise_dist <- round(1-((2*PC_com)/(PC_A + PC_B)), digits = 2)
    }else
    {
      pairwise_dist <- 1
    }
    
    
    # pairwise_dist_jaccard <- 1 - (PC_com/(PC_A + PC_B - PC_com))
    
    # out_ls <- c(pairwise_dist, pairwise_dist_jaccard)
    
    return(pairwise_dist)
  }
  library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
  library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
  library(purrr, warn.conflicts = FALSE, quietly = FALSE)
  library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
  
  
  mydistDF <- matrix(nrow = ncol(data), ncol = ncol(data)+1) %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(mydistDF)[1] <- "genome_name"
  mydistDF$genome_name <- colnames(data)
  colnames(mydistDF)[2:ncol(mydistDF)] <- colnames(data) 
  
  mydistDF_gathered <- mydistDF %>%
    replace(is.na(.), 1) %>%
    gather(subjects, "n", 2:ncol(mydistDF)) %>%
    select(genome_name, subjects)
  
  mydistDF_gathered$myPC_dist <- map2(.x = mydistDF_gathered$genome_name, .y = mydistDF_gathered$subjects, .f = myPC_pairwise_dist_fun, data=data) %>%
    unlist()
  
  mydistDF_spread <- mydistDF_gathered %>%
    spread(subjects, myPC_dist)
  row.names(mydistDF_spread) <- mydistDF_spread$genome_name
  mydistDF_spread <- mydistDF_spread %>%
    select(-genome_name)
  
  mydistDF <- as.dist(mydistDF_spread)    # !!!! for this function to work correctly, the order of the rows needs to be the same as the columns
  
  return(mydistDF)
}

#### Calculate and save distance matrix ----
hDF_T <- t(hDF) %>%
  as.data.frame(strigsAsFactors = FALSE)

myPC_distMA <- myPC_distMA_fun(hDF_T) 

myPC_distMA_DF <- myPC_distMA%>%
  as.matrix() %>%
  as.data.frame(stringsAsFactors = FALSE)

myDist_p <- paste0(out_dir, "/MyDistPCs_MA.RDS")
saveRDS(myPC_distMA_DF, myDist_p)

myDist_tsvp <- paste0(out_dir, "/MyDistPCs_MA.tsv")
write.table(x = myPC_distMA_DF, file = myDist_tsvp, sep = "\t", row.names = TRUE, col.names = TRUE)

#### Calculate and save Hclust tree

hc <- myPC_distMA %>%
  hclust(method = aglom)
tree_hc <- as.phylo(hc)

hc_p <- paste0(out_dir, "/hc.RDS")
saveRDS(hc, hc_p)

tree_hc_p <- paste0(out_dir, "/hc_tree.newick")
write.tree(phy = tree_hc, file = tree_hc_p, append = FALSE)

rm(hc_p, tree_hc_p, tree_hc)


### Order and save the distance matrix based on the Hclust tree
myPC_distMA_DF_ord <- myPC_distMA_DF[hc$order, hc$order] 

myDistOrd_p <- paste0(out_dir, "/MyDistPCs_MA_ordered.RDS")
saveRDS(myPC_distMA_DF_ord, myDistOrd_p)

myDistOrd_tsvp <- paste0(out_dir, "/MyDistPCs_MA_ordered.tsv")
write.table(x = myPC_distMA_DF_ord %>% rownames_to_column("genome_name"), file = myDistOrd_tsvp, sep = "\t", row.names = FALSE, col.names = TRUE)

rm(myDistOrd_p, myDistOrd_tsvp, myPC_distMA_DF_ord, hc)

#### Calculate and save Pvclust tree ----
if(boot_pv == "yes")
{
  pv <- NULL
  cnt <- 0 
  while (is.null(pv)) { 
    pv <- tryCatch(
      {
        cnt <- cnt + 1
        pvclust(data = hDF_T, method.hclust = aglom, method.dist = myPC_distMA_fun, nboot = bootstrap_no, store = FALSE, parallel = cpu)
      },
      error = function(cond) {
        message("Catched PV-Error, retry ", cnt)
        message(cond)
        if (cnt > 4) { 
          print("Bootstrapping failed, due to difficulties with the parallelization module. Please try step 4 again.")
          message("Give up"); break}
        #return (") ###NOT needed
        return(NULL)
      }
    )
  }
  rm(cnt)
  
  
  
  #save pv object
  pv_p <- paste0(out_dir, "/pv.RDS")
  saveRDS(pv, pv_p)
  
  #function to save tree with bootstrap values
  as.phylo.hclust.with.nodenames <- function (x, nodenames, ...) #We add a nodenames argument
  {
    N <- dim(x$merge)[1]
    edge <- matrix(0L, 2 * N, 2)
    edge.length <- numeric(2 * N)
    node <- integer(N)
    node[N] <- N + 2L
    cur.nod <- N + 3L
    j <- 1L
    for (i in N:1) {
      edge[j:(j + 1), 1] <- node[i]
      for (l in 1:2) {
        k <- j + l - 1L
        y <- x$merge[i, l]
        if (y > 0) {
          edge[k, 2] <- node[y] <- cur.nod
          cur.nod <- cur.nod + 1L
          edge.length[k] <- x$height[i] - x$height[y]
        }
        else {
          edge[k, 2] <- -y
          edge.length[k] <- x$height[i]
        }
      }
      j <- j + 2L
    }
    if (is.null(x$labels)) 
      x$labels <- as.character(1:(N + 1))
    node.lab <- nodenames[order(node)] #Here modified by CLM, bp=clust_stats, to define our node labels
    obj <- list(edge = edge, edge.length = edge.length/2, tip.label = x$labels, 
                Nnode = N, node.label = node.lab) #And you put them in the final object
    class(obj) <- "phylo"
    reorder(obj)
  }
  
  clust_stats <- (round(pv$edges,2)*100)[,c("si", "au", "bp")]
  
  out_dir <- paste0(out_dir, "/pv_trees")
  dir.create(out_dir)
  
  #bp tree
  tree_bp_pv <- as.phylo.hclust.with.nodenames(pv$hclust, nodenames = clust_stats[,"bp"])
  tree_bp_pv_p <- paste0(out_dir, "/pv_tree_bp.newick")
  write.tree(phy = tree_bp_pv, file = tree_bp_pv_p, append = FALSE)
  
  #au tree
  tree_au_pv <- as.phylo.hclust.with.nodenames(pv$hclust, nodenames = clust_stats[,"au"])
  tree_au_pv_p <- paste0(out_dir, "/pv_tree_au.newick")
  write.tree(phy = tree_au_pv, file = tree_au_pv_p, append = FALSE)
  
  #si tree
  tree_si_pv <- as.phylo.hclust.with.nodenames(pv$hclust, nodenames = clust_stats[,"si"])
  tree_si_pv_p <- paste0(out_dir, "/pv_tree_si.newick")
  write.tree(phy = tree_si_pv, file = tree_si_pv_p, append = FALSE)
  
  zipr(zipfile = paste0(out_dir, ".zip"), files = out_dir)
  
  rm(pv, pv_p, tree_bp_pv, tree_bp_pv_p, tree_au_pv, tree_au_pv_p, tree_si_pv, tree_si_pv_p, clust_stats)
  
}

rm(hDF, hDF_T, myPC_distMA, myPC_distMA_DF, myDist_tsvp, myDist_p, boot_pv)

