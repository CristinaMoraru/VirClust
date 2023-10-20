library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(tibble, warn.conflicts = FALSE, quietly = FALSE)
library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)

## ------------------------------------------------------------------------
translate_genes <- function(start, end, strand, frame, numcode, genome)
{
  gene_seq <- stringr::str_sub(genome, start = start, end = end)
  
  # convert the sequence to RC if the strand is "-" and not "+"
  if(strand == "-")
  {
    gene_seq <- rev(comp(s2c(gene_seq)))
  }else
  {
    gene_seq <- s2c(gene_seq)
  }
  
  # translate DNA -> AA
  protein_seq <- c2s(translate(gene_seq, frame =frame, numcode = numcode))
  return(protein_seq)
}

## ----paths---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)

#manual run
# input0 <- ""
# input0[1] <- "projdir=/home/cristinam/ICBM-Essen_workflows/VirClust/test/pd1"
# input0[2] <- "gene_code=11"
# input0[3] <- "functions_path=/home/cristinam/ICBM-Essen_workflows/VirClust/virclust_scripts/VirClust_functions.R"


###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)


#params and paths
projdir <- get_params_simple_fun(input0, param_name = "projdir")
gene_code <- get_params_simple_fun(input0, param_name= "gene_code", type="Int")
print(paste0("Genetic code used ...", gene_code, ". Its type is ", typeof(gene_code)))

P_D <- paste0(projdir, "/01")
dir.create(P_D)

input_dir <- paste0(projdir, "/00/00_out")

faa_dir <- paste0(P_D, "/01_out_faa")
dir.create(faa_dir)

csv_dir <- paste0(P_D, "/01_out_csv")
dir.create(csv_dir)
dir.create(paste0(P_D, "/01_mga_err"))

## ----------------------------------------------------------------------------
input_fna <- list.files(path = input_dir, full.names = TRUE)

df <- data.frame(stringsAsFactors = FALSE,
                 input_fna = input_fna) %>%
  mutate(genome_name = str_remove(input_fna, "\\.fasta$")) %>%
  mutate(genome_name = str_remove(genome_name, "\\.fna$")) %>%
  mutate(genome_name = str_remove(genome_name, paste0(input_dir, "/"))) %>%
  mutate(output_faa = paste0(faa_dir, "/", genome_name, ".faa")) %>%
  mutate(output_csv = paste0(csv_dir, "/", genome_name, ".csv")) %>%
  mutate(output_mga = paste0(csv_dir, "/", genome_name, "___mga.csv")) %>%
  mutate(stderr_mga = paste0(P_D, "/01_mga_err/", genome_name, "___err.txt"))


## for loop to predict orfs and translate proteins for each genome --------------

for(i in 1:nrow(df))
{
  #mga_cmd <- paste0("mga -s ", df$input_fna[i], " > ", df$output_mga[i])
  #system(mga_cmd)
  df[i, "mga_status"] <- sys::exec_wait(cmd = "mga",
                                        args = c("-s", df$input_fna[i]),
                                        std_out = df$output_mga[i],
                                        std_err = df$stderr_mga[i])
  
  # df[i, "mga_status"] <- system2(command = "mga",
  #                               args = paste0(" -s ", df$input_fna[i]),
  #                               stdout = df$output_mga[i], 
  #                               stderr = df$stderr_mga[i])
  
  orfs_table <- data.table::fread (df$output_mga[i], skip = 3, header=FALSE, stringsAsFactors = FALSE)
  colnames(orfs_table) <- c("gene_ID","start_pos.","end_pos.","strand","frame",
                            "complete_partial","genscore","used_model",
                            "rbs_start","rbs_end","rbs_score")
  
  genome_TB_local <- orfs_table %>%
    mutate(genome_name = df$genome_name[i]) %>%
    mutate(protein_name = paste0(genome_name, "_", gene_ID)) %>%
    rename(gene_start = start_pos., gene_end = end_pos.) %>%
    mutate(protein_seq = "") %>%
    select(-complete_partial, -genscore, -used_model, -rbs_start, -rbs_end, -rbs_score)
  
  genome <- read.fasta(df$input_fna[i], as.string = TRUE)
  genome_sequence <- unlist(getSequence(genome, as.string = TRUE))
  
  genome_TB_local$protein_seq <- unlist(mapply(translate_genes, start=genome_TB_local$gene_start,  end= genome_TB_local$gene_end, strand=genome_TB_local$strand, 
                                               frame=genome_TB_local$frame, numcode=gene_code, genome=genome_sequence)) 
  
  genome_TB_local$protein_seq <- genome_TB_local$protein_seq %>%
    str_replace("\\*$", "")
  
  write.table(genome_TB_local, file = df$output_csv[i], sep = "\t", row.names = FALSE, col.names = TRUE, append = FALSE)
  
  write.fasta(sequences = as.list(genome_TB_local$protein_seq), names = as.list(genome_TB_local$protein_name), 
              file.out = df$output_faa[i], open = "w", as.string = TRUE)
  
  rm(orfs_table, genome, genome_sequence, genome_TB_local)
}
rm(i)

######check if all genomes have been translated
mga_st <- unique(df$mga_status)
if(length(mga_st) > 1 | (length(mga_st) == 1 & mga_st != 0))
{
  mga_report_DF <- df %>%
    filter(mga_status > 0)
  
  saveRDS(mga_report_DF, file = paste0(P_D, "/error_DF_01.RDS"))
  
  print("Incomplete protein prediction and translation in step 01. From ", nrow(df), " genomes, ", nrow(df), " were not processed. Aborting VirClust.")
  
  quit()
}else
{
  print("Protein prediction and translation has finished successfully.")
}
rm(mga_st)


####Export all proteins in a single FASTA, with protein ID ------
ALL_prot_path <- paste0(P_D, "/01_all_proteins.faa")
if(file.exists(ALL_prot_path)){file.remove(ALL_prot_path)}

ALL_prots_dir <- paste0(P_D, "/01_all_proteins_indiv")
if(dir.exists(ALL_prots_dir)){unlink(ALL_prots_dir, recursive = TRUE)}
dir.create(ALL_prots_dir)

prot_DF_ls <- lapply(df$output_csv, read.csv, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

protDF <- bind_rows(prot_DF_ls) %>%
  mutate(gene_length = gene_end - gene_start + 1) %>%
  mutate(protein_length = gene_length/3) %>%
  mutate(protein_length = round(protein_length, digits=0)) %>%
  mutate(strand = str_replace(strand, "\\+", "1")) %>%
  mutate(strand = str_replace(strand, "-", "-1")) %>%
  mutate(protein_ID = "") %>%
  mutate(PC_ID = "")


# give unique proteins IDs, short
d <- nrow(protDF) %>%
  as.character() %>%
  nchar()
d <- paste0("%0", d, "d") 
for(i in 1:nrow(protDF)){
  protDF$protein_ID[i] <- paste0("prot_", sprintf(d, i))
}
rm(d, i)

protDF <- protDF  %>%
  mutate(export_path = paste0(ALL_prots_dir, "/", protein_ID, ".faa"))


todel <- mapply(write.fasta, sequences = protDF$protein_seq, names = protDF$protein_ID, file.out = ALL_prot_path, open = "a", as.string = TRUE)
todel <- mapply(write.fasta, sequences = protDF$protein_seq, names = protDF$protein_ID, file.out = protDF$export_path, open = "w", as.string = TRUE)
rm(todel, prot_DF_ls)



#save protDF -------
protDF_p <- paste0(P_D, "/01_genome_protDF_PCs.RDS")
if(file.exists(protDF_p)){file.remove(protDF_p)}
protDF <- protDF %>%
  select(genome_name, gene_ID, gene_start, gene_end, gene_length, strand, frame, protein_name, protein_ID, protein_length, 
         protein_seq)
saveRDS(protDF, protDF_p)
rm(protDF_p)

protTSV_p <- paste0(P_D, "/01_genome_protDF_PCs.tsv")
write.table(x = protDF, file = protTSV_p, sep = "\t", row.names = FALSE, col.names = TRUE)
rm(protTSV_p)

##
print("Step 1A has finished successfully.")