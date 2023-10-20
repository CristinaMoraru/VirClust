library(stringr, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library(zip, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)

input0 <- as.character(commandArgs(trailingOnly = TRUE))


#manual run
# input0 <- ""
# input0[1] <- "projdir=/home/cristinam/ICBM-Essen_workflows/VirClust/virclust_scripts/test/pd1"
# input0[2] <- "infile=/home/cristinam/ICBM-Essen_workflows/VirClust/virclust_scripts/test/four_very_short_zobellviridae_genomes_input.fasta"
# input0[3] <- "shiny=no"
# input0[4] <- "sing=no"
# input0[4] <- "step1A=F"
# input0[5] <- "step2A=F"
# input0[6] <- "step1B=F"
# input0[7] <- "step3A=F"
# input0[3] <- "step4A=T"
# input0[9] <- "gene_code=11"
# input0[9] <- "step4A_Plot=F"
# input0[10] <- "step5A=F"
# input0[11] <- "step2B=F"
# input0[12] <- "step3B=F"
# input0[13] <- "step3B_Plot=F"
# input0[14] <- "step4B=F"
# input0[15] <- "step8=F"
# input0[16] <- "continue=yes"
# input0[17] <- "multiF=yes"
# input0[18] <- "clust_dist_a=0.9"
# input0[19] <- "clust_dist_b=0.8"
# input0[20] <- "show_tree=no"
# input0[21] <- "tree_width=30"
# input0[22] <- "inc_fact_w=0.04"
# input0[23] <- "max_cols_HT=4500"
# input0[24] <- "font_row=10"
# input0[25] <- "font_col=20"
# input0[26] <- "interpro=T"
# input0[27] <- "pident_PC=0"
# input0[28] <- "inc_fact_w_Pd=0.04"
# input0[29] <- "lgd_width_Pd=2"

print(input0)

#no param given ----
if(length(input0) == 0)
{
  print("At least one parameter needs to be given. Aborting VirClust.")
  quit()
}


#determine VirClust path ----
virclust_path <-  this.path::here() # getwd()  ## this assumes I'm in the virclust folder

#shiny flag ---
shiny_n <- str_which(input0, "^shiny=")                                         ## this is not accessible for the users, only for me from Shiny
if(length(shiny_n) == 1)
{
  shiny <- str_remove(input0[shiny_n], "shiny=")
  if(shiny == "yes")
  {
    virclust_path <- paste0(virclust_path, "/vir_clust_standalone")
  }
}else
{
  shiny <- "no"
}
rm(shiny_n)

#functions file path
functions_path <- paste0(virclust_path, "/VirClust_functions.R")
source(functions_path)

shared_functions_path <- paste0(virclust_path, "/VirClust_functions_shared_with_shiny.R")
source(shared_functions_path)


# manual/help path ----
help_path <- paste0(virclust_path, "/VirClust-v2_manual_standalone.txt")

if(sum(str_detect(input0, "^help$")) ==1)
{
  
  cat_cmd <- paste0("cat ", help_path)
  system(cat_cmd)
}else
{
  if(sum(str_detect(input0, "^version$")) ==1)
  {
    print("VirClust version 2.0")
  }else
  {
    #VirClust ----
    library(magrittr, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
    library(dplyr, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
    library(tibble, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
    library(readr, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
    
    ## Params - mandatory--------
    projdir <- get_params_fun(input0, param_name = "projdir", type= "Text")
    infile <- get_params_fun(input0, param_name = "infile", type= "Text")
    
    ##Other paths and params ----
    sing <- get_params_fun(input0, param_name = "sing", type = "Text", default_val = "no", allowed = c("sing", "rhea", "conda"))
    condaenvpath <- get_params_fun(input0, param_name = "condaenvpath", type = "Text")
    blastdb <- get_params_fun(input0, param_name="blastdb", type="Text")
    interproscan <- get_params_fun(input0, param_name="interproscan", type="Text")
    databases <- get_params_fun(input0, param_name="databases", type="Text")
    log_path <- paste0(projdir, "/log.txt")
    status_path <- paste0(projdir, "/status.txt")
    #print(status_path)
    
    ### Params - steps -----
    #branchA
    step1A <- get_step_fun(input0, step="step1A")
    step2A <- get_step_fun(input0, step="step2A")
    step3A <- get_step_fun(input0, step="step3A")
    step3A_Plot <- get_step_fun(input0, step="step3A_Plot")
    step4A <- get_step_fun(input0, step="step4A")
    step4A_Plot <- get_step_fun(input0, step="step4A_Plot")
    step5A <- get_step_fun(input0, step="step5A")
    step6AI <- get_step_fun(input0, step="step6AI")
    step6ApV <- get_step_fun(input0, step="step6ApV")
    step6AVO <- get_step_fun(input0, step="step6AVO")
    step6APH <- get_step_fun(input0, step="step6APH")
    step6AE <- get_step_fun(input0, step="step6AE")
    step6AXC <- get_step_fun(input0, step="step6AXC")
    step6AN <- get_step_fun(input0, step="step6AN")
    step6AM <- get_step_fun(input0, step="step6AM")
    
    #branchB
    step1B <- get_step_fun(input0, step="step1B")
    step2B <- get_step_fun(input0, step="step2B")
    step2B_Plot <- get_step_fun(input0, step="step2B_Plot")
    step3B <- get_step_fun(input0, step="step3B")
    step3B_Plot <- get_step_fun(input0, step="step3B_Plot")
    step4B <- get_step_fun(input0, step="step4B")
    step5BI <- get_step_fun(input0, step="step5BI")
    step5BpV <- get_step_fun(input0, step="step5BpV")
    step5BVO <- get_step_fun(input0, step="step5BVO")
    step5BPH <- get_step_fun(input0, step="step5BPH")
    step5BE <- get_step_fun(input0, step="step5BE")
    step5BXC <- get_step_fun(input0, step="step5BXC")
    step5BN <- get_step_fun(input0, step="step5BN")
    step5BM <- get_step_fun(input0, step="step5BM")
    
    #branchC
    step1C <- get_step_fun(input0, step="step1C")
    step2C <- get_step_fun(input0, step="step2C")
    step2C_Plot <- get_step_fun(input0, step="step2C_Plot")
    step3C <- get_step_fun(input0, step="step3C")
    step3C_Plot <- get_step_fun(input0, step="step3C_Plot")
    step4C <- get_step_fun(input0, step="step4C")
    step5CI <- get_step_fun(input0, step="step5CI")
    step5CpV <- get_step_fun(input0, step="step5CpV")
    step5CVO <- get_step_fun(input0, step="step5CVO")
    step5CPH <- get_step_fun(input0, step="step5CPH")
    step5CE <- get_step_fun(input0, step="step5CE")
    step5CXC <- get_step_fun(input0, step="step5CXC")
    step5CN <- get_step_fun(input0, step="step5CN")
    step5CM <- get_step_fun(input0, step="step5CM")
    
    
    ####
    if(step1A == "F" & step2A == "F" & step3A == "F" & step3A_Plot == "F" & step4A == "F"
       & step4A_Plot == "F" & step5A == "F"
       & step1B == "F" & step2B == "F" & step2B_Plot == "F" & step3B == "F"
       & step3B_Plot == "F" & step4B == "F"
       & step1C == "F" & step2C == "F" & step2C_Plot == "F" & step3C == "F"
       & step3C_Plot == "F" & step4C == "F"
       & step6AI == "F" & step6ApV == "F" & step6AVO == "F" & step6APH == "F" & step6AE == "F" & step6AXC == "F" & step6AN == "F" & step6AM == "F"
       & step5BI == "F" & step5BpV == "F" & step5BVO == "F" & step5BPH == "F" & step5BE == "F" & step5BXC == "F" & step5BN == "F" & step5BM == "F"
       & step5CI == "F" & step5CpV == "F" & step5CVO == "F" & step5CPH == "F" & step5CE == "F" & step5CXC == "F" & step5CN == "F" & step5CM == "F")
    {
      print(paste0("At least one step parameter needs to be given. Aborting VirClust."))
      quit()
    }
    
    
    ##Params - shiny--------
    multiF_n <- str_which(input0, "^multiF=")
    if(length(multiF_n) == 0)
    {
      multiF <- "yes"                #multiF variable is only to be used in conjuction with Shiny; for standalone, it should always be "yes", because it is copying the multifasta input file in the 01_out folder.
    }else
    {
      multiF <- str_remove(input0[multiF_n], "^multiF=")
    }
    rm(multiF_n)
    
    
    ###Params - general ----
    cpu <- get_params_fun(input0, param_name = "cpu", type= "Int", default_val = "2", min_val = 1)
    continue <- get_params_fun(input0, param_name = "continue", type= "Text", default_val = "no", allowed = c("yes", "no"), status_path = status_path)
    
    ###Params - step 1A ---
    gene_code <- get_params_fun(input0, param_name = "gene_code", type= "Int", default_val = "11", min_val = 1, max_val = 26)
    
    ###Params - step2A ----
    clust_PC <- get_params_fun(input0, param_name = "clust_PC", type= "Text", default = "evalue_log", allowed = c("evalue", "bitscore", "evalue_log", "norm_bitscore"))
    eval_PC <- get_params_fun(input0, param_name = "eval_PC", type= "Num", default_val = "0.00001", min_val = 0)#, max_val = 0.01)
    bitsc_PC <- get_params_fun(input0, param_name = "bitsc_PC", type= "Int", default_val = "50", min_val = 20)
    cov_PC <- get_params_fun(input0, param_name = "cov_PC", type= "Int", default_val = "0", min_val = 0)
    pident_PC <- get_params_fun(input0, param_name = "pident_PC", type= "Int", default_val = "0", min_val = 0)
    
    ###Params - step1B ----
    clust_PSC <- get_params_fun(input0, param_name = "clust_PSC", type= "Text", default = "evalue_log", allowed = c("evalue", "bitscore", "evalue_log", "norm_bitscore"))
    prob1_PSC <- get_params_fun(input0, param_name = "prob1_PSC", type= "Int", default_val = "90", min_val = 1)
    prob2_PSC <- get_params_fun(input0, param_name = "prob2_PSC", type= "Int", default_val = "99", min_val = 1)
    cov1_PSC <- get_params_fun(input0, param_name = "cov1_PSC", type= "Int", default_val = "50", min_val = 1)
    cov2_PSC <- get_params_fun(input0, param_name = "cov2_PSC", type= "Int", default_val = "20", min_val = 1)
    alig_PSC <- get_params_fun(input0, param_name = "alig_PSC", type= "Int", default_val = "100", min_val = 1)
    
    ###Params - step1C ----
    clust_PSSC <- get_params_fun(input0, param_name = "clust_PSSC", type= "Text", default = "evalue_log", allowed = c("evalue", "bitscore", "evalue_log", "norm_bitscore"))
    prob1_PSSC <- get_params_fun(input0, param_name = "prob1_PSSC", type= "Int", default_val = "90", min_val = 1)
    prob2_PSSC <- get_params_fun(input0, param_name = "prob2_PSSC", type= "Int", default_val = "99", min_val = 1)
    cov1_PSSC <- get_params_fun(input0, param_name = "cov1_PSSC", type= "Int", default_val = "50", min_val = 1)
    cov2_PSSC <- get_params_fun(input0, param_name = "cov2_PSSC", type= "Int", default_val = "20", min_val = 1)
    alig_PSSC <- get_params_fun(input0, param_name = "alig_PSSC", type= "Int", default_val = "100", min_val = 1)
    
    ###Params - step 3A, (4A and 3B) -------
    aglom_a <- get_params_fun(input0, param_name = "aglom_a", type= "Text", default = "complete", allowed = c("complete", "average"))
    boot_pv_a <- get_params_fun(input0, param_name = "boot_pv_a", type= "Text", default = "no", allowed = c("yes", "no"))
    bootstrap_no_a <- get_params_fun(input0, param_name = "bootstrap_no_a", type= "Int", default_val = "100", min_val = 1)
    
    ###Params - step 2B ----
    aglom_b <- get_params_fun(input0, param_name = "aglom_b", type= "Text", default = "complete", allowed = c("complete", "average"))
    boot_pv_b <- get_params_fun(input0, param_name = "boot_pv_b", type= "Text", default = "no", allowed = c("yes", "no"))
    bootstrap_no_b <- get_params_fun(input0, param_name = "bootstrap_no_b", type= "Int", default_val = "100", min_val = 1)
    
    ###Params - step 2C ----
    aglom_c <- get_params_fun(input0, param_name = "aglom_c", type= "Text", default = "complete", allowed = c("complete", "average"))
    boot_pv_c <- get_params_fun(input0, param_name = "boot_pv_c", type= "Text", default = "no", allowed = c("yes", "no"))
    bootstrap_no_c <- get_params_fun(input0, param_name = "bootstrap_no_c", type= "Int", default_val = "100", min_val = 1)
    
    ####Params - steps 3A_Plot, 2B_Plot and 2C_Plot ------
    inc_fact_w_Pd <- get_params_fun(input0, param_name = "inc_fact_w_Pd", type= "Num", default_val = "0.3", min_val = 0.02)
    font_row_Pd <- get_params_fun(input0, param_name = "font_row_Pd", type= "Int", default_val = "12", min_val = 1)
    font_col_Pd <- get_params_fun(input0, param_name = "font_col_Pd", type= "Int", default_val = "12", min_val = 1)
    font_cell_Pd <- get_params_fun(input0, param_name = "font_cell_Pd", type= "Int", default_val = "6", min_val = 1)
    
    
    lgd_width_Pd <- get_params_fun(input0, param_name = "lgd_width_Pd", type= "Int", default_val = "15", min_val = 1) / 100
    lgd_height_Pd <- get_params_fun(input0, param_name = "lgd_height_Pd", type= "Int", default_val = "9", min_val = 1) / 100
    lgd_font_Pd <- get_params_fun(input0, param_name = "lgd_font_Pd", type= "Int", default_val = "5", min_val = 0) / 100
    lgd_lab_font_Pd <- get_params_fun(input0, param_name = "lgd_lab_font_Pd", type= "Int", default_val = "4", min_val = 0) / 100
    lgd_pos_Pd <- get_params_fun(input0, param_name = "lgd_pos_Pd", type = "Text", default_val = "leftcenter-rot", 
                                 allowed = c("topleft", "topcenter", "leftcenter", "lefttop", "leftcenter-rot", "lefttop-rot"))
    
    
    ###Params 4A ----
    clust_dist_a <- get_params_fun(input0, param_name = "clust_dist_a", type= "Num", default_val = "0.9", min_val = 0.1, max_val = 1)
    max_cols_HT <- get_params_fun(input0, param_name = "max_cols_HT", type= "Int", default_val = "3000", min_val = 0)
    ###Params 3B ----
    clust_dist_b <- get_params_fun(input0, param_name = "clust_dist_b", type= "Num", default_val = "0.9", min_val = 0.1, max_val = 1)
    
    ###Params 3B ----  
    clust_dist_c <- get_params_fun(input0, param_name = "clust_dist_c", type= "Num", default_val = "0.9", min_val = 0.1, max_val = 1)
    
    
    ####Params - steps 4A_Plot, 3B_Plot and 3C_plot ----
    show_tree <- get_params_fun(input0, param_name = "show_tree", type = "Text", default_val = "yes", allowed = c("yes", "no"))
    show_heat <- get_params_fun(input0, param_name = "show_heat", type = "Text", default_val = "yes", allowed = c("yes", "no"))
    show_protein_stats <- get_params_fun(input0, param_name = "show_protein_stats", type = "Text", default_val = "yes", allowed = c("yes", "no"))
    show_clust_ID <- get_params_fun(input0, param_name = "show_clust_ID", type = "Text", default_val = "yes", allowed = c("yes", "no"))
    show_sil <- get_params_fun(input0, param_name = "show_sil", type = "Text", default_val = "yes", allowed = c("yes", "no"))
    
    tree_width <- get_params_fun(input0, param_name = "tree_width", type= "Int", default_val = "50", min_val = 0) / 100
    stats_width <- get_params_fun(input0, param_name = "stats_width", type= "Int", default_val = "40", min_val = 0) / 100
    sil_stats_width <- get_params_fun(input0, param_name = "sil_stats_width", type= "Int", default_val = "5", min_val = 1)
    clustID_width <- get_params_fun(input0, param_name = "clustID_width", type= "Int", default_val = "10", min_val = 1)
    lgd_width <- get_params_fun(input0, param_name = "lgd_width", type= "Int", default_val = "40", min_val = 0) / 100
    lgd_height <- get_params_fun(input0, param_name = "lgd_height", type= "Int", default_val = "6", min_val = 0) / 100
    
    inc_fact_w <- get_params_fun(input0, param_name = "inc_fact_w", type= "Num", default_val = "0.03", min_val = 0.005)
    
    font_row <- get_params_fun(input0, param_name = "font_row", type= "Int", default_val = "12", min_val = 1)
    font_col <- get_params_fun(input0, param_name = "font_col", type= "Int", default_val = "2", min_val = 1)
    stats_font <- get_params_fun(input0, param_name = "stats_font", type= "Int", default_val = "5", min_val = 1) / 100
    stats_lab_font <- get_params_fun(input0, param_name = "stats_lab_font", type= "Int", default_val = "5", min_val = 1) / 100
    lgd_font <- get_params_fun(input0, param_name = "lgd_font", type= "Int", default_val = "30", min_val = 0) / 100
    lgd_lab_font <- get_params_fun(input0, param_name = "lgd_lab_font", type= "Int", default_val = "15", min_val = 0) / 100
    
    lgd_pos <- get_params_fun(input0, param_name = "lgd_pos", type = "Text", default_val = "leftcenter-rot", 
                              allowed = c("topleft", "topcenter", "leftcenter", "lefttop", "leftcenter-rot", "lefttop-rot"))
    
    
    #### Params - annot steps ----
    prot_type <- get_params_fun(input0, param_name = "prot_type", type = "Text", default_val = "all_PCs", 
                                allowed = c("all_PCs", "core_PCs", "all_PSCs", "core_PSCs", "all_PSSCs", "core_PSSCs"))
    
    
    ##RUN VirClust -----------
    
    if(continue == "no" & shiny=="no")
    {
      if(dir.exists(projdir)){unlink(projdir, recursive = TRUE)}
      dir.create(projdir)
    }
    
    
    if(continue == "no")
    {
      status_df <- make_status_df_fun(projdir)
      
      step0_d <- paste0(projdir, "/00/00_in")
      ###multi to single genome ---- 
      
      if(multiF=="yes")
      {
        if(shiny == "no")
        {
          dir.create(step0_d, recursive = TRUE)
          file.copy(from=infile, to=paste0(step0_d, "/"))
        }
        
        write(x="running", file = log_path)
        print("Starting VirClust.")
        print("Running VirClust step 0: writing one viral genome per fasta file.")
        
        VirClust_cmd0 <- paste0("Rscript ", virclust_path, "/VirClust_00_multigenomes_2_singlegenomes.R projdir=", projdir)
        system(VirClust_cmd0)
        rm(VirClust_cmd0)
      }
    }else
    {
      ### Reset previous steps ----
      status_df <- read.csv(file = status_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      
      #reset post steps by position
      status_df <- reset_post_steps_by_pos_fun(cur_step_abrev = "1A", cur_step_val = step1A, status_df)
      status_df <- reset_post_steps_by_pos_fun(cur_step_abrev = "2A", cur_step_val = step2A, status_df)
      status_df <- reset_post_steps_by_pos_fun(cur_step_abrev = "1B", cur_step_val = step1B, status_df)
      status_df <- reset_post_steps_by_pos_fun(cur_step_abrev = "1C", cur_step_val = step1C, status_df)
      
      #reset post steps by name
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "3A", cur_step_val = step3A, status_df = status_df,
                                                del_step_abrev = c("3A", "3A_Plot", "4A", "4A_Plot", "5A", 
                                                                   "6AI_core", "6ApV_core", "6AVO_core", "6APH_core", "6AE_core", "6AXC_core", "6AN_core", "6AM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "4A", cur_step_val = step4A, status_df = status_df,
                                                del_step_abrev = c("4A", "4A_Plot", "5A", 
                                                                   "6AI_core", "6ApV_core", "6AVO_core", "6APH_core", "6AE_core", "6AXC_core", "6AN_core", "6AM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "5A", cur_step_val = step5A, status_df = status_df,
                                                del_step_abrev = c("5A", 
                                                                   "6AI_core", "6ApV_core", "6AVO_core", "6APH_core", "6AE_core", "6AXC_core", "6AN_core", "6AM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "2B", cur_step_val = step2B, status_df = status_df,
                                                del_step_abrev = c("2B", "2B_Plot", "3B", "3B_Plot", "4B", 
                                                                   "5BI_core", "5BpV_core", "5BVO_core", "5BPH_core", "5BE_core", "5BXC_core", "5BN_core", "5BM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "3B", cur_step_val = step3B, status_df = status_df,
                                                del_step_abrev = c("3B", "3B_Plot", "4B", 
                                                                   "5BI_core", "5BpV_core", "5BVO_core", "5BPH_core", "5BE_core", "5BXC_core", "5BN_core", "5BM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "4B", cur_step_val = step4B, status_df = status_df,
                                                del_step_abrev = c("4B", 
                                                                   "5BI_core", "5BpV_core", "5BVO_core", "5BPH_core", "5BE_core", "5BXC_core", "5BN_core", "5BM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "2C", cur_step_val = step2C, status_df = status_df,
                                                del_step_abrev = c("2C", "2C_Plot", "3C", "3C_Plot", "4C", 
                                                                   "5CI_core", "5CpV_core", "5CVO_core", "5CPH_core", "5CE_core", "5CXC_core", "5CN_core", "5CM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "3C", cur_step_val = step3C, status_df = status_df,
                                                del_step_abrev = c("3C", "3C_Plot", "4C", 
                                                                   "5CI_core", "5CpV_core", "5CVO_core", "5CPH_core", "5CE_core", "5CXC_core", "5CN_core", "5CM_core"))
      
      status_df <- reset_post_steps_by_name_fun(cur_step_abrev = "4C", cur_step_val = step4C, status_df = status_df,
                                                del_step_abrev = c("4C", 
                                                                   "5CI_core", "5CpV_core", "5CVO_core", "5CPH_core", "5CE_core", "5CXC_core", "5CN_core", "5CM_core"))
      
      #reset post steps by list, if they are true (from these steps don't depend any other steps)
      status_df <- reset_steps_by_list_fun(steps_to_del = c("3A_Plot", "4A_Plot", "2B_Plot", "3B_Plot", "2C_Plot", "3C_Plot"),
                                           step_val = c(step3A_Plot, step4A_Plot, step2B_Plot, step3B_Plot, step2C_Plot, step3C_Plot),
                                           status_df = status_df)
      
      #reset annot steps by list, with aditional merge step to delete
      status_df <- reset_annot_steps_fun(steps_to_del = c("6AI", "6ApV", "6AVO", "6APH", "6AE", "6AXC", "6AN", "6AM"),
                                         step_val = c(step6AI, step6ApV, step6AVO, step6APH, step6AE, step6AXC, step6AN, step6AM),
                                         m_to_del = "6AM", status_df=status_df, prot_type = prot_type)
      status_df <- reset_annot_steps_fun(steps_to_del = c("5BI", "5BpV", "5BVO", "5BPH", "5BE", "5BXC", "5BN", "5BM"), 
                                         step_val = c(step5BI, step5BpV, step5BVO, step5BPH, step5BE, step5BXC, step5BN, step5BM),
                                         m_to_del = "5BM", status_df=status_df, prot_type = prot_type)
      status_df <- reset_annot_steps_fun(steps_to_del = c("5CI", "5CpV", "5CVO", "5CPH", "5CE", "5CXC", "5CN", "5CM"), 
                                         step_val = c(step5CI, step5CpV, step5CVO, step5CPH, step5CE, step5CXC, step5CN, step5CM),
                                         m_to_del = "5CM", status_df=status_df, prot_type = prot_type)
      
    }
    
    
    ##Activate previous steps ----
    step4C <- check_prev_steps_annots_fun(status_df = status_df, cur_steps_val = c(step5CI, step5CpV, step5CVO, step5CPH, step5CE, step5CXC, step5CN, step5CM), 
                                          prev_step = "4C", prev_step_val = step4C, prot_type = prot_type)
    step3C <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step3C_Plot, step4C), prev_step = "3C", prev_step_val = step3C)
    step2C <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step2C_Plot, step3C), prev_step = "2C", prev_step_val = step2C)
    
    
    step4B <- check_prev_steps_annots_fun(status_df = status_df, cur_steps_val = c(step5BI, step5BpV, step5BVO, step5BPH, step5BE, step5BXC, step5BN, step5BM), 
                                          prev_step = "4B", prev_step_val = step4B, prot_type = prot_type)
    step3B <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step3B_Plot, step4B), prev_step = "3B", prev_step_val = step3B)
    step2B <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step2B_Plot, step3B), prev_step = "2B", prev_step_val = step2B)
    
    
    step5A <- check_prev_steps_annots_fun(status_df = status_df, cur_steps_val = c(step6AI, step6ApV, step6AVO, step6APH, step6AE, step6AXC, step6AN, step6AM), 
                                          prev_step = "5A", prev_step_val = step5A, prot_type = prot_type)
    step4A <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step4A_Plot, step5A), prev_step = "4A", prev_step_val = step4A)
    step3A <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step3A_Plot, step4A), prev_step = "3A", prev_step_val = step3A)
    
    
    
    step1C <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step2C, step5CI, step5CpV, step5CVO, step5CPH, step5CE, step5CXC, step5CN, step5CM),
                                   prev_step = "1C", prev_step_val = step1C)
    step1B <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step2B, step5BI, step5BpV, step5BVO, step5BPH, step5BE, step5BXC, step5BN, step5BM, step1C), 
                                   prev_step = "1B", prev_step_val = step1B)
    
    step2A <- check_prev_steps_fun(status_df = status_df, cur_steps_val = c(step3A, step6AI, step6ApV, step6AVO, step6APH, step6AE, step6AXC, step6AN, step6AM, step1B), 
                                   prev_step = "2A", prev_step_val = step2A)
    step1A <- check_prev_steps_fun(status_df = status_df, cur_steps_val = step2A, prev_step = "1A", prev_step_val = step1A)
    
    
    ###START pipe ----
    
    ## log "running"
    
    if(step1A == "T" | step2A == "T" | step3A == "T" | step3A_Plot == "T" | step4A == "T"
       | step4A_Plot == "T" | step5A == "T"
       | step1B == "T" | step2B == "T" | step2B_Plot == "T" | step3B == "T"
       | step3B_Plot == "T" | step4B == "T"
       | step1C == "T" | step2C == "T" | step2C_Plot == "T" | step3C == "T"
       | step3C_Plot == "T" | step4C == "T" 
       | step6AI == "T" | step6ApV == "T" | step6AVO == "T" | step6APH == "T" | step6AE == "T" | step6AXC == "T" | step6AN == "T" | step6AM == "T"
       | step5BI == "T" | step5BpV == "T" | step5BVO == "T" | step5BPH == "T" | step5BE == "T" | step5BXC == "T" | step5BN == "T" | step5BM == "T"
       | step5CI == "T" | step5CpV == "T" | step5CVO == "T" | step5CPH == "T" | step5CE == "T" | step5CXC == "T" | step5CN == "T" | step5CM == "T")
    {
      write(x="running", file = log_path)
    }
    
    ## step 1A ----
    if(step1A == "T")
    {
      status_df <- before_step_exec_fun(step= "1A", status_df=status_df, status_path=status_path, text = "predict ORFs and translate to proteins.")
      
      status_step1A <- sys::exec_wait(cmd = "Rscript",
                                      args = c(paste0(virclust_path, "/VirClust_01_genomes_2_proteins.R"),
                                               paste0("projdir=", projdir),
                                               paste0("gene_code=", gene_code),
                                               paste0("functions_path=", functions_path)),
                                      std_out = paste0(projdir, "/step1A_std_out.txt"),
                                      std_err = paste0(projdir, "/step1A_std_err.txt"))
      
      # status_step1A <- system2(command = "Rscript", 
      #                          args = paste0(virclust_path, "/VirClust_01_genomes_2_proteins.R",
      #                                        " projdir=", projdir,
      #                                        " gene_code=", gene_code,
      #                                        " functions_path=", functions_path), 
      #                          stdout = paste0(projdir, "/step1A_std_out.txt"), 
      #                          stderr = paste0(projdir, "/step1A_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "1A", status_df=status_df, status_path=status_path, status_step=status_step1A) 
      
      tozip1_p <- paste0(projdir, "/01/01_out_faa")
      outzip1_p <- paste0(projdir, "/01/01_out_faa.zip")
      zipr(zipfile = outzip1_p, files = tozip1_p)
      rm(tozip1_p, outzip1_p, status_step1A)
    }
    
    
    ## step 2A ----
    if(step2A == "T")
    {
      status_df <- before_step_exec_fun(step = "2A", status_df=status_df, status_path=status_path, text = "clustering proteins based on BLASTp.")
      
      status_step2A <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_02_proteins_2_PCs.R"),
                                               paste0("projdir=", projdir),
                                               paste0("cpu=", cpu),
                                               paste0("clust_PC=", clust_PC),
                                               paste0("eval_PC=", eval_PC),
                                               paste0("bitsc_PC=", bitsc_PC),
                                               paste0("cov_PC=", cov_PC),
                                               paste0("pident_PC=", pident_PC),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step2A_std_out.txt"), 
                                      std_err = paste0(projdir, "/step2A_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "2A", status_df=status_df, status_path=status_path, status_step=status_step2A) 
      rm(status_step2A)
    }
    
    ## step 1B ----
    if(step1B == "T")
    {
      status_df <- before_step_exec_fun(step = "1B", status_df=status_df, status_path=status_path, text = "calculate protein superclusters based on HMMs")
      
      status_step1B <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_03_PCs_2_PSCs.R"),
                                               paste0("projdir=", projdir),
                                               paste0("cpu=", cpu),
                                               paste0("clust_PSC=", clust_PSC),
                                               paste0("prob1_PSC=", prob1_PSC),
                                               paste0("prob2_PSC=", prob2_PSC),
                                               paste0("cov1_PSC=", cov1_PSC),
                                               paste0("cov2_PSC=", cov2_PSC),
                                               paste0("alig_PSC=", alig_PSC),
                                               paste0("sing=", sing),
                                               paste0("condaenvpath=", condaenvpath),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step1B_std_out.txt"), 
                                      std_err = paste0(projdir, "/step1B_std_err.txt"))
      status_df <- after_step_exec_fun(step= "1B", status_df=status_df, status_path=status_path, status_step=status_step1B) 
      rm(status_step1B)
    }
    
    
    ## step 1C ----
    if(step1C == "T")
    {
      status_df <- before_step_exec_fun(step = "1C", status_df=status_df, status_path=status_path, text = "calculate protein super-superclusters based on HMMs")
      
      status_step1C <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_03C_PSCs_2_PSSCs.R"),
                                               paste0("projdir=", projdir),
                                               paste0("cpu=", cpu),
                                               paste0("clust_PSSC=", clust_PSSC),
                                               paste0("prob1_PSSC=", prob1_PSSC),
                                               paste0("prob2_PSSC=", prob2_PSSC),
                                               paste0("cov1_PSSC=", cov1_PSSC),
                                               paste0("cov2_PSSC=", cov2_PSSC),
                                               paste0("alig_PSSC=", alig_PSSC),
                                               paste0("sing=", sing),
                                               paste0("condaenvpath=", condaenvpath),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step1C_std_out.txt"), 
                                      std_err = paste0(projdir, "/step1C_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "1C", status_df=status_df, status_path=status_path, status_step=status_step1C) 
      rm(status_step1C)
    }
    
    ## step 3A ----
    if(step3A == "T")
    {
      status_df["3A", "boot_pv"] <- boot_pv_a
      status_df["3A", "aglom"] <- aglom_a
      status_df <- before_step_exec_fun(step = "3A", status_df=status_df, status_path=status_path, text = "cluster viral genomes hierarchically.")
      
      status_step3A <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_04_clustGenomes_distMA_trees.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PC",
                                               paste0("boot_pv=", boot_pv_a),
                                               paste0("bootstrap_no=", bootstrap_no_a),
                                               paste0("aglom=", aglom_a),
                                               paste0("cpu=", cpu)), 
                                      std_out = paste0(projdir, "/step3A_std_out.txt"), 
                                      std_err = paste0(projdir, "/step3A_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "3A", status_df=status_df, status_path=status_path, status_step=status_step3A) 
      rm(status_step3A)
    }
    
    
    ##step 3A_Plot -----
    if(step3A_Plot == "T")
    {
      status_df <- before_step_exec_fun(step = "3A_Plot", status_df=status_df, status_path=status_path, 
                           text = "Plot PC-based intergenomic distances as heatmap.")
      
      status_step3A_Plot <- sys::exec_wait(cmd= "Rscript",
                                           args = c(paste0(virclust_path, "/VirClust_04_PLOT-dist.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PC",
                                                    paste0("inc_fact_w=", inc_fact_w_Pd),
                                                    paste0("font_row=", font_row_Pd),
                                                    paste0("font_col=", font_col_Pd),
                                                    paste0("font_cell=", font_cell_Pd),
                                                    paste0("lgd_width=", lgd_width_Pd), 
                                                    paste0("lgd_height=", lgd_height_Pd),
                                                    paste0("lgd_font=", lgd_font_Pd),
                                                    paste0("lgd_pos=", lgd_pos_Pd),
                                                    paste0("lgd_lab_font=", lgd_lab_font_Pd),
                                                    paste0("functions_path=", functions_path)),
                                           std_out = paste0(projdir, "/step3A_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step3A_Plot_std_err.txt")
      )
      
      status_df <- after_step_exec_fun(step= "3A_Plot", status_df=status_df, status_path=status_path, status_step=status_step3A_Plot) 
      rm(status_step3A_Plot)
    }
    
    
    ## step 4A ----
    if(step4A == "T")
    {
      status_df["4A", "max_cols_HT"] <- max_cols_HT
      status_df["4A", "clust_dist"] <- clust_dist_a
      status_df <- before_step_exec_fun(step = "4A", status_df=status_df, status_path=status_path, 
                           text = "split the PC-based genome clustering tree into viral genome clusters based on a distance threshold.")
      
      status_step4A <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_05_genomeClusters_tree-annots.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PC",
                                               paste0("boot_pv=", status_df["3A", "boot_pv"]),
                                               paste0("clust_dist=", clust_dist_a),
                                               paste0("max_cols_HT=", max_cols_HT),
                                               paste0("aglom=", status_df["3A", "aglom"]),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step4A_std_out.txt"), 
                                      std_err = paste0(projdir, "/step4A_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "4A", status_df=status_df, status_path=status_path, status_step=status_step4A) 
      rm(status_step4A)
    }
    
    ## step 4A_Plot ----
    if(step4A_Plot == "T")
    {
      script4A_status <- paste0(projdir, "/script5_PC.txt")
      if(file.exists(script4A_status))
      {
        quit()
      }
      rm(script4A_status)
      
      status_df <- before_step_exec_fun(step = "4A_Plot", status_df=status_df, status_path=status_path, 
                           text = "plot PC-based PDF.")
      
      status_step4A_Plot <- sys::exec_wait(cmd = "Rscript", 
                                           args = c(paste0(virclust_path, "/VirClust_06_PLOT_tree_heatmap_annots.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PC",
                                                    paste0("boot_pv=", status_df["3A", "boot_pv"]),
                                                    paste0("clust_dist=", status_df["4A", "clust_dist"]),
                                                    paste0("font_row=", font_row), 
                                                    paste0("font_col=", font_col),
                                                    paste0("stats_width=", stats_width), 
                                                    paste0("sil_stats_width=", sil_stats_width),
                                                    paste0("tree_width=", tree_width), 
                                                    paste0("stats_font=", stats_font), 
                                                    paste0("stats_lab_font=", stats_lab_font), 
                                                    paste0("lgd_width=", lgd_width), 
                                                    paste0("lgd_height=", lgd_height), 
                                                    paste0("lgd_font=", lgd_font), 
                                                    paste0("lgd_lab_font=", lgd_lab_font), 
                                                    paste0("inc_fact_w=", inc_fact_w), 
                                                    paste0("lgd_pos=", lgd_pos), 
                                                    paste0("show_tree=", show_tree), 
                                                    paste0("show_heat=", show_heat), 
                                                    paste0("show_protein_stats=", show_protein_stats), 
                                                    paste0("show_sil=", show_sil), 
                                                    paste0("show_clust_ID=", show_clust_ID), 
                                                    paste0("clustID_width=", clustID_width),
                                                    paste0("functions_path=", functions_path)), 
                                           std_out = paste0(projdir, "/step4A_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step4A_Plot_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "4A_Plot", status_df=status_df, status_path=status_path, status_step=status_step4A_Plot) 
      rm(status_step4A_Plot)
    }
    
    
    ## step 2B ----
    if(step2B == "T")
    {
      status_df["2B", "boot_pv"] <- boot_pv_b
      status_df["2B", "aglom"] <- aglom_b
      status_df <- before_step_exec_fun(step = "2B", status_df=status_df, status_path=status_path, 
                           text = "cluster viral genomes hierarchically based on protein superclusters.")
      
      status_step2B <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_04_clustGenomes_distMA_trees.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSC",
                                               paste0("boot_pv=", boot_pv_b),
                                               paste0("bootstrap_no=", bootstrap_no_b),
                                               paste0("aglom=", aglom_b),
                                               paste0("cpu=", cpu)), 
                                      std_out = paste0(projdir, "/step2B_std_out.txt"), 
                                      std_err = paste0(projdir, "/step2B_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "2B", status_df=status_df, status_path=status_path, status_step=status_step2B) 
      rm(status_step2B)
    }
    
    
    ##step 2B_Plot -----
    if(step2B_Plot == "T")
    {
      status_df <- before_step_exec_fun(step = "2B_Plot", status_df=status_df, status_path=status_path, 
                           text = "Plot PSC-based intergenomic distances as heatmap.")
      
      status_step2B_Plot <- sys::exec_wait(cmd= "Rscript",
                                           args = c(paste0(virclust_path, "/VirClust_04_PLOT-dist.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PSC",
                                                    paste0("inc_fact_w=", inc_fact_w_Pd),
                                                    paste0("font_row=", font_row_Pd),
                                                    paste0("font_col=", font_col_Pd),
                                                    paste0("font_cell=", font_cell_Pd),
                                                    paste0("lgd_width=", lgd_width_Pd), 
                                                    paste0("lgd_height=", lgd_height_Pd),
                                                    paste0("lgd_font=", lgd_font_Pd),
                                                    paste0("lgd_pos=", lgd_pos_Pd),
                                                    paste0("lgd_lab_font=", lgd_lab_font_Pd),
                                                    paste0("functions_path=", functions_path)),
                                           std_out = paste0(projdir, "/step2B_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step2B_Plot_std_err.txt")
      )
      
      status_df <- after_step_exec_fun(step= "2B_Plot", status_df=status_df, status_path=status_path, status_step=status_step2B_Plot) 
      rm(status_step2B_Plot)
    }
    
    ## step 3B ---- 
    if(step3B == "T")
    {
      status_df["3B", "max_cols_HT"] <- max_cols_HT
      status_df["3B", "clust_dist"] <- clust_dist_b
      status_df <- before_step_exec_fun(step = "3B", status_df=status_df, status_path=status_path, 
                           text = "split the PSC-based genome clustering tree into viral genome clusters based on a distance threshold.")
      
      status_step3B <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_05_genomeClusters_tree-annots.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSC",
                                               paste0("boot_pv=", status_df["2B", "boot_pv"]),
                                               paste0("clust_dist=", clust_dist_b),
                                               paste0("max_cols_HT=", max_cols_HT),
                                               paste0("aglom=", status_df["2B", "aglom"]),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step3B_std_out.txt"), 
                                      std_err = paste0(projdir, "/step3B_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "3B", status_df=status_df, status_path=status_path, status_step=status_step3B) 
      rm(status_step3B)
    }
    
    ## step 3B_Plot ----
    if(step3B_Plot == "T")
    {
      script3B_status <- paste0(projdir, "/script5_PSC.txt")
      if(file.exists(script3B_status))
      {
        quit()
      }
      rm(script3B_status)
      
      status_df <- before_step_exec_fun(step = "3B_Plot", status_df=status_df, status_path=status_path, 
                           text = "plot PSC-based PDF.")
      
      status_step3B_Plot <- sys::exec_wait(cmd = "Rscript", 
                                           args = c(paste0(virclust_path, "/VirClust_06_PLOT_tree_heatmap_annots.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PSC",
                                                    paste0("boot_pv=", status_df["2B", "boot_pv"]),
                                                    paste0("clust_dist=", status_df["3B", "clust_dist"]),
                                                    paste0("font_row=", font_row), 
                                                    paste0("font_col=", font_col),
                                                    paste0("stats_width=", stats_width), 
                                                    paste0("sil_stats_width=", sil_stats_width),
                                                    paste0("tree_width=", tree_width), 
                                                    paste0("stats_font=", stats_font), 
                                                    paste0("stats_lab_font=", stats_lab_font), 
                                                    paste0("lgd_width=", lgd_width), 
                                                    paste0("lgd_height=", lgd_height), 
                                                    paste0("lgd_font=", lgd_font), 
                                                    paste0("lgd_lab_font=", lgd_lab_font), 
                                                    paste0("inc_fact_w=", inc_fact_w), 
                                                    paste0("lgd_pos=", lgd_pos), 
                                                    paste0("show_tree=", show_tree), 
                                                    paste0("show_heat=", show_heat), 
                                                    paste0("show_protein_stats=", show_protein_stats), 
                                                    paste0("show_sil=", show_sil), 
                                                    paste0("show_clust_ID=", show_clust_ID), 
                                                    paste0("clustID_width=", clustID_width),
                                                    paste0("functions_path=", functions_path)), 
                                           std_out = paste0(projdir, "/step3B_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step3B_Plot_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "3B_Plot", status_df=status_df, status_path=status_path, status_step=status_step3B_Plot) 
      rm(status_step3B_Plot)
    }
    
    
    ## step 2C ----
    if(step2C == "T")
    {
      status_df["2C", "boot_pv"] <- boot_pv_c
      status_df["2C", "aglom"] <- aglom_c
      status_df <- before_step_exec_fun(step = "2B", status_df=status_df, status_path=status_path, 
                           text = "cluster viral genomes hierarchically based on protein super-superclusters.")
      
      status_step2C <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_04_clustGenomes_distMA_trees.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSSC",
                                               paste0("boot_pv=", boot_pv_c),
                                               paste0("bootstrap_no=", bootstrap_no_c),
                                               paste0("aglom=", aglom_c),
                                               paste0("cpu=", cpu)), 
                                      std_out = paste0(projdir, "/step2C_std_out.txt"), 
                                      std_err = paste0(projdir, "/step2C_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "2C", status_df=status_df, status_path=status_path, status_step=status_step2C) 
      rm(status_step2C)
    }
    
    
    ##step 2C_Plot -----
    if(step2C_Plot == "T")
    {
      status_df <- before_step_exec_fun(step = "2C_Plot", status_df=status_df, status_path=status_path, 
                           text = "Plot PSC-based intergenomic distances as heatmap.")
      
      status_step2C_Plot <- sys::exec_wait(cmd= "Rscript",
                                           args = c(paste0(virclust_path, "/VirClust_04_PLOT-dist.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PSSC",
                                                    paste0("inc_fact_w=", inc_fact_w_Pd),
                                                    paste0("font_row=", font_row_Pd),
                                                    paste0("font_col=", font_col_Pd),
                                                    paste0("font_cell=", font_cell_Pd),
                                                    paste0("lgd_width=", lgd_width_Pd), 
                                                    paste0("lgd_height=", lgd_height_Pd),
                                                    paste0("lgd_font=", lgd_font_Pd),
                                                    paste0("lgd_pos=", lgd_pos_Pd),
                                                    paste0("lgd_lab_font=", lgd_lab_font_Pd),
                                                    paste0("functions_path=", functions_path)),
                                           std_out = paste0(projdir, "/step2C_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step2C_Plot_std_err.txt")
      )
      
      status_df <- after_step_exec_fun(step= "2C_Plot", status_df=status_df, status_path=status_path, status_step=status_step2C_Plot) 
      rm(status_step2C_Plot)
    }
    
    ## step 3C ---- 
    if(step3C == "T")
    {
      status_df["3C", "max_cols_HT"] <- max_cols_HT
      status_df["3C", "clust_dist"] <- clust_dist_c
      status_df <- before_step_exec_fun(step = "3C", status_df=status_df, status_path=status_path, 
                           text = "split the PSSC-based genome clustering tree into viral genome clusters based on a distance threshold.")
      
      status_step3C <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_05_genomeClusters_tree-annots.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSSC",
                                               paste0("boot_pv=", status_df["2C", "boot_pv"]),
                                               paste0("clust_dist=", clust_dist_c),
                                               paste0("max_cols_HT=", max_cols_HT),
                                               paste0("aglom=", status_df["2C", "aglom"]),
                                               paste0("functions_path=", functions_path)), 
                                      std_out = paste0(projdir, "/step3C_std_out.txt"), 
                                      std_err = paste0(projdir, "/step3C_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "3C", status_df=status_df, status_path=status_path, status_step=status_step3C) 
      rm(status_step3C)
    }
    
    
    ## step 3C_plot ----
    if(step3C_Plot == "T")
    {
      script3C_status <- paste0(projdir, "/script5_PSSC.txt")
      if(file.exists(script3C_status))
      {
        quit()
      }
      rm(script3C_status)
      
      status_df <- before_step_exec_fun(step = "3C_plot", status_df=status_df, status_path=status_path, 
                           text = "plot PSSC-based PDF.")
      
      status_step3C_Plot <- sys::exec_wait(cmd = "Rscript", 
                                           args = c(paste0(virclust_path, "/VirClust_06_PLOT_tree_heatmap_annots.R"),
                                                    paste0("projdir=", projdir),
                                                    "pc_type=PSSC",
                                                    paste0("boot_pv=", status_df["2C", "boot_pv"]),
                                                    paste0("clust_dist=", status_df["3C", "clust_dist"]),
                                                    paste0("font_row=", font_row), 
                                                    paste0("font_col=", font_col),
                                                    paste0("stats_width=", stats_width), 
                                                    paste0("sil_stats_width=", sil_stats_width),
                                                    paste0("tree_width=", tree_width), 
                                                    paste0("stats_font=", stats_font), 
                                                    paste0("stats_lab_font=", stats_lab_font), 
                                                    paste0("lgd_width=", lgd_width), 
                                                    paste0("lgd_height=", lgd_height), 
                                                    paste0("lgd_font=", lgd_font), 
                                                    paste0("lgd_lab_font=", lgd_lab_font), 
                                                    paste0("inc_fact_w=", inc_fact_w), 
                                                    paste0("lgd_pos=", lgd_pos), 
                                                    paste0("show_tree=", show_tree), 
                                                    paste0("show_heat=", show_heat), 
                                                    paste0("show_protein_stats=", show_protein_stats), 
                                                    paste0("show_sil=", show_sil), 
                                                    paste0("show_clust_ID=", show_clust_ID), 
                                                    paste0("clustID_width=", clustID_width),
                                                    paste0("functions_path=", functions_path)), 
                                           std_out = paste0(projdir, "/step3C_Plot_std_out.txt"), 
                                           std_err = paste0(projdir, "/step3C_Plot_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "3C_Plot", status_df=status_df, status_path=status_path, status_step=status_step3C_Plot) 
      rm(status_step3C_Plot)
    }
    
    ###step 5A----
    if(step5A == "T")
    {
      status_df <- before_step_exec_fun(step = "5A", status_df=status_df, status_path=status_path, 
                           text = "calculate core proteins based on protein clusters.")
      
      status_step5A <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_07_core_proteins.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PC"), 
                                      std_out = paste0(projdir, "/step5A_std_out.txt"), 
                                      std_err = paste0(projdir, "/step5A_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "5A", status_df=status_df, status_path=status_path, status_step=status_step5A) 
      rm(status_step5A)
      
      tozip5A_p <- paste0(projdir, "/07/core_a")
      outzip5A_p <- paste0(projdir, "/07/core_a/core_a.zip")
      zipr(zipfile = outzip5A_p, files = tozip5A_p)
      rm(tozip5A_p, outzip5A_p)
    }
    
    ###step 4B----  
    if(step4B == "T")
    {
      status_df <- before_step_exec_fun(step = "4B", status_df=status_df, status_path=status_path, 
                           text = "calculate core proteins based on protein superclusters.")
      
      status_step4B <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_07_core_proteins.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSC"), 
                                      std_out = paste0(projdir, "/step4B_std_out.txt"), 
                                      std_err = paste0(projdir, "/step4B_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "4B", status_df=status_df, status_path=status_path, status_step=status_step4B) 
      rm(status_step4B)
      
      tozip4B_p <- paste0(projdir, "/07/core_b")
      outzip4B_p <- paste0(projdir, "/07/core_b/core_b.zip")
      zipr(zipfile = outzip4B_p, files = tozip4B_p)
      
      rm(tozip4B_p, outzip4B_p)
    }
    
    
    ###step 4C----  
    if(step4C == "T")
    {
      status_df <- before_step_exec_fun(step = "4C", status_df=status_df, status_path=status_path, 
                           text = "calculate core proteins based on protein super-superclusters.")
      
      status_step4C <- sys::exec_wait(cmd = "Rscript", 
                                      args = c(paste0(virclust_path, "/VirClust_07_core_proteins.R"),
                                               paste0("projdir=", projdir),
                                               "pc_type=PSSC"), 
                                      std_out = paste0(projdir, "/step4C_std_out.txt"), 
                                      std_err = paste0(projdir, "/step4C_std_err.txt"))
      
      status_df <- after_step_exec_fun(step= "4C", status_df=status_df, status_path=status_path, status_step=status_step4C) 
      rm(status_step4C)
      
      tozip4C_p <- paste0(projdir, "/07/core_c")
      outzip4C_p <- paste0(projdir, "/07/core_c/core_c.zip")
      zipr(zipfile = outzip4C_p, files = tozip4C_p)
      
      rm(tozip4C_p, outzip4C_p)
    }
    
    
    ## step 6AI ----
    status_df <- status_df <- run_annot_steps_fun(step_abrev = "6AI", step_val = step6AI, 
                        script = "VirClust_08_annotations_InterPro.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = interproscan)
    
    ## step 6ApV ----
    status_df <- run_annot_steps_fun(step_abrev = "6ApV", step_val = step6ApV, 
                        script = "VirClust_08_annotations_pVOGs.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 6AVO ----
    status_df <- run_annot_steps_fun(step_abrev = "6AVO", step_val = step6AVO, 
                        script = "VirClust_08_annotations_VOGDB.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 6APH ----
    status_df <- run_annot_steps_fun(step_abrev = "6APH", step_val = step6APH, 
                        script = "VirClust_08_annotations_PHROGS.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 6AE ----
    status_df <- run_annot_steps_fun(step_abrev = "6AE", step_val = step6AE, 
                        script = "VirClust_08_annotations_Efam.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 6AXC ----
    status_df <- run_annot_steps_fun(step_abrev = "6AXC", step_val = step6AXC, 
                        script = "VirClust_08_annotations_Efam-XC.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 6AN ----
    status_df <- run_annot_steps_fun(step_abrev = "6AN", step_val = step6AN, 
                        script = "VirClust_08_annotations_BLASTp.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = blastdb)
    
    ###step 6AM----  
    status_df <- run_merge_annot_fun(step_abrev = "6AM", step_val = step6AM, 
                        script = "VirClust_08b_consolidate_annotations.R", 
                        status_df, status_path, projdir, prot_type, virclust_path, functions_path)
    
    ## step 5BI ----
    status_df <- run_annot_steps_fun(step_abrev = "5BI", step_val = step5BI, 
                        script = "VirClust_08_annotations_InterPro.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = interproscan)
    
    ## step 5BpV ----
    status_df <- run_annot_steps_fun(step_abrev = "5BpV", step_val = step5BpV, 
                        script = "VirClust_08_annotations_pVOGs.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5BVO ----
    status_df <- run_annot_steps_fun(step_abrev = "5BVO", step_val = step5BVO, 
                        script = "VirClust_08_annotations_VOGDB.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5BPH ----
    status_df <- run_annot_steps_fun(step_abrev = "5BPH", step_val = step5BPH, 
                        script = "VirClust_08_annotations_PHROGS.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5BE ----
    status_df <- run_annot_steps_fun(step_abrev = "5BE", step_val = step5BE, 
                        script = "VirClust_08_annotations_Efam.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5BXC ----
    status_df <- run_annot_steps_fun(step_abrev = "5BXC", step_val = step5BXC, 
                        script = "VirClust_08_annotations_Efam-XC.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5BN ----
    status_df <- run_annot_steps_fun(step_abrev = "5BN", step_val = step5BN, 
                        script = "VirClust_08_annotations_BLASTp.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = blastdb)
    
    ###step 5BM----  
    status_df <- run_merge_annot_fun(step_abrev = "5BM", step_val = step5BM, 
                        script = "VirClust_08b_consolidate_annotations.R", 
                        status_df, status_path, projdir, prot_type, virclust_path, functions_path)
    
    
    ## step 5CI ----
    status_df <- run_annot_steps_fun(step_abrev = "5CI", step_val = step5CI, 
                        script = "VirClust_08_annotations_InterPro.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = interproscan)
    
    ## step 5CpV ----
    status_df <- run_annot_steps_fun(step_abrev = "5CpV", step_val = step5CpV, 
                        script = "VirClust_08_annotations_pVOGs.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5CVO ----
    status_df <- run_annot_steps_fun(step_abrev = "5CVO", step_val = step5CVO, 
                        script = "VirClust_08_annotations_VOGDB.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5CPH ----
    status_df <- run_annot_steps_fun(step_abrev = "5CPH", step_val = step5CPH, 
                        script = "VirClust_08_annotations_PHROGS.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5CE ----
    status_df <- run_annot_steps_fun(step_abrev = "5CE", step_val = step5CE, 
                        script = "VirClust_08_annotations_Efam.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5CXC ----
    status_df <- run_annot_steps_fun(step_abrev = "5CXC", step_val = step5CXC, 
                        script = "VirClust_08_annotations_Efam-XC.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = databases)
    
    ##step 5CN ----
    status_df <- run_annot_steps_fun(step_abrev = "5CN", step_val = step5CN, 
                        script = "VirClust_08_annotations_BLASTp.R", 
                        status_df, status_path, projdir, prot_type, cpu, virclust_path, functions_path, annot_path = blastdb)
    
    ###step 5CM----  
    status_df <- run_merge_annot_fun(step_abrev = "5CM", step_val = step5CM, 
                        script = "VirClust_08b_consolidate_annotations.R", 
                        status_df, status_path, projdir, prot_type, virclust_path, functions_path)
    
    
    print("VirClust has finished.")
    write(x="finished", file = log_path)
    
  }
}
