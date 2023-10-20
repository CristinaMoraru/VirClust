### Master Script -----------------
reset_dir_fun <- function(dir)
{
  if(dir.exists(dir)){unlink(dir, recursive = TRUE)}
  dir.create(dir, recursive = TRUE)
    
}


before_step_exec_fun <- function(step, status_df, status_path, text)
{
  status_df[step, "status"] <- "running"
  write.table(x = status_df, file = status_path, append = FALSE, sep = "\t", row.names = TRUE, col.names = TRUE)
  print(paste0("Running VirClust step ", step, ": ", text))
  
  return(status_df)
}

after_step_exec_fun <- function(step, status_df, status_path, status_step)
{
  if(status_step > 0)
  {
    print(paste0("Step ", step, " has finished with status ", status_step, ", which represents an error. Aborting Virclust. Check the log files."))
    
    status_df[step, "status"] <- "not_run"
    write.table(x = status_df, file = status_path, append = FALSE, sep = "\t", row.names = TRUE, col.names = TRUE)
    
    write(x="finished", file = log_path)
    
    quit()
  }else
  {
    print(paste0("Step ", step, " has finished successfully with status 0."))
    
    status_df[step, "status"] <- "done"
    write.table(x = status_df, file = status_path, append = FALSE, sep = "\t",  row.names = TRUE, col.names = TRUE)
  }
  
  return(status_df)
}

###checking exec_wait status for regular runs
check_exec_st_fun <- function(st, who)
{
  if(st != 0)
  {
    print(paste0(who, " finished with error status ", st, ". Aborting VirClust"))
    quit()
  }
}

###get step function
get_step_fun <- function(input0, step_name)
{
  step_n <- str_which(input0, paste0("^", step_name, "="))
  if(length(step_n) > 1)
  {
    print(paste0("The step ", step_name, " parameter was given more than once. Aborting VirClust."))
    quit()
  }
  if(length(step_n) == 0)
  {
    step_value <- "F"
  }
  if(length(step_n) == 1)
  {
    step_value <- str_remove(input0[step_n], paste0("^", step_name, "="))
    
    if(!step_value %in% c("T", "F"))
    {
      print(paste0("Invalid step parameter: ", step_value, ". Aborting VirClust."))
      quit()
    }
  }
  rm(step_n)
  
  return(step_value)
}


###get params function
get_params_fun <- function(input0, param_name, type, default_val="", min_val=0, max_val = 100, allowed = "", status_path = "")
{
  param_n <- str_which(input0, paste0("^", param_name, "="))
  
  #duplicated param names
  if(length(param_n) > 1)
  {
    print(paste0("The ", param_name, " parameter was given more than once. Aborting VirClust."))
    quit()
  }
  
  #default param values
  if(length(param_n) == 0)
  {
    #cpu param
    if(param_name == "cpu")
    {
      param_value <- parallel::detectCores()/2
    }else
    {
      #params without default
      if(param_name == "projdir")
      {
        print(paste0("Missing ", param_name, " parameter. Aborting VirClust."))
        quit()
      }
      
      if(param_name == "infile" & shiny == "no")
      {
        print(paste0("Missing ", param_name, " parameter. Aborting VirClust."))
        quit()
      }
      
      #params with default
      param_value <- default_val
      
      if(type == "Int")
      {param_value <- as.integer(param_value)}
      
      if(type == "Num")
      {param_value <- as.numeric(param_value)}
    }
  }
  
  #specified params
  if(length(param_n) == 1)
  {
    param_value <- str_remove(input0[param_n], paste0("^", param_name, "="))
    
    if(type == "Int")
    {
      param_value <- as.integer(param_value)
      
      if(is.na(param_value) | param_value < min_val)
      {
        print(paste0("Non-numeric / invalid ", param_name, " parameter. Aborting VirClust."))
        quit()
      }
    }
    
    if(type == "Num")
    {
      param_value <- as.numeric(param_value)
      
      if(is.na(param_value) | param_value < min_val)
      {
        print(paste0("Non-numeric / invalid ", param_name, " parameter. Aborting VirClust."))
        quit()
      }
      
      if(str_detect(param_name, "clust_dist_"))
      {
        if(param_value > max_val)
        {
          print(paste0("Non-numeric / invalid ", param_name, " parameter. Aborting VirClust."))
          quit()
        }
      }
    }
    
    if(type == "Text")
    {
      if(!param_name %in% c("projdir", "infile", "condaenvpath", "blastdb", "interproscan", "databases"))
      {
        if(!param_value %in% allowed)
        {
          print(paste0("Invalid ", param_name, " parameter value: ", param_value, ". Aborting VirClust."))
          quit()
        }
      }
      
      
      if(param_name == "continue" & param_value == "yes")
      {
        if(file.exists(status_path) == FALSE)
        {
          print(paste0("Cannot continue, previous project doesn't seem to exit. Aborting VirClust."))
          quit()
        }
      }
    }
  }
  rm(param_n)
  
  return(param_value)
}


### check previous steps fun
check_prev_steps_fun <- function(status_df, cur_steps_val, prev_step, prev_step_val)
{
  for(i in 1:length(cur_steps_val))
  {
    if(status_df[prev_step, "status"] == "not_run" & (cur_steps_val[i] == "T"))# & prot_type == "core_PSSCs"))
    {
      prev_step_val <- "T"
      print(paste0("Step ", prev_step," was automatically activated (if you have not done so yourself), because it is required by other selected steps."))
    }
  }
  rm(i)
  
  return(prev_step_val)
}

check_prev_steps_annots_fun <- function(status_df, cur_steps_val, prev_step, prev_step_val, prot_type)
{
  if(prot_type %in% c("core_PCs", "core_PSCs", "core_PSSCs"))
  {
  
  for(i in 1:length(cur_steps_val))
  {
    if(status_df[prev_step, "status"] == "not_run" & (cur_steps_val[i] == "T"))
    {
      prev_step_val <- "T"
      print(paste0("Step ", prev_step," was automatically activated (if you have not done so yourself), because it is required by other selected steps."))
    }
  }
  rm(i)
  }
  return(prev_step_val)
}


### run annotation steps
run_annot_steps_fun <- function(step_abrev, step_val, script, status_df, status_path,
                                projdir, prot_type, cpu, virclust_path, functions_path, annot_path)
{
  if(step_val == "T")
  {
    if(prot_type %in% c("core_PCs", "core_PSCs", "core_PSSCs"))
    {
      step_abrev <- paste0(step_abrev, "_core")
    }
    
    status_df <- before_step_exec_fun(step = step_abrev, status_df=status_df, status_path=status_path, 
                         text = "annotate proteins")
    
    status_step <- sys::exec_wait(cmd = "Rscript", 
                                  args = c(paste0(virclust_path, "/", script),
                                           paste0("projdir=", projdir),
                                           paste0("prot_type=", prot_type),
                                           paste0("cpu=", cpu),
                                           paste0("virclust_path=", virclust_path),
                                           paste0("functions_path=", functions_path),
                                           paste0("annot_path=", annot_path)
                                  ),
                                  std_out = paste0(projdir, "/step", step_abrev, "_std_out.txt"), 
                                  std_err = paste0(projdir, "/step", step_abrev, "_std_err.txt"))
    
    status_df <- after_step_exec_fun(step= step_abrev, status_df=status_df, status_path=status_path, status_step=status_step) 
    rm(status_step)
  }
  
  return(status_df)
}


run_merge_annot_fun <- function(step_abrev, step_val, script, status_df, status_path,
                                projdir, prot_type, virclust_path, functions_path)
{
  if(step_val == "T")
  {
    if(prot_type %in% c("core_PCs", "core_PSCs", "core_PSSCs"))
    {
      step_abrev <- paste0(step_abrev, "_core")
    }
    
    status_df <- before_step_exec_fun(step = step_abrev, status_df=status_df, status_path=status_path, 
                         text = "annotate proteins")
    
    status_step <- sys::exec_wait(cmd = "Rscript", 
                                  args = c(paste0(virclust_path, "/", script),
                                           paste0("projdir=", projdir),
                                           paste0("prot_type=", prot_type),
                                           paste0("functions_path=", functions_path)
                                  ),
                                  std_out = paste0(projdir, "/step", step_abrev, "_std_out.txt"), 
                                  std_err = paste0(projdir, "/step", step_abrev, "_std_err.txt"))
    
    status_df <- after_step_exec_fun(step= step_abrev, status_df=status_df, status_path=status_path, status_step=status_step) 
    rm(status_step)
  }
  
  return(status_df)
}

###reset at run previous steps
reset_post_steps_by_pos_fun <- function(cur_step_abrev, cur_step_val, status_df)#, dir_to_del, files_to_del)
{
  if(cur_step_val == "T")
  {
    cur_row <- str_which(rownames(status_df), paste0("^", cur_step_abrev, "$"))
    
    for(i in (cur_row):nrow(status_df))
    {
      todel_row <- i
      #print(paste("Row is", todel_row))
      
      if(status_df[[todel_row, "path_type"]] == "file" & status_df[[todel_row, "status"]] == "done")
      {
        file.remove(status_df[[todel_row, "out_path"]])
      }
      
      if(status_df[[todel_row, "path_type"]] == "dir" & status_df[[todel_row, "status"]] == "done")
      {
        unlink(status_df[[todel_row, "out_path"]], recursive = TRUE)
      }
      
      if(rownames(status_df)[todel_row] %in% c("3A", "2B", "2C"))
      {
        status_df[todel_row, c("boot_pv", "aglom")] <- ""
      }
      
      if(rownames(status_df)[todel_row] %in% c("4A", "3B", "3C"))
      {
        status_df[todel_row, c("max_cols_HT", "clust_dist")] <- 0
      }
      
      status_df[[todel_row, "status"]] <- "not_run"
      rm(todel_row)
    }
  }
  
  return(status_df)
}

reset_post_steps_by_name_fun <- function(cur_step_abrev, cur_step_val, del_step_abrev, status_df, core = NULL)
{
  if(cur_step_val == "T")
  {
    for(i in 1:length(del_step_abrev))
    {
      todel_row <- str_which(rownames(status_df), paste0("^", del_step_abrev[i], "$"))
      
      if(status_df[[todel_row, "path_type"]] == "file" & status_df[[todel_row, "status"]] == "done")
      {
        file.remove(status_df[[todel_row, "out_path"]])
      }
      
      if(status_df[[todel_row, "path_type"]] == "dir" & status_df[[todel_row, "status"]] == "done")
      {
        unlink(status_df[[todel_row, "out_path"]], recursive = TRUE)
      }
      
      if(rownames(status_df)[todel_row] %in% c("3A", "2B", "2C"))
      {
        status_df[todel_row, c("boot_pv", "aglom")] <- ""
      }
      
      if(rownames(status_df)[todel_row] %in% c("4A", "3B", "3C"))
      {
        status_df[todel_row, c("max_cols_HT", "clust_dist")] <- 0
      }
      
      status_df[[todel_row, "status"]] <- "not_run"
      rm(todel_row)
    }
  }
  
  return(status_df)
}

reset_steps_by_list_fun <- function(steps_to_del, step_val, status_df)
{
  for(i in 1:length(steps_to_del))
  {
    if(step_val[i] == "T")
    {
      todel_row <- str_which(rownames(status_df),  paste0("^", steps_to_del[i], "$"))
      
      if(status_df[[todel_row, "path_type"]] == "file" & status_df[[todel_row, "status"]] == "done")
      {
        file.remove(status_df[[todel_row, "out_path"]])
      }
      
      if(status_df[[todel_row, "path_type"]] == "dir" & status_df[[todel_row, "status"]] == "done")
      {
        unlink(status_df[[todel_row, "out_path"]], recursive = TRUE)
      }
      
      if(rownames(status_df)[todel_row] %in% c("3A", "2B", "2C"))
      {
        status_df[todel_row, c("boot_pv", "aglom")] <- ""
      }
      
      if(rownames(status_df)[todel_row] %in% c("4A", "3B", "3C"))
      {
        status_df[todel_row, c("max_cols_HT", "clust_dist")] <- 0
      }
      
      status_df[[todel_row, "status"]] <- "not_run"
      
      rm(todel_row)
    }
  }
  return(status_df)
}



reset_annot_steps_fun <- function(steps_to_del, step_val, m_to_del, status_df, prot_type)
{
  an <- 0
  for(i in 1:length(steps_to_del))
  {
    if(step_val[i] == "T")
    {
      if(!steps_to_del[i] %in% c("6AM", "5BM", "5CM"))
      {
        an <- an + 1
      }
      
      if(prot_type %in% c("core_PCs", "core_PSCs", "core_PSSCs"))
      {
        steps_to_del[i] <- paste0(steps_to_del[i], "_core")
      }
      
      todel_row <- str_which(rownames(status_df),  paste0("^", steps_to_del[i], "$"))
      
      if(status_df[[todel_row, "path_type"]] == "file" & status_df[[todel_row, "status"]] == "done")
      {
        file.remove(status_df[[todel_row, "out_path"]])
      }
      
      if(status_df[[todel_row, "path_type"]] == "dir" & status_df[[todel_row, "status"]] == "done")
      {
        unlink(status_df[[todel_row, "out_path"]], recursive = TRUE)
      }
      
      if(rownames(status_df)[todel_row] %in% c("3A", "2B", "2C"))
      {
        status_df[todel_row, c("boot_pv", "aglom")] <- ""
      }
      
      if(rownames(status_df)[todel_row] %in% c("4A", "3B", "3C"))
      {
        status_df[todel_row, c("max_cols_HT", "clust_dist")] <- 0
      }
      
      status_df[[todel_row, "status"]] <- "not_run"
      
      rm(todel_row)
    }
  }
  rm(i)
  
  if(an > 0)
  {
    if(prot_type %in% c("core_PCs", "core_PSCs", "core_PSSCs"))
    {
      m_to_del <- paste0(m_to_del, "_core")
    }
    
    todel_row <- str_which(rownames(status_df),  paste0("^", m_to_del, "$"))
    
    if(status_df[[todel_row, "path_type"]] == "file" & status_df[[todel_row, "status"]] == "done")
    {
      file.remove(status_df[[todel_row, "out_path"]])
    }
    
    if(status_df[[todel_row, "path_type"]] == "dir" & status_df[[todel_row, "status"]] == "done")
    {
      unlink(status_df[[todel_row, "out_path"]], recursive = TRUE)
    }
    
    if(rownames(status_df)[todel_row] %in% c("3A", "2B", "2C"))
    {
      status_df[todel_row, c("boot_pv", "aglom")] <- ""
    }
    
    if(rownames(status_df)[todel_row] %in% c("4A", "3B", "3C"))
    {
      status_df[todel_row, c("max_cols_HT", "clust_dist")] <- 0
    }
    
    status_df[[todel_row, "status"]] <- "not_run"
    
    rm(todel_row)
  }
  
  return(status_df)
}

#########Functions for the individual scripts ------------------

###get params simple function

get_params_simple_fun <- function(input0, param_name, type="text")
{
  param_pat <- paste0("^", param_name, "=")
  param_pos <- str_which(input0, param_pat)
  param_value <- str_remove(input0[param_pos], param_pat)
  
  if(type == "numeric")
  {
    param_value <- as.numeric(param_value)
  }
  
  if(type == "Int")
  {
    param_value <- as.integer(param_value)
  }
  
  return(param_value)
}

### protein clustering based on norm bitscore for BLASTP 
norm_bitscore_fun <- function(prot1, prot2, DF)
{
  DF_t <- DF %>%
    filter((V1 == prot1 & V2 == prot1) | 
             (V1 == prot1 & V2 == prot2) | 
             (V1 == prot2 & V2 == prot2) | 
             (V1 == prot2 & V2 == prot1))
  
  DF_11 <- DF_t %>%
    filter(V1 == prot1 & V2 == prot1)
  bit_11 <- DF_11$bitscore %>%
    max()
  
  DF_12 <- DF_t %>%
    filter(V1 == prot1 & V2 == prot2)
  bit_12 <- DF_12$bitscore %>%
    max()
  
  DF_22 <- DF_t %>%
    filter(V1 == prot2 & V2 == prot2)
  bit_22 <- DF_22$bitscore %>%
    max()
  
  DF_21 <- DF_t %>%
    filter(V1 == prot2 & V2 == prot1)
  bit_21 <- DF_21$bitscore %>%
    max()
  
  norm_bit_1 <- bit_12/bit_11 
  norm_bit_2 <- bit_21/bit_22
  
  rel_12 <- max(norm_bit_1, norm_bit_2) %>%
    round(digits = 5)
  
  return(rel_12)
}


### protein clustering based on norm bitscore for HMMs 
norm_Score_HMM_fun <- function(prot1, prot2, DF)
{
  DF_t <- DF %>%
    filter((query_cluster == prot1 & target_cluster == prot1) | 
             (query_cluster == prot1 & target_cluster == prot2) | 
             (query_cluster == prot2 & target_cluster == prot2) | 
             (query_cluster == prot2 & target_cluster == prot1))
  
  DF_11 <- DF_t %>%
    filter(query_cluster == prot1 & target_cluster == prot1)
  bit_11 <- DF_11$Score %>%
    max()
  
  DF_12 <- DF_t %>%
    filter(query_cluster == prot1 & target_cluster == prot2)
  bit_12 <- DF_12$Score %>%
    max()
  
  DF_22 <- DF_t %>%
    filter(query_cluster == prot2 & target_cluster == prot2)
  bit_22 <- DF_22$Score %>%
    max()
  
  DF_21 <- DF_t %>%
    filter(query_cluster == prot2 & target_cluster == prot1)
  bit_21 <- DF_21$Score %>%
    max()
  
  norm_bit_1 <- bit_12/bit_11 
  norm_bit_2 <- bit_21/bit_22
  
  rel_12 <- max(norm_bit_1, norm_bit_2) %>%
    round(digits = 5)
  
  return(rel_12)
}

### Return path for proteins to be annotated
multiFasta_AnnotProt_path_fun <- function(prot_type, projdir)
{
  if(prot_type %in% c("all_PCs","all_PSCs", "all_PSSCs"))
  {
    faa_p <- paste0(projdir, "/01/01_all_proteins.faa")
  }
  
  if(prot_type == "core_PCs")
  {
    faa_p <- paste0(projdir, "/07/core_a/core_prots_for_annots_all.faa")
  }
  
  if(prot_type == "core_PSCs")
  {
    faa_p <- paste0(projdir, "/07/core_b/core_prots_for_annots_all.faa")
  }
  
  
  if(prot_type == "core_PSSCs")
  {
    faa_p <- paste0(projdir, "/07/core_c/core_prots_for_annots_all.faa")
  }
  
  return(faa_p)
}

#return folder for proteins to be annotated
dir_AnnotProt_path_fun <- function(prot_type, projdir)
{
  if(prot_type %in% c("all_PCs","all_PSCs", "all_PSSCs"))
  {
    faa_p <- paste0(projdir, "/01/01_all_proteins_indiv")
  }
  
  if(prot_type == "core_PCs")
  {
    faa_p <- paste0(projdir, "/07/core_a/all_proteins_indiv")
  }
  
  if(prot_type == "core_PSCs")
  {
    faa_p <- paste0(projdir, "/07/core_b/all_proteins_indiv")
  }
  
  
  if(prot_type == "core_PSSCs")
  {
    faa_p <- paste0(projdir, "/07/core_c/all_proteins_indiv")
  }
  
  return(faa_p)
}

### Return protDF for proteins to be annotated
protDFAnnot_fun <- function(prot_type, projdir)
{
  if(prot_type == "all_PCs")
  {
    file_ls <- paste0(projdir, "/02/02_04_genome_protDF_PCs.RDS")
    prot_DF <- readRDS(file_ls)
    rm(file_ls)
  }
  
  if(prot_type == "core_PCs")
  {
    folder <- paste0(projdir, "/07/core_a")
    file_ls <- list.files(path = folder, pattern = "\\.RDS$", full.names = TRUE)
    protDF_ls <- lapply(X = file_ls, FUN = readRDS)
    prot_DF <- bind_rows(protDF_ls)
    rm(folder, file_ls, protDF_ls)
  }
  
  if(prot_type == "all_PSCs")
  {
    file_ls <- paste0(projdir, "/03/03_09_genome_protDF_PCs_PSCs.RDS")
    prot_DF <- readRDS(file_ls)
    rm(file_ls)
  }
  
  if(prot_type == "core_PSCs")
  {
    folder <- paste0(projdir, "/07/core_b")
    file_ls <- list.files(path = folder, pattern = "\\.RDS$", full.names = TRUE)
    protDF_ls <- lapply(X = file_ls, FUN = readRDS)
    prot_DF <- bind_rows(protDF_ls)
    rm(folder, file_ls, protDF_ls)
  }
  
  if(prot_type == "all_PSSCs")
  {
    file_ls <- paste0(projdir, "/03C/03C_09_genome_protDF_PSCs_PSSCs.RDS")
    prot_DF <- readRDS(file_ls)
    rm(file_ls)
  }
  
  if(prot_type == "core_PSSCs")
  {
    folder <- paste0(projdir, "/07/core_c")
    file_ls <- list.files(path = folder, pattern = "\\.RDS$", full.names = TRUE)
    protDF_ls <- lapply(X = file_ls, FUN = readRDS)
    prot_DF <- bind_rows(protDF_ls)
    rm(folder, file_ls, protDF_ls)
  }
  
  return(prot_DF)
}

##return the folder where the annotation results should be saved
ret_annots_dir_fun <- function(prot_type, projdir)
{
  if(prot_type %in% c("all_PCs", "all_PSCs", "all_PSSCs"))
  {P_D <- paste0(projdir, "/08_annots")}
  
  if(prot_type == "core_PCs")
  {P_D <- paste0(projdir, "/08_annots_core_a")}
  
  if(prot_type == "core_PSCs")
  {P_D <- paste0(projdir, "/08_annots_core_b")}
  
  if(prot_type == "core_PSSCs")
  {P_D <- paste0(projdir, "/08_annots_core_c")}
  
  return(P_D)  
}


###functions for hhsuite
hhsearch_fun <- function(prot_file_p, hrr_p, hhsearch_tsv_p, hhsearch_log_p, hhsearch_err_p, hhsuite_DB)
{
  hhsearch_args <-  paste0("hhsearch -i ", prot_file_p,
                           " -o ", hrr_p,
                           " -blasttab ", hhsearch_tsv_p,
                           " -d ", hhsuite_DB,
                           " -id 100 -diff 0 -cpu ", cpu, " -p 50 -z 1 -Z 600 ",
                           " > ", hhsearch_log_p, " 2> ", hhsearch_err_p)
  
  system(hhsearch_args)
}

import_hhsuite_VOGDB_res_fun <- function(hhsuite_path) 
{
  score_TB <- readr::read_table2(file = hhsuite_path , col_names = FALSE, progress = FALSE,
                                 col_types = cols(
                                   X1 = col_character(),
                                   X2 = col_character(),
                                   X3 = col_double(),
                                   X4 = col_double(),
                                   X5 = col_double(),
                                   X6 = col_double(),
                                   X7 = col_double(),
                                   X8 = col_double(),
                                   X9 = col_double(),
                                   X10 = col_double(),
                                   X11 = col_double(),
                                   X12 = col_double()
                                 )) %>%
    as_tibble() %>%
    dplyr::rename(query= X1, DB_ID=X2, eval=X11, score= X12) %>%
    filter(eval < 0.01)
  
  annots <- ""
  if(nrow(score_TB) > 0)
  {
    score_TB <- score_TB %>%
      arrange(desc(score))
    
    annots[1] <- score_TB$DB_ID[1]
    annots[2] <- score_TB$score[1]
    annots[3] <- score_TB$eval[1]
  }else
  {
    annots[1] <- "no_hit"
    annots[2] <- "no_hit"
    annots[3] <- "no_hit"
  }
  
  return(annots)
}
