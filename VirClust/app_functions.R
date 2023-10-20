###########
proj_dir_fun <- function(prefix, User_name, Project_name, dir_path)
{
  sufi1 <- paste0(as.character(prefix+1), "___", User_name, "___",  Project_name, "___") %>% 
    str_replace_all(">", "_") %>%
    str_replace_all("<", "_") %>%
    str_replace_all("/", "_") %>%
    str_replace_all("'\'", "_") %>%
    str_replace_all(":", "_") %>%
    str_replace_all("\\.", "_") %>%
    str_replace_all("'", "_") %>%
    str_replace_all("´", "_") %>%
    str_replace_all("`", "_") %>%
    #str_replace_all("-", "") %>%
    str_replace_all(" ", "_")
  
  sufi2 <- as.character(Sys.time()) %>%
    str_replace_all( "-", "") %>%
    str_replace_all(":", "") %>%
    str_replace_all(" ", "")
  
  sufi <- paste0(sufi1, sufi2)
  
  path<-paste0(dir_path, "P", sufi)
  return(path)
}

###########
concat_prots_fun <- function(DF, genome_name, multi_d)
{
  DF_exp <- DF %>%
    arrange(cluster_ID)
  
  prot_p <- paste0(multi_d, "/", genome_name, "_core_prot_clust.faa")
  
  todel <- mapply(write.fasta, sequences = DF_exp$protein_seq, names = DF_exp$protein_name, file.out = prot_p, open = "a", as.string = TRUE)
  
  prot_ct <- paste0(DF_exp$protein_seq, collapse = "")
  return(prot_ct)
}

###########
export_prots_fun <- function(in_p, sel_clust, out_d, multi_d)
{
  tb <- readRDS(in_p) %>%
    filter(cluster_ID %in% sel_clust) %>%
    group_by(genome_name) %>%
    nest()
  
  tb$concat_prot <- mapply(concat_prots_fun, DF=tb$data, genome_name = tb$genome_name, multi_d=multi_d) %>%
    unlist()
  
  outcat_p <- paste0(out_d, "/concat_protclusters.faa")
  todel <- mapply(write.fasta, sequences = tb$concat_prot, names = tb$genome_name, file.out = outcat_p, open = "a", as.string = TRUE)
  
  
  return("ok")
}


##send email function
send_email <- function(to, body)
{
  from <- "liliana.cristina.moraru@uni-oldenburg.de"
  subject <- "VirClust"
  SMTP= list(smtpServer ="smtp.uni-oldenburg.de")
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = body, control = SMTP)
}

#######
exits_core_fun <- function(projdir, core_type)
{
  core_p <- paste0(projdir, "/07/core_", core_type, "/core_prots_for_annots_all.faa")
  return(file.size(core_p))
  #{return(TRUE)}else{return(FALSE)}
}

############
mes_after_run_fun <- function(step, status_step)
{
  if(status_step > 0)
  {
    return(paste0("Step ", step, " finished with status ", status_step, ", which represents an error. Aborting Virclust."))  #yeah, good question if I will see here anything else then status 0, because any errors will be captured by teh master script
  }else
  {
    return(paste0("Step ", step, " finished successfully with status 0."))
  }
}

check_exit_5_fun <- function(projdir, pc_type)
{
  s5_p <- paste0(projdir, "/script5_", pc_type, ".txt")
  if(file.exists(s5_p))
  {
    return(TRUE)
  }else
  {
    return(FALSE)
  }
}

check_singlegenomes_inVGC_fun <- function(projdir, pc_type)
{
  if(pc_type == "PC")
  {
    VGCdir <- paste0(projdir, "/04a-06a_genome_clustering_PC/05/VGCs_dist/")
  }
  
  if(pc_type == "PSC")
  {
    VGCdir <- paste0(projdir, "/04b-06b_genome_clustering_PSC/05/VGCs_dist/")
  }
  
  if(pc_type == "PSSC")
  {
    VGCdir <- paste0(projdir, "/04c-06c_genome_clustering_PSSC/05/VGCs_dist/")
  }
  
  in_dir <- paste0(projdir, "/00/00_out/")
  
  noVGCs <- list.dirs(path = VGCdir, recursive = FALSE) %>% length()
  noGenomes <- list.files(path = in_dir) %>% length()
  
  if(noVGCs == noGenomes)
  {return(TRUE)}else{return(FALSE)}
}

mes_after_run_split_stats_fun <- function(step, status_step, projdir, pc_type)
{
  if(check_exit_5_fun(projdir, pc_type))
  {
    text <- paste0("Only one ", pc_type, " was found in all genomes. The remaining proteins are singletons. Aborting this step. Plotting of the heatmap is disabled.")
  }else
  {
    if(check_singlegenomes_inVGC_fun(projdir, pc_type))
    {
      text <- "Each VGC has only one genome. Plotting of the heatmap is disabled."
    }else
    {
      if(status_step > 0)
      {
        text <- (paste0("Step ", step, " finished with status ", status_step, ", which represents an error. Aborting Virclust."))  #yeah, good question if I will see here anything else then status 0, because any errors will be captured by teh master script
      }else
      {
        text <- (paste0("Step ", step, " finished successfully with status 0."))
      }
    }
  }
  
  return(text)
}

###check if annot results exist
check_res_annots_fun <- function(projdir, prot_type, annot_suf)
{
  main_dir <- ret_annots_dir_fun(prot_type, projdir)
  annot_path <- paste0(main_dir, annot_suf)
  
  if(file.exists(annot_path) & file.size(annot_path) > 0)
  {return(TRUE)}else{return(FALSE)}
}

############
mes_after_run_annots_fun <- function(step, status_step, projdir, prot_type, annot_suf)
{
  if(status_step > 0)
  {
    return(paste0("Step ", step, " finished with status ", status_step, ", which represents an error. Aborting Virclust."))  #yeah, good question if I will see here anything else then status 0, because any errors will be captured by teh master script
  }else
  {
    if(check_res_annots_fun(projdir, prot_type, annot_suf))
    {
      return(paste0("Step ", step, " finished successfully with status 0."))
    }else
    {
      return(paste0("Step ", step, " finished successfully, but no annotations were found."))
    }
  }
}


############
mes_after_run_core_fun <- function(step, status_step, projdir, core_type)
{
  if(status_step > 0)
  {
    return(paste0("Step ", step, " finished with status ", status_step, ", which represents an error. Aborting Virclust."))  #yeah, good question if I will see here anything else then status 0, because any errors will be captured by teh master script
  }else
  {
    file_size <- exits_core_fun(projdir, core_type)
    
    if(file_size > 0)
    {
      return(paste0("Step ", step, " finished successfully with status 0."))
    }else
    {
      return(paste0("Step ", step, " finished successfully, but no core proteins were found."))
    }
  }
}

#########
start_step_email_fun <- function(email, user, name, path, step)
{
  
  send_email(to = email, 
             body = paste0("You are running step ", step, " from VirClust project ID ", str_remove(name, path),
                           ". Please save the projectID and use it to reload the project at a later time, to see the results or, if the server disconnects"))
  
  send_email(to = "liliana.cristina.moraru@uni-oldenburg.de", 
             body =  paste0("VirClust is running the ", step, " step, for user name ", user, " and project name ", name,
                            ". The unique ID of this project is ", str_remove(name, path)))
  
}

end_step_email_fun <- function(email, name, path, step, status)
{
  
  send_email(to = email, 
             body = paste0("The VirClust calculations for project ", str_remove(name, path), " and step ", step, ", have finished. 
                             Please load the project to check your results."))
  
  send_email(to = "liliana.cristina.moraru@uni-oldenburg.de", 
             body =  paste0("The VirClust calculations for project ", str_remove(name, path), " and step ", step, ", have finished with status ", status, 
                            " Please load the project to check your results."))
  
}


