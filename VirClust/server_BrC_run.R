#BRANCH C

### run 1C --------
run_1C <- eventReactive(input$actPSCs_2_PSSCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1C")}
    
    #### VirClust command
    st_1C <- system2(command = "Rscript", 
                     args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step1C=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " clust_PSSC=", input$clust_PSSC,
                                   " prob1_PSSC=", input$prob1_PSSC,
                                   " prob2_PSSC=", input$prob2_PSSC,
                                   " cov1_PSSC=", input$cov1_PSSC,
                                   " cov2_PSSC=", input$cov2_PSSC,
                                   " alig_PSSC=", input$alig_PSSC),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_1C.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_1C.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "1C", status = st_1C)}
    
    return(mes_after_run_fun(step= "1C", status_step = st_1C))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actPSCs_2_PSSCs, {
  rval_mes_1C$data <- run_1C()
  source("server_afterRunButtons.R", local = TRUE)
  
  row_del <- str_which(row.names(rval_shiny_opt_TB$data), "clust_PSSC")
  rval_shiny_opt_TB$data[row_del:nrow(rval_shiny_opt_TB$data), "value"] <- "NULL"
  
  rval_shiny_opt_TB$data[["clust_PSSC", "value"]] <- input$clust_PSSC
  rval_shiny_opt_TB$data[["prob1_PSSC", "value"]] <- input$prob1_PSSC
  rval_shiny_opt_TB$data[["cov1_PSSC", "value"]] <- input$cov1_PSSC
  rval_shiny_opt_TB$data[["prob2_PSSC", "value"]] <- input$prob2_PSSC
  rval_shiny_opt_TB$data[["cov2_PSSC", "value"]] <- input$cov1_PSSC
  rval_shiny_opt_TB$data[["alig_PSSC", "value"]] <- input$alig_PSSC
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_1C <- renderText({
  rval_mes_1C$data
})

###run 2C -----------
run_2C <- eventReactive(input$actCluster_genomes_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2C")}
    
    ###params
    if(input$boot_c == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
    
    #### VirClust command
    st_2C <- system2(command = "Rscript", 
                     args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step2C=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " pc_type=PSSC",
                                   " boot_pv_c=", boot_pv,
                                   " bootstrap_no_c=", input$boot_no_c,
                                   " aglom_c=", input$aglom_c),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_2C.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_2C.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "2C", status = st_2C)}
    
    return(mes_after_run_fun(step= "2C", status_step = st_2C))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCluster_genomes_PSSC, {
  rval_mes_2C$data <- run_2C()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(#2C
    "aglom_c", "boot_c", "boot_no_c",
    #2C_Plot
    "inc_fact_w_Pd_C", "font_row_Pd_C", "font_col_Pd_C", "font_cell_Pd_C",
    "lgd_font_Pd_C", "lgd_lab_font_Pd_C", "lgd_height_Pd_C", "lgd_width_Pd_C",
    #3C
    "Clust_dist_PSSC", "sel_PSSCs_heatmap_c",
    #3C_Plot
    "Show_Tree_c", "tree_width_c", "show_clust_ID_c", "show_sil_c",
    "show_protein_stats_c", "clustID_width_c", "Sil_stats_width_c", "Stats_width_c",
    "stats_font_c", "stats_lab_font_c", "show_heat_c", "Heat_width_c",
    "font_col_c", "font_row_c", "lgd_font_c", "lgd_lab_font_c",
    "lgd_h_c", "lgd_w_c")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["aglom_c", "value"]] <- input$aglom_c
  rval_shiny_opt_TB$data[["boot_c", "value"]] <- input$boot_c
  rval_shiny_opt_TB$data[["boot_no_c", "value"]] <- input$boot_no_c
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_2C <- renderText({
  rval_mes_2C$data
})

###run 2C_Plot ------
run_2C_Plot <- eventReactive(input$actOut_pdf_2C, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2C_Plot")}
    
    #### VirClust command
    st_2C_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                       " projdir=", rval_proj_name$data,
                                       " step2C_Plot=T",
                                       " continue=", rval_cont$data,
                                       " shiny=yes",
                                       
                                       #step options
                                       " pc_type=PSSC",
                                       " inc_fact_w_Pd=", input$inc_fact_w_Pd_C,
                                       " font_row_Pd=", input$font_row_Pd_C,
                                       " font_col_Pd=", input$font_col_Pd_C,
                                       " font_cell_Pd=", input$font_cell_Pd_C,
                                       " lgd_width_Pd=", input$lgd_width_Pd_C, 
                                       " lgd_height_Pd=", input$lgd_height_Pd_C,
                                       " lgd_font_Pd=", input$lgd_font_Pd_C,
                                       #"lgd_pos_Pd=", input$lgd_pos_Pd_A,
                                       " lgd_lab_font_Pd=", input$lgd_lab_font_Pd_C),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_2C_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_2C_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "2C_Plot", status = st_2C_Plot)}
    
    return(mes_after_run_fun(step= "2C_Plot", status_step = st_2C_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_2C, {
  rval_mes_2C_Plot$data <- run_2C_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]] <- input$inc_fact_w_Pd_C
  rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]] <- input$font_row_Pd_C
  rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]] <- input$font_col_Pd_C
  rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]] <- input$font_cell_Pd_C
  rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]] <- input$lgd_font_Pd_C
  rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]] <- input$lgd_lab_font_Pd_C
  rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]] <- input$lgd_height_Pd_C
  rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]] <- input$lgd_width_Pd_C
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_2C_Plot <- renderText({
  rval_mes_2C_Plot$data
})


### run 3C --------
run_3C <- eventReactive(input$actSplit_clust_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3C")}
    
    #### VirClust command
    st_3C <- system2(command = "Rscript", 
                     args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step3C=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " pc_type=PSSC",
                                   " clust_dist_c=", input$Clust_dist_PSSC,
                                   " max_cols_HT=", input$sel_PSSCs_heatmap_c),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_3C.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_3C.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3C", status = st_3C)}
    
    return(mes_after_run_split_stats_fun(step= "3C", status_step = st_3C, projdir =  rval_proj_name$data, pc_type="PSSC"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actSplit_clust_PSSC, {
  rval_mes_3C$data <- run_3C()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(#3C
    "Clust_dist_PSSC", "sel_PSSCs_heatmap_c",
    #3C_Plot
    "Show_Tree_c", "tree_width_c", "show_clust_ID_c", "show_sil_c",
    "show_protein_stats_c", "clustID_width_c", "Sil_stats_width_c", "Stats_width_c",
    "stats_font_c", "stats_lab_font_c", "show_heat_c", "Heat_width_c",
    "font_col_c", "font_row_c", "lgd_font_c", "lgd_lab_font_c",
    "lgd_h_c", "lgd_w_c")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]] <- input$Clust_dist_PSSC
  rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]] <- input$sel_PSSCs_heatmap_c
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3C <- renderText({
  rval_mes_3C$data
})

### run 3C_Plot ------
run_3C_Plot <- eventReactive(input$actOut_pdf_3C, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3C_Plot")}
    
    
    #show params 
    if(input$Show_Tree_c == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
    if(input$show_heat_c == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
    if(input$show_protein_stats_c == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
    if(input$show_sil_c == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
    if(input$show_clust_ID_c == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
    
    #### VirClust command
    st_3C_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                        " projdir=", rval_proj_name$data,
                                        " step3C_Plot=T",
                                        " continue=", rval_cont$data,
                                        " shiny=yes",
                                        
                                        #step options
                                        " pc_type=PSSC",
                                        " font_row=", input$font_row_c, 
                                        " font_col=", input$font_col_c,
                                        " stats_width=", input$Stats_width_c, 
                                        " sil_stats_width=", input$Sil_stats_width_c,
                                        " tree_width=", input$tree_width_c, 
                                        " stats_font=", input$stats_font_c, 
                                        " stats_lab_font=", input$stats_lab_font_c, 
                                        " lgd_width=", input$lgd_w_c, 
                                        " lgd_height=", input$lgd_h_c, 
                                        " lgd_font=", input$lgd_font_c, 
                                        " lgd_lab_font=", input$lgd_lab_font_c, 
                                        " inc_fact_w=", input$Heat_width_c, 
                                        #"lgd_pos=", , 
                                        " show_tree=", show_tree, 
                                        " show_heat=", show_heat, 
                                        " show_protein_stats=", show_protein_stats, 
                                        " show_sil=", show_sil, 
                                        " show_clust_ID=", show_clust_ID, 
                                        " clustID_width=", input$clustID_width_c),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_3C_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_3C_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3C_Plot", status = st_3C_Plot)}
    
    return(mes_after_run_fun(step= "3C_Plot", status_step = st_3C_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_3C, {
  rval_mes_3C_Plot$data <- run_3C_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["Show_Tree_c", "value"]] <- input$Show_Tree_c
  rval_shiny_opt_TB$data[["tree_width_c", "value"]] <- input$tree_width_c
  rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]] <- input$show_clust_ID_c
  rval_shiny_opt_TB$data[["show_sil_c", "value"]] <- input$show_sil_c
  rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]] <- input$show_protein_stats_c
  rval_shiny_opt_TB$data[["clustID_width_c", "value"]] <- input$clustID_width_c
  rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]] <- input$Sil_stats_width_c
  rval_shiny_opt_TB$data[["Stats_width_c", "value"]] <- input$Stats_width_c
  rval_shiny_opt_TB$data[["stats_font_c", "value"]] <- input$stats_font_c
  rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]] <- input$stats_lab_font_c
  rval_shiny_opt_TB$data[["show_heat_c", "value"]] <- input$show_heat_c
  rval_shiny_opt_TB$data[["Heat_width_c", "value"]] <- input$Heat_width_c
  rval_shiny_opt_TB$data[["font_col_c", "value"]] <- input$font_col_c
  rval_shiny_opt_TB$data[["font_row_c", "value"]] <- input$font_row_c
  rval_shiny_opt_TB$data[["lgd_font_c", "value"]] <- input$lgd_font_c
  rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]] <- input$lgd_lab_font_c
  rval_shiny_opt_TB$data[["lgd_h_c", "value"]] <- input$lgd_h_c
  rval_shiny_opt_TB$data[["lgd_w_c", "value"]] <- input$lgd_w_c
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3C_Plot <- renderText({
  rval_mes_3C_Plot$data
})


### run 4C --------
run_4C <- eventReactive(input$actCore_PSSCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4C")}
    
    #### VirClust command
    st_4C <- system2(command = "Rscript", 
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step4C=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " pc_type=PSSC"),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_4C.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_4C.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "4C", status = st_4C)}
    
    return(mes_after_run_core_fun(step= "4C", status_step = st_4C, projdir = rval_proj_name$data, core_type = "c"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCore_PSSCs, {
  rval_mes_4C$data <- run_4C()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]] <- input$Annot_prots_PSSCs
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_4C <- renderText({
  rval_mes_4C$data
})


#####Annots ---

### run 5CI --------
run_5CI <- eventReactive(input$actAnnotInterPro_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CI")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CI <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step5CI=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CI.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CI.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CI", status = st_5CI)}
    
    return(mes_after_run_annots_fun(step= "5CI", status_step = st_5CI,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotInterPro_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CI$data <- run_5CI()}else{rval_mes_5CI_core$data <- run_5CI()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CI <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CI$data
  }else
  {
    text <- rval_mes_5CI_core$data
  }
  return(text)
})


### run 5CpV --------
run_5CpV <- eventReactive(input$actAnnotpVOGs_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CpV")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CpV <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step5CpV=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CpV.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CpV.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CpV", status = st_5CpV)}
    
    return(mes_after_run_annots_fun(step= "5CpV", status_step = st_5CpV,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotpVOGs_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CpV$data <- run_5CpV()}else{rval_mes_5CpV_core$data <- run_5CpV()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CpV <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CpV$data
  }else
  {
    text <- rval_mes_5CpV_core$data
  }
  return(text)
})


### run 5CVO --------
run_5CVO <- eventReactive(input$actAnnotVOGDB_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CVO")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CVO <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step5CVO=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CVO.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CVO.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CVO", status = st_5CVO)}
    
    return(mes_after_run_annots_fun(step= "5CVO", status_step = st_5CVO,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotVOGDB_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CVO$data <- run_5CVO()}else{rval_mes_5CVO_core$data <- run_5CVO()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CVO <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CVO$data
  }else
  {
    text <- rval_mes_5CVO_core$data
  }
  return(text)
})

### run 5CPH --------
run_5CPH <- eventReactive(input$actAnnotPHROGS_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CPH")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CPH <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step5CPH=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CPH.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CPH.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CPH", status = st_5CPH)}
    
    return(mes_after_run_annots_fun(step= "5CPH", status_step = st_5CPH,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotPHROGS_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CPH$data <- run_5CPH()}else{rval_mes_5CPH_core$data <- run_5CPH()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CPH <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CPH$data
  }else
  {
    text <- rval_mes_5CPH_core$data
  }
  return(text)
})

### run 5CE --------
run_5CE <- eventReactive(input$actAnnotEfam_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CE")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CE <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step5CE=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CE.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CE.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CE", status = st_5CE)}
    
    return(mes_after_run_annots_fun(step= "5CE", status_step = st_5CE,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotEfam_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CE$data <- run_5CE()}else{rval_mes_5CE_core$data <- run_5CE()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CE <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CE$data
  }else
  {
    text <- rval_mes_5CE_core$data
  }
  return(text)
})


### run 5CXC --------
run_5CXC <- eventReactive(input$`actAnnotEfam-XC_PSSC`, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CXC")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CXC <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step5CXC=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CXC.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CXC.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CXC", status = st_5CXC)}
    
    return(mes_after_run_annots_fun(step= "5CXC", status_step = st_5CXC,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$`actAnnotEfam-XC_PSSC`, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CXC$data <- run_5CXC()}else{rval_mes_5CXC_core$data <- run_5CXC()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CXC <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CXC$data
  }else
  {
    text <- rval_mes_5CXC_core$data
  }
  return(text)
})

### run 5CN --------
run_5CN <- eventReactive(input$actAnnotNCBI_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CN")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CN <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step5CN=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CN.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CN.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CN", status = st_5CN)}
    
    return(mes_after_run_annots_fun(step= "5CN", status_step = st_5CN,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotNCBI_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CN$data <- run_5CN()}else{rval_mes_5CN_core$data <- run_5CN()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CN <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CN$data
  }else
  {
    text <- rval_mes_5CN_core$data
  }
  return(text)
})


### run 5CM --------
run_5CM <- eventReactive(input$actMergeAnnot_PSSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CM")}
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
    
    
    #### VirClust command
    st_5CM <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step5CM=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5CM.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5CM.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5CM", status = st_5CM)}
    
    return(mes_after_run_fun(step= "5CM", status_step = st_5CM))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actMergeAnnot_PSSC, {
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {rval_mes_5CM$data <- run_5CM()}else{rval_mes_5CM_core$data <- run_5CM()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5CM <- renderText({
  if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
  {
    text <- rval_mes_5CM$data
  }else
  {
    text <- rval_mes_5CM_core$data
  }
  return(text)
})