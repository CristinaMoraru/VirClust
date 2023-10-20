#BRANCH B

### run 1B --------
run_1B <- eventReactive(input$actPCs_2_PSCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1B")}
    
    #### VirClust cmd
    # st_1B <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step1B=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  paste0("clust_PSC=", input$clust_PSC),
    #                                  paste0("prob1_PSC=", input$prob1_PSC),
    #                                  paste0("prob2_PSC=", input$prob2_PSC),
    #                                  paste0("cov1_PSC=", input$cov1_PSC),
    #                                  paste0("cov2_PSC=", input$cov2_PSC),
    #                                  paste0("alig_PSC=", input$alig_PSC)),
    #                         
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_1B.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_1B.txt"))
    
    st_1B <- system2(command = "Rscript", 
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step1B=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " clust_PSC=", input$clust_PSC,
                              " prob1_PSC=", input$prob1_PSC,
                              " prob2_PSC=", input$prob2_PSC,
                              " cov1_PSC=", input$cov1_PSC,
                              " cov2_PSC=", input$cov2_PSC,
                              " alig_PSC=", input$alig_PSC),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_1B.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_1B.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "1B", status = st_1B)}
    
    return(mes_after_run_fun(step= "1B", status_step = st_1B))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actPCs_2_PSCs, {
  rval_mes_1B$data <- run_1B()
  source("server_afterRunButtons.R", local = TRUE)
  
  row_del <- str_which(row.names(rval_shiny_opt_TB$data), "clust_PSC")
  rval_shiny_opt_TB$data[row_del:nrow(rval_shiny_opt_TB$data), "value"] <- "NULL"
  
  rval_shiny_opt_TB$data[["clust_PSC", "value"]] <- input$clust_PSC
  rval_shiny_opt_TB$data[["prob1_PSC", "value"]] <- input$prob1_PSC
  rval_shiny_opt_TB$data[["cov1_PSC", "value"]] <- input$cov1_PSC
  rval_shiny_opt_TB$data[["prob2_PSC", "value"]] <- input$prob2_PSC
  rval_shiny_opt_TB$data[["cov2_PSC", "value"]] <- input$cov1_PSC
  rval_shiny_opt_TB$data[["alig_PSC", "value"]] <- input$alig_PSC
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_1B <- renderText({
  rval_mes_1B$data
})

###run 2B -----------
run_2B <- eventReactive(input$actCluster_genomes_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2B")}
    
    ###params
    if(input$boot_b == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
    
    #### VirClust cmd
    # st_2B <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step2B=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  "pc_type=PSC",
    #                                  paste0("boot_pv_b=", boot_pv),
    #                                  paste0("bootstrap_no_b=", input$boot_no_b),
    #                                  paste0("aglom_b=", input$aglom_b)),
    #                         
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_2B.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_2B.txt"))
    
    st_2B <- system2(command = "Rscript", 
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step2B=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " pc_type=PSC",
                              " boot_pv_b=", boot_pv,
                              " bootstrap_no_b=", input$boot_no_b,
                              " aglom_b=", input$aglom_b),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_2B.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_2B.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "2B", status = st_2B)}
    
    return(mes_after_run_fun(step= "2B", status_step = st_2B))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCluster_genomes_PSC, {
  rval_mes_2B$data <- run_2B()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(#2B
    "aglom_b", "boot_b", "boot_no_b",
    #2B_Plot
    "inc_fact_w_Pd_B", "font_row_Pd_B", "font_col_Pd_B", "font_cell_Pd_B",
    "lgd_font_Pd_B", "lgd_lab_font_Pd_B", "lgd_height_Pd_B", "lgd_width_Pd_B",
    #3B
    "Clust_dist_PSC", "sel_PSCs_heatmap_b",
    #3B_Plot
    "Show_Tree_b", "tree_width_b", "show_clust_ID_b", "show_sil_b",
    "show_protein_stats_b", "clustID_width_b", "Sil_stats_width_b", "Stats_width_b",
    "stats_font_b", "stats_lab_font_b", "show_heat_b", "Heat_width_b",
    "font_col_b", "font_row_b", "lgd_font_b", "lgd_lab_font_b",
    "lgd_h_b", "lgd_w_b")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["aglom_b", "value"]] <- input$aglom_b
  rval_shiny_opt_TB$data[["boot_b", "value"]] <- input$boot_b
  rval_shiny_opt_TB$data[["boot_no_b", "value"]] <- input$boot_no_b
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_2B <- renderText({
  rval_mes_2B$data
})


###run 2B_Plot ------
run_2B_Plot <- eventReactive(input$actOut_pdf_2B, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2B_Plot")}
    
    #### VirClust cmd
    # st_2B_Plot <- sys::exec_wait(cmd = "Rscript", 
    #                              args = c(master_p,
    #                                       paste0("projdir=", rval_proj_name$data),
    #                                       "step2B_Plot=T",
    #                                       paste0("continue=", rval_cont$data),
    #                                       "shiny=yes",
    #                                       
    #                                       #step options
    #                                       "pc_type=PSC",
    #                                       paste0("inc_fact_w_Pd=", input$inc_fact_w_Pd_B),
    #                                       paste0("font_row_Pd=", input$font_row_Pd_B),
    #                                       paste0("font_col_Pd=", input$font_col_Pd_B),
    #                                       paste0("font_cell_Pd=", input$font_cell_Pd_B),
    #                                       paste0("lgd_width_Pd=", input$lgd_width_Pd_B), 
    #                                       paste0("lgd_height_Pd=", input$lgd_height_Pd_B),
    #                                       paste0("lgd_font_Pd=", input$lgd_font_Pd_B),
    #                                       #paste0("lgd_pos_Pd=", input$lgd_pos_Pd_A),
    #                                       paste0("lgd_lab_font_Pd=", input$lgd_lab_font_Pd_B)),
    #                              
    #                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_2B_Plot.txt"), 
    #                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_2B_Plot.txt"))
    
    st_2B_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step2B_Plot=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " pc_type=PSC",
                                   " inc_fact_w_Pd=", input$inc_fact_w_Pd_B,
                                   " font_row_Pd=", input$font_row_Pd_B,
                                   " font_col_Pd=", input$font_col_Pd_B,
                                   " font_cell_Pd=", input$font_cell_Pd_B,
                                   " lgd_width_Pd=", input$lgd_width_Pd_B, 
                                   " lgd_height_Pd=", input$lgd_height_Pd_B,
                                   " lgd_font_Pd=", input$lgd_font_Pd_B,
                                   #"lgd_pos_Pd=", input$lgd_pos_Pd_A,
                                   " lgd_lab_font_Pd=", input$lgd_lab_font_Pd_B),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_2B_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_2B_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "2B_Plot", status = st_2B_Plot)}
    
    return(mes_after_run_fun(step= "2B_Plot", status_step = st_2B_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_2B, {
  rval_mes_2B_Plot$data <- run_2B_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]] <- input$inc_fact_w_Pd_B
  rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]] <- input$font_row_Pd_B
  rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]] <- input$font_col_Pd_B
  rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]] <- input$font_cell_Pd_B
  rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]] <- input$lgd_font_Pd_B
  rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]] <- input$lgd_lab_font_Pd_B
  rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]] <- input$lgd_height_Pd_B
  rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]] <- input$lgd_width_Pd_B
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_2B_Plot <- renderText({
  rval_mes_2B_Plot$data
})


### run 3B --------
run_3B <- eventReactive(input$actSplit_clust_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3B")}
    
    #### VirClust cmd
    # st_3B <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step3B=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  "pc_type=PSC",
    #                                  paste0("clust_dist_b=", input$Clust_dist_PSC),
    #                                  paste0("max_cols_HT=", input$sel_PSCs_heatmap_b)),
    #                         
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_3B.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_3B.txt"))
    
    st_3B <- system2(command = "Rscript", 
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step3B=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " pc_type=PSC",
                              " clust_dist_b=", input$Clust_dist_PSC,
                              " max_cols_HT=", input$sel_PSCs_heatmap_b),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_3B.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_3B.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3B", status = st_3B)}
    
    return(mes_after_run_split_stats_fun(step= "3B", status_step = st_3B, projdir =  rval_proj_name$data, pc_type="PSC"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actSplit_clust_PSC, {
  rval_mes_3B$data <- run_3B()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(#3B
    "Clust_dist_PSC", "sel_PSCs_heatmap_b",
    #3B_Plot
    "Show_Tree_b", "tree_width_b", "show_clust_ID_b", "show_sil_b",
    "show_protein_stats_b", "clustID_width_b", "Sil_stats_width_b", "Stats_width_b",
    "stats_font_b", "stats_lab_font_b", "show_heat_b", "Heat_width_b",
    "font_col_b", "font_row_b", "lgd_font_b", "lgd_lab_font_b",
    "lgd_h_b", "lgd_w_b")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]] <- input$Clust_dist_PSC
  rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]] <- input$sel_PSCs_heatmap_b
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3B <- renderText({
  rval_mes_3B$data
})

### run 3B_Plot ------
run_3B_Plot <- eventReactive(input$actOut_pdf_3B, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3B_Plot")}
    
    
    #show params 
    if(input$Show_Tree_b == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
    if(input$show_heat_b == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
    if(input$show_protein_stats_b == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
    if(input$show_sil_b == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
    if(input$show_clust_ID_b == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
    
    #### VirClust command
    st_3B_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step3B_Plot=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " pc_type=PSC",
                                   " font_row=", input$font_row_b, 
                                   " font_col=", input$font_col_b,
                                   " stats_width=", input$Stats_width_b, 
                                   " sil_stats_width=", input$Sil_stats_width_b,
                                   " tree_width=", input$tree_width_b, 
                                   " stats_font=", input$stats_font_b, 
                                   " stats_lab_font=", input$stats_lab_font_b, 
                                   " lgd_width=", input$lgd_w_b, 
                                   " lgd_height=", input$lgd_h_b, 
                                   " lgd_font=", input$lgd_font_b, 
                                   " lgd_lab_font=", input$lgd_lab_font_b, 
                                   " inc_fact_w=", input$Heat_width_b, 
                                   #"lgd_pos=", , 
                                   " show_tree=", show_tree, 
                                   " show_heat=", show_heat, 
                                   " show_protein_stats=", show_protein_stats, 
                                   " show_sil=", show_sil, 
                                   " show_clust_ID=", show_clust_ID, 
                                   " clustID_width=", input$clustID_width_b),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_3B_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_3B_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3B_Plot", status = st_3B_Plot)}
    
    return(mes_after_run_fun(step= "3B_Plot", status_step = st_3B_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_3B, {
  rval_mes_3B_Plot$data <- run_3B_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["Show_Tree_b", "value"]] <- input$Show_Tree_b
  rval_shiny_opt_TB$data[["tree_width_b", "value"]] <- input$tree_width_b
  rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]] <- input$show_clust_ID_b
  rval_shiny_opt_TB$data[["show_sil_b", "value"]] <- input$show_sil_b
  rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]] <- input$show_protein_stats_b
  rval_shiny_opt_TB$data[["clustID_width_b", "value"]] <- input$clustID_width_b
  rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]] <- input$Sil_stats_width_b
  rval_shiny_opt_TB$data[["Stats_width_b", "value"]] <- input$Stats_width_b
  rval_shiny_opt_TB$data[["stats_font_b", "value"]] <- input$stats_font_b
  rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]] <- input$stats_lab_font_b
  rval_shiny_opt_TB$data[["show_heat_b", "value"]] <- input$show_heat_b
  rval_shiny_opt_TB$data[["Heat_width_b", "value"]] <- input$Heat_width_b
  rval_shiny_opt_TB$data[["font_col_b", "value"]] <- input$font_col_b
  rval_shiny_opt_TB$data[["font_row_b", "value"]] <- input$font_row_b
  rval_shiny_opt_TB$data[["lgd_font_b", "value"]] <- input$lgd_font_b
  rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]] <- input$lgd_lab_font_b
  rval_shiny_opt_TB$data[["lgd_h_b", "value"]] <- input$lgd_h_b
  rval_shiny_opt_TB$data[["lgd_w_b", "value"]] <- input$lgd_w_b
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3B_Plot <- renderText({
  rval_mes_3B_Plot$data
})

### run 4B --------
run_4B <- eventReactive(input$actCore_PSCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4B")}
    
    #### VirClust command
    st_4B <- system2(command = "Rscript", 
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step4B=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " pc_type=PSC"),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_4B.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_4B.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "4B", status = st_4B)}
    
    return(mes_after_run_core_fun(step= "4B", status_step = st_4B, projdir = rval_proj_name$data, core_type = "b"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCore_PSCs, {
  rval_mes_4B$data <- run_4B()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]] <- input$Annot_prots_PSCs
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_4B <- renderText({
  rval_mes_4B$data
})



### Annots ---
### run 5BI --------
run_5BI <- eventReactive(input$actAnnotInterPro_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BI")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BI <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step5BI=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BI.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BI.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BI", status = st_5BI)}
    
    return(mes_after_run_annots_fun(step= "5BI", status_step = st_5BI,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotInterPro_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BI$data <- run_5BI()}else{rval_mes_5BI_core$data <- run_5BI()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BI <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BI$data
  }else
  {
    text <- rval_mes_5BI_core$data
  }
  return(text)
})


### run 5BpV --------
run_5BpV <- eventReactive(input$actAnnotpVOGs_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BpV")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BpV <- system2(command = "Rscript",
                       args = paste0(master_p,
                                " projdir=", rval_proj_name$data,
                                " step5BpV=T",
                                " continue=", rval_cont$data,
                                " shiny=yes",
                                
                                #step options
                                " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BpV.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BpV.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BpV", status = st_5BpV)}
    
    return(mes_after_run_annots_fun(step= "5BpV", status_step = st_5BpV,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotpVOGs_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BpV$data <- run_5BpV()}else{rval_mes_5BpV_core$data <- run_5BpV()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BpV <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  { 
    text <- rval_mes_5BpV$data
  }else
  {
    text <- rval_mes_5BpV_core$data
  }
  return(text)
})


### run 5BVO --------
run_5BVO <- eventReactive(input$actAnnotVOGDB_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BVO")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BVO <- system2(command = "Rscript",
                       args = paste0(master_p,
                                " projdir=", rval_proj_name$data,
                                " step5BVO=T",
                                " continue=", rval_cont$data,
                                " shiny=yes",
                                
                                #step options
                                " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BVO.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BVO.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BVO", status = st_5BVO)}
    
    return(mes_after_run_annots_fun(step= "5BVO", status_step = st_5BVO,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotVOGDB_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BVO$data <- run_5BVO()}else{rval_mes_5BVO_core$data <- run_5BVO()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BVO <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BVO$data
  }else
  {
    text <- rval_mes_5BVO_core$data
  }
  return(text)
})

### run 5BPH --------
run_5BPH <- eventReactive(input$actAnnotPHROGS_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BPH")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BPH <- system2(command = "Rscript",
                       args = paste0(master_p,
                                " projdir=", rval_proj_name$data,
                                " step5BPH=T",
                                " continue=", rval_cont$data,
                                " shiny=yes",
                                
                                #step options
                                " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BPH.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BPH.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BPH", status = st_5BPH)}
    
    return(mes_after_run_annots_fun(step= "5BPH", status_step = st_5BPH,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotPHROGS_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BPH$data <- run_5BPH()}else{rval_mes_5BPH_core$data <- run_5BPH()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BPH <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BPH$data
  }else
  {
    text <- rval_mes_5BPH_core$data
  }
  return(text)
})

### run 5BE --------
run_5BE <- eventReactive(input$actAnnotEfam_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BE")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BE <- system2(command = "Rscript",
                      args = paste0(master_p,
                               " projdir=", rval_proj_name$data,
                               " step5BE=T",
                               " continue=", rval_cont$data,
                               " shiny=yes",
                               
                               #step options
                               " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BE.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BE.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BE", status = st_5BE)}
    
    return(mes_after_run_annots_fun(step= "5BE", status_step = st_5BE,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotEfam_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BE$data <- run_5BE()}else{rval_mes_5BE_core$data <- run_5BE()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BE <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BE$data
  }else
  {
    text <- rval_mes_5BE_core$data
  }
  return(text)
})


### run 5BXC --------
run_5BXC <- eventReactive(input$`actAnnotEfam-XC_PSC`, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BXC")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BXC <- system2(command = "Rscript",
                       args = paste0(master_p,
                                " projdir=", rval_proj_name$data,
                                " step5BXC=T",
                                " continue=", rval_cont$data,
                                " shiny=yes",
                                
                                #step options
                                " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BXC.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BXC.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BXC", status = st_5BXC)}
    
    return(mes_after_run_annots_fun(step= "5BXC", status_step = st_5BXC,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$`actAnnotEfam-XC_PSC`, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BXC$data <- run_5BXC()}else{rval_mes_5BXC_core$data <- run_5BXC()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BXC <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BXC$data
  }else
  {
    text <- rval_mes_5BXC_core$data
  }
  return(text)
})

### run 5BN --------
run_5BN <- eventReactive(input$actAnnotNCBI_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BN")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BN <- system2(command = "Rscript",
                      args = paste0(master_p,
                               " projdir=", rval_proj_name$data,
                               " step5BN=T",
                               " continue=", rval_cont$data,
                               " shiny=yes",
                               
                               #step options
                               " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BN.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BN.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BN", status = st_5BN)}
    
    return(mes_after_run_annots_fun(step= "5BN", status_step = st_5BN,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotNCBI_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BN$data <- run_5BN()}else{rval_mes_5BN_core$data <- run_5BN()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BN <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BN$data
  }else
  {
    text <- rval_mes_5BN_core$data
  }
  return(text)
})


### run 5BM --------
run_5BM <- eventReactive(input$actMergeAnnot_PSC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BM")}
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
    
    
    #### VirClust command
    st_5BM <- system2(command = "Rscript",
                             args = paste0(master_p,
                                      " projdir=", rval_proj_name$data,
                                      " step5BM=T",
                                      " continue=", rval_cont$data,
                                      " shiny=yes",
                                      
                                      #step options
                                      " prot_type=", prot_type),
                             
                             stdout = paste0(rval_proj_name$data, "/shiny_stdout_5BM.txt"),
                             stderr = paste0(rval_proj_name$data, "/shiny_stderr_5BM.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5BM", status = st_5BM)}
    
    return(mes_after_run_fun(step= "5BM", status_step = st_5BM))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actMergeAnnot_PSC, {
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {rval_mes_5BM$data <- run_5BM()}else{rval_mes_5BM_core$data <- run_5BM()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_5BM <- renderText({
  if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
  {
    text <- rval_mes_5BM$data
  }else
  {
    text <- rval_mes_5BM_core$data
  }
  return(text)
})

