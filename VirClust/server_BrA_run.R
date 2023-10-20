#run1A ----------
run_1A <- eventReactive(eventExpr = input$actGenom_2_Prot,
                        {
                          validate(
                            need(is.null(rval_proj_name$data) == FALSE, ""),
                            need(rval_valid_fasta$data == "Valid file type(s).", "")
                          )
                          
                          withProgress(expr = {
                            
                            if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
                            {
                              start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1A")
                            }
                            
                            
                            ## options
                            if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                            {
                              multiF <- "yes"
                            }
                            if(input$in_type == "Nucleic acids, one genome per file**")
                            {
                              multiF <- "no"
                            }
                            
                            gene_code <- str_split(input$Genetic_code, " --- ")[[1]][1]
                            gene <<- gene_code
                            
                            #### VirClust cmd
                            # st_1A <- sys::exec_wait(cmd = "Rscript", 
                            #                         args = c(master_p,
                            #                                  paste0("projdir=", rval_proj_name$data),
                            #                                  "step1A=T",
                            #                                  paste0("multiF=", multiF),
                            #                                  paste0("continue=", rval_cont$data),
                            #                                  "shiny=yes",
                            #                                  
                            #                                  #step options
                            #                                  paste0("gene_code=", gene_code)
                            #                         ), 
                            #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_1A.txt"), 
                            #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_1A.txt"))
                            
                            st_1A <- system2(command = "Rscript",
                                                    args = paste0(master_p,
                                                             " projdir=", rval_proj_name$data,
                                                             " step1A=T",
                                                             " multiF=", multiF,
                                                             " continue=", rval_cont$data,
                                                             " shiny=yes",

                                                             #step options
                                                             " gene_code=", gene_code),
                                                    stdout = paste0(rval_proj_name$data, "/shiny_stdout_1A.txt"),
                                                    stderr = paste0(rval_proj_name$data, "/shiny_stderr_1A.txt"))
                            #system2
                            
                            if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
                            {
                              end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "1A", status = st_1A)
                            }
                            
                            
                            #return(mes_after_run_fun(step= "1A", status_step = st_1A))
                            return("Step 1A has finished no status.")
                          }, message = rval_progress_mess$data
                          )
                        })


observeEvent(eventExpr = input$actGenom_2_Prot, {
  ##do work
  rval_mes_1A$data <- run_1A()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[1:nrow(rval_shiny_opt_TB$data), "value"] <- "NULL"
  rval_shiny_opt_TB$data[["Genetic_code", "value"]] <- input$Genetic_code
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_1A <- renderText({
  rval_mes_1A$data
})

###run 2A -----------
run_2A <- eventReactive(input$actProt_2_PCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {
      start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2A") 
    }
    
    #### VirClust cmd
    # st_2A <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step2A=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  paste0("clust_PC=", input$clust_PC),
    #                                  paste0("eval_PC=", input$eval_PC),
    #                                  paste0("bitsc_PC=", input$bitsc_PC),
    #                                  paste0("cov_PC=", input$cov_PC),
    #                                  paste0("pident_PC=", input$pident_PC)), 
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_2A.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_2A.txt"))
    
    
    st_2A <- system2(command = "Rscript", 
                            args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step2A=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " clust_PC=", input$clust_PC,
                                     " eval_PC=", input$eval_PC,
                                     " bitsc_PC=", input$bitsc_PC,
                                     " cov_PC=", input$cov_PC,
                                     " pident_PC=", input$pident_PC), 
                            stdout = paste0(rval_proj_name$data, "/shiny_stdout_2A.txt"), 
                            stderr = paste0(rval_proj_name$data, "/shiny_stderr_2A.txt"))
    
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {
      end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "2A", status = st_2A)
    }
    
    return(mes_after_run_fun(step= "2A", status_step = st_2A))
  }, message = rval_progress_mess$data
  )
  
})


observeEvent(eventExpr = input$actProt_2_PCs, {
  #do work
  rval_mes_2A$data <- run_2A()
  source("server_afterRunButtons.R", local = TRUE)

  rval_shiny_opt_TB$data[2:nrow(rval_shiny_opt_TB$data), "value"] <- "NULL"
  rval_shiny_opt_TB$data[["clust_PC", "value"]] <- input$clust_PC
  rval_shiny_opt_TB$data[["eval_PC", "value"]] <- input$eval_PC
  rval_shiny_opt_TB$data[["bitsc_PC", "value"]] <- input$bitsc_PC
  rval_shiny_opt_TB$data[["cov_PC", "value"]] <- input$cov_PC
  rval_shiny_opt_TB$data[["pident_PC", "value"]] <- input$pident_PC

  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_2A <- renderText({
  rval_mes_2A$data
})


###run 3A -----------

run_3A <- eventReactive(input$actCluster_genomes_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3A")}
    
    ###params
    if(input$boot_a == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
    
    #### VirClust cmd
    # st_3A <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step3A=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  "pc_type=PC",
    #                                  paste0("boot_pv_a=", boot_pv),
    #                                  paste0("bootstrap_no_a=", input$boot_no_a),
    #                                  paste0("aglom_a=", input$aglom_a)),
    #                         
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_3A.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_3A.txt"))
    
    st_3A <- system2(command = "Rscript", 
                            args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step3A=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " pc_type=PC",
                                     " boot_pv_a=", boot_pv,
                                     " bootstrap_no_a=", input$boot_no_a,
                                     " aglom_a=", input$aglom_a),
                            
                            stdout = paste0(rval_proj_name$data, "/shiny_stdout_3A.txt"), 
                            stderr = paste0(rval_proj_name$data, "/shiny_stderr_3A.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3A", status = st_3A)}
    
    return(mes_after_run_fun(step= "3A", status_step = st_3A))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCluster_genomes_PC, {
  #do work
  rval_mes_3A$data <- run_3A()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(#3A
    "aglom_a", "boot_a", "boot_no_a",
    #3A_Plot
    "inc_fact_w_Pd_A", "font_row_Pd_A", "font_col_Pd_A", "font_cell_Pd_A",
    "lgd_font_Pd_A", "lgd_lab_font_Pd_A", "lgd_height_Pd_A", "lgd_width_Pd_A",
    #4A
    "Clust_dist_PC", "sel_PCs_heatmap_a",
    #4A_Plot
    "Show_Tree_a", "tree_width_a", "show_clust_ID_a", "show_sil_a",
    "show_protein_stats_a", "clustID_width_a", "Sil_stats_width_a", "Stats_width_a",
    "stats_font_a", "stats_lab_font_a", "show_heat_a", "Heat_width_a",
    "font_col_a", "font_row_a", "lgd_font_a", "lgd_lab_font_a",
    "lgd_h_a", "lgd_w_a")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["aglom_a", "value"]] <- input$aglom_a
  rval_shiny_opt_TB$data[["boot_a", "value"]] <- input$boot_a
  rval_shiny_opt_TB$data[["boot_no_a", "value"]] <- input$boot_no_a
  
  
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3A <- renderText({
  rval_mes_3A$data
})


###run 3A_Plot ------
run_3A_Plot <- eventReactive(input$actOut_pdf_3A, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3A_Plot")}
    
    #### VirClust cmd
    # st_3A_Plot <- sys::exec_wait(cmd = "Rscript", 
    #                              args = c(master_p,
    #                                       paste0("projdir=", rval_proj_name$data),
    #                                       "step3A_Plot=T",
    #                                       paste0("continue=", rval_cont$data),
    #                                       "shiny=yes",
    #                                       
    #                                       #step options
    #                                       "pc_type=PC",
    #                                       paste0("inc_fact_w_Pd=", input$inc_fact_w_Pd_A),
    #                                       paste0("font_row_Pd=", input$font_row_Pd_A),
    #                                       paste0("font_col_Pd=", input$font_col_Pd_A),
    #                                       paste0("font_cell_Pd=", input$font_cell_Pd_A),
    #                                       paste0("lgd_width_Pd=", input$lgd_width_Pd_A), 
    #                                       paste0("lgd_height_Pd=", input$lgd_height_Pd_A),
    #                                       paste0("lgd_font_Pd=", input$lgd_font_Pd_A),
    #                                       #paste0("lgd_pos_Pd=", input$lgd_pos_Pd_A),
    #                                       paste0("lgd_lab_font_Pd=", input$lgd_lab_font_Pd_A)),
    #                              
    #                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_3A_Plot.txt"), 
    #                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_3A_Plot.txt"))
    
    st_3A_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                        " projdir=", rval_proj_name$data,
                                        " step3A_Plot=T",
                                        " continue=", rval_cont$data,
                                        " shiny=yes",
                                        
                                        #step options
                                        " pc_type=PC",
                                        " inc_fact_w_Pd=", input$inc_fact_w_Pd_A,
                                        " font_row_Pd=", input$font_row_Pd_A,
                                        " font_col_Pd=", input$font_col_Pd_A,
                                        " font_cell_Pd=", input$font_cell_Pd_A,
                                        " lgd_width_Pd=", input$lgd_width_Pd_A, 
                                        " lgd_height_Pd=", input$lgd_height_Pd_A,
                                        " lgd_font_Pd=", input$lgd_font_Pd_A,
                                        #"lgd_pos_Pd=", input$lgd_pos_Pd_A,
                                        " lgd_lab_font_Pd=", input$lgd_lab_font_Pd_A),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_3A_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_3A_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "3A_Plot", status = st_3A_Plot)}
    
    return(mes_after_run_fun(step= "3A_Plot", status_step = st_3A_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_3A, {
  rval_mes_3A_Plot$data <- run_3A_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]] <- input$inc_fact_w_Pd_A
  rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]] <- input$font_row_Pd_A
  rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]] <- input$font_col_Pd_A
  rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]] <- input$font_cell_Pd_A
  rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]] <- input$lgd_font_Pd_A
  rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]] <- input$lgd_lab_font_Pd_A
  rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]] <- input$lgd_height_Pd_A
  rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]] <- input$lgd_width_Pd_A
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_3A_Plot <- renderText({
  rval_mes_3A_Plot$data
})


### run 4A --------
run_4A <- eventReactive(input$actSplit_clust_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4A")}
    
    #### VirClust cmd
    # st_4A <- sys::exec_wait(cmd = "Rscript",
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step4A=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    # 
    #                                  #step options
    #                                  "pc_type=PC",
    #                                  paste0("clust_dist_a=", input$Clust_dist_PC),
    #                                  paste0("max_cols_HT=", input$sel_PCs_heatmap_a)),
    # 
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_4A.txt"),
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_4A.txt"))
    
    st_4A <- system2(command = "Rscript",
                     args = paste0(master_p,
                              " projdir=", rval_proj_name$data,
                              " step4A=T",
                              " continue=", rval_cont$data,
                              " shiny=yes",
                              
                              #step options
                              " pc_type=PC",
                              " clust_dist_a=", input$Clust_dist_PC,
                              " max_cols_HT=", input$sel_PCs_heatmap_a),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_4A.txt"),
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_4A.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "4A", status = st_4A)}
    
    return(mes_after_run_split_stats_fun(step= "4A", status_step = st_4A, projdir =  rval_proj_name$data, pc_type="PC"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actSplit_clust_PC, {
  rval_mes_4A$data <- run_4A()
  source("server_afterRunButtons.R", local = TRUE)
  
  rows_del <- c(
    #4A
    "Clust_dist_PC", "sel_PCs_heatmap_a",
    #4A_Plot
    "Show_Tree_a", "tree_width_a", "show_clust_ID_a", "show_sil_a",
    "show_protein_stats_a", "clustID_width_a", "Sil_stats_width_a", "Stats_width_a",
    "stats_font_a", "stats_lab_font_a", "show_heat_a", "Heat_width_a",
    "font_col_a", "font_row_a", "lgd_font_a", "lgd_lab_font_a",
    "lgd_h_a", "lgd_w_a")
  rval_shiny_opt_TB$data[rows_del, "value"] <- "NULL"
  rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]] <- input$Clust_dist_PC
  rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]] <- input$sel_PCs_heatmap_a
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_4A <- renderText({
  rval_mes_4A$data
})


### run 4A_Plot ------
run_4A_Plot <- eventReactive(input$actOut_pdf_4A, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4A_Plot")}
    
    
    #show params 
    if(input$Show_Tree_a == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
    if(input$show_heat_a == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
    if(input$show_protein_stats_a == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
    if(input$show_sil_a == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
    if(input$show_clust_ID_a == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
    
    #### VirClust cmd
    # st_4A_Plot <- sys::exec_wait(cmd = "Rscript", 
    #                              args = c(master_p,
    #                                       paste0("projdir=", rval_proj_name$data),
    #                                       "step4A_Plot=T",
    #                                       paste0("continue=", rval_cont$data),
    #                                       "shiny=yes",
    #                                       
    #                                       #step options
    #                                       "pc_type=PC",
    #                                       paste0("font_row=", input$font_row_a), 
    #                                       paste0("font_col=", input$font_col_a),
    #                                       paste0("stats_width=", input$Stats_width_a), 
    #                                       paste0("sil_stats_width=", input$Sil_stats_width_a),
    #                                       paste0("tree_width=", input$tree_width_a), 
    #                                       paste0("stats_font=", input$stats_font_a), 
    #                                       paste0("stats_lab_font=", input$stats_lab_font_a), 
    #                                       paste0("lgd_width=", input$lgd_w_a), 
    #                                       paste0("lgd_height=", input$lgd_h_a), 
    #                                       paste0("lgd_font=", input$lgd_font_a), 
    #                                       paste0("lgd_lab_font=", input$lgd_lab_font_a), 
    #                                       paste0("inc_fact_w=", input$Heat_width_a), 
    #                                       #paste0("lgd_pos=", ), 
    #                                       paste0("show_tree=", show_tree), 
    #                                       paste0("show_heat=", show_heat), 
    #                                       paste0("show_protein_stats=", show_protein_stats), 
    #                                       paste0("show_sil=", show_sil), 
    #                                       paste0("show_clust_ID=", show_clust_ID), 
    #                                       paste0("clustID_width=", input$clustID_width_a)
    #                                       
    #                              ),
    #                              
    #                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_4A_Plot.txt"), 
    #                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_4A_Plot.txt"))
    
    
    st_4A_Plot <- system2(command = "Rscript", 
                          args = paste0(master_p,
                                        " projdir=", rval_proj_name$data,
                                        " step4A_Plot=T",
                                        " continue=", rval_cont$data,
                                        " shiny=yes",
                                        
                                        #step options
                                        " pc_type=PC",
                                        " font_row=", input$font_row_a, 
                                        " font_col=", input$font_col_a,
                                        " stats_width=", input$Stats_width_a, 
                                        " sil_stats_width=", input$Sil_stats_width_a,
                                        " tree_width=", input$tree_width_a, 
                                        " stats_font=", input$stats_font_a, 
                                        " stats_lab_font=", input$stats_lab_font_a, 
                                        " lgd_width=", input$lgd_w_a, 
                                        " lgd_height=", input$lgd_h_a, 
                                        " lgd_font=", input$lgd_font_a, 
                                        " lgd_lab_font=", input$lgd_lab_font_a, 
                                        " inc_fact_w=", input$Heat_width_a, 
                                        #"lgd_pos=", , 
                                        " show_tree=", show_tree, 
                                        " show_heat=", show_heat, 
                                        " show_protein_stats=", show_protein_stats, 
                                        " show_sil=", show_sil, 
                                        " show_clust_ID=", show_clust_ID, 
                                        " clustID_width=", input$clustID_width_a
                                        
                          ),
                          
                          stdout = paste0(rval_proj_name$data, "/shiny_stdout_4A_Plot.txt"), 
                          stderr = paste0(rval_proj_name$data, "/shiny_stderr_4A_Plot.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "4A_Plot", status = st_4A_Plot)}
    
    return(mes_after_run_fun(step= "4A_Plot", status_step = st_4A_Plot))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actOut_pdf_4A, {
  rval_mes_4A_Plot$data <- run_4A_Plot()
  source("server_afterRunButtons.R", local = TRUE)
  
  rval_shiny_opt_TB$data[["Show_Tree_a", "value"]] <- input$Show_Tree_a
  rval_shiny_opt_TB$data[["tree_width_a", "value"]] <- input$tree_width_a
  rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]] <- input$show_clust_ID_a
  rval_shiny_opt_TB$data[["show_sil_a", "value"]] <- input$show_sil_a
  rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]] <- input$show_protein_stats_a
  rval_shiny_opt_TB$data[["clustID_width_a", "value"]] <- input$clustID_width_a
  rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]] <- input$Sil_stats_width_a
  rval_shiny_opt_TB$data[["Stats_width_a", "value"]] <- input$Stats_width_a
  rval_shiny_opt_TB$data[["stats_font_a", "value"]] <- input$stats_font_a
  rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]] <- input$stats_lab_font_a
  rval_shiny_opt_TB$data[["show_heat_a", "value"]] <- input$show_heat_a
  rval_shiny_opt_TB$data[["Heat_width_a", "value"]] <- input$Heat_width_a
  rval_shiny_opt_TB$data[["font_col_a", "value"]] <- input$font_col_a
  rval_shiny_opt_TB$data[["font_row_a", "value"]] <- input$font_row_a
  rval_shiny_opt_TB$data[["lgd_font_a", "value"]] <- input$lgd_font_a
  rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]] <- input$lgd_lab_font_a
  rval_shiny_opt_TB$data[["lgd_h_a", "value"]] <- input$lgd_h_a
  rval_shiny_opt_TB$data[["lgd_w_a", "value"]] <- input$lgd_w_a
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_4A_Plot <- renderText({
  rval_mes_4A_Plot$data
})


### run 5A --------
run_5A <- eventReactive(input$actCore_PCs, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5A")}
    
    #### VirClust cmd
    # st_5A <- sys::exec_wait(cmd = "Rscript", 
    #                         args = c(master_p,
    #                                  paste0("projdir=", rval_proj_name$data),
    #                                  "step5A=T",
    #                                  paste0("continue=", rval_cont$data),
    #                                  "shiny=yes",
    #                                  
    #                                  #step options
    #                                  "pc_type=PC"),
    #                         
    #                         std_out = paste0(rval_proj_name$data, "/shiny_stdout_5A.txt"), 
    #                         std_err = paste0(rval_proj_name$data, "/shiny_stderr_5A.txt"))
    
    st_5A <- system2(command = "Rscript", 
                     args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step5A=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " pc_type=PC"),
                     
                     stdout = paste0(rval_proj_name$data, "/shiny_stdout_5A.txt"), 
                     stderr = paste0(rval_proj_name$data, "/shiny_stderr_5A.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "5A", status = st_5A)}
    
    return(mes_after_run_core_fun(step= "5A", status_step = st_5A, projdir = rval_proj_name$data, core_type = "a"))
  }, message = rval_progress_mess$data
  )
  
})

observeEvent(eventExpr = input$actCore_PCs, {
  rval_mes_5A$data <- run_5A()
  source("server_afterRunButtons.R", local = TRUE)
  
  
  rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]] <- input$Annot_prots_PCs
  
  saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
  write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
})

output$mes_5A <- renderText({
  rval_mes_5A$data
})


###Annots ---
### run 6AI --------
run_6AI <- eventReactive(input$actAnnotInterPro_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AI")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6AI <- sys::exec_wait(cmd = "Rscript",
    #                          args = c(master_p,
    #                                   paste0("projdir=", rval_proj_name$data),
    #                                   "step6AI=T",
    #                                   paste0("continue=", rval_cont$data),
    #                                   "shiny=yes",
    #                                   
    #                                   #step options
    #                                   paste0("prot_type=", prot_type)),
    #                          
    #                          std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AI.txt"),
    #                          std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AI.txt"))
    
    st_6AI <- system2(command = "Rscript",
                      args = paste0(master_p,
                                   " projdir=", rval_proj_name$data,
                                   " step6AI=T",
                                   " continue=", rval_cont$data,
                                   " shiny=yes",
                                   
                                   #step options
                                   " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AI.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AI.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AI", status = st_6AI)}
    
    return(mes_after_run_annots_fun(step= "6AI", status_step = st_6AI,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotInterPro_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AI$data <- run_6AI()}else{rval_mes_6AI_core$data <- run_6AI()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AI <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AI$data
  }else
  {
    text <- rval_mes_6AI_core$data
  }
  return(text)
})


### run 6ApV --------
run_6ApV <- eventReactive(input$actAnnotpVOGs_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6ApV")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6ApV <- sys::exec_wait(cmd = "Rscript",
    #                           args = c(master_p,
    #                                    paste0("projdir=", rval_proj_name$data),
    #                                    "step6ApV=T",
    #                                    paste0("continue=", rval_cont$data),
    #                                    "shiny=yes",
    #                                    
    #                                    #step options
    #                                    paste0("prot_type=", prot_type)),
    #                           
    #                           std_out = paste0(rval_proj_name$data, "/shiny_stdout_6ApV.txt"),
    #                           std_err = paste0(rval_proj_name$data, "/shiny_stderr_6ApV.txt"))
    
    st_6ApV <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step6ApV=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_6ApV.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_6ApV.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6ApV", status = st_6ApV)}
    
    return(mes_after_run_annots_fun(step= "6ApV", status_step = st_6ApV,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotpVOGs_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6ApV$data <- run_6ApV()}else{rval_mes_6ApV_core$data <- run_6ApV()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6ApV <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6ApV$data
  }else
  {
    text <- rval_mes_6ApV_core$data
  }
  return(text)
})


### run 6AVO --------
run_6AVO <- eventReactive(input$actAnnotVOGDB_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AVO")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    st_6AVO <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step6AVO=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AVO.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AVO.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AVO", status = st_6AVO)}
    
    return(mes_after_run_annots_fun(step= "6AVO", status_step = st_6AVO,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotVOGDB_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AVO$data <- run_6AVO()}else{rval_mes_6AVO_core$data <- run_6AVO()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AVO <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AVO$data
  }else
  {
    text <- rval_mes_6AVO_core$data
  }
  return(text)
})

### run 6APH --------
run_6APH <- eventReactive(input$actAnnotPHROGS_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6APH")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    st_6APH <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step6APH=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_6APH.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_6APH.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6APH", status = st_6APH)}
    
    return(mes_after_run_annots_fun(step= "6APH", status_step = st_6APH,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotPHROGS_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6APH$data <- run_6APH()}else{rval_mes_6APH_core$data <- run_6APH()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6APH <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    return(rval_mes_6APH$data)
  }else
  {
    return(rval_mes_6APH_core$data)
  }
})

### run 6AE --------
run_6AE <- eventReactive(input$actAnnotEfam_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AE")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6AE <- sys::exec_wait(cmd = "Rscript",
    #                          args = c(master_p,
    #                                   paste0("projdir=", rval_proj_name$data),
    #                                   "step6AE=T",
    #                                   paste0("continue=", rval_cont$data),
    #                                   "shiny=yes",
    #                                   
    #                                   #step options
    #                                   paste0("prot_type=", prot_type)),
    #                          
    #                          std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AE.txt"),
    #                          std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AE.txt"))
    
    st_6AE <- system2(command = "Rscript",
                             args = paste0(master_p,
                                      " projdir=", rval_proj_name$data,
                                      " step6AE=T",
                                      " continue=", rval_cont$data,
                                      " shiny=yes",
                                      
                                      #step options
                                      " prot_type=", prot_type),
                             
                             stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AE.txt"),
                             stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AE.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AE", status = st_6AE)}
    
    return(mes_after_run_annots_fun(step= "6AE", status_step = st_6AE,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotEfam_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AE$data <- run_6AE()}else{rval_mes_6AE_core$data <- run_6AE()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AE <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AE$data
  }else
  {
    text <- rval_mes_6AE_core$data
  }
  return(text)
})


### run 6AXC --------
run_6AXC <- eventReactive(input$`actAnnotEfam-XC_PC`, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AXC")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6AXC <- sys::exec_wait(cmd = "Rscript",
    #                           args = c(master_p,
    #                                    paste0("projdir=", rval_proj_name$data),
    #                                    "step6AXC=T",
    #                                    paste0("continue=", rval_cont$data),
    #                                    "shiny=yes",
    #                                    
    #                                    #step options
    #                                    paste0("prot_type=", prot_type)),
    #                           
    #                           std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AXC.txt"),
    #                           std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AXC.txt"))
    
    st_6AXC <- system2(command = "Rscript",
                       args = paste0(master_p,
                                     " projdir=", rval_proj_name$data,
                                     " step6AXC=T",
                                     " continue=", rval_cont$data,
                                     " shiny=yes",
                                     
                                     #step options
                                     " prot_type=", prot_type),
                       
                       stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AXC.txt"),
                       stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AXC.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AXC", status = st_6AXC)}
    
    return(mes_after_run_annots_fun(step= "6AXC", status_step = st_6AXC,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$`actAnnotEfam-XC_PC`, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AXC$data <- run_6AXC()}else{rval_mes_6AXC_core$data <- run_6AXC()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AXC <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AXC$data
  }else
  {
    text <- rval_mes_6AXC_core$data
  }
  return(text)
})

### run 6AN --------
run_6AN <- eventReactive(input$actAnnotNCBI_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AN")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6AN <- sys::exec_wait(cmd = "Rscript",
    #                          args = c(master_p,
    #                                   paste0("projdir=", rval_proj_name$data),
    #                                   "step6AN=T",
    #                                   paste0("continue=", rval_cont$data),
    #                                   "shiny=yes",
    #                                   
    #                                   #step options
    #                                   paste0("prot_type=", prot_type)),
    #                          
    #                          std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AN.txt"),
    #                          std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AN.txt"))
    
    st_6AN <- system2(command = "Rscript",
                      args = paste0(master_p,
                                    " projdir=", rval_proj_name$data,
                                    " step6AN=T",
                                    " continue=", rval_cont$data,
                                    " shiny=yes",
                                    
                                    #step options
                                    " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AN.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AN.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AN", status = st_6AN)}
    
    return(mes_after_run_annots_fun(step= "6AN", status_step = st_6AN,
                                    projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actAnnotNCBI_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AN$data <- run_6AN()}else{rval_mes_6AN_core$data <- run_6AN()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AN <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AN$data
  }else
  {
    text <- rval_mes_6AN_core$data
  }
  return(text)
})


### run 6AM --------
run_6AM <- eventReactive(input$actMergeAnnot_PC, {
  withProgress(expr = {
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {start_step_email_fun(email = input$email_2, user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AM")}
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
    
    
    #### VirClust cmd
    # st_6AM <- sys::exec_wait(cmd = "Rscript",
    #                          args = c(master_p,
    #                                   paste0("projdir=", rval_proj_name$data),
    #                                   "step6AM=T",
    #                                   paste0("continue=", rval_cont$data),
    #                                   "shiny=yes",
    #                                   
    #                                   #step options
    #                                   paste0("prot_type=", prot_type)),
    #                          
    #                          std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AM.txt"),
    #                          std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AM.txt"))
    
    st_6AM <- system2(command = "Rscript",
                      args = paste0(master_p,
                               " projdir=", rval_proj_name$data,
                               " step6AM=T",
                               " continue=", rval_cont$data,
                               " shiny=yes",
                               
                               #step options
                               " prot_type=", prot_type),
                      
                      stdout = paste0(rval_proj_name$data, "/shiny_stdout_6AM.txt"),
                      stderr = paste0(rval_proj_name$data, "/shiny_stderr_6AM.txt"))
    
    if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
    {end_step_email_fun(email = input$email_2, name=rval_proj_name$data, path=data_VirClust_path, step = "6AM", status = st_6AM)}
    
    return(mes_after_run_fun(step= "6AM", status_step = st_6AM))
  }, message = rval_progress_mess$data
  )
})

observeEvent(eventExpr = input$actMergeAnnot_PC, {
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {rval_mes_6AM$data <- run_6AM()}else{rval_mes_6AM_core$data <- run_6AM()}
  source("server_afterRunButtons.R", local = TRUE)
})

output$mes_6AM <- renderText({
  if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
  {
    text <- rval_mes_6AM$data
  }else
  {
    text <- rval_mes_6AM_core$data
  }
  return(text)
})