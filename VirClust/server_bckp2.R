options(shiny.maxRequestSize=300*1024^2)

library(zip)
library(DT)
library(shinyjs)
library(stringr)
library(shinyWidgets)
library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(seqinr)
library(utils)




data_VirClust_path <- "dataButtons/"
stand_alone_p <- "vir_clust_standalone"
master_p <- paste0(stand_alone_p,"/VirClust_MASTER.R")
source(paste0(stand_alone_p, "/VirClust_functions_shared_with_shiny.R"), local = TRUE)
source(paste0(stand_alone_p, "/VirClust_functions.R"), local = TRUE)
source("app_functions.R", local = TRUE)
source(file = "server_options_TB.R", local = TRUE)


shinyServer(function(input, output, session)
{
  ##### declare ALL reactive values ---------
  rval_upload <- reactiveValues(data = "NULL")
  rval_proj_name <- reactiveValues(data = NULL)
  rval_status_mes <- reactiveValues(data = "NULL")
  rval_status_DF <- reactiveValues(data = make_status_df_fun("empty"))
  rval_cont <- reactiveValues(data = "no")
  rval_create_load <- reactiveValues(data = "no")
  rval_valid_fasta <- reactiveValues(data = "NULL")
  rval_genome_no <- reactiveValues(data = 0)
  rval_progress_mess <- reactiveValues(data = "Calculating. Please be patient! Don't press any other buttons in this page until calculations have finished, or you will have to reload.")
  rval_shiny_opt_TB <- reactiveValues(data = shiny_opt_TB_fun())
  
  
  rval_mes_1A <- reactiveValues(data="")
  rval_mes_2A <- reactiveValues(data="")
  rval_mes_3A <- reactiveValues(data="")
  rval_mes_3A_Plot <- reactiveValues(data="")
  rval_mes_4A <- reactiveValues(data="")
  rval_mes_4A_Plot <- reactiveValues(data="")
  rval_mes_5A <- reactiveValues(data="")
  rval_mes_6AI <- reactiveValues(data="")
  rval_mes_6ApV <- reactiveValues(data="")
  rval_mes_6AVO <- reactiveValues(data="")
  rval_mes_6APH <- reactiveValues(data="")
  rval_mes_6AE <- reactiveValues(data="")
  rval_mes_6AXC <- reactiveValues(data="")
  rval_mes_6AN <- reactiveValues(data="")
  rval_mes_6AM <- reactiveValues(data="")
  rval_mes_6AI_core <- reactiveValues(data="")
  rval_mes_6ApV_core <- reactiveValues(data="")
  rval_mes_6AVO_core <- reactiveValues(data="")
  rval_mes_6APH_core <- reactiveValues(data="")
  rval_mes_6AE_core <- reactiveValues(data="")
  rval_mes_6AXC_core <- reactiveValues(data="")
  rval_mes_6AN_core <- reactiveValues(data="")
  rval_mes_6AM_core <- reactiveValues(data="")
  
  rval_mes_1B <- reactiveValues(data="")
  rval_mes_2B <- reactiveValues(data="")
  rval_mes_2B_Plot <- reactiveValues(data="")
  rval_mes_3B <- reactiveValues(data="")
  rval_mes_3B_Plot <- reactiveValues(data="")
  rval_mes_4B <- reactiveValues(data="")
  rval_mes_5BI <- reactiveValues(data="")
  rval_mes_5BpV <- reactiveValues(data="")
  rval_mes_5BVO <- reactiveValues(data="")
  rval_mes_5BPH <- reactiveValues(data="")
  rval_mes_5BE <- reactiveValues(data="")
  rval_mes_5BXC <- reactiveValues(data="")
  rval_mes_5BN <- reactiveValues(data="")
  rval_mes_5BM <- reactiveValues(data="")
  rval_mes_5BI_core <- reactiveValues(data="")
  rval_mes_5BpV_core <- reactiveValues(data="")
  rval_mes_5BVO_core <- reactiveValues(data="")
  rval_mes_5BPH_core <- reactiveValues(data="")
  rval_mes_5BE_core <- reactiveValues(data="")
  rval_mes_5BXC_core <- reactiveValues(data="")
  rval_mes_5BN_core <- reactiveValues(data="")
  rval_mes_5BM_core <- reactiveValues(data="")
  
  rval_mes_1C <- reactiveValues(data="")
  rval_mes_2C <- reactiveValues(data="")
  rval_mes_2C_Plot <- reactiveValues(data="")
  rval_mes_3C <- reactiveValues(data="")
  rval_mes_3C_Plot <- reactiveValues(data="")
  rval_mes_4C <- reactiveValues(data="")
  rval_mes_5CI <- reactiveValues(data="")
  rval_mes_5CpV <- reactiveValues(data="")
  rval_mes_5CVO <- reactiveValues(data="")
  rval_mes_5CPH <- reactiveValues(data="")
  rval_mes_5CE <- reactiveValues(data="")
  rval_mes_5CXC <- reactiveValues(data="")
  rval_mes_5CN <- reactiveValues(data="")
  rval_mes_5CM <- reactiveValues(data="")
  rval_mes_5CI_core <- reactiveValues(data="")
  rval_mes_5CpV_core <- reactiveValues(data="")
  rval_mes_5CVO_core <- reactiveValues(data="")
  rval_mes_5CPH_core <- reactiveValues(data="")
  rval_mes_5CE_core <- reactiveValues(data="")
  rval_mes_5CXC_core <- reactiveValues(data="")
  rval_mes_5CN_core <- reactiveValues(data="")
  rval_mes_5CM_core <- reactiveValues(data="")
  
  # #### make buttons and options visible/invisible ----
  observe({
    #   #status_DF <- rval_status_DF$data
    #   
    ####DEFINE PROJECT -----
    
    #Create button
    if(rval_upload$data == "uploaded" & 
       str_detect(input$User_name, "minimum 6 characters") != TRUE & str_count(input$User_name) >= 6 &
       str_detect(input$Project_name, "minimum 6 characters") != TRUE & str_count(input$Project_name) >= 6)
    {
      enable("Create")
    }else
    {
      disable("Create")
    }
    
    #Load button
    if(input$Proj_ID_load == "")
    {
      disable("Load")
    }else
    {
      enable("Load")
    }
    
    
    if(input$want_email2 == TRUE)
    {
      enable("email_2")
    }else
    {
      disable("email_2")
    }
    
    
    #   #### CALCULATIONS ------
    
    source(file = "server_BrA.R", local = TRUE)
    source(file = "server_BrB.R", local = TRUE)
    source(file = "server_BrC.R", local = TRUE)
  })
  
  
  ##### NEW PROJECT - CREATE -------------------
  ##### Reset outputs at create -----
  #load upload state
  observeEvent(input$Upload_genome,
               {rval_upload$data <- "uploaded"})
  
  observeEvent(input$Create, {
    #enable("Run")
    #updateActionButton(session, inputId = "Run", label = "START calculations")
    rval_upload$data <- "NULL"
    rval_proj_name$data <- NULL
    rval_status_mes$data <- "NULL"
    rval_status_DF$data <- make_status_df_fun("empty")
    rval_cont$data <- "no"
    rval_create_load$data <- "yes"
    rval_shiny_opt_TB <- reactiveValues(data = shiny_opt_TB_fun())
  })
  
  # ############################## Hm, there is no single run event, tehre are multiple, one for each button----------
  #   observeEvent(input$Run, {
  #     rval_VirClust_mes$data <- "NULL"
  #     rval_status_mes$data <- "NULL"
  #     #rval_status_DF$data <- tibble(step = c("1A","2A","3A", "4A", "5A","6A","7A", "1B", "2B", "3B", "4B", "5B", "6B", "1C", "2C", "3C", "4C", "5C", "6C"), 
  #     #status = "not_run") %>%
  #     #column_to_rownames(var="step")
  #     #rval_cont$data <- "no"
  #   })
  
  
  #### Create project ----
  validate_fasta <- eventReactive(eventExpr = input$Create,
                                  {
                                    validate(
                                      need(str_count(input$User_name) >= 6, ""),
                                      need(str_count(input$Project_name) >= 6, ""),
                                      need(rval_upload$data == "uploaded", "")
                                    )
                                    
                                    in_df <- input$Upload_genome
                                    
                                    if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                                    {
                                      
                                      if(nrow(in_df) > 1)
                                      {
                                        out_text <- "Please input only one file!"
                                      }
                                      
                                      if(nrow(in_df) == 1)
                                      {
                                        accep_exten <- c(".fasta",".fna", ".fa")
                                        ind_df <- in_df%>%
                                          mutate(exten = str_extract(name, "\\.[^\\.]+$")) %>%
                                          filter(!exten %in% accep_exten)
                                        if(nrow(ind_df) > 0)
                                        {
                                          out_text <- "Invalid file type! File extension needs to be .fasta, .fa or .fna"
                                        }else{
                                          out_text  <- "Valid file type(s)."
                                        }
                                        
                                        rm(accep_exten, ind_df)
                                      }
                                      
                                      
                                    }
                                    
                                    if(input$in_type == "Nuclei acids, one genome per file")
                                    {
                                      accep_exten <- c(".fasta",".fna", ".fa")
                                      
                                      ind_df <- in_df%>%
                                        mutate(exten = str_extract(name, "\\.[^\\.]+$")) %>%
                                        filter(!exten %in% accep_exten)
                                      
                                      if(nrow(ind_df) > 0)
                                      {
                                        out_text <- "Invalid file type! File extension needs to be .fasta, .fa or .fna"
                                      }else
                                      {
                                        out_text  <- "Valid file type(s)."
                                      }
                                      rm(accep_exten, ind_df)
                                    }
                                    
                                    # if(input$in_type == "Proteins, one fasta file per genome")
                                    # {
                                    #   accep_exten <- c(".fasta",".faa")
                                    #
                                    #   ind_df <- in_df%>%
                                    #     mutate(exten = str_extract(name, "\\.[^\\.]+$")) %>%
                                    #     filter(!exten %in% accep_exten)
                                    #
                                    #   if(nrow(ind_df) > 0)
                                    #   {
                                    #     out_text <- "Invalid file type! File extension needs to be .fasta or .faa"
                                    #   }else{
                                    #     out_text  <- "Valid file type(s)."
                                    #   }
                                    #   rm(accep_exten, ind_df)
                                    # }
                                    #
                                    return(out_text)
                                  })
  
  dir_path <- eventReactive(eventExpr = input$Create,
                            {
                              validate(
                                need(str_count(input$User_name) >= 6, ""),
                                need(str_count(input$Project_name) >= 6, ""),
                                need(rval_upload$data <- "uploaded", ""),
                                need(validate_fasta() == "Valid file type(s).", "")
                              )
                              
                              pref <- list.dirs(data_VirClust_path, recursive = FALSE)%>%
                                length()
                              
                              dir_path <- proj_dir_fun(prefix = pref, User_name = input$User_name, Project_name = input$Project_name, dir_path = data_VirClust_path)
                              dir.create(dir_path)
                              
                              #log_name <- paste0(dir_path, "_LOG.txt")
                              #write(x = "running\n", file = log_name)
                              
                              if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                              {
                                in_d <- "/00/00_in"
                              }
                              if(input$in_type == "Nuclei acids, one genome per file")
                              {
                                in_d <- "/00/00_out"
                              }
                              
                              in_p <- paste0(dir_path, in_d)
                              dir.create(in_p, recursive = TRUE)
                              
                              in_df <- input$Upload_genome %>%
                                mutate(in_path = paste0(in_p, "/", name))
                              
                              todel <- mapply(file.copy, from = in_df$datapath, to = in_df$in_path)
                              
                              
                              if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                              {
                                geno_num <- read.fasta(file = in_df$in_path[1], seqtype = "DNA", as.string = TRUE) %>%
                                  length()
                              }
                              if(input$in_type == "Nuclei acids, one genome per file")
                              {
                                geno_num <- list.files(in_p) %>%
                                  length()
                              }
                              
                              
                              output_ls <- ""
                              output_ls[1] <- dir_path
                              output_ls[2] <- geno_num
                              
                              return(output_ls)
                            })
  
  observeEvent(input$Create,{
    rval_proj_name$data <- dir_path()[1]
    rval_genome_no$data <- dir_path()[2] %>%
      as.numeric()
    
    rval_valid_fasta$data <- validate_fasta()
  })
  
  
  
  
  #### messages after create --------
  output$fasta_val <- renderText(expr = validate_fasta())
  output$Current <- renderText(expr=
  {
    validate(need(is.null(rval_proj_name$data) != TRUE, ""))
    text <- "Current project ID: "
    return(text)
  })
  output$Proj_ID <- renderText(expr =
  {
    validate(need(is.null(rval_proj_name$data) != TRUE, ""))
    proj_name <- str_remove( rval_proj_name$data, data_VirClust_path )
    return(proj_name)
  })
  
  
  ##### NEW PROJECT - RUN Buttons ----- 
  
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
                                start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1A")
                              }
                              
                              
                              ## options
                              if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                              {
                                multiF <- "yes"
                              }
                              if(input$in_type == "Nuclei acids, one genome per file")
                              {
                                multiF <- "no"
                              }
                              
                              #### VirClust cmd
                              st_1A <- sys::exec_wait(cmd = "Rscript", 
                                                      args = c(master_p,
                                                               paste0("projdir=", rval_proj_name$data),
                                                               "step1A=T",
                                                               paste0("multiF=", multiF),
                                                               paste0("continue=", rval_cont$data),
                                                               "shiny=yes"
                                                      ), 
                                                      std_out = paste0(rval_proj_name$data, "/shiny_stdout_1A.txt"), 
                                                      std_err = paste0(rval_proj_name$data, "/shiny_stderr_1A.txt"))
                              
                              if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
                              {
                                end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "1A", status = st_1A)
                              }
                              
                              
                              return(mes_after_run_fun(step= "1A", status_step = st_1A))
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
    # if(rval_mes_1A$data == "NULL")
    # {
    #   text <- ""
    # }else
    # {
    #   text <- rval_mes_1A$data
    # }
    #return(text)
    rval_mes_1A$data
  })
  
  ###run 2A -----------
  run_2A <- eventReactive(input$actProt_2_PCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {
        start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2A") 
      }
      
      #### VirClust cmd
      st_2A <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step2A=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       paste0("clust_PC=", input$clust_PC),
                                       paste0("eval_PC=", input$eval_PC),
                                       paste0("bitsc_PC=", input$bitsc_PC),
                                       paste0("cov_PC=", input$cov_PC),
                                       paste0("pident_PC=", input$pident_PC)), 
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_2A.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_2A.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {
        end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "2A", status = st_2A)
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
    # if(rval_mes_2A$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_2A$data}
    # return(text)
    rval_mes_2A$data
  })
  
  
  ###run 3A -----------
  
  run_3A <- eventReactive(input$actCluster_genomes_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3A")}
      
      ###params
      if(input$boot_a == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
      
      #### VirClust cmd
      st_3A <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step3A=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PC",
                                       paste0("boot_pv_a=", boot_pv),
                                       paste0("bootstrap_no_a=", input$boot_no_a),
                                       paste0("aglom_a=", input$aglom_a)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_3A.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_3A.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3A", status = st_3A)}
      
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
    # if(rval_mes_3A$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3A$data}
    # return(text)
    rval_mes_3A$data
  })
  
  
  ###run 3A_Plot ------
  run_3A_Plot <- eventReactive(input$actOut_pdf_3A, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3A_Plot")}
      
      #### VirClust cmd
      st_3A_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step3A_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PC",
                                            paste0("inc_fact_w_Pd=", input$inc_fact_w_Pd_A),
                                            paste0("font_row_Pd=", input$font_row_Pd_A),
                                            paste0("font_col_Pd=", input$font_col_Pd_A),
                                            paste0("font_cell_Pd=", input$font_cell_Pd_A),
                                            paste0("lgd_width_Pd=", input$lgd_width_Pd_A), 
                                            paste0("lgd_height_Pd=", input$lgd_height_Pd_A),
                                            paste0("lgd_font_Pd=", input$lgd_font_Pd_A),
                                            #paste0("lgd_pos_Pd=", input$lgd_pos_Pd_A),
                                            paste0("lgd_lab_font_Pd=", input$lgd_lab_font_Pd_A)),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_3A_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_3A_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3A_Plot", status = st_3A_Plot)}
      
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
    # if(rval_mes_3A_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3A_Plot$data}
    # return(text)
    rval_mes_3A_Plot$data
  })
  
  
  ### run 4A --------
  run_4A <- eventReactive(input$actSplit_clust_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4A")}
      
      #### VirClust cmd
      st_4A <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step4A=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PC",
                                       paste0("clust_dist_a=", input$Clust_dist_PC),
                                       paste0("max_cols_HT=", input$sel_PCs_heatmap_a)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_4A.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_4A.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "4A", status = st_4A)}
      
      return(mes_after_run_fun(step= "4A", status_step = st_4A))
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
    # if(rval_mes_4A$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_4A$data}
    # return(text)
    rval_mes_4A$data
  })
  
  
  ### run 4A_Plot ------
  run_4A_Plot <- eventReactive(input$actOut_pdf_4A, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4A_Plot")}
      
      
      #show params 
      if(input$Show_Tree_a == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
      if(input$show_heat_a == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
      if(input$show_protein_stats_a == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
      if(input$show_sil_a == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
      if(input$show_clust_ID_a == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
      
      #### VirClust cmd
      st_4A_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step4A_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PC",
                                            paste0("font_row=", input$font_row_a), 
                                            paste0("font_col=", input$font_col_a),
                                            paste0("stats_width=", input$Stats_width_a), 
                                            paste0("sil_stats_width=", input$Sil_stats_width_a),
                                            paste0("tree_width=", input$tree_width_a), 
                                            paste0("stats_font=", input$stats_font_a), 
                                            paste0("stats_lab_font=", input$stats_lab_font_a), 
                                            paste0("lgd_width=", input$lgd_w_a), 
                                            paste0("lgd_height=", input$lgd_h_a), 
                                            paste0("lgd_font=", input$lgd_font_a), 
                                            paste0("lgd_lab_font=", input$lgd_lab_font_a), 
                                            paste0("inc_fact_w=", input$Heat_width_a), 
                                            #paste0("lgd_pos=", ), 
                                            paste0("show_tree=", show_tree), 
                                            paste0("show_heat=", show_heat), 
                                            paste0("show_protein_stats=", show_protein_stats), 
                                            paste0("show_sil=", show_sil), 
                                            paste0("show_clust_ID=", show_clust_ID), 
                                            paste0("clustID_width=", input$clustID_width_a)
                                            
                                   ),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_4A_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_4A_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "4A_Plot", status = st_4A_Plot)}
      
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
    # if(rval_mes_4A_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_4A_Plot$data}
    # return(text)
    rval_mes_4A_Plot$data
  })
  
  
  ### run 5A --------
  run_5A <- eventReactive(input$actCore_PCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5A")}
      
      #### VirClust cmd
      st_5A <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step5A=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PC"),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_5A.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_5A.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5A", status = st_5A)}
      
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
    # if(rval_mes_5A$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_5A$data}
    # return(text)
    rval_mes_5A$data
  })
  
  #BRANCH B
  
  ### run 1B --------
  run_1B <- eventReactive(input$actPCs_2_PSCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1B")}
      
      #### VirClust cmd
      st_1B <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step1B=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       paste0("clust_PSC=", input$clust_PSC),
                                       paste0("prob1_PSC=", input$prob1_PSC),
                                       paste0("prob2_PSC=", input$prob2_PSC),
                                       paste0("cov1_PSC=", input$cov1_PSC),
                                       paste0("cov2_PSC=", input$cov2_PSC),
                                       paste0("alig_PSC=", input$alig_PSC)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_1B.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_1B.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "1B", status = st_1B)}
      
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
    rval_shiny_opt_TB$data[["eval_PSC", "value"]] <- input$eval_PSC
    rval_shiny_opt_TB$data[["bitsc_PSC", "value"]] <- input$bitsc_PSC
    rval_shiny_opt_TB$data[["cov_PSC", "value"]] <- input$cov_PSC
    rval_shiny_opt_TB$data[["pident_PSC", "value"]] <- input$pident_PSC
    
    saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
    write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
  })
  
  output$mes_1B <- renderText({
    # if(rval_mes_1B$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_1B$data}
    # return(text)
    rval_mes_1B$data
  })
  
  ###run 2B -----------
  run_2B <- eventReactive(input$actCluster_genomes_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2B")}
      
      ###params
      if(input$boot_b == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
      
      #### VirClust cmd
      st_2B <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step2B=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSC",
                                       paste0("boot_pv_b=", boot_pv),
                                       paste0("bootstrap_no_b=", input$boot_no_b),
                                       paste0("aglom_b=", input$aglom_b)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_2B.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_2B.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "2B", status = st_2B)}
      
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
    # if(rval_mes_2B$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_2B$data}
    # return(text)
    rval_mes_2B$data
  })
  
  
  ###run 2B_Plot ------
  run_2B_Plot <- eventReactive(input$actOut_pdf_2B, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2B_Plot")}
      
      #### VirClust cmd
      st_2B_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step2B_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PSC",
                                            paste0("inc_fact_w_Pd=", input$inc_fact_w_Pd_B),
                                            paste0("font_row_Pd=", input$font_row_Pd_B),
                                            paste0("font_col_Pd=", input$font_col_Pd_B),
                                            paste0("font_cell_Pd=", input$font_cell_Pd_B),
                                            paste0("lgd_width_Pd=", input$lgd_width_Pd_B), 
                                            paste0("lgd_height_Pd=", input$lgd_height_Pd_B),
                                            paste0("lgd_font_Pd=", input$lgd_font_Pd_B),
                                            #paste0("lgd_pos_Pd=", input$lgd_pos_Pd_A),
                                            paste0("lgd_lab_font_Pd=", input$lgd_lab_font_Pd_B)),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_2B_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_2B_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "2B_Plot", status = st_2B_Plot)}
      
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
    # if(rval_mes_2B_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_2B_Plot$data}
    # return(text)
    rval_mes_2B_Plot$data
  })
  
  
  ### run 3B --------
  run_3B <- eventReactive(input$actSplit_clust_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3B")}
      
      #### VirClust cmd
      st_3B <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step3B=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSC",
                                       paste0("clust_dist_b=", input$Clust_dist_PSC),
                                       paste0("max_cols_HT=", input$sel_PSCs_heatmap_b)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_3B.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_3B.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3B", status = st_3B)}
      
      return(mes_after_run_fun(step= "3B", status_step = st_3B))
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
    rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]] <- input$Clust_dist_PSC
    rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]] <- input$sel_PSCs_heatmap_b
    
    saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
    write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
  })
  
  output$mes_3B <- renderText({
    # if(rval_mes_3B$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3B$data}
    # return(text)
    rval_mes_3B$data
  })
  
  ### run 3B_Plot ------
  run_3B_Plot <- eventReactive(input$actOut_pdf_3B, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3B_Plot")}
      
      
      #show params 
      if(input$Show_Tree_b == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
      if(input$show_heat_b == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
      if(input$show_protein_stats_b == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
      if(input$show_sil_b == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
      if(input$show_clust_ID_b == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
      
      #### VirClust cmd
      st_3B_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step3B_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PSC",
                                            paste0("font_row=", input$font_row_b), 
                                            paste0("font_col=", input$font_col_b),
                                            paste0("stats_width=", input$Stats_width_b), 
                                            paste0("sil_stats_width=", input$Sil_stats_width_b),
                                            paste0("tree_width=", input$tree_width_b), 
                                            paste0("stats_font=", input$stats_font_b), 
                                            paste0("stats_lab_font=", input$stats_lab_font_b), 
                                            paste0("lgd_width=", input$lgd_w_b), 
                                            paste0("lgd_height=", input$lgd_h_b), 
                                            paste0("lgd_font=", input$lgd_font_b), 
                                            paste0("lgd_lab_font=", input$lgd_lab_font_b), 
                                            paste0("inc_fact_w=", input$Heat_width_b), 
                                            #paste0("lgd_pos=", ), 
                                            paste0("show_tree=", show_tree), 
                                            paste0("show_heat=", show_heat), 
                                            paste0("show_protein_stats=", show_protein_stats), 
                                            paste0("show_sil=", show_sil), 
                                            paste0("show_clust_ID=", show_clust_ID), 
                                            paste0("clustID_width=", input$clustID_width_b)
                                            
                                   ),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_3B_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_3B_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3B_Plot", status = st_3B_Plot)}
      
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
    # if(rval_mes_3B_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3B_Plot$data}
    # return(text)
    rval_mes_3B_Plot$data
  })
  
  ### run 4B --------
  run_4B <- eventReactive(input$actCore_PSCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4B")}
      
      #### VirClust cmd
      st_4B <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step4B=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSC"),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_4B.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_4B.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "4B", status = st_4B)}
      
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
    # if(rval_mes_4B$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_4B$data}
    # return(text)
    rval_mes_4B$data
  })
  
  
  #BRANCH C
  
  ### run 1C --------
  run_1C <- eventReactive(input$actPSCs_2_PSSCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "1C")}
      
      #### VirClust cmd
      st_1C <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step1C=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       paste0("clust_PSC=", input$clust_PSSC),
                                       paste0("prob1_PSC=", input$prob1_PSSC),
                                       paste0("prob2_PSC=", input$prob2_PSSC),
                                       paste0("cov1_PSC=", input$cov1_PSSC),
                                       paste0("cov2_PSC=", input$cov2_PSSC),
                                       paste0("alig_PSC=", input$alig_PSSC)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_1C.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_1C.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "1C", status = st_1C)}
      
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
    rval_shiny_opt_TB$data[["eval_PSSC", "value"]] <- input$eval_PSSC
    rval_shiny_opt_TB$data[["bitsc_PSSC", "value"]] <- input$bitsc_PSSC
    rval_shiny_opt_TB$data[["cov_PSSC", "value"]] <- input$cov_PSSC
    rval_shiny_opt_TB$data[["pident_PSSC", "value"]] <- input$pident_PSSC
    
    saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
    write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
  })
  
  output$mes_1C <- renderText({
    # if(rval_mes_1C$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_1C$data}
    # return(text)
    rval_mes_1C$data
  })
  
  ###run 2C -----------
  run_2C <- eventReactive(input$actCluster_genomes_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2C")}
      
      ###params
      if(input$boot_c == TRUE){boot_pv <- "yes"}else{boot_pv <- "no"}
      
      #### VirClust cmd
      st_2C <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step2C=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSSC",
                                       paste0("boot_pv_c=", boot_pv),
                                       paste0("bootstrap_no_c=", input$boot_no_c),
                                       paste0("aglom_c=", input$aglom_c)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_2C.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_2C.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "2C", status = st_2C)}
      
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
    # if(rval_mes_2C$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_2C$data}
    # return(text)
    rval_mes_2C$data
  })
  
  ###run 2C_Plot ------
  run_2C_Plot <- eventReactive(input$actOut_pdf_2C, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "2C_Plot")}
      
      #### VirClust cmd
      st_2C_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step2C_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PSSC",
                                            paste0("inc_fact_w_Pd=", input$inc_fact_w_Pd_C),
                                            paste0("font_row_Pd=", input$font_row_Pd_C),
                                            paste0("font_col_Pd=", input$font_col_Pd_C),
                                            paste0("font_cell_Pd=", input$font_cell_Pd_C),
                                            paste0("lgd_width_Pd=", input$lgd_width_Pd_C), 
                                            paste0("lgd_height_Pd=", input$lgd_height_Pd_C),
                                            paste0("lgd_font_Pd=", input$lgd_font_Pd_C),
                                            #paste0("lgd_pos_Pd=", input$lgd_pos_Pd_A),
                                            paste0("lgd_lab_font_Pd=", input$lgd_lab_font_Pd_C)),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_2C_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_2C_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "2C_Plot", status = st_2C_Plot)}
      
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
    # if(rval_mes_2C_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_2C_Plot$data}
    # return(text)
    rval_mes_2C_Plot$data
  })
  
  
  ### run 3C --------
  run_3C <- eventReactive(input$actSplit_clust_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3C")}
      
      #### VirClust cmd
      st_3C <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step3C=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSSC",
                                       paste0("clust_dist_c=", input$Clust_dist_PSSC),
                                       paste0("max_cols_HT=", input$sel_PSCs_heatmap_c)),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_3C.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_3C.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3C", status = st_3C)}
      
      return(mes_after_run_fun(step= "3C", status_step = st_3C))
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
    rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]] <- input$Clust_dist_PSSC
    rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]] <- input$sel_PSSCs_heatmap_c
    
    saveRDS(rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.RDS"))
    write.table(x = rval_shiny_opt_TB$data, file = paste0(rval_proj_name$data, "/shiny_options.tsv"), sep = "\t", col.names = TRUE, row.names = TRUE)
  })
  
  output$mes_3C <- renderText({
    # if(rval_mes_3C$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3C$data}
    # return(text)
    rval_mes_3C$data
  })
  
  ### run 3C_Plot ------
  run_3C_Plot <- eventReactive(input$actOut_pdf_3C, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "3C_Plot")}
      
      
      #show params 
      if(input$Show_Tree_c == TRUE){show_tree <- "yes"}else{show_tree <- "no"}
      if(input$show_heat_c == TRUE){show_heat <- "yes"}else{show_heat <- "no"}
      if(input$show_protein_stats_c == TRUE){show_protein_stats <- "yes"}else{show_protein_stats <- "no"}
      if(input$show_sil_c == TRUE){show_sil <- "yes"}else{show_sil <- "no"}
      if(input$show_clust_ID_c == TRUE){show_clust_ID <- "yes"}else{show_clust_ID <- "no"}
      
      #### VirClust cmd
      st_3C_Plot <- sys::exec_wait(cmd = "Rscript", 
                                   args = c(master_p,
                                            paste0("projdir=", rval_proj_name$data),
                                            "step3C_Plot=T",
                                            paste0("continue=", rval_cont$data),
                                            "shiny=yes",
                                            
                                            #step options
                                            "pc_type=PSSC",
                                            paste0("font_row=", input$font_row_c), 
                                            paste0("font_col=", input$font_col_c),
                                            paste0("stats_width=", input$Stats_width_c), 
                                            paste0("sil_stats_width=", input$Sil_stats_width_c),
                                            paste0("tree_width=", input$tree_width_c), 
                                            paste0("stats_font=", input$stats_font_c), 
                                            paste0("stats_lab_font=", input$stats_lab_font_c), 
                                            paste0("lgd_width=", input$lgd_w_c), 
                                            paste0("lgd_height=", input$lgd_h_c), 
                                            paste0("lgd_font=", input$lgd_font_c), 
                                            paste0("lgd_lab_font=", input$lgd_lab_font_c), 
                                            paste0("inc_fact_w=", input$Heat_width_c), 
                                            #paste0("lgd_pos=", ), 
                                            paste0("show_tree=", show_tree), 
                                            paste0("show_heat=", show_heat), 
                                            paste0("show_protein_stats=", show_protein_stats), 
                                            paste0("show_sil=", show_sil), 
                                            paste0("show_clust_ID=", show_clust_ID), 
                                            paste0("clustID_width=", input$clustID_width_c)
                                            
                                   ),
                                   
                                   std_out = paste0(rval_proj_name$data, "/shiny_stdout_3C_Plot.txt"), 
                                   std_err = paste0(rval_proj_name$data, "/shiny_stderr_3C_Plot.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "3C_Plot", status = st_3C_Plot)}
      
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
    # if(rval_mes_3C_Plot$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_3C_Plot$data}
    # return(text)
    rval_mes_3C_Plot$data
  })
  
  
  ### run 4C --------
  run_4C <- eventReactive(input$actCore_PSSCs, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "4C")}
      
      #### VirClust cmd
      st_4C <- sys::exec_wait(cmd = "Rscript", 
                              args = c(master_p,
                                       paste0("projdir=", rval_proj_name$data),
                                       "step4C=T",
                                       paste0("continue=", rval_cont$data),
                                       "shiny=yes",
                                       
                                       #step options
                                       "pc_type=PSSC"),
                              
                              std_out = paste0(rval_proj_name$data, "/shiny_stdout_4C.txt"), 
                              std_err = paste0(rval_proj_name$data, "/shiny_stderr_4C.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "4C", status = st_4C)}
      
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
    # if(rval_mes_4C$data == "NULL")
    # {text <- ""}else
    # {text <- rval_mes_4C$data}
    # return(text)
    rval_mes_4C$data
  })
  
  
  ### run 6AI --------
  run_6AI <- eventReactive(input$actAnnotInterPro_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AI")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AI <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step6AI=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AI.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AI.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AI", status = st_6AI)}
      
      return(mes_after_run_annots_fun(step= "6AI", status_step = st_6AI,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotInterPro_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AI$data <- run_6AI()}else{rval_mes_6AI_core$data <- run_6AI}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AI <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AI$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AI$data}
      text <- rval_mes_6AI$data
    }else
    {
      # if(rval_mes_6AI_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AI_core$data}
      text <- rval_mes_6AI_core$data
    }
    return(text)
  })
  
  
  ### run 6ApV --------
  run_6ApV <- eventReactive(input$actAnnotpVOGs_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6ApV")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6ApV <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step6ApV=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_6ApV.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_6ApV.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6ApV", status = st_6ApV)}
      
      return(mes_after_run_annots_fun(step= "6ApV", status_step = st_6ApV,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
      
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotpVOGs_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6ApV$data <- run_6ApV()}else{rval_mes_6ApV_core$data <- run_6ApV()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6ApV <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6ApV$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6ApV$data}
      text <- rval_mes_6ApV$data
    }else
    {
      # if(rval_mes_6ApV_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6ApV_cor$data}
      text <- rval_mes_6ApV_cor$data
    }
    return(text)
  })
  
  
  ### run 6AVO --------
  run_6AVO <- eventReactive(input$actAnnotVOGDB_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AVO")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AVO <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step6AVO=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AVO.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AVO.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AVO", status = st_6AVO)}
      
      return(mes_after_run_annots_fun(step= "6AVO", status_step = st_6AVO,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotVOGDB_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AVO$data <- run_6AVO()}else{rval_mes_6AVO_core$data <- run_6AVO()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AVO <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AVO$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AVO$data}
      text <- rval_mes_6AVO$data
    }else
    {
      # if(rval_mes_6AVO_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AVO_core$data}
      text <- rval_mes_6AVO_core$data
    }
    return(text)
  })
  
  ### run 6APH --------
  run_6APH <- eventReactive(input$actAnnotPHROGS_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6APH")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6APH <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step6APH=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_6APH.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_6APH.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6APH", status = st_6APH)}
      
      return(mes_after_run_annots_fun(step= "6APH", status_step = st_6APH,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotPHROGS_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6APH$data <- run_6APH()}else{rval_mes_6APH_core$data <- run_6APH()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6APH <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6APH$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6APH$data}
      return(rval_mes_6APH$data)
    }else
    {
      # if(rval_mes_6APH_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6APH_core$data}
      return(rval_mes_6APH_core$data)
    }
  })
  
  ### run 6AE --------
  run_6AE <- eventReactive(input$actAnnotEfam_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AE")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AE <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step6AE=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AE.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AE.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AE", status = st_6AE)}
      
      return(mes_after_run_annots_fun(step= "6AE", status_step = st_6AE,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotEfam_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AE$data <- run_6AE()}else{rval_mes_6AE_core$data <- run_6AE()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AE <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AE$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AE$data}
      text <- rval_mes_6AE$data
    }else
    {
      # if(rval_mes_6AE_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AE_core$data}  
      text <- rval_mes_6AE_core$data
    }
    return(text)
  })
  
  
  ### run 6AXC --------
  run_6AXC <- eventReactive(input$`actAnnotEfam-XC_PC`, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AXC")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AXC <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step6AXC=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AXC.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AXC.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AXC", status = st_6AXC)}
      
      return(mes_after_run_annots_fun(step= "6AXC", status_step = st_6AXC,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$`actAnnotEfam-XC_PC`, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AXC$data <- run_6AXC()}else{rval_mes_6AXC_core$data <- run_6AXC()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AXC <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AXC$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AXC$data}
      text <- rval_mes_6AXC$data
    }else
    {
      # if(rval_mes_6AXC_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AXC_core$data}
      text <- rval_mes_6AXC_core$data
    }
    return(text)
  })
  
  ### run 6AN --------
  run_6AN <- eventReactive(input$actAnnotNCBI_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AN")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AN <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step6AN=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AN.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AN.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AN", status = st_6AN)}
      
      return(mes_after_run_annots_fun(step= "6AN", status_step = st_6AN,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotNCBI_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AN$data <- run_6AN()}else{rval_mes_6AN_core$data <- run_6AN()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AN <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AN$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AN$data}
      text <- rval_mes_6AN$data
    }else
    {
      # if(rval_mes_6AN_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AN_core$data}
      text <- rval_mes_6AN_core$data
    }
    return(text)
  })
  
  
  ### run 6AM --------
  run_6AM <- eventReactive(input$actMergeAnnot_PC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "6AM")}
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {prot_type <- "all_PCs"}else{prot_type <- "core_PCs"}
      
      
      #### VirClust cmd
      st_6AM <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step6AM=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_6AM.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_6AM.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "6AM", status = st_6AM)}
      
      return(mes_after_run_fun(step= "6AM", status_step = st_6AM))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actMergeAnnot_PC, {
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {rval_mes_6AM$data <- run_6AM()}else{rval_mes_6AM_core$data <- run_6AM()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_6AM <- renderText({
    if(Annot_prots_PCs == "all proteins and relate them to PCs")
    {
      # if(rval_mes_6AM$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AM$data}
      text <- rval_mes_6AM$data
    }else
    {
      # if(rval_mes_6AM_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_6AM_core$data}
      text <- rval_mes_6AM_core$data
    }
    return(text)
  })
  
  
  ### run 5BI --------
  run_5BI <- eventReactive(input$actAnnotInterPro_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BI")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BI <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5BI=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BI.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BI.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BI", status = st_5BI)}
      
      return(mes_after_run_annots_fun(step= "5BI", status_step = st_5BI,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotInterPro_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BI$data <- run_5BI()}else{rval_mes_5BI_core$data <- run_5BI()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BI <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      # if(rval_mes_5BI$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BI$data}
      text <- rval_mes_5BI$data
    }else
    {
      # if(rval_mes_5BI_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BI_core$data}
      text <- rval_mes_5BI_core$data
    }
    return(text)
  })
  
  
  ### run 5BpV --------
  run_5BpV <- eventReactive(input$actAnnotpVOGs_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BpV")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BpV <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5BpV=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BpV.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BpV.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BpV", status = st_5BpV)}
      
      return(mes_after_run_annots_fun(step= "5BpV", status_step = st_5BpV,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotpVOGs_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BpV$data <- run_5BpV()}else{rval_mes_5BpV_core$data <- run_5BpV()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BpV <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {    
      # if(rval_mes_5BpV$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BpV$data}
      text <- rval_mes_5BpV$data
    }else
    {
      # if(rval_mes_5BpV_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BpV_core$data}
      text <- rval_mes_5BpV_core$data
    }
    return(text)
  })
  
  
  ### run 5BVO --------
  run_5BVO <- eventReactive(input$actAnnotVOGDB_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BVO")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BVO <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5BVO=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BVO.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BVO.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BVO", status = st_5BVO)}
      
      return(mes_after_run_annots_fun(step= "5BVO", status_step = st_5BVO,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotVOGDB_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BVO$data <- run_5BVO()}else{rval_mes_5BVO_core$data <- run_5BVO()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BVO <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      #   if(rval_mes_5BVO$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BVO$data}
      text <- rval_mes_5BVO$data
    }else
    {
      # if(rval_mes_5BVO_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BVO_core$data}
      text <- rval_mes_5BVO_core$data
    }
    return(text)
  })
  
  ### run 5BPH --------
  run_5BPH <- eventReactive(input$actAnnotPHROGS_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BPH")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BPH <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5BPH=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BPH.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BPH.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BPH", status = st_5BPH)}
      
      return(mes_after_run_annots_fun(step= "5BPH", status_step = st_5BPH,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotPHROGS_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BPH$data <- run_5BPH()}else{rval_mes_5BPH_core$data <- run_5BPH()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BPH <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      #   if(rval_mes_5BPH$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BPH$data}
      text <- rval_mes_5BPH$data
    }else
    {
      #   if(rval_mes_5BPH_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BPH_core$data}
      text <- rval_mes_5BPH_core$data
    }
    return(text)
  })
  
  ### run 5BE --------
  run_5BE <- eventReactive(input$actAnnotEfam_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BE")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BE <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5BE=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BE.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BE.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BE", status = st_5BE)}
      
      return(mes_after_run_annots_fun(step= "5BE", status_step = st_5BE,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotEfam_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BE$data <- run_5BE()}else{rval_mes_5BE_core$data <- run_5BE()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BE <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      #   if(rval_mes_5BE$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BE$data}
      text <- rval_mes_5BE$data
    }else
    {
      #   if(rval_mes_5BE_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BE_core$data}
      text <- rval_mes_5BE_core$data
    }
    return(text)
  })
  
  
  ### run 5BXC --------
  run_5BXC <- eventReactive(input$`actAnnotEfam-XC_PSC`, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BXC")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BXC <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5BXC=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BXC.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BXC.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BXC", status = st_5BXC)}
      
      return(mes_after_run_annots_fun(step= "5BXC", status_step = st_5BXC,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$`actAnnotEfam-XC_PSC`, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BXC$data <- run_5BXC()}else{rval_mes_5BXC_core$data <- run_5BXC()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BXC <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      #   if(rval_mes_5BXC$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BXC$data}
      text <- rval_mes_5BXC$data
    }else
    {
      #   if(rval_mes_5BXC_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BXC_core$data}
      text <- rval_mes_5BXC_core$data
    }
    return(text)
  })
  
  ### run 5BN --------
  run_5BN <- eventReactive(input$actAnnotNCBI_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BN")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BN <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5BN=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BN.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BN.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BN", status = st_5BN)}
      
      return(mes_after_run_annots_fun(step= "5BN", status_step = st_5BN,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotNCBI_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BN$data <- run_5BN()}else{rval_mes_5BN_core$data <- run_5BN()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BN <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      # if(rval_mes_5BN$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BN$data}
      text <- rval_mes_5BN$data
    }else
    {
      # if(rval_mes_5BN_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BN_core$data}
      text <- rval_mes_5BN_core$data
    }
    return(text)
  })
  
  
  ### run 5BM --------
  run_5BM <- eventReactive(input$actMergeAnnot_PSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5BM")}
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {prot_type <- "all_PSCs"}else{prot_type <- "core_PSCs"}
      
      
      #### VirClust cmd
      st_5BM <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5BM=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5BM.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5BM.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5BM", status = st_5BM)}
      
      return(mes_after_run_fun(step= "5BM", status_step = st_5BM))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actMergeAnnot_PSC, {
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {rval_mes_5BM$data <- run_5BM()}else{rval_mes_5BM_core$data <- run_5BM()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5BM <- renderText({
    if(Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {
      #   if(rval_mes_5BM$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BM$data}
      text <- rval_mes_5BM$data
    }else
    {
      #   if(rval_mes_5BM_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5BM_core$data}
      text <- rval_mes_5BM_core$data
    }
    return(text)
  })
  
  
  ### run 5CI --------
  run_5CI <- eventReactive(input$actAnnotInterPro_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CI")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CI <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5CI=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CI.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CI.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CI", status = st_5CI)}
      
      return(mes_after_run_annots_fun(step= "5CI", status_step = st_5CI,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/interpro/interpro_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotInterPro_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CI$data <- run_5CI()}else{rval_mes_5CI_core$data <- run_5CI()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CI <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      #   if(rval_mes_5CI$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CI$data}
      text <- rval_mes_5CI$data
    }else
    {
      #   if(rval_mes_5CI_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CI_core$data}
      text <- rval_mes_5CI_core$data
    }
    return(text)
  })
  
  
  ### run 5CpV --------
  run_5CpV <- eventReactive(input$actAnnotpVOGs_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CpV")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CpV <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5CpV=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CpV.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CpV.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CpV", status = st_5CpV)}
      
      return(mes_after_run_annots_fun(step= "5CpV", status_step = st_5CpV,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotpVOGs_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CpV$data <- run_5CpV()}else{rval_mes_5CpV_core$data <- run_5CpV()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CpV <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      #   if(rval_mes_5CpV$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CpV$data}
      text <- rval_mes_5CpV$data
    }else
    {
      #   if(rval_mes_5CpV_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CpV_core$data}
      text <- rval_mes_5CpV_core$data
    }
    return(text)
  })
  
  
  ### run 5CVO --------
  run_5CVO <- eventReactive(input$actAnnotVOGDB_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CVO")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CVO <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5CVO=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CVO.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CVO.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CVO", status = st_5CVO)}
      
      return(mes_after_run_annots_fun(step= "5CVO", status_step = st_5CVO,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotVOGDB_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CVO$data <- run_5CVO()}else{rval_mes_5CVO_core$data <- run_5CVO()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CVO <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      # if(rval_mes_5CVO$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CVO$data}
      text <- rval_mes_5CVO$data
    }else
    {
      # if(rval_mes_5CVO_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CVO_core$data}
      text <- rval_mes_5CVO_core$data
    }
    return(text)
  })
  
  ### run 5CPH --------
  run_5CPH <- eventReactive(input$actAnnotPHROGS_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CPH")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CPH <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5CPH=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CPH.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CPH.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CPH", status = st_5CPH)}
      
      return(mes_after_run_annots_fun(step= "5CPH", status_step = st_5CPH,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotPHROGS_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CPH$data <- run_5CPH()}else{rval_mes_5CPH_core$data <- run_5CPH()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CPH <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      #   if(rval_mes_5CPH$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CPH$data}
      text <- rval_mes_5CPH$data
    }else
    {
      #   if(rval_mes_5CPH_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CPH_core$data}
      text <- rval_mes_5CPH_core$data
    }
    return(text)
  })
  
  ### run 5CE --------
  run_5CE <- eventReactive(input$actAnnotEfam_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CE")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CE <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5CE=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CE.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CE.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CE", status = st_5CE)}
      
      return(mes_after_run_annots_fun(step= "5CE", status_step = st_5CE,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam/hmmscan_Efam.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotEfam_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CE$data <- run_5CE()}else{rval_mes_5CE_core$data <- run_5CE()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CE <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      #   if(rval_mes_5CE$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CE$data}
      text <- rval_mes_5CE$data
    }else
    {
      #   if(rval_mes_5CE_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CE_core$data}
      text <- rval_mes_5CE_core$data
    }
    return(text)
  })
  
  
  ### run 5CXC --------
  run_5CXC <- eventReactive(input$`actAnnotEfam-XC_PSSC`, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CXC")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CXC <- sys::exec_wait(cmd = "Rscript",
                                args = c(master_p,
                                         paste0("projdir=", rval_proj_name$data),
                                         "step5CXC=T",
                                         paste0("continue=", rval_cont$data),
                                         "shiny=yes",
                                         
                                         #step options
                                         paste0("prot_type=", prot_type)),
                                
                                std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CXC.txt"),
                                std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CXC.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CXC", status = st_5CXC)}
      
      return(mes_after_run_annots_fun(step= "5CXC", status_step = st_5CXC,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$`actAnnotEfam-XC_PSSC`, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CXC$data <- run_5CXC()}else{rval_mes_5CXC_core$data <- run_5CXC()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CXC <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      #   if(rval_mes_5CXC$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CXC$data}
      text <- rval_mes_5CXC$data
    }else
    {
      #   if(rval_mes_5CXC_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CXC_core$data}
      text <- rval_mes_5CXC_core$data
    }
    return(text)
  })
  
  ### run 5CN --------
  run_5CN <- eventReactive(input$actAnnotNCBI_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CN")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CN <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5CN=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CN.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CN.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CN", status = st_5CN)}
      
      return(mes_after_run_annots_fun(step= "5CN", status_step = st_5CN,
                                      projdir=rval_proj_name$data, prot_type= prot_type, annot_suf = "/BlastP_NR/blastp_TB.RDS"))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actAnnotNCBI_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CN$data <- run_5CN()}else{rval_mes_5CN_core$data <- run_5CN()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CN <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      # if(rval_mes_5CN$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CN$data}
      text <- rval_mes_5CN$data
    }else
    {
      # if(rval_mes_5CN_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CN_core$data}
      text <- rval_mes_5CN_core$data
    }
    return(text)
  })
  
  
  ### run 5CM --------
  run_5CM <- eventReactive(input$actMergeAnnot_PSSC, {
    withProgress(expr = {
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {start_step_email_fun(user=input$User_name, name=rval_proj_name$data, path=data_VirClust_path, step = "5CM")}
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {prot_type <- "all_PSSCs"}else{prot_type <- "core_PSSCs"}
      
      
      #### VirClust cmd
      st_5CM <- sys::exec_wait(cmd = "Rscript",
                               args = c(master_p,
                                        paste0("projdir=", rval_proj_name$data),
                                        "step5CM=T",
                                        paste0("continue=", rval_cont$data),
                                        "shiny=yes",
                                        
                                        #step options
                                        paste0("prot_type=", prot_type)),
                               
                               std_out = paste0(rval_proj_name$data, "/shiny_stdout_5CM.txt"),
                               std_err = paste0(rval_proj_name$data, "/shiny_stderr_5CM.txt"))
      
      if(str_detect(input$email_2, ".+@.+") == TRUE & input$want_email2 == TRUE)
      {end_step_email_fun(name=rval_proj_name$data, path=data_VirClust_path, step = "5CM", status = st_5CM)}
      
      return(mes_after_run_fun(step= "5CM", status_step = st_5CM))
    }, message = rval_progress_mess$data
    )
  })
  
  observeEvent(eventExpr = input$actMergeAnnot_PSSC, {
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {rval_mes_5CM$data <- run_5CM()}else{rval_mes_5CM_core$data <- run_5CM()}
    source("server_afterRunButtons.R", local = TRUE)
  })
  
  output$mes_5CM <- renderText({
    if(Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {
      # if(rval_mes_5CM$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CM$data}
      text <- rval_mes_5CM$data
    }else
    {
      # if(rval_mes_5CM_core$data == "NULL")
      # {text <- ""}else
      # {text <- rval_mes_5CM_core$data}
      text <- rval_mes_5CM_core$data
    }
    return(text)
  })
  
  
  #### Load project after load ----
  observeEvent(input$Load,
               {
                 ###this code is to reset all the states if a previous project was ran in the webapp
                 #enable("Run")
                 #updateActionButton(session, inputId = "Run", label = "CONTINUE calculations")
                 rval_upload$data <- "NULL"
                 rval_proj_name$data <- NULL
                 rval_VirClust_mes$data <- "NULL"
                 rval_status_mes$data <- "NULL"
                 rval_status_DF$data <- make_status_df_fun("empty")
                 rval_cont$data <- "no"
                 rval_create_load$data <- "yes"
                 rval_valid_fasta$data <- "Valid file type(s)."
                 rval_genome_no$data <- 0
                 rval_shiny_opt_TB <- reactiveValues(data = NULL)
               })
  
  
  observeEvent(input$Load,
               {
                 proj_dir <- paste0(data_VirClust_path, input$Proj_ID_load)
                 validate(need(dir.exists(proj_dir) == TRUE, ""))
                 rval_proj_name$data <- proj_dir
                 rval_genome_no$data <- list.files(path = paste0(proj_dir, "/00/00_out")) %>%
                   length()
                 #rval_cont$data <- "yes"   ###I think this should be yes; in the sourced code below it it set to yes
                 rm(proj_dir)
                 
                 source("server_afterRunButtons.R", local = TRUE)
                 
               })
  
  
  output$Status <- renderText(expr=
  {
    validate(need(rval_status_mes$data != "NULL", ""))
    
    if(rval_status_mes$data == "running")
    {
      text <- paste0("The project is ", rval_status_mes$data, ", re-load it later. Don't start any other calculations until is finished.")
    }
    if(rval_status_mes$data == "inexistent")
    {
      text <- paste0("No project with this name exists. Check the name and re-load.")
    }
    if(rval_status_mes$data == "finished")
    {
      text <- paste0("The project is ", rval_status_mes$data, ".")
    }
    
    return(text)
  })
  
  observeEvent(input$Load,
               {
                 shiny_options_p <- paste0(rval_proj_name$data, "/shiny_options.RDS")
                 validate(need(file.exists(shiny_options_p) == TRUE, ""))
                 rval_shiny_opt_TB$data <- readRDS(shiny_options_p)
                 
                 shiny_options_TB <<- rval_shiny_opt_TB$data
                 
                 #Branch A 
                 #1A
                 if(rval_shiny_opt_TB$data[["Genetic_code", "value"]] != "NULL")
                 {updateSelectInput(session = session, "Genetic_code", label = "Translation table",
                                    choices = c("11- bacteria, archaea and prokaryotic viruses", "non-canonical bases"), 
                                    selected = rval_shiny_opt_TB$data[["Genetic_code", "value"]])}
                 
                 #2A
                 if(rval_shiny_opt_TB$data[["clust_PC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PC", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["eval_PC", "value"]] != "NULL")
                 {updateNumericInput(session =session, "eval_PC", label = "e-value >", value = rval_shiny_opt_TB$data[["eval_PC", "value"]], 
                                     min = 0, max = 0.01, step = 0.00001)}
                 
                 if(rval_shiny_opt_TB$data[["bitsc_PC", "value"]] != "NULL")
                 {updateNumericInput(session =session, "bitsc_PC", label = "bitscore <", value = rval_shiny_opt_TB$data[["bitsc_PC", "value"]], 
                                     min = 20, step=1)}
                 
                 if(rval_shiny_opt_TB$data[["cov_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov_PC", label = "coverage <", value = rval_shiny_opt_TB$data[["cov_PC", "value"]], 
                                     min = 0, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["pident_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "pident_PC", label = "% identity <", value = rval_shiny_opt_TB$data[["pident_PC", "value"]], 
                                     min = 0, max = 100)}
                 
                 #3A
                 if(rval_shiny_opt_TB$data[["aglom_a", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_a", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_a", label = "Enable bootstrapping", value = rval_shiny_opt_TB$data[["boot_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_no_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_a", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_a", "value"]], step = 1)}
                 
                 #3A_Plot
                 if(rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_A", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_A", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_A", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_A", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_A", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_A", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_A", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_A", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]], min=1)}
                 
                 #4A
                 if(rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]], step = 0.01)}
                 
                 if(rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PCs_heatmap_a", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]], min =1)}
                 
                 #4A_Plot
                 if(rval_shiny_opt_TB$data[["Show_Tree_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_a", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["tree_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_a", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_a", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_sil_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_a", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_a", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["clustID_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_a", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_a", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Stats_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_a", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["stats_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_a", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_a", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["show_heat_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_a", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_a", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["Heat_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_a", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_a", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_a", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_a", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_a", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_a", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_a", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_a", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_h_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_a", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_a", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_w_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_a", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_a", "value"]], min=1)}
                 
                 #5A
                 if(rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs", "core proteins based on PCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]], inline = FALSE)}
                 
                 
                 
                 #1B
                 if(rval_shiny_opt_TB$data[["clust_PSC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PSC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PSC", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["prob1_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob1_PSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob1_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["cov1_PSC", "value"]] != "NULL")
                 {updateNumericInput(session=session, "cov1_PSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov1_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["prob2_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob2_PSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob2_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["cov2_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov2_PSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov2_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["alig_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "alig_PSC", label = "alignment length <", 
                                     value =  rval_shiny_opt_TB$data[["alig_PSC", "value"]], step = 1, min = 1)}
                 
                 #2B
                 if(rval_shiny_opt_TB$data[["aglom_b", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_b", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_b", label = "Enable bootstrapping", value = rval_shiny_opt_TB$data[["boot_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_no_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_b", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_b", "value"]], step = 1)}
                 
                 #2B_Plot
                 if(rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_B", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_B", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_B", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_B", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_B", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_B", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_B", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_B", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]], min=1)}
                 
                 #3B
                 if(rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PSC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]], step = 0.01)}
                 
                 if(rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PSCs_heatmap_b", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]], min =1)}
                 
                 #3B_Plot
                 if(rval_shiny_opt_TB$data[["Show_Tree_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_b", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["tree_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_b", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_b", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_sil_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_b", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_b", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["clustID_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_b", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_b", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Stats_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_b", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["stats_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_b", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_b", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["show_heat_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_b", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_b", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["Heat_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_b", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_b", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_b", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_b", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_b", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_b", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_b", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_b", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_h_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_b", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_b", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_w_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_b", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_b", "value"]], min=1)}
                 
                 #4B
                 if(rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PSCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs and PSCs", "core proteins based on PSCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]], inline = FALSE)}
                 
                 
                 #1C
                 if(rval_shiny_opt_TB$data[["clust_PSSC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PSSC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PSSC", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["prob1_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob1_PSSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob1_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["cov1_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session=session, "cov1_PSSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov1_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["prob2_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob2_PSSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob2_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["cov2_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov2_PSSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov2_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(rval_shiny_opt_TB$data[["alig_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "alig_PSSC", label = "alignment length <", 
                                     value =  rval_shiny_opt_TB$data[["alig_PSSC", "value"]], step = 1, min = 1)}
                 
                 #2C
                 if(rval_shiny_opt_TB$data[["aglom_c", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_c", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_c", label = "Enable bootstrapping", value = rval_shiny_opt_TB$data[["boot_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["boot_no_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_c", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_c", "value"]], step = 1)}
                 
                 #2C_Plot
                 if(rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_C", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_C", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_C", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_C", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_C", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_C", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_C", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_C", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]], min=1)}
                 
                 #3C
                 if(rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PSSC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]], step = 0.01)}
                 
                 if(rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PSSCs_heatmap_c", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]], min =1)}
                 
                 #3C_Plot
                 if(rval_shiny_opt_TB$data[["Show_Tree_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_c", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["tree_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_c", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_c", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_sil_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_c", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_c", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["clustID_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_c", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_c", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["Stats_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_c", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["stats_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_c", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_c", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["show_heat_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_c", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_c", "value"]])}
                 
                 if(rval_shiny_opt_TB$data[["Heat_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_c", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_c", "value"]], step = 0.001)}
                 
                 if(rval_shiny_opt_TB$data[["font_col_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_c", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_c", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["font_row_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_c", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_c", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_c", "value"]], min=1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_c", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_h_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_c", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_c", "value"]], min = 1)}
                 
                 if(rval_shiny_opt_TB$data[["lgd_w_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_c", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_c", "value"]], min=1)}
                 
                 #5C
                 if(rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PSSCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs, PCSs and PSSCs", "core proteins based on PSSCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]], inline = FALSE)}
                 
                 
                 rm(shiny_options_p)
               })
  
  
  #### Download data ----
  #Branch A --------
  output$Down_1A_single_faas <- downloadHandler(
    filename = "1A_Proteins_per_genome.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/01/01_out_faa.zip"), to = file)
    }
  )
  
  output$Down_2A_Table <- downloadHandler(
    filename = "2A_Genomes_proteins_PCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/02/02_04_genome_protDF_PCs.tsv"), to = file)
    }
  )
  
  output$Down_2A_all_faa <- downloadHandler(
    filename = "2A_All_proteins.faa",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/01/01_all_proteins.faa"), to = file)
    }
  )
  
  
  output$Down_3A_distMA <- downloadHandler(
    filename = "3A_Intergenome_distance_matrix.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/04/MyDistPCs_MA.tsv"), to = file)
    }
  )
  
  output$Down_3A_tree <- downloadHandler(
    filename = function(filename){
      if(input$boot_a == FALSE)
      {
        filename="3A_genome_tree.newick"
      }else
      {
        filename="3A_pv_trees.zip"
      }
    },
    content = function(file){
      if(input$boot_a == FALSE)
      {
        file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/04/hc_tree.newick"), to = file)
      }else
      {
        file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/04/pv_trees.zip"), to = file)
      }
    }
  )
  
  output$Down_3A_PDF <- downloadHandler(
    filename = "3A_DistanceHeatmap_PCs.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/04/Dist_heatmap_PC_all_genomes.PDF"), to = file)
    }
  )
  
  output$Down_4A_genPCs_Table <- downloadHandler(
    filename = "4A_Genome_vs_PCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/05/ord_hDF.tsv"), to = file)
    }
  )
  
  output$Down_4A_stats_Table <- downloadHandler(
    filename = "4A_genom_clusters_stats.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/05/virDF.tsv"), to = file)
    }
  )
  
  output$Down_4A_Plot <- downloadHandler(
    filename = "4A_clustering.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/06-Heatmap_PC.PDF"), to = file)
    }
  )
  
  output$Down_5A <- downloadHandler(
    filename = "5A_core_proteins_based_on_PCs.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/07/core_a.zip"), to = file)   ##this does not exist, I should mak it here or change the path to something else
    }
  )
  
  output$downInterPro_PC <- downloadHandler(
    filename =  "6A_interpro_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB.tsv"), to = file)   
    }
  )
  
  
  output$downpVOGs_PC <- downloadHandler(
    filename =  "6A_pVOGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB.tsv"), to = file)   
    }
  )
  
  
  output$downVOGDB_PC <- downloadHandler(
    filename =  "6A_VOGDB_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB.tsv"), to = file)   
    }
  )
  
  output$downPHROGS_PC <- downloadHandler(
    filename =  "6A_PHROGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB.tsv"), to = file)   
    }
  )
  
  output$downEfam_PC <- downloadHandler(
    filename =  "6A_Efam_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam.tsv"), to = file)   
    }
  )
  
  output$`downEfam-XC_PC`  <- downloadHandler(
    filename =  "6A_Efam-XC_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC.tsv"), to = file)   
    }
  )
  
  output$downNCBI_PC  <- downloadHandler(
    filename =  "6A_BLAST-NCBI-NR_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB.tsv"), to = file)   
    }
  )
  
  
  
  #Branch B ---------  
  output$Down_1B_Table <- downloadHandler(
    filename = "1B_Genomes_proteins_PCs_PSCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/03/03_09_genome_protDF_PCs_PSCs.tsv"), to = file)
    }
  )
  
  output$Down_1B_ALigned_PCs <- downloadHandler(
    filename = "1B_multiple_alignments_PCs.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/03/03_02_aligned_PCs.zip"), to = file)
    }
  )
  
  output$Down_2B_distMA <- downloadHandler(
    filename = "2B_Intergenome_distance_matrix.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/MyDistPCs_MA.tsv"), to = file)
    }
  )
  
  output$Down_2B_tree <- downloadHandler(
    
    filename = function(filename){
      if(input$boot_b == FALSE)
      {
        filename="2B_genome_tree.newick"
      }else
      {
        filename="2B_pv_trees.zip"
      }
    },
    content = function(file){
      if(input$boot_b == FALSE)
      {
        file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/hc_tree.newick"), to = file)
      }else
      {
        file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/pv_trees.zip"), to = file)
      }
    }
  )
  
  output$Down_2B_PDF <- downloadHandler(
    filename = "2B_DistanceHeatmap_PCs.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/Dist_heatmap_PSC_all_genomes.PDF"), to = file)
    }
  )
  
  output$Down_3B_genPSCs_Table <- downloadHandler(
    filename = "3B_Genome_vs_PCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/05/ord_hDF.tsv"), to = file)
    }
  )
  
  output$Down_3B_stats_Table <- downloadHandler(
    filename = "3B_cluster_stats.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/05/virDF.tsv"), to = file)
    }
  )
  
  output$Down_3B_Plot <- downloadHandler(
    filename = "3B_genome_clustering.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/06-Heatmap_PSC.PDF"), to = file)
    }
  )
  
  output$Down_4B <- downloadHandler(
    filename = "4B_core_proteins_based_on_PSCs.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/07/core_b.zip"), to = file) ##does not exist!!!!
    }
  )
  
  output$downInterPro_PSC <- downloadHandler(
    filename =  "5B_interpro_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB.tsv"), to = file)   
    }
  )
  
  
  output$downpVOGs_PSC <- downloadHandler(
    filename =  "5B_pVOGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB.tsv"), to = file)   
    }
  )
  
  
  output$downVOGDB_PSC <- downloadHandler(
    filename =  "5B_VOGDB_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB.tsv"), to = file)   
    }
  )
  
  output$downPHROGS_PSC <- downloadHandler(
    filename =  "5B_PHROGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB.tsv"), to = file)   
    }
  )
  
  output$downEfam_PSC <- downloadHandler(
    filename =  "5B_Efam_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam.tsv"), to = file)   
    }
  )
  
  output$`downEfam-XC_PSC`  <- downloadHandler(
    filename =  "5B_Efam-XC_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC.tsv"), to = file)   
    }
  )
  
  output$downNCBI_PSC  <- downloadHandler(
    filename =  "5B_BLAST-NCBI-NR_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB.tsv"), to = file)   
    }
  )
  
  #Branch C ---------  
  output$Down_1C_Table <- downloadHandler(
    filename = "1C_Genomes_proteins_PCs_PSCs_PSSCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/03C/03C_09_genome_protDF_PSCs_PSSCs.tsv"), to = file)
    }
  )
  
  output$Down_1C_ALigned_PCs <- downloadHandler(
    filename = "1C_multiple_alignments_PSCs.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/03C/03C_02_aligned_PSCs.zip"), to = file)
    }
  )
  
  output$Down_2C_distMA <- downloadHandler(
    filename = "2C_Intergenome_distance_matrix.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/MyDistPCs_MA.tsv"), to = file)
    }
  )
  
  output$Down_2C_tree <- downloadHandler(
    filename = function(filename){
      if(input$boot_a == FALSE)
      {
        filename="2C_genome_tree.newick"
      }else
      {
        filename="2C_pv_trees.zip"
      }
    },
    content = function(file){
      if(input$boot_a == FALSE)
      {
        file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/hc_tree.newick"), to = file)
      }else
      {
        file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/pv_trees.zip"), to = file)
      }
    }
  )
  
  output$Down_2C_PDF <- downloadHandler(
    filename = "2C_DistanceHeatmap_PCs.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/Dist_heatmap_PSSC_all_genomes.PDF"), to = file)
    }
  )
  
  output$Down_3C_genPSSCs_Table <- downloadHandler(
    filename = "3C_Genome_vs_PSSCs.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/05/ord_hDF.tsv"), to = file)
    }
  )
  
  output$Down_3C_stats_Table <- downloadHandler(
    filename = "3C_cluster_stats.tsv",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/05/virDF.tsv"), to = file)
    }
  )
  
  output$Down_3C_Plot <- downloadHandler(
    filename = "3C_genome_clustering.PDF",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/06-Heatmap_PSSC.PDF"), to = file)
    }
  )
  
  
  output$Down_4C <- downloadHandler(
    filename = "4C_core_proteins_based_on_PSSCs.zip",
    content = function(file){
      file.copy(from = paste0(rval_proj_name$data, "/07/core_c.zip"), to = file)
    }
  )
  
  output$downInterPro_PSSC <- downloadHandler(
    filename =  "5C_interpro_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB.tsv"), to = file)   
    }
  )
  
  
  output$downpVOGs_PSSC <- downloadHandler(
    filename =  "5C_pVOGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB.tsv"), to = file)   
    }
  )
  
  
  output$downVOGDB_PSSC <- downloadHandler(
    filename =  "5C_VOGDB_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB.tsv"), to = file)   
    }
  )
  
  output$downPHROGS_PSSC <- downloadHandler(
    filename =  "5C_PHROGS_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB.tsv"), to = file)   
    }
  )
  
  output$downEfam_PSSC <- downloadHandler(
    filename =  "5C_Efam_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam.tsv"), to = file)   
    }
  )
  
  output$`downEfam-XC_PSSC`  <- downloadHandler(
    filename =  "5C_Efam-XC_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC.tsv"), to = file)   
    }
  )
  
  output$downNCBI_PSSC  <- downloadHandler(
    filename =  "5C_BLAST-NCBI-NR_annots.tsv",
    content = function(file){
      
      if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
      {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
      
      file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB.tsv"), to = file)   
    }
  )
  
  
})


