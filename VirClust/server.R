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
#dir.create(data_VirClust_path)
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
    
    
    # if(input$want_email2 == TRUE)
    # {
    #   enable("email_2")
    # }else
    # {
      disable("email_2")
    #}
    disable("want_email2")
    
    #   #### CALCULATIONS ------
    
    source(file = "server_BrA_obs.R", local = TRUE)
    source(file = "server_BrB_obs.R", local = TRUE)
    source(file = "server_BrC_obs.R", local = TRUE)
  })
  
  
  ##### NEW PROJECT - CREATE -------------------
  ##### Reset outputs at create -----
  #load upload state
  observeEvent(input$Upload_genome,
               {rval_upload$data <- "uploaded"})
  
  observeEvent(input$Create, {
    rval_upload$data <- "NULL"
    rval_proj_name$data <- NULL
    rval_status_mes$data <- "NULL"
    rval_status_DF$data <- make_status_df_fun("empty")
    rval_cont$data <- "no"
    rval_create_load$data <- "yes"
    rval_shiny_opt_TB <- reactiveValues(data = shiny_opt_TB_fun())
    rval_genome_no <- reactiveValues(data = 0)
    rval_valid_fasta <- reactiveValues(data = "NULL")
  })
  

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
                                    
                                    if(input$in_type == "Nucleic acids, one genome per file**")
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
                              
                              log_name <- paste0(dir_path, "/log.txt")
                              write(x = "created", file = log_name)
                              
                              status_p <- paste0(dir_path, "/status.txt")
                              write.table(x = rval_status_DF$data, file = status_p, append = FALSE, sep = "\t", row.names = TRUE, col.names = TRUE)
                              
                              if(input$in_type == "Nucleic acids, all genomes in a fasta file")
                              {
                                in_d <- "/00/00_in"
                              }
                              if(input$in_type == "Nucleic acids, one genome per file**")
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
                              if(input$in_type == "Nucleic acids, one genome per file**")
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
  
  source(file = "server_BrA_run.R", local = TRUE)
  source(file = "server_BrB_run.R", local = TRUE)
  source(file = "server_BrC_run.R", local = TRUE)


  
  
  #### Load project after load ----
  observeEvent(input$Load,
               {
                 ###this code is to reset all the states if a previous project was ran in the webapp
                 rval_upload$data <- "NULL"
                 rval_proj_name$data <- NULL
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
    
    if(rval_status_mes$data == "created")
    {
      text <- paste0("The project was ", rval_status_mes$data, ", no calculations were performed yet.")
    }
    
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
      text <- paste0("The calculations have ", rval_status_mes$data, ".")
    }
    
    return(text)
  })
  
  observeEvent(input$Load,
               {
                 shiny_options_p <- paste0(rval_proj_name$data, "/shiny_options.RDS")
                 validate(need(file.exists(shiny_options_p) == TRUE, ""))
                 rval_shiny_opt_TB$data <- readRDS(shiny_options_p)
                 
                 #shiny_options_TB <<- rval_shiny_opt_TB$data
                 
                 #Branch A 
                 #1A
                 if(is.na(rval_shiny_opt_TB$data[["Genetic_code", "value"]])==FALSE && rval_shiny_opt_TB$data[["Genetic_code", "value"]] != "NULL")
                 {updateSelectInput(session = session, "Genetic_code", label = "Translation table",
                                    choices = c("1 --- standard",
                                                "2 --- vertebrate mitochondrial",
                                                "3 --- yeast mitochondrial",
                                                "4 --- protozoan mitochondrial and mycoplasma",
                                                "5 --- invertebrate mitochondrial",
                                                "6 --- ciliate and dasycladaceal",
                                                "9 --- echinoderm and flatworm mitochondrial",
                                                "10 --- euplotid",
                                                "11 --- bacteria, archaea, prokaryotic viruses and plant plastid",
                                                "12 --- alternative yeast",
                                                "13 --- ascidian mitochondrial",
                                                "14 --- alternative flatworm mitochondrial",
                                                "15 --- blepharism",
                                                "16 --- chlorophycean mitochondrial",
                                                "21 --- trematode mitochondrial",
                                                "22 --- scenedesmus mitochondrial",
                                                "23 --- thraustochytrium mitochondrial",
                                                "24 --- Pterobranchia mitochondrial",
                                                "25 --- Candidate Division SR1 and Gracilibacteria",
                                                "26 --- Pachysolen tannophilus"),
                                    selected = rval_shiny_opt_TB$data[["Genetic_code", "value"]])}
                 
                 #2A
                 if(is.na(rval_shiny_opt_TB$data[["clust_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["clust_PC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PC", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["eval_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["eval_PC", "value"]] != "NULL")
                 {updateNumericInput(session =session, "eval_PC", label = "e-value >", value = rval_shiny_opt_TB$data[["eval_PC", "value"]], 
                                     min = 0, max = 0.01, step = 0.00001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["bitsc_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["bitsc_PC", "value"]] != "NULL")
                 {updateNumericInput(session =session, "bitsc_PC", label = "bitscore <", value = rval_shiny_opt_TB$data[["bitsc_PC", "value"]], 
                                     min = 20, step=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["cov_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["cov_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov_PC", label = "coverage <", value = rval_shiny_opt_TB$data[["cov_PC", "value"]], 
                                     min = 0, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["pident_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["pident_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "pident_PC", label = "% identity <", value = rval_shiny_opt_TB$data[["pident_PC", "value"]], 
                                     min = 0, max = 100)}
                 
                 #3A
                 if(is.na(rval_shiny_opt_TB$data[["aglom_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["aglom_a", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_a", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_a", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_a", label = "Enable bootstrapping", value = rval_shiny_opt_TB$data[["boot_a", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_no_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_no_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_a", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_a", "value"]], step = 1)}
                 
                 #3A_Plot
                 if(is.na(rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_A", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_A", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_A", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_A", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_A", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_A", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_A", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_A", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_A", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_A", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_A", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_A", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_A", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_A", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_A", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_A", "value"]], min=1)}
                 
                 #4A
                 if(is.na(rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]])==FALSE && rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PC", "value"]], step = 0.01)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PCs_heatmap_a", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PCs_heatmap_a", "value"]], min =1)}
                 
                 #4A_Plot
                 if(is.na(rval_shiny_opt_TB$data[["Show_Tree_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["Show_Tree_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_a", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_a", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["tree_width_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["tree_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_a", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_a", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_a", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_a", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_sil_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_sil_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_a", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_a", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_a", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_a", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["clustID_width_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["clustID_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_a", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_a", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Stats_width_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["Stats_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_a", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_a", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_font_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_a", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_a", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_heat_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_heat_a", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_a", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_a", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Heat_width_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["Heat_width_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_a", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_a", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_a", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_a", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_a", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_a", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_a", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_a", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_a", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_h_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_h_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_a", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_a", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_w_a", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_w_a", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_a", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_a", "value"]], min=1)}
                 
                 #6A
                 if(is.na(rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]])==FALSE && rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs", "core proteins based on PCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PCs", "value"]], inline = TRUE)}
                 
                 
                 
                 #1B
                 if(is.na(rval_shiny_opt_TB$data[["clust_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["clust_PSC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PSC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PSC", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["prob1_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["prob1_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob1_PSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob1_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["cov1_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["cov1_PSC", "value"]] != "NULL")
                 {updateNumericInput(session=session, "cov1_PSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov1_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["prob2_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["prob2_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob2_PSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob2_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["cov2_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["cov2_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov2_PSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov2_PSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["alig_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["alig_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "alig_PSC", label = "alignment length <", 
                                     value =  rval_shiny_opt_TB$data[["alig_PSC", "value"]], step = 1, min = 1)}
                 
                 #2B
                 if(is.na(rval_shiny_opt_TB$data[["aglom_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["aglom_b", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_b", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_b", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_b", label = "Enable bootstrapping", 
                                      value = rval_shiny_opt_TB$data[["boot_b", "value"]]%>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_no_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_no_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_b", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_b", "value"]], step = 1)}
                 
                 #2B_Plot
                 if(is.na(rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_B", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_B", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_B", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_B", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_B", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_B", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_B", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_B", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_B", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_B", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_B", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_B", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_B", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_B", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_B", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_B", "value"]], min=1)}
                 
                 #3B
                 if(is.na(rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PSC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PSC", "value"]], step = 0.01)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PSCs_heatmap_b", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PSCs_heatmap_b", "value"]], min =1)}
                 
                 #3B_Plot
                 if(is.na(rval_shiny_opt_TB$data[["Show_Tree_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["Show_Tree_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_b", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_b", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["tree_width_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["tree_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_b", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_b", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_b", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_b", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_sil_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_sil_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_b", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_b", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_b", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_b", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["clustID_width_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["clustID_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_b", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_b", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Stats_width_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["Stats_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_b", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_b", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_font_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_b", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_b", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_heat_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_heat_b", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_b", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_b", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Heat_width_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["Heat_width_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_b", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_b", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_b", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_b", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_b", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_b", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_b", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_b", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_b", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_h_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_h_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_b", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_b", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_w_b", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_w_b", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_b", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_b", "value"]], min=1)}
                 
                 #5B
                 if(is.na(rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]])==FALSE && rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PSCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs and PSCs", "core proteins based on PSCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PSCs", "value"]], inline = TRUE)}
                 
                 
                 #1C
                 if(is.na(rval_shiny_opt_TB$data[["clust_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["clust_PSSC", "value"]] != "NULL")
                 {updateSelectInput(session = session, "clust_PSSC", label = "Cluster based on", 
                                    choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"), 
                                    selected = rval_shiny_opt_TB$data[["clust_PSSC", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["prob1_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["prob1_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob1_PSSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob1_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["cov1_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["cov1_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session=session, "cov1_PSSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov1_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["prob2_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["prob2_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "prob2_PSSC", label = "probability <", 
                                     value =  rval_shiny_opt_TB$data[["prob2_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["cov2_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["cov2_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "cov2_PSSC", label = "coverage <", 
                                     value =  rval_shiny_opt_TB$data[["cov2_PSSC", "value"]], step = 1, min = 1, max = 100)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["alig_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["alig_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "alig_PSSC", label = "alignment length <", 
                                     value =  rval_shiny_opt_TB$data[["alig_PSSC", "value"]], step = 1, min = 1)}
                 
                 #2C
                 if(is.na(rval_shiny_opt_TB$data[["aglom_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["aglom_c", "value"]] != "NULL")
                 {updateSelectInput(session = session, "aglom_c", label = "Aglomeration method", choices = c("complete", "average"), 
                                    selected = rval_shiny_opt_TB$data[["aglom_c", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "boot_c", label = "Enable bootstrapping", 
                                      value = rval_shiny_opt_TB$data[["boot_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["boot_no_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["boot_no_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "boot_no_c", label = "Number of bootstraps", min = 2, max = 1000, 
                                     value = rval_shiny_opt_TB$data[["boot_no_c", "value"]], step = 1)}
                 
                 #2C_Plot
                 if(is.na(rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "inc_fact_w_Pd_C", label = "Cell width", min = 0.005, max = 1,
                                     value = rval_shiny_opt_TB$data[["inc_fact_w_Pd_C", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_Pd_C", label = "Row font",
                                     value = rval_shiny_opt_TB$data[["font_row_Pd_C", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_Pd_C", label = "column font",
                                     value = rval_shiny_opt_TB$data[["font_col_Pd_C", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_cell_Pd_C", label = "Cell font",
                                     value = rval_shiny_opt_TB$data[["font_cell_Pd_C", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_Pd_C", label = "Legend font",
                                     value = rval_shiny_opt_TB$data[["lgd_font_Pd_C", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_Pd_C", label = "Legend label font",
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_Pd_C", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_height_Pd_C", label = "Legend heigth",
                                     value = rval_shiny_opt_TB$data[["lgd_height_Pd_C", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_width_Pd_C", label = "Legend width",
                                     value = rval_shiny_opt_TB$data[["lgd_width_Pd_C", "value"]], min=1)}
                 
                 #3C
                 if(is.na(rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]])==FALSE && rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Clust_dist_PSSC", label = "Clustering distance*", min = 0.1, max = 1, 
                                     value =  rval_shiny_opt_TB$data[["Clust_dist_PSSC", "value"]], step = 0.01)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "sel_PSSCs_heatmap_c", label = "Show only common PCs if >:",
                                     value =  rval_shiny_opt_TB$data[["sel_PSSCs_heatmap_c", "value"]], min =1)}
                 
                 #3C_Plot
                 if(is.na(rval_shiny_opt_TB$data[["Show_Tree_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["Show_Tree_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "Show_Tree_c", label = "Show tree", 
                                      value = rval_shiny_opt_TB$data[["Show_Tree_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["tree_width_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["tree_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "tree_width_c", label = "Tree width", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["tree_width_c", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_clust_ID_c", label = "Show VGC ID", 
                                      value = rval_shiny_opt_TB$data[["show_clust_ID_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_sil_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_sil_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_sil_c", label = "Show silhoutte width", 
                                      value = rval_shiny_opt_TB$data[["show_sil_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_protein_stats_c", label = "Show protein stats", 
                                      value = rval_shiny_opt_TB$data[["show_protein_stats_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["clustID_width_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["clustID_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "clustID_width_c", label = "Width of VGC ID column", value = 
                                       rval_shiny_opt_TB$data[["clustID_width_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]] != "NULL")
                 { updateNumericInput(session = session, "Sil_stats_width_c", label = "Width of silhoutte column", 
                                      value = rval_shiny_opt_TB$data[["Sil_stats_width_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Stats_width_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["Stats_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Stats_width_c", label = "Width of protein stats", min = 1, max = 100, 
                                     value = rval_shiny_opt_TB$data[["Stats_width_c", "value"]])}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_font_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_font_c", label = "Font stat name", 
                                     value = rval_shiny_opt_TB$data[["stats_font_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "stats_lab_font_c", "Fonts protein stats axis", 
                                     value = rval_shiny_opt_TB$data[["stats_lab_font_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["show_heat_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["show_heat_c", "value"]] != "NULL")
                 {updateCheckboxInput(session = session, "show_heat_c", label = "Show heatmap", 
                                      value = rval_shiny_opt_TB$data[["show_heat_c", "value"]] %>% as.logical)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["Heat_width_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["Heat_width_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "Heat_width_c", label = "Column width (inches)", min = 0.005, max = 1, 
                                     value = rval_shiny_opt_TB$data[["Heat_width_c", "value"]], step = 0.001)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_col_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_col_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_col_c", label = "Font P(S)Cs names", 
                                     value = rval_shiny_opt_TB$data[["font_col_c", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["font_row_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["font_row_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "font_row_c", label = "Font genomes/VGCs IDs ", 
                                     value = rval_shiny_opt_TB$data[["font_row_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_font_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_font_c", label = "Legend font", 
                                     value = rval_shiny_opt_TB$data[["lgd_font_c", "value"]], min=1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_lab_font_c", label = "Legend label font", 
                                     value = rval_shiny_opt_TB$data[["lgd_lab_font_c", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_h_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_h_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_h_c", label = "Legend heigth", 
                                     value = rval_shiny_opt_TB$data[["lgd_h_c", "value"]], min = 1)}
                 
                 if(is.na(rval_shiny_opt_TB$data[["lgd_w_c", "value"]])==FALSE && rval_shiny_opt_TB$data[["lgd_w_c", "value"]] != "NULL")
                 {updateNumericInput(session = session, "lgd_w_c", label = "Legend width", 
                                     value = rval_shiny_opt_TB$data[["lgd_w_c", "value"]], min=1)}
                 
                 #5C
                 if(is.na(rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]])==FALSE && rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]] != "NULL")
                 {updateRadioButtons(session = session, "Annot_prots_PSSCs", label = "of:",
                                     choices = c("all proteins and relate them to PCs, PCSs and PSSCs", "core proteins based on PSSCs"),
                                     selected = rval_shiny_opt_TB$data[["Annot_prots_PSSCs", "value"]], inline = TRUE)}
                 
                 
                 rm(shiny_options_p)
               })
  
  
  #### Download data ----

  source(file = "server_BrA_down.R", local = TRUE)
  source(file = "server_BrB_down.R", local = TRUE)
  source(file = "server_BrC_down.R", local = TRUE)
  
  output$Down_manual_web <- downloadHandler(
    filename = "VirClust-v2_manual_web-server.pdf",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/manuals/VirClust-v2_manual_web-server.pdf", to = file)
    }
  )
  
  output$Down_manual_stand <- downloadHandler(
    filename = "VirClust-v2_manual_standalone.pdf",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/manuals/VirClust-v2_manual_standalone.pdf", to = file)
    }
  )
  
  output$Down_standalone <- downloadHandler(
    filename = "VirClust-v2_stand-alone.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/virclustv2_singularity.tar.gz", to = file)
    }
  )
  
  output$Down_Efam <- downloadHandler(
    filename = "Efam.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/dbs/Efam.tar.gz", to = file)
    }
  )
  
  output$Down_Efam_XC <- downloadHandler(
    filename = "Efam_XC.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/manuals/Efam_XC.tar.gz", to = file)
    }
  )
  
  output$Down_PHROG <- downloadHandler(
    filename = "PHROGS.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/dbs/PHROGS.tar.gz", to = file)
    }
  )
  
  output$Down_VOG_DB <- downloadHandler(
    filename = "VOGDB.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/manuals/VOGDB.tar.gz", to = file)
    }
  )
  
  output$Down_pVOGs <- downloadHandler(
    filename = "pVOGs.tar.gz",
    content = function(file){
      file.copy(from = "/bioinf/shiny-server/VirClust/vir_clust_standalone/manuals/pVOGs.tar.gz", to = file)
    }
  )
  
  
})


