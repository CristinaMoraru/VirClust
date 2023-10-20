# #### BRACH A - make buttons and options visible/invisible ----
#### CALCULATIONS ------

## Buttons -----------

#### Start 1A button ---------
if(rval_create_load$data == "no")
{
  disable("actGenom_2_Prot")
  disable("Genetic_code")
}else
{
  enable("actGenom_2_Prot")
  enable("Genetic_code")
}

output$done1A <- renderText(expr = {
  validate(need(rval_status_DF$data["1A","status"] == "done", ""))
  text <- "DONE"
  return(text)
})


if(rval_status_DF$data["1A", "status"] == "done")
{
  enable("Down_1A_single_faas")
  enable("Down_1A_all_faa")
  enable("Down_1A_Table")
}else
{
  disable("Down_1A_single_faas")
  disable("Down_1A_all_faa")
  disable("Down_1A_Table")
  rval_mes_1A$data <- ""
}

#### Start 2A button ----------
if(rval_status_DF$data["1A", "status"] == "not_run" | rval_status_DF$data["1A", "status"] == "running")
{
  disable("actProt_2_PCs")
  
  disable("clust_PC")
  disable("eval_PC")
  disable("bitsc_PC")
  disable("cov_PC")
  disable("pident_PC")
}else
{
  enable("actProt_2_PCs")
  
  enable("clust_PC")
  enable("eval_PC")
  enable("bitsc_PC")
  enable("cov_PC")
  enable("pident_PC")
  
  if(is.na(input$eval_PC) == FALSE && input$eval_PC <= 0)
  {
    updateNumericInput(session=session, "eval_PC", label = "e-value >", value = 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001, min = 0, max = 0.01, step = 0.00001)
  }
  
  if(is.na(input$eval_PC) == FALSE && input$eval_PC > 0.01)
  {
    updateNumericInput(session=session, "eval_PC", label = "e-value >", value = 0.01, min = 0, max = 0.01, step = 0.00001)
  }
  
  if(is.na(input$bitsc_PC)== FALSE && input$bitsc_PC < 20)
  {
    updateNumericInput(session=session, "bitsc_PC", label = "bitscore <", value = 20, min = 20, step=1)
  }
  
  if(is.na(input$cov_PC)== FALSE && input$cov_PC < 0)
  {
    updateNumericInput(session=session, "cov_PC", label = "coverage <", value = 0, min = 0, max = 100)
  }
  
  if(is.na(input$cov_PC)== FALSE && input$cov_PC > 100)
  {
    updateNumericInput(session=session, "cov_PC", label = "coverage <", value = 100, min = 0, max = 100)
  }
  
  if(is.na(input$pident_PC)== FALSE && input$pident_PC < 0)
  {
    updateNumericInput(session=session, "pident_PC", label = "% identity <", value = 0, min = 0, max = 100)
  }
  
  if(is.na(input$pident_PC)== FALSE && input$pident_PC > 100)
  {
    updateNumericInput(session=session, "pident_PC", label = "% identity <", value = 100, min = 0, max = 100)
  }
}

output$done2A <- renderText(expr = {
  validate(need(rval_status_DF$data["2A","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["2A", "status"] == "done")
{
  enable("Down_2A_Table")
}else
{
  disable("Down_2A_Table")
  rval_mes_2A$data <- ""
}


#### Start 3A button -----------
if(rval_status_DF$data["2A", "status"] == "not_run" | rval_status_DF$data["2A", "status"] == "running")
{
  disable("actCluster_genomes_PC")
  disable("aglom_a")
  disable("boot_a")
  
}else
{
  enable("actCluster_genomes_PC")
  enable("aglom_a")
  
  if(rval_genome_no$data < 1050)
  {
    enable("boot_a")
  }else
  {
    updateCheckboxInput(session = session, "boot_a", label = "Enable bootstrapping", value = FALSE)
    disable("boot_a")
  }
}

if(input$boot_a == TRUE)
{
  enable("boot_no_a")
  
  if(is.na(input$boot_no_a)==FALSE && input$boot_no_a < 2)
  {
    updateNumericInput(session=session, "boot_no_a", label = "Number of bootstraps", min = 2, max = 1000, value = 2, step = 1)
  }
  
  if(is.na(input$boot_no_a)==FALSE && input$boot_no_a > 1000)
  {
    updateNumericInput(session=session, "boot_no_a", label = "Number of bootstraps", min = 2, max = 1000, value = 1000, step = 1)
  }
  
}else
{
  disable("boot_no_a")
}


output$done3A <- renderText(expr = {
  validate(need(rval_status_DF$data["3A","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3A", "status"] == "done")
{
  enable("Down_3A_tree")
  enable("Down_3A_distMA")
}else
{
  disable("Down_3A_tree")
  disable("Down_3A_distMA")
  rval_mes_3A$data <- ""
}

### Start 3A_plot button ---------
if(rval_status_DF$data["3A", "status"] == "not_run" | rval_status_DF$data["3A", "status"] == "running")
{
  disable("inc_fact_w_Pd_A")
  disable("font_row_Pd_A")
  disable("font_col_Pd_A")
  disable("font_cell_Pd_A")
  disable("lgd_font_Pd_A")
  disable("lgd_lab_font_Pd_A")
  disable("lgd_height_Pd_A")
  disable("lgd_width_Pd_A")
  disable("actOut_pdf_3A")
}else
{
  enable("inc_fact_w_Pd_A")
  enable("font_row_Pd_A")
  enable("font_col_Pd_A")
  enable("font_cell_Pd_A")
  enable("lgd_font_Pd_A")
  enable("lgd_lab_font_Pd_A")
  enable("lgd_height_Pd_A")
  enable("lgd_width_Pd_A")
  enable("actOut_pdf_3A")
  
  if(is.na(input$inc_fact_w_Pd_A)==FALSE && input$inc_fact_w_Pd_A < 0.02)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_A", label = "Cell width", min = 0.02, max = 1, value = 0.02, step = 0.001)
  }
  
  if(is.na(input$inc_fact_w_Pd_A)==FALSE && input$inc_fact_w_Pd_A > 1)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_A", label = "Cell width", min = 0.005, max = 1, value = 1, step = 0.001)
  }
  
  
  if(is.na(input$font_row_Pd_A)==FALSE && input$font_row_Pd_A < 1)
  {
    updateNumericInput(session=session, "font_row_Pd_A", label = "Row font", value = 1, min=1)
  }
  
  if(is.na(input$font_col_Pd_A)==FALSE && input$font_col_Pd_A < 1)
  {
    updateNumericInput(session=session, "font_col_Pd_A", label = "Column font", value = 1, min=1)
  }
  
  if(is.na(input$font_cell_Pd_A)==FALSE && input$font_cell_Pd_A < 1)
  {
    updateNumericInput(session=session, "font_cell_Pd_A", label = "Cell font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_font_Pd_A)==FALSE && input$lgd_font_Pd_A < 1)
  {
    updateNumericInput(session=session, "lgd_font_Pd_A", label = "Legend font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_lab_font_Pd_A)==FALSE && input$lgd_lab_font_Pd_A < 1)
  {
    updateNumericInput(session=session, "lgd_lab_font_Pd_A", label = "Legend label font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_height_Pd_A)==FALSE && input$lgd_height_Pd_A < 1)
  {
    updateNumericInput(session=session, "lgd_height_Pd_A", label = "Legend heigth", value = 1, min=1)
  }
  
  if(is.na(input$lgd_width_Pd_A)==FALSE && input$lgd_width_Pd_A < 1)
  {
    updateNumericInput(session=session, "lgd_width_Pd_A", label = "Legend width", value = 1, min=1)
  }
}

output$done3A_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["3A_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3A_Plot", "status"] == "done")
{
  enable("Down_3A_PDF")
}else
{
  disable("Down_3A_PDF")
  rval_mes_3A_Plot$data <- ""
}


# Start 4A button -------------

if(rval_status_DF$data["3A", "status"] == "not_run" | rval_status_DF$data["3A", "status"] == "running")
{
  disable("actSplit_clust_PC")
  disable("Clust_dist_PC")
  disable("sel_PCs_heatmap_a")
}else
{
  enable("actSplit_clust_PC")
  enable("Clust_dist_PC") 
  enable("sel_PCs_heatmap_a")
  
  if(is.na(input$Clust_dist_PC)==FALSE && input$Clust_dist_PC < 0.1)
  {
    updateNumericInput(session=session, "Clust_dist_PC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.1, step = 0.01)
  }
  
  if(is.na(input$Clust_dist_PC)==FALSE && input$Clust_dist_PC > 2)
  {
    updateNumericInput(session=session, "Clust_dist_PC", label = "Clustering distance*", min = 0.1, max = 1, value = 1, step = 0.01)
  }
  
  if(is.na(input$sel_PCs_heatmap_a)==FALSE && input$sel_PCs_heatmap_a < 1)
  {
    updateNumericInput(session=session, "sel_PCs_heatmap_a", label = "Show only common PCs if >:", value = 1, min =1)
  }
}

output$done4A <- renderText(expr = {
  validate(need(rval_status_DF$data["4A","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["4A", "status"] == "done" & check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PC") == FALSE)
{
  enable("Down_4A_genPCs_Table")
  enable("Down_4A_stats_Table")
  enable("Down_4A_VGCs")
}else
{
  disable("Down_4A_genPCs_Table")
  disable("Down_4A_stats_Table")
  disable("Down_4A_VGCs")
  rval_mes_4A$data <- ""
}


# Start 4A_Plot button ------
disable_4A_Plot_fun <- function()
{
  disable("actOut_pdf_4A")
  
  updateCheckboxInput(session, inputId = "Show_Tree_a", label = "Show tree", value = TRUE)
  disable("Show_Tree_a")
  
  updateCheckboxInput(session, inputId = "show_clust_ID_a", label = "Show VGC ID", value = TRUE)
  disable("show_clust_ID_a")
  
  updateCheckboxInput(session, inputId = "show_sil_a", label = "Show silhoutte width", value = TRUE)
  disable("show_sil_a")
  
  updateCheckboxInput(session, inputId = "show_protein_stats_a", label = "Show protein stats", value = TRUE)
  disable("show_protein_stats_a")
  
  updateCheckboxInput(session, inputId = "show_heat_a", label = "Show heatmap", value = TRUE)
  disable("show_heat_a")
}

if(rval_status_DF$data["4A", "status"] == "not_run" | rval_status_DF$data["4A", "status"] == "running")
{
  disable_4A_Plot_fun()
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PC"))
  {
    disable_4A_Plot_fun()
  }else
  {
    enable("actOut_pdf_4A")
    
    enable("Show_Tree_a")
    enable("show_clust_ID_a")
    enable("show_protein_stats_a")
    enable("show_sil_a")
    enable("show_heat_a")
  }
}


#PDF options 4A_Plot
if(input$Show_Tree_a == FALSE)
{
  disable("tree_width_a")
}else
{
  enable("tree_width_a")
  
  if(is.na(input$tree_width_a)==FALSE && input$tree_width_a > 100)
  {
    updateNumericInput(session=session, "tree_width_a", label = "Tree width", min = 1, max = 100, value = 100)
  }
  
  if(is.na(input$tree_width_a)==FALSE && input$tree_width_a < 1)
  {
    updateNumericInput(session=session, "tree_width_a", label = "Tree width", min = 1, max = 100, value = 1)
  }
}

if(input$show_protein_stats_a == FALSE & input$show_sil_a == FALSE & input$show_clust_ID_a == FALSE)
{
  disable("stats_font_a")
}else
{
  enable("stats_font_a")
  
  if(is.na(input$stats_font_a)==FALSE && input$stats_font_a < 1)
  {updateNumericInput(session=session, "stats_font_a", label = "Font stats name", value = 1, min=1)}
}

if(input$show_clust_ID_a == FALSE)
{
  disable("clustID_width_a")
}else
{
  enable("clustID_width_a")
  if(is.na(input$clustID_width_a)==FALSE && input$clustID_width_a < 1)
  {updateNumericInput(session=session, "clustID_width_a", label = "Width of VGC ID column", value = 1, min=1)}
}

if(input$show_sil_a == FALSE)
{
  disable("Sil_stats_width_a")
}else
{
  enable("Sil_stats_width_a")
  
  if(is.na(input$Sil_stats_width_a)==FALSE && input$Sil_stats_width_a < 1)
  {updateNumericInput(session=session, "Sil_stats_width_a", label = "Width of silhoutte column", value = 1, min=1)}
}

if(input$show_protein_stats_a == FALSE)
{
  disable("Stats_width_a")
  disable("stats_lab_font_a")
}else
{
  enable("Stats_width_a")
  enable("stats_lab_font_a")
  
  if(is.na(input$Stats_width_a)==FALSE && input$Stats_width_a < 1)
  {updateNumericInput(session=session, "Stats_width_a", label = "Width of protein stats", min = 1, max = 100, value = 1)}
  
  if(is.na(input$Stats_width_a)==FALSE && input$Stats_width_a > 100)
  {updateNumericInput(session=session, "Stats_width_a", label = "Width of protein stats", min = 1, max = 100, value = 100)}
  
  if(is.na(input$stats_lab_font_a)==FALSE && input$stats_lab_font_a < 1)
  {updateNumericInput(session=session, "stats_lab_font_a", "Font stats axis", value = 1, min=1)}
}


if(input$show_heat_a == FALSE)
{
  disable("Heat_width_a")
  disable("font_col_a")
}else
{
  enable("Heat_width_a")
  enable("font_col_a")
  
  if(is.na(input$Heat_width_a)==FALSE && input$Heat_width_a < 0.005)
  {updateNumericInput(session=session, "Heat_width_a", label = "Column width (inches)", min = 0.005, max = 1, value = 0.005, step = 0.001)}
  
  if(is.na(input$Heat_width_a)==FALSE && input$Heat_width_a > 1)
  {updateNumericInput(session=session, "Heat_width_a", label = "Column width (inches)", min = 0.005, max = 1, value = 1, step = 0.001)}
  
  if(is.na(input$font_col_a)==FALSE && input$font_col_a < 1)
  {updateNumericInput(session=session, "font_col_a", label = "Font PC names", value = 1, min = 1)}
}

if(input$show_sil_a == FALSE & input$show_heat_a == FALSE)
{
  disable("lgd_w_a")
  disable("lgd_h_a")
  disable("lgd_font_a")
  disable("lgd_lab_font_a")
}else
{
  enable("lgd_w_a")
  enable("lgd_h_a")
  enable("lgd_font_a")
  enable("lgd_lab_font_a")
  
  if(is.na(input$lgd_font_a)==FALSE && input$lgd_font_a < 1)
  {updateNumericInput(session=session, "lgd_font_a", label = "Legend font", value = 1, min=1)}

  if(is.na(input$lgd_lab_font_a)==FALSE && input$lgd_lab_font_a < 1)
  {updateNumericInput(session=session, "lgd_lab_font_a", label = "Legend label font", value = 1, min = 1)}

  if(is.na(input$lgd_h_a)==FALSE && input$lgd_h_a < 1)
  {updateNumericInput(session=session, "lgd_h_a", label = "Legend heigth", value = 1, min = 1)}

  if(is.na(input$lgd_w_a)==FALSE && input$lgd_w_a < 1)
  {updateNumericInput(session=session, "lgd_w_a", label = "Legend width", value = 1, min=1)}
}


if(input$Show_Tree_a == FALSE & input$show_clust_ID_a == FALSE & input$show_protein_stats_a == FALSE & input$show_sil_a == FALSE & input$show_heat_a == FALSE)
{
  disable("font_row_a")
}else
{
  enable("font_row_a")
  if(is.na(input$font_row_a)==FALSE && input$font_row_a < 1)
  {updateNumericInput(session=session, "font_row_a", label = "Font genomes/VGCs IDs ", value = 1, min=1)}
}


#other 4A_Plot
output$done4A_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["4A_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["4A_Plot", "status"] == "done")
{
  enable("Down_4A_Plot")
}else
{
  disable("Down_4A_Plot")
  rval_mes_4A_Plot$data <- ""
}


# Start 5A button Core PCs ------

if(rval_status_DF$data["4A", "status"] == "not_run" | rval_status_DF$data["4A", "status"] == "running")
{
  disable("actCore_PCs")
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PC"))
  {
    disable("actCore_PCs")
  }else
  {
    enable("actCore_PCs")
  }
}

output$done5A <- renderText(expr = {
  validate(need(rval_status_DF$data["5A","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["5A", "status"] == "done")
{
  enable("Down_5A")
}else
{
  disable("Down_5A")
  rval_mes_5A$data <- ""
}


# Start 6A button ---------

if(rval_status_DF$data["2A", "status"] == "not_run" | rval_status_DF$data["2A", "status"] == "running")
{
  disable("Annot_prots_PCs")
  
  disable("actAnnotInterPro_PC")
  disable("actAnnotpVOGs_PC")
  disable("actAnnotVOGDB_PC")
  disable("actAnnotPHROGS_PC")
  disable("actAnnotEfam_PC")
  disable("actAnnotEfam-XC_PC")
  disable("actAnnotNCBI_PC")
  
}else
{
  
  if(rval_status_DF$data["5A", "status"] == "not_run" | rval_status_DF$data["5A", "status"] == "running")
  {
    updateRadioButtons(session, "Annot_prots_PCs", label = "Select proteins to annotate",
                       choices = c("all proteins and relate them to PCs", "core proteins based on PCs"),
                       selected = "all proteins and relate them to PCs", inline = TRUE)
    
    shinyjs::delay(100, disable("Annot_prots_PCs"))
    
  }else
  {
    file_size <- exits_core_fun(rval_proj_name$data, "a")
    
    if(file_size == 0)
    {
      disable("Annot_prots_PCs")
    }else
    {
      enable("Annot_prots_PCs")
    }
  }
  
  enable("actAnnotInterPro_PC")
  enable("actAnnotpVOGs_PC")
  enable("actAnnotVOGDB_PC")
  enable("actAnnotPHROGS_PC")
  enable("actAnnotEfam_PC")
  enable("actAnnotEfam-XC_PC")
  enable("actAnnotNCBI_PC")
}

##all_PCs --------
if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
{
  #the done messages -------
  output$done6AI <- renderText(expr = {
    if(rval_status_DF$data["6AI","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done6ApV <- renderText(expr = {
    if(rval_status_DF$data["6ApV","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AVO <- renderText(expr = {
    if(rval_status_DF$data["6AVO","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6APH <- renderText(expr = {
    if(rval_status_DF$data["6APH","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AE <- renderText(expr = {
    if(rval_status_DF$data["6AE","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AXC <- renderText(expr = {
    if(rval_status_DF$data["6AXC","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AN <- renderText(expr = {
    if(rval_status_DF$data["6AN","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons ------
  if(rval_status_DF$data["6AI", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PC")}else{disable("downInterPro_PC")}
  }else
  {
    disable("downInterPro_PC")
    rval_mes_6AI$data <- ""
  }
  
  if(rval_status_DF$data["6ApV", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PC")}else{disable("downpVOGs_PC")}
  }else
  {
    disable("downpVOGs_PC")
    rval_mes_6ApV$data <- ""
  }
  
  if(rval_status_DF$data["6AVO", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PC")}else{disable("downVOGDB_PC")}
  }else
  {
    disable("downVOGDB_PC")
    rval_mes_6AVO$data <- ""
  }
  
  if(rval_status_DF$data["6APH", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PC")}else{disable("downPHROGS_PC")}
  }else
  {
    disable("downPHROGS_PC")
    rval_mes_6APH$data <- ""
  }
  
  if(rval_status_DF$data["6AE", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PC")}else{disable("downEfam_PC")}
  }else
  {
    disable("downEfam_PC")
    rval_mes_6AE$data <- ""
  }
  
  if(rval_status_DF$data["6AXC", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PC")}else{disable("downEfam-XC_PC")}
  }else
  {
    disable("downEfam-XC_PC")
    rval_mes_6AXC$data <- ""
  }
  
  if(rval_status_DF$data["6AN", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PC")}else{disable("downNCBI_PC")}
  }else
  {
    disable("downNCBI_PC")
    rval_mes_6AN$data <- ""
  }
  
  ###other ----
  
  ###this is to activate the button for merging annots
  if((rval_status_DF$data["6AI", "status"] == "not_run" | rval_status_DF$data["6AI", "status"] == "running") &
     (rval_status_DF$data["6ApV", "status"] == "not_run" | rval_status_DF$data["6ApV", "status"] == "running") &
     (rval_status_DF$data["6AVO", "status"] == "not_run" | rval_status_DF$data["6AVO", "status"] == "running") &
     (rval_status_DF$data["6APH", "status"] == "not_run" | rval_status_DF$data["6APH", "status"] == "running") &
     (rval_status_DF$data["6AE", "status"] == "not_run" | rval_status_DF$data["6AE", "status"] == "running") &
     (rval_status_DF$data["6AXC", "status"] == "not_run" | rval_status_DF$data["6AXC", "status"] == "running") &
     (rval_status_DF$data["6AN", "status"] == "not_run" | rval_status_DF$data["6AN", "status"] == "running"))
  {
    disable("actMergeAnnot_PC")
    
  }else
  {
    enable("actMergeAnnot_PC")
  }
  
  output$done6AM <- renderText(expr = {
    validate(need(rval_status_DF$data["6AM","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["6AM", "status"] == "done")
  {
    enable("downMergedAnnots_PC")
  }else
  {
    disable("downMergedAnnots_PC")
    rval_mes_6AN$data <- ""
  }
  
}else
{
  ##core PCs --------
  #the done messages -----
  output$done6AI <- renderText(expr = {
    if(rval_status_DF$data["6AI_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done6ApV <- renderText(expr = {
    if(rval_status_DF$data["6ApV_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AVO <- renderText(expr = {
    if(rval_status_DF$data["6AVO_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6APH <- renderText(expr = {
    if(rval_status_DF$data["6APH_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AE <- renderText(expr = {
    if(rval_status_DF$data["6AE_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AXC <- renderText(expr = {
    if(rval_status_DF$data["6AXC_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done6AN <- renderText(expr = {
    if(rval_status_DF$data["6AN_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons -------
  if(rval_status_DF$data["6AI_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PC")}else{disable("downInterPro_PC")}
  }else
  {
    disable("downInterPro_PC")
    rval_mes_6AI_core$data <- ""
  }
  
  if(rval_status_DF$data["6ApV_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PC")}else{disable("downpVOGs_PC")}
  }else
  {
    disable("downpVOGs_PC")
    rval_mes_6ApV_core$data <- ""
  }
  
  if(rval_status_DF$data["6AVO_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PC")}else{disable("downVOGDB_PC")}
  }else
  {
    disable("downVOGDB_PC")
    rval_mes_6AVO_core$data <- ""
  }
  
  if(rval_status_DF$data["6APH_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PC")}else{disable("downPHROGS_PC")}
  }else
  {
    disable("downPHROGS_PC")
    rval_mes_6APH_core$data <- ""
  }
  
  if(rval_status_DF$data["6AE_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PC")}else{disable("downEfam_PC")}
  }else
  {
    disable("downEfam_PC")
    rval_mes_6AE_core$data <- ""
  }
  
  if(rval_status_DF$data["6AXC_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PC")}else{disable("downEfam-XC_PC")}
  }else
  {
    disable("downEfam-XC_PC")
    rval_mes_6AXC_core$data <- ""
  }
  
  if(rval_status_DF$data["6AN_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PC")}else{disable("downNCBI_PC")}
  }else
  {
    disable("downNCBI_PC")
    rval_mes_6AN_core$data <- ""
  }
  
  #other ----
  
  ###this is to activate the button for merging annots
  if((rval_status_DF$data["6AI_core", "status"] == "not_run" | rval_status_DF$data["6AI_core", "status"] == "running") &
     (rval_status_DF$data["6ApV_core", "status"] == "not_run" | rval_status_DF$data["6ApV_core", "status"] == "running") &
     (rval_status_DF$data["6AVO_core", "status"] == "not_run" | rval_status_DF$data["6AVO_core", "status"] == "running") &
     (rval_status_DF$data["6APH_core", "status"] == "not_run" | rval_status_DF$data["6APH_core", "status"] == "running") &
     (rval_status_DF$data["6AE_core", "status"] == "not_run" | rval_status_DF$data["6AE_core", "status"] == "running") &
     (rval_status_DF$data["6AXC_core", "status"] == "not_run" | rval_status_DF$data["6AXC_core", "status"] == "running") &
     (rval_status_DF$data["6AN_core", "status"] == "not_run" | rval_status_DF$data["6AN_core", "status"] == "running"))
  {
    disable("actMergeAnnot_PC")
    
  }else
  {
    enable("actMergeAnnot_PC")
  }
  
  output$done6AM <- renderText(expr = {
    validate(need(rval_status_DF$data["6AM_core","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["6AM_core", "status"] == "done")
  {
    enable("downMergedAnnots_PC")
  }else
  {
    disable("downMergedAnnots_PC")
    rval_mes_6AM_core$data <- ""
  }
  
}








