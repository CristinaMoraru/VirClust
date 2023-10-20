# #### BRACH B - make buttons and options visible/invisible ----

#### Start 1B button ----------
if(rval_status_DF$data["2A", "status"] == "not_run" | rval_status_DF$data["2A", "status"] == "running")
{
  disable("actPCs_2_PSCs")
  
  disable("clust_PSC")
  disable("prob1_PSC")
  disable("cov1_PSC")
  disable("prob2_PSC")
  disable("cov2_PSC")
  disable("alig_PSC")
}else
{
  enable("actPCs_2_PSCs")
  
  enable("clust_PSC")
  enable("prob1_PSC")
  enable("cov1_PSC")
  enable("prob2_PSC")
  enable("cov2_PSC")
  enable("alig_PSC")
  
  if(is.na(input$prob1_PSC)== FALSE && input$prob1_PSC < 1)
  {
    updateNumericInput(session=session, "prob1_PSC", label = "probability <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob1_PSC)== FALSE && input$prob1_PSC > 100)
  {
    updateNumericInput(session=session, "prob1_PSC", label = "probability <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov1_PSC)== FALSE && input$cov1_PSC < 1)
  {
    updateNumericInput(session=session, "cov1_PSC", label = "coverage <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov1_PSC)== FALSE && input$cov1_PSC > 100)
  {
    updateNumericInput(session=session, "cov1_PSC", label = "coverage <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob2_PSC)== FALSE && input$prob2_PSC < 1)
  {
    updateNumericInput(session=session, "prob2_PSC", label = "probability <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob2_PSC)== FALSE && input$prob2_PSC > 100)
  {
    updateNumericInput(session=session, "prob2_PSC", label = "probability <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov2_PSC)== FALSE && input$cov2_PSC < 1)
  {
    updateNumericInput(session=session, "cov2_PSC", label = "coverage <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov2_PSC)== FALSE && input$cov2_PSC > 100)
  {
    updateNumericInput(session=session, "cov2_PSC", label = "coverage <", value = 100, step = 1, min = 1, max = 100)
  }
  
  
  if(is.na(input$alig_PSC)== FALSE && input$alig_PSC < 1)
  {updateNumericInput(session=session, "alig_PSC", label = "alignment length >=", value = 1, step = 1, min = 1)}
}

output$done1B <- renderText(expr = {
  validate(need(rval_status_DF$data["1B","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["1B", "status"] == "done")
{
  enable("Down_1B_Table")
  enable("Down_1B_ALigned_PCs")
}else
{
  disable("Down_1B_Table")
  disable("Down_1B_ALigned_PCs")
  rval_mes_1B$data <- ""
}


#### Start 2B button -----------
if(rval_status_DF$data["1B", "status"] == "not_run" | rval_status_DF$data["1B", "status"] == "running")
{
  disable("actCluster_genomes_PSC")
  disable("aglom_b")
  disable("boot_b")
}else
{
  enable("actCluster_genomes_PSC")
  enable("aglom_b")
  
  if(rval_genome_no$data < 1050)
  {
    enable("boot_b")
  }else
  {
    updateCheckboxInput(session = session, "boot_a", label = "Enable bootstrapping", value = FALSE)
    disable("boot_b")
  }
}

if(input$boot_b == TRUE)
{
  enable("boot_no_b")
  
  if(is.na(input$boot_no_b)==FALSE && input$boot_no_b < 2)
  {
    updateNumericInput(session=session, "boot_no_b", label = "Number of bootstraps", min = 2, max = 1000, value = 2, step = 1)
  }
  
  if(is.na(input$boot_no_b)==FALSE && input$boot_no_b > 1000)
  {
    updateNumericInput(session=session, "boot_no_b", label = "Number of bootstraps", min = 2, max = 1000, value = 1000, step = 1)
  }
  
}else
{
  disable("boot_no_b")
}


output$done2B <- renderText(expr = {
  validate(need(rval_status_DF$data["2B","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["2B", "status"] == "done")
{
  enable("Down_2B_tree")
  enable("Down_2B_distMA")
}else
{
  disable("Down_2B_tree")
  disable("Down_2B_distMA")
  rval_mes_2B$data <- ""
}

### Start 2B_plot button ----
if(rval_status_DF$data["2B", "status"] == "not_run" | rval_status_DF$data["2B", "status"] == "running")
{
  disable("inc_fact_w_Pd_B")
  disable("font_row_Pd_B")
  disable("font_col_Pd_B")
  disable("font_cell_Pd_B")
  disable("lgd_font_Pd_B")
  disable("lgd_lab_font_Pd_B")
  disable("lgd_height_Pd_B")
  disable("lgd_width_Pd_B")
  disable("actOut_pdf_2B")
}else
{
  enable("inc_fact_w_Pd_B")
  enable("font_row_Pd_B")
  enable("font_col_Pd_B")
  enable("font_cell_Pd_B")
  enable("lgd_font_Pd_B")
  enable("lgd_lab_font_Pd_B")
  enable("lgd_height_Pd_B")
  enable("lgd_width_Pd_B")
  enable("actOut_pdf_2B")
  
  if(is.na(input$inc_fact_w_Pd_B)==FALSE && input$inc_fact_w_Pd_B < 0.02)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_B", label = "Cell width", min = 0.02, max = 1, value = 0.02, step = 0.001)
  }
  
  if(is.na(input$inc_fact_w_Pd_B)==FALSE && input$inc_fact_w_Pd_B > 1)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_B", label = "Cell width", min = 0.005, max = 1, value = 1, step = 0.001)
  }
  
  
  if(is.na(input$font_row_Pd_B)==FALSE && input$font_row_Pd_B < 1)
  {
    updateNumericInput(session=session, "font_row_Pd_B", label = "Row font", value = 1, min=1)
  }
  
  if(is.na(input$font_col_Pd_B)==FALSE && input$font_col_Pd_B < 1)
  {
    updateNumericInput(session=session, "font_col_Pd_B", label = "Column font", value = 1, min=1)
  }
  
  if(is.na(input$font_cell_Pd_B)==FALSE && input$font_cell_Pd_B < 1)
  {
    updateNumericInput(session=session, "font_cell_Pd_B", label = "Cell font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_font_Pd_B)==FALSE && input$lgd_font_Pd_B < 1)
  {
    updateNumericInput(session=session, "lgd_font_Pd_B", label = "Legend font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_lab_font_Pd_B)==FALSE && input$lgd_lab_font_Pd_B < 1)
  {
    updateNumericInput(session=session, "lgd_lab_font_Pd_B", label = "Legend label font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_height_Pd_B)==FALSE && input$lgd_height_Pd_B < 1)
  {
    updateNumericInput(session=session, "lgd_height_Pd_B", label = "Legend heigth", value = 1, min=1)
  }
  
  if(is.na(input$lgd_width_Pd_B)==FALSE && input$lgd_width_Pd_B < 1)
  {
    updateNumericInput(session=session, "lgd_width_Pd_B", label = "Legend width", value = 1, min=1)
  }
}

output$done2B_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["2B_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["2B_Plot", "status"] == "done")
{
  enable("Down_2B_PDF")
}else
{
  disable("Down_2B_PDF")
  rval_mes_2B_Plot$data <- ""
}

# Start 3B button -------------

if(rval_status_DF$data["2B", "status"] == "not_run" | rval_status_DF$data["2B", "status"] == "running")
{
  disable("actSplit_clust_PSC")
  disable("Clust_dist_PSC")
  disable("sel_PSCs_heatmap_b")
}else
{
  enable("actSplit_clust_PSC")
  enable("Clust_dist_PSC")
  enable("sel_PSCs_heatmap_b")
  
  if(is.na(input$Clust_dist_PSC)==FALSE && input$Clust_dist_PSC < 0.1)
  {
    updateNumericInput(session=session, "Clust_dist_PSC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.1, step = 0.01)
  }
  
  if(is.na(input$Clust_dist_PSC)==FALSE && input$Clust_dist_PSC > 2)
  {
    updateNumericInput(session=session, "Clust_dist_PSC", label = "Clustering distance*", min = 0.1, max = 1, value = 1, step = 0.01)
  }
  
  if(is.na(input$sel_PSCs_heatmap_b)==FALSE && input$sel_PSCs_heatmap_b < 1)
  {
    updateNumericInput(session=session, "sel_PSCs_heatmap_b", label = "Show only common PSCs if >:", value = 1, min =1)
  }
}

output$done3B <- renderText(expr = {
  validate(need(rval_status_DF$data["3B","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3B", "status"] == "done" & check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSC") == FALSE)
{
  enable("Down_3B_genPSCs_Table")
  enable("Down_3B_stats_Table")
  enable("Down_3B_VGCs")
}else
{
  disable("Down_3B_genPSCs_Table")
  disable("Down_3B_stats_Table")
  disable("Down_3B_VGCs")
  rval_mes_3B$data <- ""
}


# Start 3B_Plot button ------

disable_3B_Plot_fun <- function()
{
  disable("actOut_pdf_3B")
  
  updateCheckboxInput(session, inputId = "Show_Tree_b", label = "Show tree", value = TRUE)
  disable("Show_Tree_b")
  
  updateCheckboxInput(session, inputId = "show_clust_ID_b", label = "Show VGC ID", value = TRUE)
  disable("show_clust_ID_b")
  
  updateCheckboxInput(session, inputId = "show_sil_b", label = "Show silhoutte width", value = TRUE)
  disable("show_sil_b")
  
  updateCheckboxInput(session, inputId = "show_protein_stats_b", label = "Show protein stats", value = TRUE)
  disable("show_protein_stats_b")
  
  updateCheckboxInput(session, inputId = "show_heat_b", label = "Show heatmap", value = TRUE)
  disable("show_heat_b")
}

if(rval_status_DF$data["3B", "status"] == "not_run" | rval_status_DF$data["3B", "status"] == "running")
{
  disable_3B_Plot_fun()
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PSC"))
  {
    disable_3B_Plot_fun()
  }else
  {
    enable("actOut_pdf_3B")
    
    enable("Show_Tree_b")
    enable("show_clust_ID_b")
    enable("show_protein_stats_b")
    enable("show_sil_b")
    enable("show_heat_b")
  }
}


#PDF options 3B_Plot
if(input$Show_Tree_b == FALSE)
{
  disable("tree_width_b")
}else
{
  enable("tree_width_b")
  
  if(is.na(input$tree_width_b)==FALSE && input$tree_width_b > 100)
  {
    updateNumericInput(session=session, "tree_width_b", label = "Tree width", min = 1, max = 100, value = 100)
  }
  
  if(is.na(input$tree_width_b)==FALSE && input$tree_width_b < 1)
  {
    updateNumericInput(session=session, "tree_width_b", label = "Tree width", min = 1, max = 100, value = 1)
  }
}

if(input$show_protein_stats_b == FALSE & input$show_sil_b == FALSE & input$show_clust_ID_b == FALSE)
{
  disable("stats_font_b")
}else
{
  enable("stats_font_b")
  if(is.na(input$stats_font_b)==FALSE && input$stats_font_b < 1)
  {updateNumericInput(session=session, "stats_font_b", label = "Font stats name", value = 1, min=1)}
}

if(input$show_clust_ID_b == FALSE)
{
  disable("clustID_width_b")
}else{
  enable("clustID_width_b")
  if(is.na(input$clustID_width_b)==FALSE && input$clustID_width_b < 1)
  {updateNumericInput(session=session, "clustID_width_b", label = "Width of VGC ID column", value = 1, min=1)}
}

if(input$show_sil_b == FALSE)
{
  disable("Sil_stats_width_b")
}else
{
  enable("Sil_stats_width_b")
  if(is.na(input$Sil_stats_width_b)==FALSE && input$Sil_stats_width_b < 1)
  {updateNumericInput(session=session, "Sil_stats_width_b", label = "Width of silhoutte column", value = 1, min=1)}
}

if(input$show_protein_stats_b == FALSE)
{
  disable("Stats_width_b")
  disable("stats_lab_font_b")
}else
{
  enable("Stats_width_b")
  enable("stats_lab_font_b")
  
  if(is.na(input$Stats_width_b)==FALSE && input$Stats_width_b < 1)
  {updateNumericInput(session=session, "Stats_width_b", label = "Width of protein stats", min = 1, max = 100, value = 1)}
  
  if(is.na(input$Stats_width_b)==FALSE && input$Stats_width_b > 100)
  {updateNumericInput(session=session, "Stats_width_b", label = "Width of protein stats", min = 1, max = 100, value = 100)}
  
  if(is.na(input$stats_lab_font_b)==FALSE && input$stats_lab_font_b < 1)
  {updateNumericInput(session=session, "stats_lab_font_b", "Font stats axis", value = 1, min=1)}
}


if(input$show_heat_b == FALSE)
{
  disable("Heat_width_b")
  disable("font_col_b")
}else
{
  enable("Heat_width_b")
  enable("font_col_b")
  
  if(is.na(input$Heat_width_b)==FALSE && input$Heat_width_b < 0.005)
  {updateNumericInput(session=session, "Heat_width_b", label = "Column width (inches)", min = 0.005, max = 1, value = 0.005, step = 0.001)}
  
  if(is.na(input$Heat_width_b)==FALSE && input$Heat_width_b > 1)
  {updateNumericInput(session=session, "Heat_width_b", label = "Column width (inches)", min = 0.005, max = 1, value = 1, step = 0.001)}
  
  if(is.na(input$font_col_b)==FALSE && input$font_col_b < 1)
  {updateNumericInput(session=session, "font_col_b", label = "Font PC names", value = 1, min = 1)}
}

if(input$show_sil_b == FALSE & input$show_heat_b == FALSE)
{
  disable("lgd_w_b")
  disable("lgd_h_b")
  disable("lgd_font_b")
  disable("lgd_lab_font_b")
}else
{
  enable("lgd_w_b")
  enable("lgd_h_b")
  enable("lgd_font_b")
  enable("lgd_lab_font_b")
  
  if(is.na(input$lgd_font_b)==FALSE && input$lgd_font_b < 1)
  {updateNumericInput(session=session, "lgd_font_b", label = "Legend font", value = 1, min=1)}
  
  if(is.na(input$lgd_lab_font_b)==FALSE && input$lgd_lab_font_b < 1)
  {updateNumericInput(session=session, "lgd_lab_font_b", label = "Legend label font", value = 1, min = 1)}
  
  if(is.na(input$lgd_h_b)==FALSE && input$lgd_h_b < 1)
  {updateNumericInput(session=session, "lgd_h_b", label = "Legend heigth", value = 1, min = 1)}
  
  if(is.na(input$lgd_w_b)==FALSE && input$lgd_w_b < 1)
  {updateNumericInput(session=session, "lgd_w_b", label = "Legend width", value = 1, min=1)}
}


if(input$Show_Tree_b == FALSE & input$show_clust_ID_b == FALSE & input$show_protein_stats_b == FALSE & input$show_sil_b == FALSE & input$show_heat_b == FALSE)
{
  disable("font_row_b")
}else
{
  enable("font_row_b")
  if(is.na(input$font_row_b)==FALSE && input$font_row_b < 1)
  {updateNumericInput(session=session, "font_row_b", label = "Font genomes/VGCs IDs ", value = 1, min=1)}
}


#other 3B_Plot
output$done3B_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["3B_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3B_Plot", "status"] == "done")
{
  enable("Down_3B_Plot")
}else
{
  disable("Down_3B_Plot")
  rval_mes_3B_Plot$data <- ""
}


# Start 4B button Core PSCs ------

if(rval_status_DF$data["3B", "status"] == "not_run" | rval_status_DF$data["3B", "status"] == "running")
{
  disable("actCore_PSCs")
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PSC"))
  {
    disable("actCore_PSCs")
  }else
  {
    enable("actCore_PSCs")
  }
  
}

output$done4B <- renderText(expr = {
  validate(need(rval_status_DF$data["4B","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["4B", "status"] == "done")
{
  enable("Down_4B")
}else
{
  disable("Down_4B")
  rval_mes_4B$data <- ""
}


# Start 5B button Annots ---------

if(rval_status_DF$data["1B", "status"] == "not_run" | rval_status_DF$data["1B", "status"] == "running")
{
  disable("Annot_prots_PSCs")
  
  disable("actAnnotInterPro_PSC")
  disable("actAnnotpVOGs_PSC")
  disable("actAnnotVOGDB_PSC")
  disable("actAnnotPHROGS_PSC")
  disable("actAnnotEfam_PSC")
  disable("actAnnotEfam-XC_PSC")
  disable("actAnnotNCBI_PSC")
  
}else
{
  
  if(rval_status_DF$data["4B", "status"] == "not_run" | rval_status_DF$data["4B", "status"] == "running")
  {
    updateRadioButtons(session, "Annot_prots_PSCs", label = "Select proteins to annotate", 
                       choices = c("all proteins and relate them to PCs and PSCs", "core proteins based on PSCs"), 
                       selected = "all proteins and relate them to PCs and PSCs", inline = TRUE)
    shinyjs::delay(100, disable("Annot_prots_PSCs"))
  }else
  {
    file_size <- exits_core_fun(rval_proj_name$data, "b")
    
    if(file_size == 0)
    {
      disable("Annot_prots_PSCs")
    }else
    {
      enable("Annot_prots_PSCs")
    }
  }
  
  enable("actAnnotInterPro_PSC")
  enable("actAnnotpVOGs_PSC")
  enable("actAnnotVOGDB_PSC")
  enable("actAnnotPHROGS_PSC")
  enable("actAnnotEfam_PSC")
  enable("actAnnotEfam-XC_PSC")
  enable("actAnnotNCBI_PSC")
}

##all PSCs -------
if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
{
  #the done messages --------
  output$done5BI <- renderText(expr = {
    if(rval_status_DF$data["5BI","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done5BpV <- renderText(expr = {
    if(rval_status_DF$data["5BpV","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BVO <- renderText(expr = {
    if(rval_status_DF$data["5BVO","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BPH <- renderText(expr = {
    if(rval_status_DF$data["5BPH","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BE <- renderText(expr = {
    if(rval_status_DF$data["5BE","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BXC <- renderText(expr = {
    if(rval_status_DF$data["5BXC","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BN <- renderText(expr = {
    if(rval_status_DF$data["5BN","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons ----------
  if(rval_status_DF$data["5BI", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PSC")}else{disable("downInterPro_PSC")}
  }else
  {
    disable("downInterPro_PSC")
    rval_mes_5BI$data <- ""
  }
  
  if(rval_status_DF$data["5BpV", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PSC")}else{disable("downpVOGs_PSC")}
  }else
  {
    disable("downpVOGs_PSC")
    rval_mes_5BpV$data <- ""
  }
  
  if(rval_status_DF$data["5BVO", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PSC")}else{disable("downVOGDB_PSC")}
  }else
  {
    disable("downVOGDB_PSC")
    rval_mes_5BVO$data <- ""
  }
  
  if(rval_status_DF$data["5BPH", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PSC")}else{disable("downPHROGS_PSC")}
  }else
  {
    disable("downPHROGS_PSC")
    rval_mes_5BPH$data <- ""
  }
  
  if(rval_status_DF$data["5BE", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PSC")}else{disable("downEfam_PSC")}
  }else
  {
    disable("downEfam_PSC")
    rval_mes_5BE$data <- ""
  }
  
  if(rval_status_DF$data["5BXC", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PSC")}else{disable("downEfam-XC_PSC")}
  }else
  {
    disable("downEfam-XC_PSC")
    rval_mes_5BXC$data <- ""
  }
  
  if(rval_status_DF$data["5BN", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PSC")}else{disable("downNCBI_PSC")}
  }else
  {
    disable("downNCBI_PSC")
    rval_mes_5BN$data <- ""
  }
  
  #others ------
  
  ###this is to activate the button for merging annots
  if(rval_status_DF$data["6AI", "status"] == "done" | rval_status_DF$data["6ApV", "status"] == "done" |
     rval_status_DF$data["6AVO", "status"] == "done" | rval_status_DF$data["6APH", "status"] == "done" |
     rval_status_DF$data["6AE", "status"] == "done" | rval_status_DF$data["6AXC", "status"] == "done" |
     rval_status_DF$data["6AN", "status"] == "done")
  {
    if(rval_status_DF$data["1B", "status"] == "done")
    {
      enable("actMergeAnnot_PSC")
    }else
    {
      disable("actMergeAnnot_PSC")
    }
  }else
  {
    if(rval_status_DF$data["1B", "status"] == "done")
    {
      if(rval_status_DF$data["5BI", "status"] == "done" | rval_status_DF$data["5BpV", "status"] == "done" |
         rval_status_DF$data["5BVO", "status"] == "done" | rval_status_DF$data["5BPH", "status"] == "done" |
         rval_status_DF$data["5BE", "status"] == "done" | rval_status_DF$data["5BXC", "status"] == "done" |
         rval_status_DF$data["5BN", "status"] == "done")
      {
        enable("actMergeAnnot_PSC")
      }else
      {
        disable("actMergeAnnot_PSC")
      }
    }else
    {
      disable("actMergeAnnot_PSC")
    }
  }
  
  
  
  output$done5BM <- renderText(expr = {
    validate(need(rval_status_DF$data["5BM","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["5BM", "status"] == "done")
  {
    enable("downMergedAnnots_PSC")
  }else
  {
    disable("downMergedAnnots_PSC")
    rval_mes_5BM$data <- ""
  }
}else
{
  #core PSCs ---------
  #the output messages ------
  output$done5BI <- renderText(expr = {
    if(rval_status_DF$data["5BI_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done5BpV <- renderText(expr = {
    if(rval_status_DF$data["5BpV_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BVO <- renderText(expr = {
    if(rval_status_DF$data["5BVO_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BPH <- renderText(expr = {
    if(rval_status_DF$data["5BPH_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BE <- renderText(expr = {
    if(rval_status_DF$data["5BE_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BXC <- renderText(expr = {
    if(rval_status_DF$data["5BXC_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5BN <- renderText(expr = {
    if(rval_status_DF$data["5BN_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons ------------
  if(rval_status_DF$data["5BI_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PSC")}else{disable("downInterPro_PSC")}
  }else
  {
    disable("downInterPro_PSC")
    rval_mes_5BI_core$data <- ""
  }
  
  if(rval_status_DF$data["5BpV_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PSC")}else{disable("downpVOGs_PSC")}
  }else
  {
    disable("downpVOGs_PSC")
    rval_mes_5BpV_core$data <- ""
  }
  
  if(rval_status_DF$data["5BVO_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PSC")}else{disable("downVOGDB_PSC")}
  }else
  {
    disable("downVOGDB_PSC")
    rval_mes_5BVO_core$data <- ""
  }
  
  if(rval_status_DF$data["5BPH_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PSC")}else{disable("downPHROGS_PSC")}
  }else
  {
    disable("downPHROGS_PSC")
    rval_mes_5BPH_core$data <- ""
  }
  
  if(rval_status_DF$data["5BE_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PSC")}else{disable("downEfam_PSC")}
  }else
  {
    disable("downEfam_PSC")
    rval_mes_5BE_core$data <- ""
  }
  
  if(rval_status_DF$data["5BXC_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PSC")}else{disable("downEfam-XC_PSC")}
  }else
  {
    disable("downEfam-XC_PSC")
    rval_mes_5BXC_core$data <- ""
  }
  
  if(rval_status_DF$data["5BN_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PSC")}else{disable("downNCBI_PSC")}
  }else
  {
    disable("downNCBI_PSC")
    rval_mes_5BN_core$data <- ""
  }
  
  #other -----
  ###this is to activate the button for merging annots
  if((rval_status_DF$data["5BI_core", "status"] == "not_run" | rval_status_DF$data["5BI_core", "status"] == "running") &
     (rval_status_DF$data["5BpV_core", "status"] == "not_run" | rval_status_DF$data["5BpV_core", "status"] == "running") &
     (rval_status_DF$data["5BVO_core", "status"] == "not_run" | rval_status_DF$data["5BVO_core", "status"] == "running") &
     (rval_status_DF$data["5BPH_core", "status"] == "not_run" | rval_status_DF$data["5BPH_core", "status"] == "running") &
     (rval_status_DF$data["5BE_core", "status"] == "not_run" | rval_status_DF$data["5BE_core", "status"] == "running") &
     (rval_status_DF$data["5BXC_core", "status"] == "not_run" | rval_status_DF$data["5BXC_core", "status"] == "running") &
     (rval_status_DF$data["5BN_core", "status"] == "not_run" | rval_status_DF$data["5BN_core", "status"] == "running"))
  {
    disable("actMergeAnnot_PSC")
    
  }else
  {
    enable("actMergeAnnot_PSC")
  }
  
  output$done5BM <- renderText(expr = {
    validate(need(rval_status_DF$data["5BM_core","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["5BM_core", "status"] == "done")
  {
    enable("downMergedAnnots_PSC")
  }else
  {
    disable("downMergedAnnots_PSC")
    rval_mes_5BM_core$data <- ""
  }
}









