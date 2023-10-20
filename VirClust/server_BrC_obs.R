# #### BRACH B - make buttons and options visible/invisible ----

#### Start 1C button ----------
if(rval_status_DF$data["1B", "status"] == "not_run" | rval_status_DF$data["1B", "status"] == "running")
{
  disable("actPSCs_2_PSSCs")
  
  disable("clust_PSSC")
  disable("prob1_PSSC")
  disable("cov1_PSSC")
  disable("prob2_PSSC")
  disable("cov2_PSSC")
  disable("alig_PSSC")
}else
{
  enable("actPSCs_2_PSSCs")
  
  enable("clust_PSSC")
  enable("prob1_PSSC")
  enable("cov1_PSSC")
  enable("prob2_PSSC")
  enable("cov2_PSSC")
  enable("alig_PSSC")
  
  if(is.na(input$prob1_PSSC)== FALSE && input$prob1_PSSC < 1)
  {
    updateNumericInput(session=session, "prob1_PSSC", label = "probability <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob1_PSSC)== FALSE && input$prob1_PSSC > 100)
  {
    updateNumericInput(session=session, "prob1_PSSC", label = "probability <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov1_PSSC)== FALSE && input$cov1_PSSC < 1)
  {
    updateNumericInput(session=session, "cov1_PSSC", label = "coverage <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov1_PSSC)== FALSE && input$cov1_PSSC > 100)
  {
    updateNumericInput(session=session, "cov1_PSSC", label = "coverage <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob2_PSSC)== FALSE && input$prob2_PSSC < 1)
  {
    updateNumericInput(session=session, "prob2_PSSC", label = "probability <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$prob2_PSSC)== FALSE && input$prob2_PSSC > 100)
  {
    updateNumericInput(session=session, "prob2_PSSC", label = "probability <", value = 100, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov2_PSSC)== FALSE && input$cov2_PSSC < 1)
  {
    updateNumericInput(session=session, "cov2_PSSC", label = "coverage <", value = 1, step = 1, min = 1, max = 100)
  }
  
  if(is.na(input$cov2_PSSC)== FALSE && input$cov2_PSSC > 100)
  {
    updateNumericInput(session=session, "cov2_PSSC", label = "coverage <", value = 100, step = 1, min = 1, max = 100)
  }
  
  
  if(is.na(input$alig_PSSC)== FALSE && input$alig_PSSC < 1)
  {updateNumericInput(session=session, "alig_PSSC", label = "alignment length >=", value = 1, step = 1, min = 1)}
}

output$done1C <- renderText(expr = {
  validate(need(rval_status_DF$data["1C","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["1C", "status"] == "done")
{
  enable("Down_1C_Table")
  enable("Down_1C_ALigned_PCs")
}else
{
  disable("Down_1C_Table")
  disable("Down_1C_ALigned_PCs")
  rval_mes_1C$data <- ""
}


#### Start 2C button -----------
if(rval_status_DF$data["1C", "status"] == "not_run" | rval_status_DF$data["1C", "status"] == "running")
{
  disable("actCluster_genomes_PSSC")
  disable("aglom_c")
  disable("boot_c")
}else
{
  enable("actCluster_genomes_PSSC")
  enable("aglom_c")
  
  if(rval_genome_no$data < 1050)
  {
    enable("boot_c")
  }else
  {
    updateCheckboxInput(session = session, "boot_a", label = "Enable bootstrapping", value = FALSE)
    disable("boot_c")
  }
}

if(input$boot_c == TRUE)
{
  enable("boot_no_c")
  
  if(is.na(input$boot_no_b)==FALSE && input$boot_no_c < 2)
  {
    updateNumericInput(session=session, "boot_no_c", label = "Number of bootstraps", min = 2, max = 1000, value = 2, step = 1)
  }
  
  if(is.na(input$boot_no_b)==FALSE && input$boot_no_c > 1000)
  {
    updateNumericInput(session=session, "boot_no_c", label = "Number of bootstraps", min = 2, max = 1000, value = 1000, step = 1)
  }
}else
{
  disable("boot_no_c")
}


output$done2C <- renderText(expr = {
  validate(need(rval_status_DF$data["2C","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["2C", "status"] == "done")
{
  enable("Down_2C_tree")
  enable("Down_2C_distMA")
}else
{
  disable("Down_2C_tree")
  disable("Down_2C_distMA")
  rval_mes_2C$data <- ""
}


### Start 2C_plot button --------
if(rval_status_DF$data["2C", "status"] == "not_run" | rval_status_DF$data["2C", "status"] == "running")
{
  disable("inc_fact_w_Pd_C")
  disable("font_row_Pd_C")
  disable("font_col_Pd_C")
  disable("font_cell_Pd_C")
  disable("lgd_font_Pd_C")
  disable("lgd_lab_font_Pd_C")
  disable("lgd_height_Pd_C")
  disable("lgd_width_Pd_C")
  disable("actOut_pdf_2C")
}else
{
  enable("inc_fact_w_Pd_C")
  enable("font_row_Pd_C")
  enable("font_col_Pd_C")
  enable("font_cell_Pd_C")
  enable("lgd_font_Pd_C")
  enable("lgd_lab_font_Pd_C")
  enable("lgd_height_Pd_C")
  enable("lgd_width_Pd_C")
  enable("actOut_pdf_2C")
  
  if(is.na(input$inc_fact_w_Pd_C)==FALSE && input$inc_fact_w_Pd_C < 0.02)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_C", label = "Cell width", min = 0.02, max = 1, value = 0.02, step = 0.001)
  }
  
  if(is.na(input$inc_fact_w_Pd_C)==FALSE && input$inc_fact_w_Pd_C > 1)
  {
    updateNumericInput(session=session, "inc_fact_w_Pd_C", label = "Cell width", min = 0.005, max = 1, value = 1, step = 0.001)
  }
  
  
  if(is.na(input$font_row_Pd_C)==FALSE && input$font_row_Pd_C < 1)
  {
    updateNumericInput(session=session, "font_row_Pd_C", label = "Row font", value = 1, min=1)
  }
  
  if(is.na(input$font_col_Pd_C)==FALSE && input$font_col_Pd_C < 1)
  {
    updateNumericInput(session=session, "font_col_Pd_C", label = "Column font", value = 1, min=1)
  }
  
  if(is.na(input$font_cell_Pd_C)==FALSE && input$font_cell_Pd_C < 1)
  {
    updateNumericInput(session=session, "font_cell_Pd_C", label = "Cell font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_font_Pd_C)==FALSE && input$lgd_font_Pd_C < 1)
  {
    updateNumericInput(session=session, "lgd_font_Pd_C", label = "Legend font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_lab_font_Pd_C)==FALSE && input$lgd_lab_font_Pd_C < 1)
  {
    updateNumericInput(session=session, "lgd_lab_font_Pd_C", label = "Legend label font", value = 1, min=1)
  }
  
  if(is.na(input$lgd_height_Pd_C)==FALSE && input$lgd_height_Pd_C < 1)
  {
    updateNumericInput(session=session, "lgd_height_Pd_C", label = "Legend heigth", value = 1, min=1)
  }
  
  if(is.na(input$lgd_width_Pd_C)==FALSE && input$lgd_width_Pd_C < 1)
  {
    updateNumericInput(session=session, "lgd_width_Pd_C", label = "Legend width", value = 1, min=1)
  }
}

output$done2C_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["2C_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["2C_Plot", "status"] == "done")
{
  enable("Down_2C_PDF")
}else
{
  disable("Down_2C_PDF")
  rval_mes_2C_Plot$data <- ""
}


# Start 3C button -------------
if(rval_status_DF$data["2C", "status"] == "not_run" | rval_status_DF$data["2C", "status"] == "running")
{
  disable("actSplit_clust_PSSC")
  disable("Clust_dist_PSSC")
  disable("sel_PSSCs_heatmap_c")
}else
{
  enable("actSplit_clust_PSSC")
  enable("Clust_dist_PSSC")
  enable("sel_PSSCs_heatmap_c")
  
  if(is.na(input$Clust_dist_PSSC)==FALSE && input$Clust_dist_PSSC < 0.1)
  {
    updateNumericInput(session=session, "Clust_dist_PSSC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.1, step = 0.01)
  }
  
  if(is.na(input$Clust_dist_PSSC)==FALSE && input$Clust_dist_PSSC > 2)
  {
    updateNumericInput(session=session, "Clust_dist_PSSC", label = "Clustering distance*", min = 0.1, max = 1, value = 1, step = 0.01)
  }
  
  if(is.na(input$sel_PSSCs_heatmap_c)==FALSE && input$sel_PSSCs_heatmap_c < 1)
  {
    updateNumericInput(session=session, "sel_PSSCs_heatmap_c", label = "Show only common PSSCs if >:", value = 1, min =1)
  }
}

output$done3C <- renderText(expr = {
  validate(need(rval_status_DF$data["3C","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3C", "status"] == "done" & check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSSC") == FALSE)
{
  enable("Down_3C_genPSSCs_Table")
  enable("Down_3C_stats_Table")
  enable("Down_3C_VGCs")
}else
{
  disable("Down_3C_genPSSCs_Table")
  disable("Down_3C_stats_Table")
  disable("Down_3C_VGCs")
  rval_mes_3C$data <- ""
}


# Start 3C_Plot button ------
disable_3C_Plot_fun <- function()
{
  disable("actOut_pdf_3C")
  
  updateCheckboxInput(session, inputId = "Show_Tree_c", label = "Show tree", value = TRUE)
  disable("Show_Tree_c")
  
  updateCheckboxInput(session, inputId = "show_clust_ID_c", label = "Show VGC ID", value = TRUE)
  disable("show_clust_ID_c")
  
  updateCheckboxInput(session, inputId = "show_sil_c", label = "Show silhoutte width", value = TRUE)
  disable("show_sil_c")
  
  updateCheckboxInput(session, inputId = "show_protein_stats_c", label = "Show protein stats", value = TRUE)
  disable("show_protein_stats_c")
  
  updateCheckboxInput(session, inputId = "show_heat_c", label = "Show heatmap", value = TRUE)
  disable("show_heat_c")
}

if(rval_status_DF$data["3C", "status"] == "not_run" | rval_status_DF$data["3C", "status"] == "running")
{
  disable_3C_Plot_fun()
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSSC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PSSC"))
  {
    disable_3C_Plot_fun()
  }else
  {
    enable("actOut_pdf_3C")
    
    enable("Show_Tree_c")
    enable("show_clust_ID_c")
    enable("show_protein_stats_c")
    enable("show_sil_c")
    enable("show_heat_c")
  }
}


#PDF options 3C_Plot
if(input$Show_Tree_c == FALSE)
{
  disable("tree_width_c")
}else
{
  enable("tree_width_c")
  
  if(is.na(input$tree_width_c)==FALSE && input$tree_width_c > 100)
  {
    updateNumericInput(session=session, "tree_width_c", label = "Tree width", min = 1, max = 100, value = 100)
  }
  
  if(is.na(input$tree_width_c)==FALSE && input$tree_width_c < 1)
  {
    updateNumericInput(session=session, "tree_width_c", label = "Tree width", min = 1, max = 100, value = 1)
  }
}

if(input$show_protein_stats_c == FALSE & input$show_sil_c == FALSE & input$show_clust_ID_c == FALSE)
{
  disable("stats_font_c")
  if(is.na(input$stats_font_c)==FALSE && input$stats_font_c < 1)
  {updateNumericInput(session=session, "stats_font_c", label = "Font stats name", value = 1, min=1)}
}else
{
  enable("stats_font_c")
}

if(input$show_clust_ID_c == FALSE)
{
  disable("clustID_width_c")
}else{
  enable("clustID_width_c")
  if(is.na(input$clustID_width_c)==FALSE && input$clustID_width_c < 1)
  {updateNumericInput(session=session, "clustID_width_c", label = "Width of VGC ID column", value = 1, min=1)}
}

if(input$show_sil_c == FALSE)
{
  disable("Sil_stats_width_c")
}else
{
  enable("Sil_stats_width_c")
  if(is.na(input$Sil_stats_width_c)==FALSE && input$Sil_stats_width_c < 1)
  {updateNumericInput(session=session, "Sil_stats_width_c", label = "Width of silhoutte column", value = 1, min=1)}
}

if(input$show_protein_stats_c == FALSE)
{
  disable("Stats_width_c")
  disable("stats_lab_font_c")
}else
{
  enable("Stats_width_c")
  enable("stats_lab_font_c")
  
  if(is.na(input$Stats_width_c)==FALSE && input$Stats_width_c < 1)
  {updateNumericInput(session=session, "Stats_width_c", label = "Width of protein stats", min = 1, max = 100, value = 1)}
  
  if(is.na(input$Stats_width_c)==FALSE && input$Stats_width_c > 100)
  {updateNumericInput(session=session, "Stats_width_c", label = "Width of protein stats", min = 1, max = 100, value = 100)}
  
  if(is.na(input$stats_lab_font_c)==FALSE && input$stats_lab_font_c < 1)
  {updateNumericInput(session=session, "stats_lab_font_c", "Font stats axis", value = 1, min=1)}
}


if(input$show_heat_c == FALSE)
{
  disable("Heat_width_c")
  disable("font_col_c")
}else
{
  enable("Heat_width_c")
  enable("font_col_c")
  
  if(is.na(input$Heat_width_c)==FALSE && input$Heat_width_c < 0.005)
  {updateNumericInput(session=session, "Heat_width_c", label = "Column width (inches)", min = 0.005, max = 1, value = 0.005, step = 0.001)}
  
  if(is.na(input$Heat_width_c)==FALSE && input$Heat_width_c > 1)
  {updateNumericInput(session=session, "Heat_width_c", label = "Column width (inches)", min = 0.005, max = 1, value = 1, step = 0.001)}
  
  if(is.na(input$font_col_c)==FALSE && input$font_col_c < 1)
  {updateNumericInput(session=session, "font_col_c", label = "Font PC names", value = 1, min = 1)}
}

if(input$show_sil_c == FALSE & input$show_heat_c == FALSE)
{
  disable("lgd_w_c")
  disable("lgd_h_c")
  disable("lgd_font_c")
  disable("lgd_lab_font_c")
}else
{
  enable("lgd_w_c")
  enable("lgd_h_c")
  enable("lgd_font_c")
  enable("lgd_lab_font_c")
  
  if(is.na(input$lgd_font_c)==FALSE && input$lgd_font_c < 1)
  {updateNumericInput(session=session, "lgd_font_c", label = "Legend font", value = 1, min=1)}
  
  if(is.na(input$lgd_lab_font_c)==FALSE && input$lgd_lab_font_c < 1)
  {updateNumericInput(session=session, "lgd_lab_font_c", label = "Legend label font", value = 1, min = 1)}
  
  if(is.na(input$lgd_h_c)==FALSE && input$lgd_h_c < 1)
  {updateNumericInput(session=session, "lgd_h_c", label = "Legend heigth", value = 1, min = 1)}
  
  if(is.na(input$lgd_w_c)==FALSE && input$lgd_w_c < 1)
  {updateNumericInput(session=session, "lgd_w_c", label = "Legend width", value = 1, min=1)}
}


if(input$Show_Tree_c == FALSE & input$show_clust_ID_c == FALSE & input$show_protein_stats_c == FALSE & input$show_sil_c == FALSE & input$show_heat_c == FALSE)
{
  disable("font_row_c")
}else
{
  enable("font_row_c")
  if(is.na(input$font_row_c)==FALSE && input$font_row_c < 1)
  {updateNumericInput(session=session, "font_row_c", label = "Font genomes/VGCs IDs ", value = 1, min=1)}
}


#other 3C_Plot
output$done3C_Plot <- renderText(expr = {
  validate(need(rval_status_DF$data["3C_Plot","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["3C_Plot", "status"] == "done")
{
  enable("Down_3C_Plot")
}else
{
  disable("Down_3C_Plot")
  rval_mes_3C_Plot$data <- ""
}


# Start 4C button Core ------

if(rval_status_DF$data["3C", "status"] == "not_run" | rval_status_DF$data["3C", "status"] == "running")
{
  disable("actCore_PSSCs")
}else
{
  if(check_exit_5_fun(projdir = rval_proj_name$data, pc_type = "PSSC") | check_singlegenomes_inVGC_fun(projdir = rval_proj_name$data, pc_type = "PSSC"))
  {
    disable("actCore_PSSCs")
  }else
  {
    enable("actCore_PSSCs")
  }
}

output$done4C <- renderText(expr = {
  validate(need(rval_status_DF$data["4C","status"] == "done", ""))
  text <- "DONE"
  return(text)
})

if(rval_status_DF$data["4C", "status"] == "done")
{
  enable("Down_4C")
}else
{
  disable("Down_4C")
  rval_mes_4C$data <- ""
}


# Start 5C button ---------

if(rval_status_DF$data["1C", "status"] == "not_run" | rval_status_DF$data["1C", "status"] == "running")
{
  disable("Annot_prots_PSSCs")
  
  disable("actAnnotInterPro_PSSC")
  disable("actAnnotpVOGs_PSSC")
  disable("actAnnotVOGDB_PSSC")
  disable("actAnnotPHROGS_PSSC")
  disable("actAnnotEfam_PSSC")
  disable("actAnnotEfam-XC_PSSC")
  disable("actAnnotNCBI_PSSC")
  
}else
{
  
  if(rval_status_DF$data["4C", "status"] == "not_run" | rval_status_DF$data["4C", "status"] == "running")
  {
    updateRadioButtons(session, "Annot_prots_PSSCs", label = "Select proteins to annotate",
                       choices = c("all proteins and relate them to PCs, PCSs and PSSCs", "core proteins based on PSSCs"),
                       selected = "all proteins and relate them to PCs, PCSs and PSSCs", inline = TRUE)#, width = "100%")
    shinyjs::delay(100, disable("Annot_prots_PSSCs"))
  }else
  {
    file_size <- exits_core_fun(rval_proj_name$data, "c")
    
    if(file_size == 0)
    {
      disable("Annot_prots_PSSCs")
    }else
    {
      enable("Annot_prots_PSSCs")
    }
  }
  
  enable("actAnnotInterPro_PSSC")
  enable("actAnnotpVOGs_PSSC")
  enable("actAnnotVOGDB_PSSC")
  enable("actAnnotPHROGS_PSSC")
  enable("actAnnotEfam_PSSC")
  enable("actAnnotEfam-XC_PSSC")
  enable("actAnnotNCBI_PSSC")
}

### All PSSCs -----
if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
{
  
  stdfC <<- rval_status_DF$data
  #the done messages -----
  output$done5CI <- renderText(expr = {
    if(rval_status_DF$data["5CI","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done5CpV <- renderText(expr = {
    if(rval_status_DF$data["5CpV","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CVO <- renderText(expr = {
    if(rval_status_DF$data["5CVO","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CPH <- renderText(expr = {
    if(rval_status_DF$data["5CPH","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CE <- renderText(expr = {
    if(rval_status_DF$data["5CE","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CXC <- renderText(expr = {
    if(rval_status_DF$data["5CXC","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CN <- renderText(expr = {
    if(rval_status_DF$data["5CN","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons ------------
  if(rval_status_DF$data["5CI", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PSSC")}else{disable("downInterPro_PSSC")}
  }else
  {
    disable("downInterPro_PSSC")
    rval_mes_5CI$data <- ""
  }
  
  if(rval_status_DF$data["5CpV", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PSSC")}else{disable("downpVOGs_PSSC")}
  }else
  {
    disable("downpVOGs_PSSC")
    rval_mes_5CpV$data <- ""
  }
  
  if(rval_status_DF$data["5CVO", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PSSC")}else{disable("downVOGDB_PSSC")}
  }else
  {
    disable("downVOGDB_PSSC")
    rval_mes_5CVO$data <- ""
  }
  
  if(rval_status_DF$data["5CPH", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PSSC")}else{disable("downPHROGS_PSSC")}
  }else
  {
    disable("downPHROGS_PSSC")
    rval_mes_5CPH$data <- ""
  }
  
  if(rval_status_DF$data["5CE", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PSSC")}else{disable("downEfam_PSSC")}
  }else
  {
    disable("downEfam_PSSC")
    rval_mes_5CE$data <- ""
  }
  
  if(rval_status_DF$data["5CXC", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PSSC")}else{disable("downEfam-XC_PSSC")}
  }else
  {
    disable("downEfam-XC_PSSC")
    rval_mes_5CXC$data <- ""
  }
  
  if(rval_status_DF$data["5CN", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="all_PSSCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PSSC")}else{disable("downNCBI_PSSC")}
  }else
  {
    disable("downNCBI_PSSC")
    rval_mes_5CN$data <- ""
  }
  
  
  #other -------
  ###this is to activate the button for merging annots
  if(rval_status_DF$data["6AI", "status"] == "done" | rval_status_DF$data["6ApV", "status"] == "done" |
     rval_status_DF$data["6AVO", "status"] == "done" | rval_status_DF$data["6APH", "status"] == "done" |
     rval_status_DF$data["6AE", "status"] == "done" | rval_status_DF$data["6AXC", "status"] == "done" |
     rval_status_DF$data["6AN", "status"] == "done" |
     rval_status_DF$data["5BI", "status"] == "done" | rval_status_DF$data["5BpV", "status"] == "done" |
     rval_status_DF$data["5BVO", "status"] == "done" | rval_status_DF$data["5BPH", "status"] == "done" |
     rval_status_DF$data["5BE", "status"] == "done" | rval_status_DF$data["5BXC", "status"] == "done" |
     rval_status_DF$data["5BN", "status"] == "done")
  {
    if(rval_status_DF$data["1C", "status"] == "done")
    {
      enable("actMergeAnnot_PSSC")
    }else
    {
      disable("actMergeAnnot_PSSC")
    }
  }else
  {
    if(rval_status_DF$data["1C", "status"] == "done")
    {
      if(rval_status_DF$data["5CI", "status"] == "done" | rval_status_DF$data["5CpV", "status"] == "done" |
         rval_status_DF$data["5CVO", "status"] == "done" | rval_status_DF$data["5CPH", "status"] == "done" |
         rval_status_DF$data["5CE", "status"] == "done" | rval_status_DF$data["5CXC", "status"] == "done" |
         rval_status_DF$data["5CN", "status"] == "done")
      {
        enable("actMergeAnnot_PSSC")
      }else
      {
        disable("actMergeAnnot_PSSC")
      }
    }else
    {
      disable("actMergeAnnot_PSSC")
    }
  }
  
  output$done5CM <- renderText(expr = {
    validate(need(rval_status_DF$data["5CM","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["5CM", "status"] == "done")
  {
    enable("downMergedAnnots_PSSC")
  }else
  {
    disable("downMergedAnnots_PSSC")
    rval_mes_5CM$data <- ""
  }
}else
{
  stdfC <<- rval_status_DF$data
  #core PSSCs -------
  #the done messages --------
  output$done5CI <- renderText(expr = {
    if(rval_status_DF$data["5CI_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    
    return(text)
  })
  
  output$done5CpV <- renderText(expr = {
    if(rval_status_DF$data["5CpV_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CVO <- renderText(expr = {
    if(rval_status_DF$data["5CVO_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CPH <- renderText(expr = {
    if(rval_status_DF$data["5CPH_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CE <- renderText(expr = {
    if(rval_status_DF$data["5CE_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CXC <- renderText(expr = {
    if(rval_status_DF$data["5CXC_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  output$done5CN <- renderText(expr = {
    if(rval_status_DF$data["5CN_core","status"] == "done")
    {text <- "DONE"}else{text <- ""}
    return(text)
  })
  
  #the download buttons --------
  if(rval_status_DF$data["5CI_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/interpro/interpro_TB.RDS"))
    {enable("downInterPro_PSSC")}else{disable("downInterPro_PSSC")}
  }else
  {
    disable("downInterPro_PSSC")
    rval_mes_5CI_core$data <- ""
  }
  
  if(rval_status_DF$data["5CpV_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/hhsearch_pVOGs/hhsearch_pVOGs_TB.RDS"))
    {enable("downpVOGs_PSSC")}else{disable("downpVOGs_PSSC")}
  }else
  {
    disable("downpVOGs_PSSC")
    rval_mes_5CpV_core$data <- ""
  }
  
  if(rval_status_DF$data["5CVO_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/hhsearch_VOGDB/hhsearch_VOGDB_TB.RDS"))
    {enable("downVOGDB_PSSC")}else{disable("downVOGDB_PSSC")}
  }else
  {
    disable("downVOGDB_PSSC")
    rval_mes_5CVO_core$data <- ""
  }
  
  if(rval_status_DF$data["5CPH_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/hhsearch_PHROGS/hhsearch_PHROGS_TB.RDS"))
    {enable("downPHROGS_PSSC")}else{disable("downPHROGS_PSSC")}
  }else
  {
    disable("downPHROGS_PSSC")
    rval_mes_5CPH_core$data <- ""
  }
  
  if(rval_status_DF$data["5CE_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/hmmscan_Efam/hmmscan_Efam.RDS"))
    {enable("downEfam_PSSC")}else{disable("downEfam_PSSC")}
  }else
  {
    disable("downEfam_PSSC")
    rval_mes_5CE_core$data <- ""
  }
  
  if(rval_status_DF$data["5CXC_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/hmmscan_Efam_XC/hmmscan_Efam_XC.RDS"))
    {enable("downEfam-XC_PSSC")}else{disable("downEfam-XC_PSSC")}
  }else
  {
    disable("downEfam-XC_PSSC")
    rval_mes_5CXC_core$data <- ""
  }
  
  if(rval_status_DF$data["5CN_core", "status"] == "done")
  {
    if(check_res_annots_fun(projdir=rval_proj_name$data, prot_type="core_PSSCs", annot_suf="/BlastP_NR/blastp_TB.RDS"))
    {enable("downNCBI_PSSC")}else{disable("downNCBI_PSSC")}
  }else
  {
    disable("downNCBI_PSSC")
    rval_mes_5CN_core$data <- ""
  }
  
  #other -------
  
  ###this is to activate the button for merging annots
  if((rval_status_DF$data["5CI_core", "status"] == "not_run" | rval_status_DF$data["5CI_core", "status"] == "running") &
     (rval_status_DF$data["5CpV_core", "status"] == "not_run" | rval_status_DF$data["5CpV_core", "status"] == "running") &
     (rval_status_DF$data["5CVO_core", "status"] == "not_run" | rval_status_DF$data["5CVO_core", "status"] == "running") &
     (rval_status_DF$data["5CPH_core", "status"] == "not_run" | rval_status_DF$data["5CPH_core", "status"] == "running") &
     (rval_status_DF$data["5CE_core", "status"] == "not_run" | rval_status_DF$data["5CE_core", "status"] == "running") &
     (rval_status_DF$data["5CXC_core", "status"] == "not_run" | rval_status_DF$data["5CXC_core", "status"] == "running") &
     (rval_status_DF$data["5CN_core", "status"] == "not_run" | rval_status_DF$data["5CN_core", "status"] == "running"))
  {
    disable("actMergeAnnot_PSSC")
    
  }else
  {
    enable("actMergeAnnot_PSSC")
  }
  
  output$done5CM <- renderText(expr = {
    validate(need(rval_status_DF$data["5CM_core","status"] == "done", ""))
    text <- "DONE"
    return(text)
  })
  
  if(rval_status_DF$data["5CM_core", "status"] == "done")
  {
    enable("downMergedAnnots_PSSC")
  }else
  {
    disable("downMergedAnnots_PSSC")
    rval_mes_5CM_core$data <- ""
  }
}









