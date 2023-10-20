shiny_opt_TB_fun <- function()
{
shiny_opt_TB <- tibble(
  param = c(
  #BranchA
  #1A
  "Genetic_code",
  #2A
  "clust_PC",
  "eval_PC",
  "bitsc_PC",
  "cov_PC",
  "pident_PC",
  #3A
  "aglom_a",
  "boot_a",
  "boot_no_a",
  #3A_Plot
  "inc_fact_w_Pd_A",
  "font_row_Pd_A",
  "font_col_Pd_A",
  "font_cell_Pd_A",
  "lgd_font_Pd_A",
  "lgd_lab_font_Pd_A", 
  "lgd_height_Pd_A",
  "lgd_width_Pd_A",
  #4A
  "Clust_dist_PC",
  "sel_PCs_heatmap_a",
  #4A_Plot
  "Show_Tree_a",
  "tree_width_a",
  "show_clust_ID_a",
  "show_sil_a",
  "show_protein_stats_a",
  "clustID_width_a",
  "Sil_stats_width_a",
  "Stats_width_a",
  "stats_font_a",
  "stats_lab_font_a",
  "show_heat_a",
  "Heat_width_a",
  "font_col_a",
  "font_row_a",
  "lgd_font_a",
  "lgd_lab_font_a",
  "lgd_h_a",
  "lgd_w_a",
  #6A
  "Annot_prots_PCs",
  
  #Branch B
  #1B
  "clust_PSC",
  "prob1_PSC",
  "cov1_PSC",
  "prob2_PSC",
  "cov2_PSC",
  "alig_PSC",
  #2B
  "aglom_b",
  "boot_b",
  "boot_no_b",
  #2B_Plot
  "inc_fact_w_Pd_B",
  "font_row_Pd_B",
  "font_col_Pd_B",
  "font_cell_Pd_B",
  "lgd_font_Pd_B",
  "lgd_lab_font_Pd_B", 
  "lgd_height_Pd_B",
  "lgd_width_Pd_B",
  #3B
  "Clust_dist_PSC",
  "sel_PSCs_heatmap_b",
  #3B_Plot
  "Show_Tree_b",
  "tree_width_b",
  "show_clust_ID_b",
  "show_sil_b",
  "show_protein_stats_b",
  "clustID_width_b",
  "Sil_stats_width_b",
  "Stats_width_b",
  "stats_font_b",
  "stats_lab_font_b",
  "show_heat_b",
  "Heat_width_b",
  "font_col_b",
  "font_row_b",
  "lgd_font_b",
  "lgd_lab_font_b",
  "lgd_h_b",
  "lgd_w_b",
  #5B
  "Annot_prots_PSCs",
  
  
  #Branch C
  #1C
  "clust_PSSC" ,
  "prob1_PSSC",
  "cov1_PSSC",
  "prob2_PSSC",
  "cov2_PSSC",
  "alig_PSSC",
  #2C
  "aglom_c",
  "boot_c",
  "boot_no_c",
  #2C_Plot
  "inc_fact_w_Pd_C",
  "font_row_Pd_C",
  "font_col_Pd_C",
  "font_cell_Pd_C",
  "lgd_font_Pd_C",
  "lgd_lab_font_Pd_C", 
  "lgd_height_Pd_C",
  "lgd_width_Pd_C",
  #3C
  "Clust_dist_PSSC",
  "sel_PSSCs_heatmap_c",
  #3C_Plot
  "Show_Tree_c",
  "tree_width_c",
  "show_clust_ID_c",
  "show_sil_c",
  "show_protein_stats_c",
  "clustID_width_c",
  "Sil_stats_width_c",
  "Stats_width_c",
  "stats_font_c",
  "stats_lab_font_c",
  "show_heat_c",
  "Heat_width_c",
  "font_col_c",
  "font_row_c",
  "lgd_font_c",
  "lgd_lab_font_c",
  "lgd_h_c",
  "lgd_w_c",
  #5C
  "Annot_prots_PSSCs"),
  value = "NULL"
) %>%
  column_to_rownames(var="param")


return(shiny_opt_TB)
}

# rval_shiny_opt_TB$data <- tibble(
#   #BranchA
#   #1A
#   Genetic_code = input$Genetic_code,
#   #2A
#   clust_PC = input$clust_PC,
#   eval_PC = input$eval_PC,
#   bitsc_PC = input$bitsc_PC,
#   cov_PC = input$cov_PC,
#   pident_PC = input$pident_PC,
#   #3A
#   aglom_a = input$aglom_a,
#   boot_a = input$boot_a,
#   boot_no_a = input$boot_no_a,
#   #3A_Plot
#   inc_fact_w_Pd_A = input$inc_fact_w_Pd_A,
#   font_row_Pd_A = input$font_row_Pd_A,
#   font_col_Pd_A = input$font_col_Pd_A,
#   font_cell_Pd_A = input$font_cell_Pd_A,
#   lgd_font_Pd_A = input$lgd_font_Pd_A,
#   lgd_lab_font_Pd_A = input$lgd_lab_font_Pd_A, 
#   lgd_height_Pd_A = input$lgd_height_Pd_A,
#   lgd_width_Pd_A = input$lgd_width_Pd_A,
#   #4A
#   Clust_dist_PC <- input$Clust_dist_PC,
#   #4A_Plot
#   Show_Tree_a = input$Show_Tree_a,
#   tree_width_a = input$tree_width_a,
#   show_clust_ID_a = input$show_clust_ID_a,
#   show_sil_a = input$show_sil_a,
#   show_protein_stats_a = input$show_protein_stats_a,
#   clustID_width_a = input$clustID_width_a,
#   Sil_stats_width_a = input$Sil_stats_width_a,
#   Stats_width_a = input$Stats_width_a,
#   stats_font_a = input$stats_font_a,
#   stats_lab_font_a = input$stats_lab_font_a,
#   show_heat_a = input$show_heat_a,
#   Heat_width_a = input$Heat_width_a,
#   font_col_a = input$font_col_a,
#   sel_PCs_heatmap_a = input$sel_PCs_heatmap_a,
#   font_row_a = input$font_row_a,
#   lgd_font_a = input$lgd_font_a,
#   lgd_lab_font_a = input$lgd_lab_font_a,
#   lgd_h_a = input$lgd_h_a,
#   lgd_w_a = input$lgd_w_a,
#   #6A
#   Annot_prots_PCs = input$Annot_prots_PCs,
#   
#   #Branch B
#   #1B
#   clust_PSC = input$clust_PSC,
#   prob1_PSC = input$prob1_PSC,
#   cov1_PSC = input$cov1_PSC,
#   prob2_PSC = input$prob2_PSC,
#   cov2_PSC = input$cov2_PSC,
#   alig_PSC = inout$alig_PSC,
#   #2B
#   aglom_b = input$aglom_b,
#   boot_b = input$boot_b,
#   boot_no_b = input$boot_no_b,
#   #2B_Plot
#   inc_fact_w_Pd_B = input$inc_fact_w_Pd_B,
#   font_row_Pd_B = input$font_row_Pd_B,
#   font_col_Pd_B = input$font_col_Pd_B,
#   font_cell_Pd_B = input$font_cell_Pd_B,
#   lgd_font_Pd_B = input$lgd_font_Pd_B,
#   lgd_lab_font_Pd_B = input$lgd_lab_font_Pd_B, 
#   lgd_height_Pd_B = input$lgd_height_Pd_B,
#   lgd_width_Pd_B = input$lgd_width_Pd_B,
#   #3B
#   Clust_dist_PSC = input$Clust_dist_PSC,
#   #3B_Plot
#   Show_Tree_b = input$Show_Tree_b,
#   tree_width_b = input$tree_width_b,
#   show_clust_ID_b = input$show_clust_ID_b,
#   show_sil_b = input$show_sil_b,
#   show_protein_stats_b = input$show_protein_stats_b,
#   clustID_width_b = input$clustID_width_b,
#   Sil_stats_width_b = input$Sil_stats_width_b,
#   Stats_width_b = input$Stats_width_b,
#   stats_font_b = input$stats_font_b,
#   stats_lab_font_b = input$stats_lab_font_b,
#   show_heat_b = input$show_heat_b,
#   Heat_width_b = input$Heat_width_b,
#   font_col_b = input$font_col_b,
#   sel_PSCs_heatmap_b = input$sel_PSCs_heatmap_b,
#   font_row_b = input$font_row_b,
#   lgd_font_b = input$lgd_font_b,
#   lgd_lab_font_b = input$lgd_lab_font_b,
#   lgd_h_b = input$lgd_h_b,
#   lgd_w_b = input$lgd_w_b,
#   #5B
#   Annot_prots_PSCs = input$Annot_prots_PSCs,
#   
#   
#   #Branch C
#   #1C
#   clust_PSSC = input$clust_PSSC,
#   prob1_PSSC = input$prob1_PSSC,
#   cov1_PSSC = input$cov1_PSSC,
#   prob2_PSSC = input$prob2_PSSC,
#   cov2_PSSC = input$cov2_PSSC,
#   alig_PSSC = inout$alig_PSSC,
#   #2C
#   aglom_c = input$aglom_c,
#   boot_c = input$boot_c,
#   boot_no_c = input$boot_no_c,
#   #2C_Plot
#   inc_fact_w_Pd_C = input$inc_fact_w_Pd_C,
#   font_row_Pd_C = input$font_row_Pd_C,
#   font_col_Pd_C = input$font_col_Pd_C,
#   font_cell_Pd_C = input$font_cell_Pd_C,
#   lgd_font_Pd_C = input$lgd_font_Pd_C,
#   lgd_lab_font_Pd_C = input$lgd_lab_font_Pd_C, 
#   lgd_height_Pd_C = input$lgd_height_Pd_C,
#   lgd_width_Pd_C = input$lgd_width_Pd_C,
#   #3C
#   Clust_dist_PSSC = input$Clust_dist_PSSC,
#   #3C_Plot
#   Show_Tree_c = input$Show_Tree_c,
#   tree_width_c = input$tree_width_c,
#   show_clust_ID_c = input$show_clust_ID_c,
#   show_sil_c = input$show_sil_c,
#   show_protein_stats_c = input$show_protein_stats_c,
#   clustID_width_c = input$clustID_width_c,
#   Sil_stats_width_c = input$Sil_stats_width_c,
#   Stats_width_c = input$Stats_width_c,
#   stats_font_c = input$stats_font_c,
#   stats_lab_font_c = input$stats_lab_font_c,
#   show_heat_c = input$show_heat_c,
#   Heat_width_c = input$Heat_width_c,
#   font_col_c = input$font_col_c,
#   sel_PSCs_heatmap_c = input$sel_PSCs_heatmap_c,
#   font_row_c = input$font_row_c,
#   lgd_font_c = input$lgd_font_c,
#   lgd_lab_font_c = input$lgd_lab_font_c,
#   lgd_h_c = input$lgd_h_c,
#   lgd_w_c = input$lgd_w_c,
#   #5B
#   Annot_prots_PSSCs = input$Annot_prots_PSSCs
# )