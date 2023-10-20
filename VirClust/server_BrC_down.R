
#Branch C ---------  
#1C ----
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

#2C -----
output$Down_2C_distMA <- downloadHandler(
  filename = "2C_Intergenome_distance_matrix.tsv",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/MyDistPCs_MA_ordered.tsv"), to = file)
  }
)

output$Down_2C_tree <- downloadHandler(
  filename = function(filename){
    if(input$boot_c == FALSE)
    {
      filename="2C_genome_tree.newick"
    }else
    {
      filename="2C_pv_trees.zip"
    }
  },
  content = function(file){
    if(input$boot_c == FALSE)
    {
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/hc_tree.newick"), to = file)
    }else
    {
      file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/pv_trees.zip"), to = file)
    }
  }
)

output$Down_2C_PDF <- downloadHandler(
  filename = "2C_DistanceHeatmap_PSSCs.PDF",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/04/Dist_heatmap_PSSC_all_genomes.PDF"), to = file)
  }
)

#3C ----
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

output$Down_3C_VGCs <- downloadHandler(
  filename = "3C_VGCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/05/VGCs.zip"), to = file)
  }
)

output$Down_3C_Plot <- downloadHandler(
  filename = "3C_genome_clustering_protein_heatmap.PDF",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04c-06c_genome_clustering_PSSC/06-Heatmap_PSSC.PDF"), to = file)
  }
)

#4C ----
output$Down_4C <- downloadHandler(
  filename = "4C_core_proteins_based_on_PSSCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/07/core_c/core_c.zip"), to = file)
  }
)

#5C ----
output$downInterPro_PSSC <- downloadHandler(
  filename =  "5C_interpro_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB_joined.tsv"), to = file)   
  }
)


output$downpVOGs_PSSC <- downloadHandler(
  filename =  "5C_pVOGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB_joined.tsv"), to = file)   
  }
)


output$downVOGDB_PSSC <- downloadHandler(
  filename =  "5C_VOGDB_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB_joined.tsv"), to = file)   
  }
)

output$downPHROGS_PSSC <- downloadHandler(
  filename =  "5C_PHROGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB_joined.tsv"), to = file)   
  }
)

output$downEfam_PSSC <- downloadHandler(
  filename =  "5C_Efam_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam_joined.tsv"), to = file)   
  }
)

output$`downEfam-XC_PSSC`  <- downloadHandler(
  filename =  "5C_Efam-XC_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC_joined.tsv"), to = file)   
  }
)

output$downNCBI_PSSC  <- downloadHandler(
  filename =  "5C_BLAST-NCBI-NR_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB_joined.tsv"), to = file)   
  }
)

output$downMergedAnnots_PSSC  <- downloadHandler(
  filename =  "5C_Merged_annots_PSSCs.zip",
  content = function(file){
    
    if(input$Annot_prots_PSSCs == "all proteins and relate them to PCs, PCSs and PSSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_c"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/all_outputs/all_outputs.zip"), to = file)   
  }
)