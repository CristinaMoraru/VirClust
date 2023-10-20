
#Branch B ---------  
#1B ----
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

#2B ----
output$Down_2B_distMA <- downloadHandler(
  filename = "2B_Intergenome_distance_matrix.tsv",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/MyDistPCs_MA_ordered.tsv"), to = file)
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
  filename = "2B_DistanceHeatmap_PSCs.PDF",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/04/Dist_heatmap_PSC_all_genomes.PDF"), to = file)
  }
)

#3B ----
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

output$Down_3B_VGCs <- downloadHandler(
  filename = "3B_VGCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/05/VGCs.zip"), to = file)
  }
)

output$Down_3B_Plot <- downloadHandler(
  filename = "3B_genome_clustering_protein_heatmap.PDF",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04b-06b_genome_clustering_PSC/06-Heatmap_PSC.PDF"), to = file)
  }
)

#4B ----
output$Down_4B <- downloadHandler(
  filename = "4B_core_proteins_based_on_PSCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/07/core_b/core_b.zip"), to = file)
  }
)

#5B ---
output$downInterPro_PSC <- downloadHandler(
  filename =  "5B_interpro_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB_joined.tsv"), to = file)   
  }
)


output$downpVOGs_PSC <- downloadHandler(
  filename =  "5B_pVOGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB_joined.tsv"), to = file)   
  }
)


output$downVOGDB_PSC <- downloadHandler(
  filename =  "5B_VOGDB_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB_joined.tsv"), to = file)   
  }
)

output$downPHROGS_PSC <- downloadHandler(
  filename =  "5B_PHROGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB_joined.tsv"), to = file)   
  }
)

output$downEfam_PSC <- downloadHandler(
  filename =  "5B_Efam_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam_joined.tsv"), to = file)   
  }
)

output$`downEfam-XC_PSC`  <- downloadHandler(
  filename =  "5B_Efam-XC_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC_joined.tsv"), to = file)   
  }
)

output$downNCBI_PSC  <- downloadHandler(
  filename =  "5B_BLAST-NCBI-NR_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB_joined.tsv"), to = file)   
  }
)

output$downMergedAnnots_PSC  <- downloadHandler(
  filename =  "5B_Merged_annots_PSCs.zip",
  content = function(file){
    
    if(input$Annot_prots_PSCs == "all proteins and relate them to PCs and PSCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_b"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/all_outputs/all_outputs.zip"), to = file)   
  }
)
