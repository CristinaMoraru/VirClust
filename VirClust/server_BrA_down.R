#Branch A --------
#1A ----
output$Down_1A_single_faas <- downloadHandler(
  filename = "1A_Proteins_per_genome.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/01/01_out_faa.zip"), to = file)
  }
)

output$Down_1A_all_faa <- downloadHandler(
  filename = "1A_All_proteins.faa",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/01/01_all_proteins.faa"), to = file)
  }
)

output$Down_1A_Table <- downloadHandler(
  filename = "1A_Genomes_proteins.tsv",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/01/01_genome_protDF_PCs.tsv"), to = file)
  }
)

#2A ----
output$Down_2A_Table <- downloadHandler(
  filename = "2A_Genomes_proteins_PCs.tsv",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/02/02_04_genome_protDF_PCs.tsv"), to = file)
  }
)



#3A ----
output$Down_3A_distMA <- downloadHandler(
  filename = "3A_Intergenome_distance_matrix.tsv",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/04/MyDistPCs_MA_ordered.tsv"), to = file)
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

#4A ----
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

output$Down_4A_VGCs <- downloadHandler(
  filename = "4A_VGCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/05/VGCs.zip"), to = file)
  }
)

output$Down_4A_Plot <- downloadHandler(
  filename = "4A_genome_clustering_protein_heatmap.PDF",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/04a-06a_genome_clustering_PC/06-Heatmap_PC.PDF"), to = file)
  }
)




#5A ----
output$Down_5A <- downloadHandler(
  filename = "5A_core_proteins_based_on_PCs.zip",
  content = function(file){
    file.copy(from = paste0(rval_proj_name$data, "/07/core_a/core_a.zip"), to = file)   
  }
)

#6A ----
output$downInterPro_PC <- downloadHandler(
  filename =  "6A_interpro_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/interpro/interpro_TB_joined.tsv"), to = file)   
  }
)


output$downpVOGs_PC <- downloadHandler(
  filename =  "6A_pVOGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_pVOGs/hhsearch_pVOGs_TB_joined.tsv"), to = file)   
  }
)


output$downVOGDB_PC <- downloadHandler(
  filename =  "6A_VOGDB_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_VOGDB/hhsearch_VOGDB_TB_joined.tsv"), to = file)   
  }
)

output$downPHROGS_PC <- downloadHandler(
  filename =  "6A_PHROGS_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hhsearch_PHROGS/hhsearch_PHROGS_TB_joined.tsv"), to = file)   
  }
)

output$downEfam_PC <- downloadHandler(
  filename =  "6A_Efam_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam/hmmscan_Efam_joined.tsv"), to = file)   
  }
)

output$`downEfam-XC_PC`  <- downloadHandler(
  filename =  "6A_Efam-XC_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/hmmscan_Efam_XC/hmmscan_Efam_XC_joined.tsv"), to = file)   
  }
)

output$downNCBI_PC  <- downloadHandler(
  filename =  "6A_BLAST-NCBI-NR_annots.tsv",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/BlastP_NR/blastp_TB_joined.tsv"), to = file)   
  }
)

output$downMergedAnnots_PC  <- downloadHandler(
  filename =  "6A_Merged_annots_PCs.zip",
  content = function(file){
    
    if(input$Annot_prots_PCs == "all proteins and relate them to PCs")
    {annot_folder <- "/08_annots"}else{annot_folder <- "/08_annots_core_a"}
    
    file.copy(from = paste0(rval_proj_name$data, annot_folder, "/all_outputs/all_outputs.zip"), to = file)   
  }
)
