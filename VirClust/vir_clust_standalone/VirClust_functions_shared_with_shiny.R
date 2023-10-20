make_status_df_fun <- function(projdir)
{
 status_df <- tibble(step = c("1A", "2A",
                  "6AI", "6ApV", "6AVO", "6APH", "6AE", "6AXC", "6AN",
                  "5BI", "5BpV", "5BVO", "5BPH", "5BE", "5BXC", "5BN", 
                  "5CI", "5CpV", "5CVO", "5CPH", "5CE", "5CXC", "5CN", 
                  "3A", "3A_Plot", "4A", "4A_Plot", "5A",
                  "6AM",
                  "6AI_core", "6ApV_core", "6AVO_core", "6APH_core", "6AE_core", "6AXC_core", "6AN_core", "6AM_core",
                  "1B", "2B", "2B_Plot", "3B", "3B_Plot", "4B", 
                  "5BM",
                  "5BI_core", "5BpV_core", "5BVO_core", "5BPH_core", "5BE_core", "5BXC_core", "5BN_core", "5BM_core",
                  "1C", "2C", "2C_Plot", "3C", "3C_Plot", "4C",
                  "5CM",
                  "5CI_core", "5CpV_core", "5CVO_core", "5CPH_core", "5CE_core", "5CXC_core", "5CN_core", "5CM_core"), 
         status = "not_run",
         out_path = c("/01", "/02",
                      "/08_annots/interpro", "/08_annots/hhsearch_pVOGs", "/08_annots/hhsearch_VOGDB", "/08_annots/hhsearch_PHROGS", 
                      "/08_annots/hmmscan_Efam", "/08_annots/hmmscan_Efam_XC", "/08_annots/BlastP_NR",
                      "/08_annots/interpro", "/08_annots/hhsearch_pVOGs", "/08_annots/hhsearch_VOGDB", "/08_annots/hhsearch_PHROGS", 
                      "/08_annots/hmmscan_Efam", "/08_annots/hmmscan_Efam_XC", "/08_annots/BlastP_NR", 
                      "/08_annots/interpro", "/08_annots/hhsearch_pVOGs", "/08_annots/hhsearch_VOGDB", "/08_annots/hhsearch_PHROGS", 
                      "/08_annots/hmmscan_Efam", "/08_annots/hmmscan_Efam_XC", "/08_annots/BlastP_NR", 
                       
                      "/04a-06a_genome_clustering_PC/04", "/04a-06a_genome_clustering_PC/04/Dist_heatmap_PC_all_genomes.PDF", 
                      "/04a-06a_genome_clustering_PC/05", "/04a-06a_genome_clustering_PC/06-Heatmap_PC.PDF", 
                      "/07/core_a", 
                      "/08_annots/all_outputs",
                      "/08_annots_core_a/interpro", "/08_annots_core_a/hhsearch_pVOGs", "/08_annots_core_a/hhsearch_VOGDB", "/08_annots_core_a/hhsearch_PHROGS", 
                      "/08_annots_core_a/hmmscan_Efam", "/08_annots_core_a/hmmscan_Efam_XC", "/08_annots_core_a/BlastP_NR", "/08_annots_core_a/all_outputs",
                      "/03", 
                      "/04b-06b_genome_clustering_PSC/04", "/04b-06b_genome_clustering_PSC/04/Dist_heatmap_PSC_all_genomes.PDF", 
                      "/04b-06b_genome_clustering_PSC/05", "/04b-06b_genome_clustering_PSC/06-Heatmap_PSC.PDF", 
                      "/07/core_b", 
                      "/08_annots/all_outputs",
                      "/08_annots_core_b/interpro", "/08_annots_core_b/hhsearch_pVOGs", "/08_annots_core_b/hhsearch_VOGDB", "/08_annots_core_b/hhsearch_PHROGS", 
                      "/08_annots_core_b/hmmscan_Efam", "/08_annots_core_b/hmmscan_Efam_XC", "/08_annots_core_b/BlastP_NR", "/08_annots_core_b/all_outputs",
                      "/03C",
                      "/04c-06c_genome_clustering_PSSC/04", "/04c-06c_genome_clustering_PSSC/04/Dist_heatmap_PSSC_all_genomes.PDF", 
                      "/04c-06c_genome_clustering_PSSC/05", "/04c-06c_genome_clustering_PSSC/06-Heatmap_PSSC.PDF", 
                      "/07/core_c", 
                      "/08_annots/all_outputs",
                      "/08_annots_core_c/interpro", "/08_annots_core_c/hhsearch_pVOGs", "/08_annots_core_c/hhsearch_VOGDB", "/08_annots_core_c/hhsearch_PHROGS", 
                      "/08_annots_core_c/hmmscan_Efam", "/08_annots_core_c/hmmscan_Efam_XC", "/08_annots_core_c/BlastP_NR", "/08_annots_core_c/all_outputs"
         ),
         path_type = c(rep("dir", 24), "file", "dir", "file", "dir",
                       rep("dir", 9),
                       rep("dir", 2), "file", "dir", "file", "dir",
                       rep("dir", 9),
                       rep("dir", 2), "file", "dir", "file", "dir",
                       rep("dir", 9)),
         param = "none", 
         max_cols_HT = 0, clust_dist = 0, boot_pv = "", aglom = "") %>%     
    column_to_rownames(var="step") %>%
    mutate(out_path = paste0(projdir, out_path))
  
  return(status_df)
}


