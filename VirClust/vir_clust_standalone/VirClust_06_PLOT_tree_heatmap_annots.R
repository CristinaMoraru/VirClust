library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(grid, warn.conflicts = FALSE, quietly = FALSE)
suppressPackageStartupMessages(library(ComplexHeatmap, warn.conflicts = FALSE, quietly = FALSE))
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
suppressPackageStartupMessages(library(dendextend, warn.conflicts = FALSE, quietly = FALSE))
suppressPackageStartupMessages(library(circlize, warn.conflicts = FALSE, quietly = FALSE))

#####Load paths and RDS ----
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#only for manual run 
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P220___CLMoraru___test_varid5___20210601124352"
# input0[2] <- "pc_type=PC"
# input0[3] <- "clust_dist=1"
# input0[4] <- "inc_fact_w=0.02"
# input0[5] <- "font_row=12"
# input0[6] <- "font_col=1"
# input0[7] <- "show_tree=yes"
# input0[8] <- "tree_width=0.5"
# input0[9] <- "show_protein_stats=yes"
# input0[10] <- "stats_font=0.02"
# input0[11] <- "stats_lab_font=0.05"
# input0[12] <- "stats_width=1.5"
# input0[13] <- "lgd_width=0.4"
# input0[14] <- "lgd_height=0.1"
# input0[15] <- "lgd_font=0.1"
# input0[16] <- "lgd_pos=leftcenter-rot"
# input0[17] <- "lgd_lab_font=0.4"
# input0[18] <- "sil_stats_width=10"
# input0[19] <- "show_clust_ID=yes"
# input0[20] <- "show_sil=yes"
# input0[21] <- "boot_pv=no"
# input0[22] <- "clustID_width=5"
# input0[23] <- "show_heat=yes"


###functions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)


#setting inputs ---
P_D <- get_params_simple_fun(input0, param_name = "projdir")
pc_type <- get_params_simple_fun(input0, param_name = "pc_type")
boot_pv <-  get_params_simple_fun(input0, param_name = "boot_pv")

### Loading data matrices (DFs) ----
if(pc_type == "PC")
{
  heatmap_dir <- paste0(P_D, "/04a-06a_genome_clustering_PC")
}
if(pc_type == "PSC")
{
  heatmap_dir <- paste0(P_D, "/04b-06b_genome_clustering_PSC")
}
if(pc_type == "PSSC")
{
  heatmap_dir <- paste0(P_D, "/04c-06c_genome_clustering_PSSC")
}
rm(P_D)

in04_dir <- paste0(heatmap_dir, "/04")
in05_dir <- paste0(heatmap_dir, "/05")

sim_matrix_p <- paste0(in05_dir, "/heatmapDF.RDS")
sim_matrix_DF <- readRDS(sim_matrix_p)
rm(sim_matrix_p)

virDF_p <- paste0(in05_dir, "/virDF.RDS")
virDF <- readRDS(virDF_p)
rm(virDF_p)


if(boot_pv == "yes")
{
  pv_p <- paste0(in04_dir, "/pv.RDS")
  pv <- readRDS(pv_p)
  hc <- pv$hclust
  rm(pv_p, pv)
}else
{
  hc_p <- paste0(in04_dir, "/hc.RDS")
  hc <- readRDS(hc_p)
  rm(hc_p)
}
rm(boot_pv, in04_dir, in05_dir)

#### heatmap options ----
show_heat <-  get_params_simple_fun(input0, param_name = "show_heat")
inc_fact_w <-  get_params_simple_fun(input0, param_name = "inc_fact_w", type = "numeric")
font_row <-  get_params_simple_fun(input0, param_name = "font_row", type = "numeric")
font_col <-  get_params_simple_fun(input0, param_name = "font_col", type = "numeric")

#### tree options ----
show_tree <-  get_params_simple_fun(input0, param_name = "show_tree")
tree_width <-  get_params_simple_fun(input0, param_name = "tree_width", type = "numeric")

show_clust_ID <-  get_params_simple_fun(input0, param_name = "show_clust_ID")
clustID_width <-  get_params_simple_fun(input0, param_name = "clustID_width", type = "numeric")

#### stats options ----
show_protein_stats <-  get_params_simple_fun(input0, param_name = "show_protein_stats")
show_sil <-  get_params_simple_fun(input0, param_name = "show_sil")

stats_font <-  get_params_simple_fun(input0, param_name = "stats_font", type = "numeric")
stats_lab_font <-  get_params_simple_fun(input0, param_name = "stats_lab_font", type = "numeric")
stats_width <-  get_params_simple_fun(input0, param_name = "stats_width", type = "numeric")
sil_stats_width <-  get_params_simple_fun(input0, param_name = "sil_stats_width", type = "numeric")
print(paste0("Stats ",stats_width))

#### legend options ----
lgd_width <-  get_params_simple_fun(input0, param_name = "lgd_width", type = "numeric")
lgd_height <-  get_params_simple_fun(input0, param_name = "lgd_height", type = "numeric")
lgd_font <-  get_params_simple_fun(input0, param_name = "lgd_font", type = "numeric")
lgd_pos <-  get_params_simple_fun(input0, param_name = "lgd_pos")
lgd_lab_font <-  get_params_simple_fun(input0, param_name = "lgd_lab_font", type = "numeric")

rm(input0)

## setting param heatmap ---------
inc_fact_h <- 0.2 #one cell has 0.3 inches in heigth
pix_fact <- 0.014  # 
annot_rot <- 90
if(show_heat == "no")
{
  inc_fact_ht_w <- 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000001/length(sim_matrix_DF)
}else
{
  inc_fact_ht_w <- inc_fact_w
}
ht_width <- inc_fact_ht_w*length(sim_matrix_DF)
ht_height <- inc_fact_h*nrow(sim_matrix_DF)
rm(inc_fact_ht_w)

##heatmap legend ---
ht_name <- "Number of P(S)Cs per genome"
legend_breaks <- sim_matrix_DF %>%
  purrr::flatten() %>%
  unlist() %>%
  unique() %>%
  sort() 
if(show_heat == "yes")
{
  colors_all <- c("#FFFFFF","#20B2AA", "#A6CEE3", "#1F78B4",  "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", 
                  "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928") 
}else
{
  colors_all <- c("#FFFFFF","#FFFFFF", "#FFFFFF", "#FFFFFF",  "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", 
                  "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF")
}
col_sim_fun <- structure(colors_all[1:length(legend_breaks)], names=legend_breaks)
rm(colors_all)


# both legends params ----
lgd_width_abs <- max(0.1, lgd_width*inc_fact_w*length(sim_matrix_DF))
lgd_height_abs <- max(0.03, lgd_height*inc_fact_h*nrow(sim_matrix_DF))
lgd_font_abs <- max(1, ceiling(lgd_font*inc_fact_w*nrow(sim_matrix_DF)/pix_fact))
lgd_lab_font_abs <- max(1, ceiling(lgd_lab_font*inc_fact_w*nrow(sim_matrix_DF)/pix_fact))
lgd_sil_height_abs <- max(0.03, (lgd_height_abs*length(legend_breaks))/3)
rm(lgd_width, lgd_height, lgd_font, lgd_lab_font)


# widths and other fonts ----
stats_width_abs <- max(0.5, stats_width * inc_fact_w*length(sim_matrix_DF))
print(paste0("Stats_abs ", stats_width_abs))
sil_stats_width <- inc_fact_w*sil_stats_width
print(paste0("Sil_abs ", sil_stats_width))
clustID_width <- inc_fact_w*clustID_width
if(show_heat == "yes")
{
  tree_width_abs <- tree_width*ht_width
}else
{
  tree_width_abs <- tree_width
}
stats_font_abs <- ceiling((stats_font*stats_width_abs)/(5*pix_fact))
stats_lab_font <- ceiling((stats_lab_font*stats_width_abs)/(5*pix_fact))
rm(stats_width, stats_font, tree_width, inc_fact_w)

# stats ----
Genecount_ma <- virDF %>%
  #mutate(clustPC_count = gene_count - single_Proteins_count) %>%
  select(Proteins_shared, Single_Proteins) %>%
  as.matrix()

PCs_inThisClust_ma <- virDF %>%
  mutate(remainingPCs = gene_count - Proteins_shared_inOwn_GC) %>%
  select(Proteins_shared_inOwn_GC, remainingPCs) %>% 
  as.matrix()

PCs_only_inThisClust_ma <- virDF %>%
  mutate(remainingPCs = gene_count - Proteins_shared_only_In_Own_GC) %>%
  select(Proteins_shared_only_In_Own_GC, remainingPCs) %>% 
  as.matrix()

Proteins_shared_also_Out_Own_GC_ma <- virDF %>%
  mutate(remainingPCs = gene_count - Proteins_shared_also_Out_Own_GC) %>%
  select(Proteins_shared_also_Out_Own_GC, remainingPCs) %>% 
  as.matrix()

Proteins_shared_only_Out_Own_GC_ma <- virDF %>%
  mutate(remainingPCs = gene_count - Proteins_shared_only_Out_Own_GC) %>%
  select(Proteins_shared_only_Out_Own_GC, remainingPCs) %>% 
  as.matrix()



##color for silhoutte ----
col_sil_fun <- colorRamp2(c(-1, 0, 1), c("red", "white", "green"))

##color for the cluster column ----
clust_no <- virDF$genome_cluster_ID %>%
  max()
col_TB <- tibble(col = "grey", genome_cluster_ID = seq(1:clust_no))

virDF_col <- left_join(virDF, col_TB) %>%
  select(genome_cluster_ID, col)
rm(col_TB)

col_clust <- virDF_col$col
names(col_clust) <- virDF_col$genome_cluster_ID
rm(virDF_col)


#lgd sil and annotations ----
if(clust_no > 1)
{
  lgd_sil <- Legend(title = "Silhoutte_width", col_fun = col_sil_fun, at = c(-1, 0, 1), legend_width = unit(lgd_width_abs, "inches"), grid_height = unit(lgd_sil_height_abs, "inches"),
                    title_position = lgd_pos, title_gp = gpar(fontsize = lgd_font_abs), labels_gp = gpar(fontsize = lgd_lab_font_abs))
  
}
rm(lgd_sil_height_abs)

ha1 <- rowAnnotation(Genome_length= anno_barplot(x = virDF$length, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))),
                     Proteins_shared= anno_barplot(x = Genecount_ma, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))),  #PCs shared with other phages vs single PCs 
                     Proteins_shared_inOwn_VGC = anno_barplot(x = PCs_inThisClust_ma, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))),
                     Proteins_shared_only_In_Own_VGC= anno_barplot(x = PCs_only_inThisClust_ma, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))),
                     Proteins_shared_also_Out_Own_VGC = anno_barplot(x = Proteins_shared_also_Out_Own_GC_ma, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))),
                     Proteins_shared_only_Out_Own_VGC = anno_barplot(x = Proteins_shared_only_Out_Own_GC_ma, border = FALSE, baseline = 0, axis_param = list(gp=gpar(fontsize = unit(stats_lab_font, "inches")))), 
                     #Habitat = anno_simple(x= habDF$habitat, col = habDF$color),
                     #ICTV_fam = anno_text(tax_annots_DF$Family, location = 0.5, just = "center", width = max_text_width(tax_annots_DF$Family)*0.2, gp = gpar(fill = "#DDDDDD")),
                     #ICTV_subfam = anno_text(tax_annots_DF$Subfamily, location = 0.5, just = "center", width = max_text_width(tax_annots_DF$Subfamily)*0.2),
                     show_annotation_name = TRUE, annotation_name_rot = annot_rot, width = unit(stats_width_abs, "inches"),
                     annotation_name_gp = gpar(fontsize = stats_font_abs), gap=unit(inc_fact_h/4, "inches"))
if(show_protein_stats == "no")
{
  ha1 <- NULL
}
rm(show_protein_stats, stats_lab_font)

if(show_clust_ID == "yes" & show_sil == "yes" & clust_no > 1)
{
  ha2 <- rowAnnotation(Silhouette_width = anno_simple(x = virDF$silhouette_width, border = TRUE, col = col_sil_fun, width = unit(sil_stats_width, "inches")),
                       Genome_cluster_ID = anno_text(x = virDF$genome_cluster_ID, location = 0.5, just = "center", gp = gpar(fill = col_clust, fontsize = font_row, col = "black"),
                                                       width = unit(clustID_width, "inches")),
                       show_annotation_name = TRUE, annotation_name_rot = annot_rot, annotation_name_gp = gpar(fontsize = stats_font_abs), gap=unit(inc_fact_h/4, "inches"))
}

if(show_clust_ID == "yes" & (show_sil == "no" | clust_no == 1))
{
  ha2 <- rowAnnotation(Genome_cluster_ID = anno_text(x = virDF$genome_cluster_ID, location = 0.5, just = "center", gp = gpar(fill = col_clust, fontsize = font_row, col = "black"),
                                                     width = unit(clustID_width, "inches")),
                       show_annotation_name = TRUE, annotation_name_rot = annot_rot, annotation_name_gp = gpar(fontsize = stats_font_abs), gap=unit(inc_fact_h/4, "inches"))
}

if(show_clust_ID == "no" & show_sil == "yes" & clust_no > 1)
{
  ha2 <- rowAnnotation(Silhouette_width = anno_simple(x = virDF$silhouette_width, border = TRUE, col = col_sil_fun, width = unit(sil_stats_width, "inches")),
                       show_annotation_name = TRUE, annotation_name_rot = annot_rot, annotation_name_gp = gpar(fontsize = stats_font_abs), gap=unit(inc_fact_h/4, "inches"))
}

if(show_clust_ID == "no" & (show_sil == "no" | clust_no == 1))
{
  ha2 <- NULL
}

#Only if taxonomy data are available
# ha2 <- rowAnnotation(ICTV_fam = anno_text(tax_annots_DF$Family, location = 0.5, just = "center", width = max_text_width(tax_annots_DF$Family)*0.2, gp = gpar(fill = "#DDDDDD")),
#                      ICTV_subfam = anno_text(tax_annots_DF$Subfamily, location = 0.5, just = "center", width = max_text_width(tax_annots_DF$Subfamily)*0.2),
#                      ICTV_genus = anno_text(tax_annots_DF$Genus, location = 0.5, just = "center", width = max_text_width(tax_annots_DF$Genus)*0.2),
#                      show_annotation_name = TRUE, annotation_name_rot = annot_rot, width = unit(stats_width_abs/4, "inches"),
#                      annotation_name_gp = gpar(fontsize = stats_font_abs))

rm(annot_rot, Genecount_ma, PCs_inThisClust_ma, PCs_only_inThisClust_ma, Proteins_shared_also_Out_Own_GC_ma, Proteins_shared_only_Out_Own_GC_ma)
rm(show_clust_ID, col_clust)


## Width row names ----
row_num_char <- row.names(sim_matrix_DF) %>%
  str_count() %>%
  max()
rown_name_width <- font_row*pix_fact*row_num_char
rm(row_num_char)


## Show_tree and show_heatmap ----
if(show_tree == "yes"){show_tree <- TRUE}
if(show_tree == "no"){show_tree <- FALSE}

if(show_heat == "yes")
{
  show_heatmap_legend <- TRUE
  show_column_names <- TRUE
  border_th <- TRUE
}else
{
  show_heatmap_legend <- FALSE
  show_column_names <- FALSE
  border_th <- FALSE
}
rm(show_heat)

##Heatmap itself ----
sim_matrix_MA <- as.matrix(sim_matrix_DF)

#cutree(hc, k=4)

if(clust_no >= 2)
{
  ht <- Heatmap(sim_matrix_MA, name = ht_name, col = col_sim_fun,  border = border_th, 
                width = unit(ht_width, "inches"),
                height =  unit(max(ht_height, 1), "inches"),
                
                right_annotation = ha1,
                left_annotation = ha2,
                
                show_heatmap_legend = show_heatmap_legend,
                heatmap_legend_param = list(labels = legend_breaks, legend_direction = "horizontal", 
                                            legend_width = unit(lgd_width_abs, "inches"), grid_height = unit(lgd_height_abs, "inches"),
                                            title_position = lgd_pos, border = "#666666",
                                            title_gp = gpar(fontsize = lgd_font_abs), labels_gp = gpar(fontsize = lgd_lab_font_abs)),
                
                #cluster rows
                cluster_rows = hc, row_dend_side = "left", row_dend_width = unit(tree_width_abs, "inches"), 
                show_row_dend = show_tree, 
                row_split = clust_no, 
                row_gap = unit(inc_fact_h/4, "inches"), row_title_rot = 90, row_title = "Clustering tree",
                show_row_names = TRUE, row_names_gp = gpar(fontsize = font_row),
                row_names_max_width = unit(rown_name_width, "inches"),
                
                cluster_columns = FALSE,
                show_column_names = show_column_names, column_names_gp = gpar(fontsize = font_col))
}

if(clust_no == 1)
{
  ht <- Heatmap(sim_matrix_MA, name = ht_name, col = col_sim_fun,  border = border_th,
                width = unit(ht_width, "inches"),
                height =  unit(max(ht_height, 1), "inches"),
                
                right_annotation = ha1,
                left_annotation = ha2,
                
                show_heatmap_legend = show_heatmap_legend,
                heatmap_legend_param = list(labels = legend_breaks, legend_direction = "horizontal",
                                            legend_width = unit(lgd_width_abs, "inches"), grid_height = unit(lgd_height_abs, "inches"),
                                            title_position = lgd_pos, border = "#666666",
                                            title_gp = gpar(fontsize = lgd_font_abs), labels_gp = gpar(fontsize = lgd_lab_font_abs)),
                
                #cluster rows
                cluster_rows = hc, row_dend_side = "left", row_dend_width = unit(tree_width_abs, "inches"),
                show_row_dend = show_tree,
                row_gap = unit(inc_fact_h/4, "inches"), row_title_rot = 90, row_title = "Clustering tree",
                show_row_names = TRUE, row_names_gp = gpar(fontsize = font_row),
                row_names_max_width = unit(rown_name_width, "inches"),
                
                cluster_columns = FALSE,
                show_column_names = show_column_names, column_names_gp = gpar(fontsize = font_col))
}

rm(inc_fact_h, show_column_names, show_tree, border_th, col_sim_fun, font_row, ha1, ha2, legend_breaks, lgd_pos)
rm(show_heatmap_legend, sim_matrix_MA, hc)

##set PDF dimensions ----
col_num_char <- names(sim_matrix_DF) %>%
  str_count() %>%
  max()
rm(sim_matrix_DF)

num_char_stats <- str_count("Clustered_PCs_alsoOutOwn_GenomeClust") 
num_char_leg <- str_count(ht_name)

pdf_heigth <- 1.2*(pix_fact*col_num_char*font_col + 2*ht_height + lgd_height_abs + pix_fact*num_char_leg*lgd_font_abs + pix_fact*lgd_lab_font_abs + num_char_stats*pix_fact*stats_font_abs)
rm(col_num_char, num_char_stats, num_char_leg, ht_name, font_col, ht_height, lgd_height_abs, stats_font_abs)

pdf_width <- 1.2*(rown_name_width + ht_width + lgd_width_abs + stats_width_abs + tree_width_abs + sil_stats_width + clustID_width + 10*pix_fact*lgd_font_abs + 10*pix_fact*lgd_lab_font_abs)
rm(rown_name_width, ht_width, lgd_width_abs, stats_width_abs, tree_width_abs, sil_stats_width, clustID_width, pix_fact, lgd_font_abs, lgd_lab_font_abs)


# drawheatmap ----
pdf(file = paste0(heatmap_dir, "/06-Heatmap_", pc_type, ".PDF"), width = pdf_width, height = pdf_heigth)

if(show_sil == "no" | clust_no == 1)
{
  plot0 <- draw(ht)
}else
{
  plot0 <- draw(ht, annotation_legend_list = list(lgd_sil))
  rm(lgd_sil)
}


dev.off()

rm(pdf_heigth, pdf_width, show_sil, plot0, heatmap_dir, clust_no)
