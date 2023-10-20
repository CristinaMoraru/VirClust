library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
library(purrr, warn.conflicts = FALSE, quietly = FALSE)
library(grid, warn.conflicts = FALSE, quietly = FALSE)
suppressPackageStartupMessages(library(ComplexHeatmap, warn.conflicts = FALSE, quietly = FALSE))
library(stringr, warn.conflicts = FALSE, quietly = FALSE)
suppressPackageStartupMessages(library(dendextend, warn.conflicts = FALSE, quietly = FALSE))
#suppressPackageStartupMessages(library(circlize, warn.conflicts = FALSE, quietly = FALSE))

#####Load paths and RDS ----
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)
#only for manual run 
# input0 <- ""
input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P867_Roseo_round4_CLM"
input0[2] <- "pc_type=PC"
input0[3] <- "inc_fact_w=0.3"
input0[4] <- "font_row=12"
input0[5] <- "font_col=12"
input0[6] <- "stats_font=0.8" ##
input0[7] <- "stats_lab_font=0.5" ##
input0[8] <- "stats_width=0.2"
input0[9] <- "lgd_width=1.5" ##
input0[10] <- "lgd_height=0.09" ##
input0[11] <- "lgd_font=0.05" ##
input0[12] <- "lgd_pos=leftcenter-rot"
input0[13] <- "lgd_lab_font=0.04"  ##
input0[14] <- "functions_path=/bioinf/shiny-server/VirClust/vir_clust_standalone/VirClust_functions.R"
input0[15] <- "font_cell=6"

#sourcing the functions file -----------
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)


# P_D ---
P_D <- get_params_simple_fun(input0 = input0, param_name = "projdir", type = "string")
pc_type <- get_params_simple_fun(input0, "pc_type", type = "string")

if(pc_type == "PC")
{
  branch_dir <- paste0(P_D, "/04a-06a_genome_clustering_PC")
}
if(pc_type == "PSC")
{
  branch_dir <- paste0(P_D, "/04b-06b_genome_clustering_PSC")
}
if(pc_type == "PSSC")
{
  branch_dir <- paste0(P_D, "/04c-06c_genome_clustering_PSSC")
}
heatmap_dir <- paste0(branch_dir, "/06_VGCs")
dir.create(heatmap_dir)
rm(P_D)

#in and out folders and files ------

in05VGC_dir <- paste0(branch_dir, "/05/VGCs_dist")

virDF_p_ls <- list.files(in05VGC_dir, pattern = "_stats.RDS", full.names = TRUE)
virDF_ls <- map(.x = virDF_p_ls, .f = readRDS)

distMA_p_ls <- str_replace(virDF_p_ls, pattern = "_stats.RDS", replacement = "_dist.RDS")
distMA_ls <- map(.x = distMA_p_ls, .f = readRDS)


#### heatmap options ----
inc_fact_w <- get_params_simple_fun(input0, "inc_fact_w", type = "numeric")
font_row <- get_params_simple_fun(input0, "font_row", type = "numeric")
font_col <- get_params_simple_fun(input0, "font_col", type = "numeric")
font_cell <- get_params_simple_fun(input0, "font_cell", type = "numeric")

#### anots options ----
stats_font <- get_params_simple_fun(input0, "stats_font", type = "numeric")
stats_lab_font <- get_params_simple_fun(input0, "stats_lab_font", type = "numeric") 
stats_width <- get_params_simple_fun(input0, "stats_width", type = "numeric") 

#### legend options ----
lgd_width <- get_params_simple_fun(input0, "lgd_width", type = "numeric") 
lgd_height <- get_params_simple_fun(input0, "lgd_height", type = "numeric") 
lgd_font <- get_params_simple_fun(input0, "lgd_font", type = "numeric") 
lgd_pos <- get_params_simple_fun(input0, "lgd_pos", type = "string") 
lgd_lab_font <- get_params_simple_fun(input0, "lgd_lab_font", type = "numeric")


rm(input0)


### Ploting params --------
###ht params
inc_fact_h <- inc_fact_w #0.2 #one cell has 0.3 inches in heigth
pix_fact <- 0.014  # 
annot_rot <- 90

###set colors
sim_cols <- RColorBrewer::brewer.pal(n = 9, name = "Oranges") #PuBuGn
colors <- c("#FFFFFF", sim_cols) #input$sim_palette
breaks <- c(0, 19.9999, 39.9999, 49.9999, 59.9999, 69.9999, 79.9999, 89.9999, 94.9999, 100)
legend_breaks <- c(0, 20, 40, 50, 60, 70, 80, 90, 100)
col_sim_fun <- circlize::colorRamp2(breaks = breaks, colors = colors)




#Ploting heatmaps ----
for(i in 1:length(distMA_ls))
{
  distMA <- distMA_ls[[i]] %>%
    as.matrix()
  distMA <- 100-(distMA*100)
  
  #htname
  VGCname <- str_remove(distMA_p_ls[i], "_dist.RDS") %>%
    str_remove(paste0(in05VGC_dir, "/"))
  htname <- paste0(pc_type, "_based_Intergenomic_Distances_", VGCname)
  
  #anots
  stats_width_abs <- max(0.5, stats_width * inc_fact_w*length(distMA_ls[[i]]))
  stats_font_abs <- ceiling((stats_font*stats_width_abs)/(5*pix_fact))
  stats_lab_font_abs <- ceiling((stats_lab_font*stats_width_abs)/(5*pix_fact))
  
  ## Width row names ----
  row_num_char <- row.names(distMA_ls[[i]]) %>%
    str_count() %>%
    max()
  rown_name_width <- font_row*pix_fact*row_num_char
  rm(row_num_char)
  
  #legend params ----
  lgd_width_abs <- max(0.1, lgd_width*inc_fact_w*length(distMA_ls[[i]]))
  lgd_height_abs <- max(0.03, lgd_height*inc_fact_h*nrow(distMA_ls[[i]]))
  lgd_font_abs <- max(1, ceiling(lgd_font*inc_fact_w*nrow(distMA_ls[[i]])/pix_fact))
  lgd_lab_font_abs <- max(1, ceiling(lgd_lab_font*inc_fact_w*nrow(distMA_ls[[i]])/pix_fact))
  
  ha1 <- rowAnnotation(Genome_length= anno_barplot(x = virDF_ls[[i]]$length, border = FALSE, baseline = 0, 
                                                   axis_param = list(gp=gpar(fontsize = unit(stats_lab_font_abs, "inches")))),
                       Total_PCs= anno_barplot(x = virDF_ls[[i]]$gene_count, border = FALSE, baseline = 0, 
                                                   axis_param = list(gp=gpar(fontsize = unit(stats_lab_font_abs, "inches")))),
                       show_annotation_name = TRUE, annotation_name_rot = annot_rot, width = unit(stats_width_abs, "inches"),
                       annotation_name_gp = gpar(fontsize = stats_font_abs), gap=unit(inc_fact_h/4, "inches"))
  
  #cell dimmensions
  ht_width <- inc_fact_w*length(distMA_ls[[i]])
  ht_height <- inc_fact_h*nrow(distMA_ls[[i]])
  
  #heatmap object
  ht <- Heatmap(matrix = distMA, name = htname, col = col_sim_fun, #rect_gp = gpar(type = "none"),
                width = unit(ht_width, "inches"),
                height =  unit(ht_height, "inches"),
                
                ##data
                cell_fun = function(j, i, x, y, width, height, fill) {
                  grid.text(sprintf("%.1f", distMA[i,j]), x, y, gp = gpar(fontsize = font_cell))},
                
                
                #annotation
                right_annotation = ha1,
                
                #colors heatmap
                show_heatmap_legend = TRUE,
                heatmap_legend_param = list(labels = legend_breaks, at = legend_breaks, legend_direction = "vertical", 
                                            legend_width = unit(lgd_width_abs, "inches"), grid_height = unit(lgd_height_abs, "inches"),
                                            title_position = lgd_pos, border = "#666666",
                                            title_gp = gpar(fontsize = lgd_font_abs), labels_gp = gpar(fontsize = lgd_lab_font_abs)),
                ##rows and columns
                cluster_rows = FALSE, 
                show_row_names = TRUE, row_names_gp = gpar(fontsize = font_row),
                row_names_max_width = unit(rown_name_width, "inches"),
                
                cluster_columns = FALSE,
                show_column_names = TRUE, column_names_gp = gpar(fontsize = font_col)
                #col_names_max_width = unit(rown_name_width, "inches")
                
  )
  
  
  ##set PDF dimensions ----
  col_num_char <- names(distMA_ls[[i]]) %>%
    str_count() %>%
    max()
  
  num_char_stats <- str_count("Genome Length") 
  num_char_leg <- str_count(htname)
  
  pdf_heigth <- 1.2*(pix_fact*col_num_char*font_col + 2*ht_height + lgd_height_abs + pix_fact*num_char_leg*lgd_font_abs + pix_fact*lgd_lab_font_abs + num_char_stats*pix_fact*stats_font_abs)
  
  pdf_width <- 1.2*(rown_name_width + ht_width + lgd_width_abs + stats_width_abs + 10*pix_fact*lgd_font_abs + 10*pix_fact*lgd_lab_font_abs)
  
  
  ##save PDF
  pdf(file = paste0(heatmap_dir, "/Dist_heatmap_", pc_type, "_", VGCname, ".PDF"), width = pdf_width, height = pdf_heigth, onefile = FALSE)
  plot0 <- draw(ht)
  
  dev.off()
  #dev.next(which = i)
  #graphics.off()
  
  rm(distMA, VGCname, htname, stats_width_abs, stats_font_abs, stats_lab_font_abs, rown_name_width, 
     lgd_width_abs, lgd_height_abs, lgd_font_abs, lgd_lab_font_abs, ha1, ht_width, ht_height, ht,
     col_num_char, num_char_stats, num_char_leg, pdf_heigth, pdf_width, plot0)
     
}
rm(i)





