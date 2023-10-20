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
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P867_Roseo_round4_CLM"
# input0[2] <- "pc_type=PC"
# input0[3] <- "inc_fact_w_Pd=0.3"
# input0[4] <- "font_row_Pd=12"
# input0[5] <- "font_col_Pd=12"
# input0[9] <- "lgd_width_Pd=1.5" ##
# input0[10] <- "lgd_height_Pd=0.09" ##
# input0[11] <- "lgd_font_Pd=0.05" ##
# input0[12] <- "lgd_pos_Pd=leftcenter-rot"
# input0[13] <- "lgd_lab_font_Pd=0.04"  ##
# input0[14] <- "functions_path=/bioinf/shiny-server/VirClust/vir_clust_standalone/VirClust_functions.R"
# input0[15] <- "font_cell_Pd=6"

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
heatmap_dir <- paste0(branch_dir, "/04")
#dir.create(heatmap_dir)

rm(P_D)

#in file ------
distMA_p <- paste0(heatmap_dir, "/MyDistPCs_MA_ordered.RDS")

#### heatmap options ----
inc_fact_w <- get_params_simple_fun(input0, "inc_fact_w", type = "numeric")
font_row <- get_params_simple_fun(input0, "font_row", type = "numeric")
font_col <- get_params_simple_fun(input0, "font_col", type = "numeric")
font_cell <- get_params_simple_fun(input0, "font_cell", type = "numeric")

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



#Ploting the heatmap ----
distDF <-  readRDS(distMA_p)
distMA <- distDF%>%
  as.matrix()
distMA <- 100-(distMA*100)

#htname
htname <- paste0(pc_type, "_based_Intergenomic_Distance")

## Width row names ----
row_num_char <- row.names(distDF) %>%
  str_count() %>%
  max()
rown_name_width <- font_row*pix_fact*row_num_char
rm(row_num_char)

#legend params ----
lgd_width_abs <- max(0.1, lgd_width*inc_fact_w*length(distDF))
lgd_height_abs <- max(0.03, lgd_height*inc_fact_h*nrow(distDF))
lgd_font_abs <- max(1, ceiling(lgd_font*inc_fact_w*nrow(distDF)/pix_fact))
lgd_lab_font_abs <- max(1, ceiling(lgd_lab_font*inc_fact_w*nrow(distDF)/pix_fact))

#cell dimmensions
ht_width <- inc_fact_w*length(distDF)
ht_height <- inc_fact_h*nrow(distDF)

#heatmap object
ht <- Heatmap(matrix = distMA, name = htname, col = col_sim_fun, #rect_gp = gpar(type = "none"),
              width = unit(ht_width, "inches"),
              height =  unit(ht_height, "inches"),
              
              ##data
              cell_fun = function(j, i, x, y, width, height, fill) {
                grid.text(sprintf("%.1f", distMA[i,j]), x, y, gp = gpar(fontsize = font_cell))},
              
              
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
col_num_char <- names(distDF) %>%
  str_count() %>%
  max()

num_char_leg <- str_count(htname)

pdf_heigth <- 1.2*(pix_fact*col_num_char*font_col + 2*ht_height + lgd_height_abs + pix_fact*num_char_leg*lgd_font_abs + pix_fact*lgd_lab_font_abs )

pdf_width <- 1.2*(rown_name_width + ht_width + lgd_width_abs + 10*pix_fact*lgd_font_abs + 10*pix_fact*lgd_lab_font_abs)


##save PDF
pdf(file = paste0(heatmap_dir, "/Dist_heatmap_", pc_type, "_all_genomes", ".PDF"), width = pdf_width, height = pdf_heigth, onefile = FALSE)
plot0 <- draw(ht)

dev.off()
#dev.next(which = i)
#graphics.off()

rm(distMA, htname, rown_name_width, 
   lgd_width_abs, lgd_height_abs, lgd_font_abs, lgd_lab_font_abs, ht_width, ht_height, ht,
   col_num_char, num_char_leg, pdf_heigth, pdf_width, plot0)






