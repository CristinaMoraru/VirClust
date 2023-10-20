library(DT)
library(shiny)
#library(shinyWidgets)
#library(shinythemes)
library(shinyjs)



shinyUI(
  tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
      #### Top website ----
      title= #"Moraru",
        a(h4("\u00A0The Moraru Phage Lab\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", style="color:#000000"),
          href = "http://moraru-phage-lab.icbm.de/", style="text-decoration: none"),
      position="fixed-top",
      #collapsible = "TRUE",
      windowTitle="VirClust - Protein clusters for viruses",
      theme = shinythemes::shinytheme("cerulean"),
      
      #### HOME ----
      tabPanel("HOME", 
               fluidRow(h1(" ")),
               fluidRow(
                 column(12, offset = 0,
                        tags$div(style = "text-align:justify;", #background-color: #eaf2f8; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 1px",
                                 h1(" "),
                                 fluidRow(column(10, offset = 1, 
                                                 shiny::tags$div(style = "background-color: #DAB1EE; width: 100%; border-radius: 5px;",
                                                                 fluidRow(h1(" ")),
                                                                 fluidRow(column(10, offset = 1, h4("VirClust is now oppened for community feedback"))),
                                                                 fluidRow(h1(" ")),
                                                                 fluidRow(column(10, offset =1, p("This is a new and higly improved version of VirClust. Testing is in progress. 
                                                            Feedback from the user community is more than welcomed. Please email me at liliana.cristina.moraru( at )uol.de 
                                                            if you experience any problems or you have suggestions for improvements."))),
                                                                 fluidRow(column(10, offset =1, p("The stand-alone version is in work at the moment and not yet available for download.")))
                                                 ))),
                                 h1(" "),
                                 fluidRow(column(10, offset = 1, h4("Whats is VirClust?"))),
                                 h1(" "),
                                 fluidRow(column(6, offset = 1,
                                                 fluidRow(column(10, offset = 1, p("VirClust is a bioinformatics tool which can be used for: "))),
                                                 fluidRow(column(4, offset=2, 
                                                                 p("• virus clustering"),
                                                                 p("• protein annotation"),
                                                                 p("• core protein calculation"))#,
                                                          #column(3, offset = 0, shiny::tags$img(src = "heatmap.PNG", width = "539px", height = "265px"))
                                                 ),
                                                 fluidRow(column(10, offset = 1, p("At its core is the grouping of viral proteins into clusters of three different levels: "))),
                                                 fluidRow(column(10, offset = 2,
                                                                 p("• at the first level, proteins are grouped based on their reciprocal BLASTP similarities into protein clusters, or PCs."),
                                                                 p("• at the second level, PCs are grouped based on their Hidden Markov Model (HMM) similarities into protein superclusters, or PSCs. "),
                                                                 p("• at the third, still experimental level, PSCs are grouped based on their HMM similarities into protein super-superclusters, or PSSC. ")
                                                 )),
                                                 fluidRow(column(10, offset = 1, p("More about the how it works can be read here ", shiny::tags$a(href= "https://doi.org/10.1101/2021.06.14.448304", "DOI: 10.1101/2021.06.14.448304.",  target = "_blank"))))
                                                 
                                 ),
                                 column(3, offset = 0, shiny::tags$img(src = "heatmap.PNG", width = "539px", height = "265px"))
                                 ),
                                 h1(" "),
                                 fluidRow(column(10, offset = 1, h4("How to cite the use of VirClust?"))),
                                 h1(" "),
                                 fluidRow(column(10, offset = 1, 
                                                 p("If you are using VirClust, please cite the following pre-print publication: • Moraru, Cristina (2021): VirClust, a tool for hierarchical clustering, core gene detection and annotation of (prokaryotic) viruses. In BioRxiv.", 
                                                   shiny::tags$a(href= "https://doi.org/10.1101/2021.06.14.448304", "DOI: 10.1101/2021.06.14.448304.",  target = "_blank")),
                                                 p("Additionally, if you are performing viral protein annotations using VirClust, please also cite the respective databases used for the annotations, see VirClust manuscript for the complete citations")
                                 )),
                                 h1(" ")
                        )
                 )
               ),
               fluidRow(),
               h1(" "),
               ##### Bottom from home ----
               fluidRow(
                 column(12, offset = 0,
                        tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 5px;",
                                 fluidRow(
                                   column(2, offset=1, tags$img(src = "https://www.uni-oldenburg.de/img/orga/f5icbm/kopflogo.png", width = "70px", height = "90px"), tags$img(src = "UniOld.png", width = "70px", height = "70px",  align = "center")),
                                   column(7,
                                          h1(" "),
                                          h5(tags$b("    Developer - Cristina Moraru, PhD")),
                                          p("Senior Scientist, Department of The Biology of Geological Processes"),
                                          p("Institute for Chemistry and Biology of the Marine Environment"),
                                          p("Carl-von-Ossietzky –Str. 9 -11, D-26111 Oldenburg, Germany"),
                                          p(tags$b("Email:"), "liliana.cristina.moraru(     at     )uni-oldenburg.de"),
                                          h1(" ")
                                   )
                                 )
                        )
                 )
               )
      ),
      #### VirClust WEB----
      tabPanel("VirClust WEB",
               #fluidPage(
               useShinyjs(),
               ##### Header ----
               tags$style(type="text/css", "body {padding-top: 70px;}"),
               # fluidRow(
               #     column(12, offset = 0,
               #            tags$div(style = "background-color: #e8f3fd; width: 100%; border-radius: 8px;",
               #                            # fluidRow(
               #                            #   h1(" "),
               #                            #   column(10, offset = 1, p("VirClust calculates cluster of viral proteins and annotates them. To run it, upload the phage genomes or the correponding proteins, create a project and press run. Save the project ID that will be displayed when the project is created. You will need it to access the data if the calculations take a long time. If you provide an email address, the project ID will be sent to you.")
               #                            #   )),
               #                            fluidRow(
               #                                h1(" "),
               #                                column(10, offset = 1, h5("VirClust is still in development. If you encounter errors or you have suggestions, please report them to me - see email below.")
               #                                ))
               #            ))),
               h1(" "),
               
               fluidRow(
                 column(10, offset = 1,
                        
                        #### Define project ----
                        tabsetPanel(
                          
                          tabPanel("I. DEFINE PROJECT",
                                   
                                   useShinyjs(),
                                   h1(" "),
                                   fluidRow(
                                     tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 1px",
                                              h1(" "),
                                              fluidRow(
                                                ##### Create new project -----
                                                column(5, offset = 1,tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                              fluidRow(column(12, offset=1, h4("CREATE NEW PROJECT"))),
                                                                              fluidRow(
                                                                                column(5, offset = 1, textInput("User_name", label = "User name*", value = "minimum 6 characters")),
                                                                                column(5, offset = 0, textInput("Project_name", label = "Project name*", value = "minimum 6 characters"))
                                                                              ),
                                                                              # fluidRow(
                                                                              #   column(10, offset = 1,  checkboxInput("want_email1", label = "Receive an email notification with the ProjectID", value = FALSE, width= "100%"))
                                                                              # ),
                                                                              # fluidRow(
                                                                              #   column(10, offset = 1, textInput("email_1", label = "User email", value = ""))
                                                                              # ),
                                                                              
                                                                              fluidRow(
                                                                                column(10, offset = 1, radioButtons("in_type", "Input type", 
                                                                                                                    choices = c("Nucleic acids, all genomes in a fasta file", "Nucleic acids, one genome per file**"))) #, "Proteins, one fasta file per genome"
                                                                              ),
                                                                              fluidRow(column(10, offset =1, p("Minimum number of genomes:"))),
                                                                              fluidRow(column(10, offset =2, p("• 3 for genome clustering (steps 4-7)"))),
                                                                              fluidRow(column(10, offset =2, p("• 1 for only protein clusters and annotations"))),
                                                                              fluidRow(column(10, offset =1, p("Accepted input formats: .fasta, .fna or .fa"))),
                                                                              fluidRow(column(10, offset =1, p("Sequence names should contain at least one letter."))),
                                                                              fluidRow(column(10, offset =1, p("** no multifasta files here"))),
                                                                              fluidRow(
                                                                                column(10, offset = 1, fileInput("Upload_genome", label = "Upload input*", multiple = TRUE))
                                                                              ),
                                                                              fluidRow(
                                                                                column(3, offset = 1, actionButton("Create", label = "Create project")),
                                                                                column(3, offset = 1, actionLink("Reset", label = a(h5("Reset Project"), target = "_top", href =paste0("htpp://VirClust.icbm.de/?page=3&a=", Sys.time()))))
                                                                              ),
                                                                              fluidRow(
                                                                                column(10, offset = 1, p("* Mandatory for new projects"))
                                                                              ),
                                                                              fluidRow(h1(" "), h1(" "))
                                                                              
                                                )) ,
                                                
                                                ##### Load project -----
                                                column(5, offset = 0, 
                                                       fluidRow(tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                         fluidRow(column(12, offset=1, h4("LOAD EXISTING PROJECT"))),
                                                                         fluidRow(column(10, offset = 1, textInput("Proj_ID_load", label = "Input a project ID", value = ""))),
                                                                         fluidRow(column(10, offset = 1, actionButton("Load", "Load project"))),
                                                                         fluidRow(h1(" "), h1(" "))
                                                       )),
                                                       fluidRow(h1(" "), h1(" ")),
                                                       fluidRow(tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 5px;border-style: solid; border-color: #c1d5e0; border-width: 1px",
                                                                         fluidRow(column(12, offset=1, h4("Info board"))),
                                                                         fluidRow(column(10, offset = 1, tags$div(style = "text-align: justify;", p("Each project you create is given a project ID and can be accessed at a later time point, as long as you have performed any calculations (basically, pressed the “Run” button in the next tab). VirClust calculations can take a long time and the browser can disconnect from the server. 
                                                                                                                                                    Save the project ID, to be able to access the results later.")))),
                                                                         fluidRow(h1("  "), h1("  ")),
                                                                         # fluidRow(column(10, offset = 1, tags$div(style = "background-color: #DAB1EE; text-align: justify;", p("Our server is currently experiencing a high load. This can interrupt some VirClust processes. If more than one day has passed after you have started your VirClust project and you have not received an email that it has finished, please email me. I will manually reset you project, 
                                                                         #                                                                            and you will be able to access the data already calculated and to continue with further calculations. Meanwhile, we are working on a more sustainable solution.")))),
                                                                         fluidRow(column(12, offset = 1, textOutput("fasta_val"))),
                                                                         fluidRow(h1("  "), h1("  ")),
                                                                         fluidRow(column(12, offset = 1, h5(textOutput("Current")))),
                                                                         fluidRow(column(12, offset = 1, textOutput("Proj_ID"))),
                                                                         fluidRow(column(12, offset = 1, tags$div(style = "color:blue", textOutput("Status")))),
                                                                         fluidRow(h1("  "), h1("  "))
                                                       ))
                                                )
                                              ),
                                              h1(" ")
                                     )
                                   )),
                          
                          #### Run calculations ----
                          tabPanel("II. RUN CALCULATIONS",
                                    useShinyjs(), 
                                    tabsetPanel(id ="Branches", 
                                                ##### BRANCH A --------------
                                               tabPanel("Branch A - based on Protein Clusters",
                                                        h1(" "),
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("PROTEIN CLUSTERING"))),
                                                                   ##Step 1A ----------
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 1A. Genomes to Proteins"))),
                                                                                     fluidRow(column (10, offset = 1,
                                                                                                      selectInput("Genetic_code", "Translation table",
                                                                                                                  choices = c("1 --- standard",
                                                                                                                              "2 --- vertebrate mitochondrial",
                                                                                                                              "3 --- yeast mitochondrial",
                                                                                                                              "4 --- protozoan mitochondrial and mycoplasma",
                                                                                                                              "5 --- invertebrate mitochondrial",
                                                                                                                              "6 --- ciliate and dasycladaceal",
                                                                                                                              "9 --- echinoderm and flatworm mitochondrial",
                                                                                                                              "10 --- euplotid",
                                                                                                                              "11 --- bacteria, archaea, prokaryotic viruses and plant plastid",
                                                                                                                              "12 --- alternative yeast",
                                                                                                                              "13 --- ascidian mitochondrial",
                                                                                                                              "14 --- alternative flatworm mitochondrial",
                                                                                                                              "15 --- blepharism",
                                                                                                                              "16 --- chlorophycean mitochondrial",
                                                                                                                              "21 --- trematode mitochondrial",
                                                                                                                              "22 --- scenedesmus mitochondrial",
                                                                                                                              "23 --- thraustochytrium mitochondrial",
                                                                                                                              "24 --- Pterobranchia mitochondrial",
                                                                                                                              "25 --- Candidate Division SR1 and Gracilibacteria",
                                                                                                                              "26 --- Pachysolen tannophilus"),
                                                                                                                  selected = "11 --- bacteria, archaea, prokaryotic viruses and plant plastid",
                                                                                                                  multiple = FALSE, width = "100%"))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actGenom_2_Prot", label = "Start this step ")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done1A")))), 
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_1A"))),

                                                                                     fluidRow(h1("  ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(3, offset = 1, downloadButton("Down_1A_single_faas", "one protein file per genome")),
                                                                                              column(3, offset = 1 , downloadButton("Down_1A_all_faa", "all proteins in a single file")),
                                                                                              column(3, offset = 0 , downloadButton("Down_1A_Table", "genome and protein table"))),
                                                                                     fluidRow(h1("  "))
                                                                            )
                                                                     )),
                                                                   fluidRow(h1("  ")),
                                                                   #Step 2A -----------
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 2A. Proteins to Protein Clusters (PCs)"))),
                                                                                     fluidRow(column(6, offset = 1, selectInput("clust_PC", label = "Cluster based on", choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"),
                                                                                                                                selected = "evalue_log"))),
                                                                                     fluidRow(column(10, offset =1, h5("Remove matches if"))),
                                                                                     fluidRow(column(2, offset = 1, numericInput("eval_PC", label = "e-value >", value = 0.00001, min = 0, max = 0.01, step = 0.00001)),
                                                                                              column(2, offset = 0, numericInput("bitsc_PC", label = "bitscore <", value = 50, min = 20, step=1)),
                                                                                              column(2, offset = 0, numericInput("cov_PC", label = "coverage <", value = 0, min = 0, max = 100)),
                                                                                              column(2, offset = 0, numericInput("pident_PC", label = "% identity <", value = 0, min = 0, max = 100))
                                                                                     ),


                                                                                     fluidRow(
                                                                                       column(6, offset = 1, actionButton("actProt_2_PCs", label = "Start this step")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done2A")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_2A"))),
                                                                                     fluidRow(h1("  ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(10, offset = 1, downloadButton("Down_2A_Table", "genome and protein table"))),
                                                                                     fluidRow(h1(" "))
                                                                            ))))),
                                                        fluidRow(h1(" ")),
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("GENOME CLUSTERING"))),

                                                                   #Step 3A -----
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 3A. Order genomes hierarchically"))),
                                                                                     fluidRow(column(5, offset = 1, selectInput("aglom_a", label = "Agglomeration method", choices = c("complete", "average"), selected = "complete", multiple = FALSE))),
                                                                                     fluidRow(column(5, offset = 1, checkboxInput("boot_a", label = "Enable bootstrapping", value = FALSE, width = "100%"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("boot_no_a", label = "Number of bootstraps", min = 2, max = 1000, value = 100, step = 1))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCluster_genomes_PC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done3A")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_3A"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(5, offset = 1 , downloadButton("Down_3A_tree", "tree (s)")),
                                                                                              column(5, offset = 0 , downloadButton("Down_3A_distMA", "intergenomic distance matrix"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1,
                                                                                              tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                       fluidRow(column(10, offset = 1, h4("Plot intergenomic similarities"))),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("inc_fact_w_Pd_A", label = "Cell width", min = 0.02, max = 1, value = 0.3, step = 0.001))),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("font_row_Pd_A", label = "Row font", value = 12, min=1)),
                                                                                                                column(3, offset = 0, numericInput("font_col_Pd_A", label = "Column font", value = 12, min=1)),
                                                                                                                column(3, offset = 0, numericInput("font_cell_Pd_A", label = "Cell font", value = 6, min=1))
                                                                                                                ),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("lgd_font_Pd_A", label = "Legend font", value = 5, min=1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_lab_font_Pd_A", label = "Legend label font", value = 4, min = 1))
                                                                                                                
                                                                                                       ),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("lgd_height_Pd_A", label = "Legend heigth", value = 9, min = 1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_width_Pd_A", label = "Legend width", value = 15, min=1))
                                                                                                       ),
                                                                                                       fluidRow(h1(" ")),
                                                                                                       fluidRow(
                                                                                                         column(5, offset = 1, actionButton("actOut_pdf_3A", label = "Generate plot")),
                                                                                                         column(3, offset = 0, tags$div(style = "color:green", textOutput("done3A_Plot")))),
                                                                                                       fluidRow(column(10, offset = 1, textOutput("mes_3A_Plot"))),
                                                                                                       fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                       fluidRow(column(5, offset = 1, downloadButton("Down_3A_PDF", "Similarity heatmap PDF"))),
                                                                                                       fluidRow(h1(" "))
                                                                                                       )
                                                                                              )),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(h1(" "))
                                                                            )
                                                                     )
                                                                   ),
                                                                   fluidRow(h1(" ")),

                                                                   #Step 4A ------
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 4A. Calculate stats and split in genome clusters (VGCs)"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("Clust_dist_PC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.9, step = 0.01)),
                                                                                              column(5, offset = 0, numericInput("sel_PCs_heatmap_a", label = "Show only common PCs if >:",
                                                                                                                                 value = 3000, min =1))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actSplit_clust_PC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done4A")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_4A"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(3, offset = 1, downloadButton("Down_4A_genPCs_Table", "genomes vs PCs table")),
                                                                                              column(3, offset = 0, downloadButton("Down_4A_stats_Table", "cluster stats")),
                                                                                              column(3, offset = 0, downloadButton("Down_4A_VGCs", "genome clusters"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1, p("*The clustering distance is minimum 0.1 and maximum 1. The higher the value, the lower the number of clusters resulted. At a value of 1, all genomes will belong to the same cluster."))),
                                                                                     fluidRow(column(10, offset=1, p("*Known issues: If the chosen clustering distance results in each genome forming its own VGC, then the output PDF will be empty. To solve this problem: increase the clustering distance progressively and recalculate steps 5 and 6."))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1,
                                                                                                     tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                              fluidRow(column(10, offset = 1, h4("Output genome clustering PDF"))),
                                                                                                              fluidRow(
                                                                                                                column(5, offset =1, checkboxInput("Show_Tree_a", label = "Show tree", value = TRUE))
                                                                                                              ),
                                                                                                              fluidRow(
                                                                                                                column(3, offset = 2, numericInput("tree_width_a", label = "Tree width", min = 1, max = 100, value = 30))
                                                                                                              ),
                                                                                                              
                                                                                                              fluidRow(
                                                                                                                column(3, offset = 1, checkboxInput("show_clust_ID_a", label = "Show VGC ID", value = TRUE)),
                                                                                                                column(3, offset = 0, checkboxInput("show_sil_a", label = "Show silhoutte width", value = TRUE)),
                                                                                                                column(3, offset = 0, checkboxInput("show_protein_stats_a", label = "Show protein stats", value = TRUE))
                                                                                                              ),
                                                                                                              fluidRow(
                                                                                                                column(3, offset = 2, numericInput("clustID_width_a", label = "Width of VGC ID column", value = 10, min=1)),
                                                                                                                column(3, offset = 0, numericInput("Sil_stats_width_a", label = "Width of silhoutte column", value = 5, min=1)),
                                                                                                                column(3, offset = 0, numericInput("Stats_width_a", label = "Width of protein stats", min = 1, max = 100, value = 30))),
                                                                                                              
                                                                                                              fluidRow(
                                                                                                                column(3, offset = 2, numericInput("stats_font_a", label = "Font stats name", value = 5, min=1)),
                                                                                                                column(3, offset = 0, numericInput("stats_lab_font_a", "Font stats axis", value = 5, min=1))),
                                                                                                              
                                                                                                              fluidRow(
                                                                                                                column(2, offset =1, checkboxInput("show_heat_a", label = "Show heatmap", value = TRUE))),
                                                                                                              
                                                                                                              fluidRow(
                                                                                                                column(3, offset = 2, numericInput("Heat_width_a", label = "Column width (inches)", min = 0.005, max = 1, value = 0.03, step = 0.001)),
                                                                                                                column(3, offset = 0, numericInput("font_col_a", label = "Font PC names", value = 2, min = 1))
                                                                                                              ),
                                                                                                              
                                                                                                              fluidRow(column(6, offset = 1, p("Other options"))),
                                                                                                              
                                                                                                              fluidRow(column(3, offset = 2, numericInput("font_row_a", label = "Font genomes/VGCs IDs ", value = 12, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_font_a", label = "Legend font", value = 20, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_lab_font_a", label = "Legend label font", value = 30, min = 1))
                                                                                                                       
                                                                                                              ),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("lgd_h_a", label = "Legend heigth", value = 5, min = 1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_w_a", label = "Legend width", value = 10, min=1))
                                                                                                              ),
                                                                                                              
                                                                                                              fluidRow(h1(" ")),
                                                                                                              fluidRow(
                                                                                                                column(5, offset = 1, actionButton("actOut_pdf_4A", label = "Generate plot")),
                                                                                                                column(3, offset = 0, tags$div(style = "color:green", textOutput("done4A_Plot")))),
                                                                                                              fluidRow(column(10, offset = 1, textOutput("mes_4A_Plot"))),
                                                                                                              fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                              fluidRow(column(5, offset = 1, downloadButton("Down_4A_Plot", "clustering PDF"))),
                                                                                                              fluidRow(h1(" "))
                                                                                                              ))),
                                                                                     
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(h1(" "))
                                                                            )
                                                                     )),
                                                                   fluidRow(h1(" "))
                                                          )),
                                                        fluidRow(h1(" ")),

                                                        #Step 5A ---------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("CORE PROTEINS"))),
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 5A. Calculate core proteins for each VGC, based on PCs"))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCore_PCs", label = "Start this step ")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done5A")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5A"))),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(10, offset = 1 , downloadButton("Down_5A", "tables and multifasta with core PCs"))),
                                                                                     fluidRow(h1(" "))
                                                                            ),
                                                                            fluidRow(h1(" "))))
                                                          )
                                                        ),
                                                        fluidRow(h1(" ")),

                                                        #Step 6A --------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(10, offset = 1, h4("PROTEIN ANNOTATIONS"))),
                                                                   fluidRow(h6(" ")),
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(h6(" ")),
                                                                                     fluidRow(column(10, offset = 1, h4("Step 6A. Annotate proteins"))),
                                                                                     fluidRow(column(10, offset = 1, radioButtons("Annot_prots_PCs", label = "Select proteins to annotate",
                                                                                                                                 choices = c("all proteins and relate them to PCs", "core proteins based on PCs"),
                                                                                                                                 selected = "all proteins and relate them to PCs", inline = TRUE, width = "100%"))),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the InterPro database using InterProScan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AI")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotInterPro_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downInterPro_PC", "InterPro results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AI"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the pVOGs database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6ApV")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotpVOGs_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downpVOGs_PC", "pVOGs results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6ApV"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the VOGDB database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AVO")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotVOGDB_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downVOGDB_PC", "VOGDB results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AVO"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the PHROG database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6APH")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotPHROGS_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downPHROGS_PC", "PHROG results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6APH"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AE")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam_PC", "Efam results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AE"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam-XC database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AXC")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam-XC_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam-XC_PC", "Efam-XC results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AXC"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the NCBI database using BLASTP")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AN")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotNCBI_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downNCBI_PC", "NCBI results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AN"))),
                                                                                     fluidRow(h1(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Merge annotation tables")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done6AM")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actMergeAnnot_PC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downMergedAnnots_PC", "Merged results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_6AM"))),
                                                                                     fluidRow(h1(" "))
                                                                            ))),

                                                                   fluidRow(h1(" "))
                                                          ))
                                               ),
                                                ##### BRANCH B --------------
                                                tabPanel("Branch B - based on Protein Superclusters",
                                                        h1(" "),

                                                        #Step 1B ----------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("PROTEIN CLUSTERING"))),
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 1B. PCs to Protein Superclusters (PSCs)"))),
                                                                                     fluidRow(column(6, offset = 1, selectInput("clust_PSC", label = "Cluster based on", choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"),
                                                                                                                                selected = "evalue_log"))),
                                                                                     fluidRow(column(10, offset = 1, h5("Keep matches if ..."))),
                                                                                     fluidRow(column(10, offset = 1, h5("conditional 1 is true"))),
                                                                                     fluidRow(column(2, offset = 1, numericInput("prob1_PSC", label = "probability >=", value = 90, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("cov1_PSC", label = "coverage >=", value = 50, step = 1, min = 1, max = 100))
                                                                                     ),
                                                                                     fluidRow(column(10, offset = 1, h5("OR"))),
                                                                                     fluidRow(column(10, offset = 1, h5("conditional 2 is true:"))),
                                                                                     fluidRow(column(2, offset = 1, numericInput("prob2_PSC", label = "probability >=", value = 99, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("cov2_PSC", label = "coverage >=", value = 20, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("alig_PSC", label = "alignment length >=", value = 100, step = 1, min = 1))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actPCs_2_PSCs", label = "Start this step")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done1B")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_1B"))),
                                                                                     fluidRow(column(10, offset =1, h1(" "))),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(5, offset = 1 , downloadButton("Down_1B_Table", "genome and protein table")),
                                                                                              column(5, offset = 0 , downloadButton("Down_1B_ALigned_PCs", "MSAs for PCs (NOT PSCs)"))),
                                                                                     fluidRow(h1(" "))

                                                                            ))),
                                                                   fluidRow(h1(" "))
                                                          )),
                                                        fluidRow(h1(" ")),

                                                         fluidRow(
                                                           tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                    fluidRow(column(12, offset = 1, h4("GENOME CLUSTERING"))),

                                                        #Step 2B -----
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 2B. Order genomes hierarchically"))),
                                                                                     fluidRow(column(5, offset = 1, selectInput("aglom_b", label = "Agglomeration method", choices = c("complete", "average"), selected = "complete", multiple = FALSE))),
                                                                                     fluidRow(column(5, offset = 1, checkboxInput("boot_b", label = "Enable bootstrapping", value = FALSE, width = "100%"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("boot_no_b", label = "Number of bootstraps", min = 2, max = 1000, value = 1000, step = 1))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCluster_genomes_PSC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done2B")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_2B"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(5, offset = 1 , downloadButton("Down_2B_tree", "tree (s)")),
                                                                                              column(5, offset = 0 , downloadButton("Down_2B_distMA", "intergenomic distance matrix"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1,
                                                                                                     tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                              fluidRow(column(10, offset = 1, h4("Plot intergenomic similarities"))),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("inc_fact_w_Pd_B", label = "Cell width", min = 0.02, max = 1, value = 0.3, step = 0.001))),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("font_row_Pd_B", label = "Row font", value = 12, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("font_col_Pd_B", label = "column font", value = 12, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("font_cell_Pd_B", label = "Cell font", value = 6, min=1))
                                                                                                              ),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("lgd_font_Pd_B", label = "Legend font", value = 5, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_lab_font_Pd_B", label = "Legend label font", value = 4, min = 1))
                                                                                                                       
                                                                                                              ),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("lgd_height_Pd_B", label = "Legend heigth", value = 9, min = 1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_width_Pd_B", label = "Legend width", value = 15, min=1))
                                                                                                              ),
                                                                                                              fluidRow(h1(" ")),
                                                                                                              fluidRow(
                                                                                                                column(5, offset = 1, actionButton("actOut_pdf_2B", label = "Generate plot")),
                                                                                                                column(3, offset = 0, tags$div(style = "color:green", textOutput("done2B_Plot")))),
                                                                                                              fluidRow(column(10, offset = 1, textOutput("mes_2B_Plot"))),
                                                                                                              fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                              fluidRow(column(5, offset = 1, downloadButton("Down_2B_PDF", "Similarity heatmap PDF"))),
                                                                                                              fluidRow(h1(" "))
                                                                                                     )
                                                                                     )),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(h1(" "))
                                                                            )
                                                                     )
                                                                   ),
                                                                   fluidRow(h1(" ")),

                                                                   #Step 3B -----
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 3B. Calculate stats and split in genome clusters (VGCs)"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("Clust_dist_PSC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.9, step = 0.01)),
                                                                                              column(5, offset = 0, numericInput("sel_PSCs_heatmap_b", label = "Show only common PSCs if >:",
                                                                                                                                 value = 3000, min =1))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actSplit_clust_PSC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done3B")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_3B"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(3, offset = 1, downloadButton("Down_3B_genPSCs_Table", "genomes vs PSCs table")),
                                                                                              column(3, offset = 0, downloadButton("Down_3B_stats_Table", "cluster stats")),
                                                                                              column(3, offset = 0, downloadButton("Down_3B_VGCs", "genome clusters"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1, p("*The clustering distance is minimum 0.1 and maximum 1. The higher the value, the lower the number of clusters resulted. At a value of 1, all genomes will belong to the same cluster."))),
                                                                                     fluidRow(column(10, offset=1, p("*Known issues: If the chosen clustering distance results in each genome forming its own VGC, then the output PDF will be empty. To solve this problem: increase the clustering distance progressively and recalculate steps 5 and 6."))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(
                                                                                       column(10, offset=1,
                                                                                              tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                       fluidRow(column(10, offset = 1, h4("Output genome clustering PDF"))),
                                                                                                       fluidRow(h1(" ")),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(5, offset =1, checkboxInput("Show_Tree_b", label = "Show tree", value = TRUE))
                                                                                                       ),
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("tree_width_b", label = "Tree width", min = 1, max = 100, value = 30))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 1, checkboxInput("show_clust_ID_b", label = "Show VGC ID", value = TRUE)),
                                                                                                         column(3, offset = 0, checkboxInput("show_sil_b", label = "Show silhoutte width", value = TRUE)),
                                                                                                         column(3, offset = 0, checkboxInput("show_protein_stats_b", label = "Show protein stats", value = TRUE))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("clustID_width_b", label = "Width of VGC ID column", value = 10, min=1)),
                                                                                                         column(3, offset = 0, numericInput("Sil_stats_width_b", label = "Width of silhoutte column", value = 5, min=1)),
                                                                                                         column(3, offset = 0, numericInput("Stats_width_b", label = "Width of protein stats", min = 1, max = 100, value = 30))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("stats_font_b", label = "Font stats name", value = 5, min=1)),
                                                                                                         column(3, offset = 0, numericInput("stats_lab_font_b", "Font stats axis", value = 5, min=1))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(2, offset =1, checkboxInput("show_heat_b", label = "Show heatmap", value = TRUE))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("Heat_width_b", label = "Column width (inches)", min = 0.005, max = 1, value = 0.03, step = 0.001)),
                                                                                                         column(3, offset = 0, numericInput("font_col_b", label = "Font PSC names", value = 2, min = 1))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(column(6, offset = 1, p("Other options"))),
                                                                                                       
                                                                                                       fluidRow(column(3, offset = 2, numericInput("font_row_b", label = "Font genomes/VGCs IDs ", value = 12, min=1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_font_b", label = "Legend font", value = 20, min=1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_lab_font_b", label = "Legend label font", value = 30, min = 1))
                                                                                                                
                                                                                                       ),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("lgd_h_b", label = "Legend heigth", value = 5, min = 1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_w_b", label = "Legend width", value = 10, min=1))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(h1(" ")),
                                                                                                       fluidRow(
                                                                                                         column(5, offset = 1, actionButton("actOut_pdf_3B", label = "Generate plot")),
                                                                                                         column(3, offset = 0, tags$div(style = "color:green", textOutput("done3B_Plot")))),
                                                                                                       fluidRow(column(10, offset = 1, textOutput("mes_3B_Plot"))),
                                                                                                       fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                       fluidRow(column(5, offset = 1, downloadButton("Down_3B_Plot", "clustering PDF"))),
                                                                                                       fluidRow(h1(" "))
                                                                                              ),
                                                                                              fluidRow(h1(" "))
                                                                                       ))
                                                                            )
                                                                     )),
                                                                   fluidRow(h1(" "))
                                                                   
                                                         
                                                           )),
                                                         fluidRow(h1(" ")),
                                                         
                                                         fluidRow(h1(" ")),
                                                         
                                                        #Step 4B --------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("CORE PROTEINS"))),
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h5("Step 4B. Calculate core proteins for each VGC, based on PSCs"))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCore_PSCs", label = "Start this step ")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done4B")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_4B"))),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(10, offset = 1 , downloadButton("Down_4B", "tables and multifasta with core PSCs"))),
                                                                                     fluidRow(h1(" "))
                                                                            ),
                                                                            fluidRow(h1(" ")))))),
                                                        fluidRow(h1(" ")),
                                                         
                                                         #Step 5B ----------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(10, offset = 1, h4("PROTEIN ANNOTATIONS"))),
                                                                   fluidRow(h6(" ")),
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(h6(" ")),
                                                                                     fluidRow(column(10, offset = 1, h4("Step 5B. Annotate proteins"))),
                                                                                     fluidRow(column(10, offset = 1, radioButtons("Annot_prots_PSCs", label = "Select proteins to annotate",
                                                                                                                                 choices = c("all proteins and relate them to PCs and PSCs", "core proteins based on PSCs"),
                                                                                                                                 selected = "all proteins and relate them to PCs and PSCs", inline = TRUE, width = "100%"))),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the InterPro database using InterProScan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BI")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotInterPro_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downInterPro_PSC", "InterPro results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BI"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the pVOGs database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BpV")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotpVOGs_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downpVOGs_PSC", "pVOGs results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BpV"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the VOGDB database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BVO")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotVOGDB_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downVOGDB_PSC", "VOGDB results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BVO"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the PHROG database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BPH")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotPHROGS_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downPHROGS_PSC", "PHROG results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BPH"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BE")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam_PSC", "Efam results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BE"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam-XC database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BXC")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam-XC_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam-XC_PSC", "Efam-XC results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BXC"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the NCBI database using BLASTP")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BN")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotNCBI_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downNCBI_PSC", "NCBI results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BN"))),
                                                                                     fluidRow(h1(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Merge annotation tables")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5BM")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actMergeAnnot_PSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downMergedAnnots_PSC", "Merged results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5BM"))),
                                                                                     fluidRow(h1(" "))
                                                                            ))),

                                                                   fluidRow(h1(" "))
                                                          ))


                                                ), #####################################################TBAL
                                                ###BRANCH C -----------------
                                                tabPanel("Branch C - based on Protein Super-superclusters",
                                                         h1(" "),
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("PROTEIN CLUSTERING"))),

                                                                   #Step 1C ---------
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 1C. PSCs to Protein Super-superclusters (PSSCs)"))),
                                                                                     fluidRow(column(6, offset = 1, selectInput("clust_PSSC", label = "Cluster based on", choices = c("evalue_log", "evalue", "norm_bitscore", "bitscore"),
                                                                                                                                selected = "evalue_log"))),
                                                                                     fluidRow(column(10, offset = 1, h5("Keep matches if ..."))),
                                                                                     fluidRow(column(10, offset = 1, h5("conditional 1 is true"))),
                                                                                     fluidRow(column(2, offset = 1, numericInput("prob1_PSSC", label = "probability >=", value = 90, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("cov1_PSSC", label = "coverage >=", value = 50, step = 1, min = 1, max = 100))
                                                                                     ),
                                                                                     fluidRow(column(10, offset = 1, h5("OR"))),
                                                                                     fluidRow(column(10, offset = 1, h5("conditional 2 is true:"))),
                                                                                     fluidRow(column(2, offset = 1, numericInput("prob2_PSSC", label = "probability >=", value = 99, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("cov2_PSSC", label = "coverage >=", value = 20, step = 1, min = 1, max = 100)),
                                                                                              column(1, offset = 0, p("AND")),
                                                                                              column(2, offset = 0, numericInput("alig_PSSC", label = "alignment length >=", value = 100, step = 1, min = 1))
                                                                                     ),
                                                                                     fluidRow(
                                                                                       column(6, offset = 1, actionButton("actPSCs_2_PSSCs", label = "Start this step")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done1C")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_1C"))),
                                                                                     fluidRow(column(10, offset =1, h1(" "))),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(5, offset = 1 , downloadButton("Down_1C_Table", "genome and protein table")),
                                                                                              column(5, offset = 0 , downloadButton("Down_1C_ALigned_PCs", "MSAs for PSCs (NOT PSSCs)")))
                                                                                     ,
                                                                                     fluidRow(h1(" "))
                                                                            ),
                                                                            fluidRow(h1(" ")))))),
                                                        fluidRow(h1(" ")),
                                                        fluidRow(h1(" ")),
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("GENOME CLUSTERING"))),

                                                                   #Step 2C -----------
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 2C. Order genomes hierarchically"))),
                                                                                     fluidRow(column(5, offset = 1, selectInput("aglom_c", label = "Agglomeration method", choices = c("complete", "average"), selected = "complete", multiple = FALSE))),
                                                                                     fluidRow(column(5, offset = 1, checkboxInput("boot_c", label = "Enable bootstrapping", value = FALSE, width = "100%"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("boot_no_c", label = "Number of bootstraps", min = 2, max = 1000, value = 1000, step = 1))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCluster_genomes_PSSC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done2C")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_2C"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(5, offset = 1 , downloadButton("Down_2C_tree", "tree (s)")),
                                                                                              column(5, offset = 0 , downloadButton("Down_2C_distMA", "intergenomic distance matrix"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1,
                                                                                                     tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                              fluidRow(column(10, offset = 1, h4("Plot intergenomic similarities"))),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("inc_fact_w_Pd_C", label = "Cell width", min = 0.02, max = 1, value = 0.3, step = 0.001))),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("font_row_Pd_C", label = "Row font", value = 12, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("font_col_Pd_C", label = "column font", value = 12, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("font_cell_Pd_C", label = "Cell font", value = 6, min=1))
                                                                                                              ),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("lgd_font_Pd_C", label = "Legend font", value = 5, min=1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_lab_font_Pd_C", label = "Legend label font", value = 4, min = 1))
                                                                                                                       
                                                                                                              ),
                                                                                                              fluidRow(column(3, offset = 2, numericInput("lgd_height_Pd_C", label = "Legend heigth", value = 9, min = 1)),
                                                                                                                       column(3, offset = 0, numericInput("lgd_width_Pd_C", label = "Legend width", value = 15, min=1))
                                                                                                              ),
                                                                                                              fluidRow(h1(" ")),
                                                                                                              fluidRow(
                                                                                                                column(5, offset = 1, actionButton("actOut_pdf_2C", label = "Generate plot")),
                                                                                                                column(3, offset = 0, tags$div(style = "color:green", textOutput("done2C_Plot")))),
                                                                                                              fluidRow(column(10, offset = 1, textOutput("mes_2C_Plot"))),
                                                                                                              fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                              fluidRow(column(5, offset = 1, downloadButton("Down_2C_PDF", "Similarity heatmap PDF"))),
                                                                                                              fluidRow(h1(" "))
                                                                                                     )
                                                                                     )),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(h1(" "))
                                                                            )
                                                                     )
                                                                   ),
                                                                   fluidRow(h1(" ")),

                                                                   #Step 3C ----
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h4("Step 3C. Calculate stats and split in genome clusters (VGCs)"))),
                                                                                     fluidRow(column(5, offset = 1, numericInput("Clust_dist_PSSC", label = "Clustering distance*", min = 0.1, max = 1, value = 0.9, step = 0.01)),
                                                                                              column(5, offset = 0, numericInput("sel_PSSCs_heatmap_c", label = "Show only common PSSCs if >:",
                                                                                                                                 value = 3000, min =1)),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actSplit_clust_PSSC", label = "Start this step ")),
                                                                                       column(3, offset = 0, tags$div(style = "color:green", textOutput("done3C")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_3C"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(3, offset = 1, downloadButton("Down_3C_genPSSCs_Table", "genomes vs PSSCs table")),
                                                                                              column(3, offset = 0, downloadButton("Down_3C_stats_Table", "cluster stats")),
                                                                                              column(3, offset = 0, downloadButton("Down_3C_VGCs", "genome clusters"))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(column(10, offset=1, p("*The clustering distance is minimum 0.1 and maximum 1. The higher the value, the lower the number of clusters resulted. At a value of 1, all genomes will belong to the same cluster."))),
                                                                                     fluidRow(column(10, offset=1, p("*Known issues: If the chosen clustering distance results in each genome forming its own VGC, then the output PDF will be empty. To solve this problem: increase the clustering distance progressively and recalculate steps 5 and 6."))),
                                                                                     fluidRow(h1(" ")),
                                                                                     fluidRow(
                                                                                       column(10, offset=1,
                                                                                              tags$div(style = "background-color: #EFF2FB; width: 100%; border-radius: 5px;",
                                                                                                       fluidRow(column(10, offset = 1, h4("Output clustering PDF"))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(5, offset =1, checkboxInput("Show_Tree_c", label = "Show tree", value = TRUE))
                                                                                                       ),
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("tree_width_c", label = "Tree width", min = 1, max = 100, value = 30))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 1, checkboxInput("show_clust_ID_c", label = "Show VGC ID", value = TRUE)),
                                                                                                         column(3, offset = 0, checkboxInput("show_sil_c", label = "Show silhoutte width", value = TRUE)),
                                                                                                         column(3, offset = 0, checkboxInput("show_protein_stats_c", label = "Show protein stats", value = TRUE))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("clustID_width_c", label = "Width of VGC ID column", value = 10, min=1)),
                                                                                                         column(3, offset = 0, numericInput("Sil_stats_width_c", label = "Width of silhoutte column", value = 5, min=1)),
                                                                                                         column(3, offset = 0, numericInput("Stats_width_c", label = "Width of protein stats", min = 1, max = 100, value = 30))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("stats_font_c", label = "Font stats name", value = 5, min=1)),
                                                                                                         column(3, offset = 0, numericInput("stats_lab_font_c", "Font stats axis", value = 5, min=1))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(2, offset =1, checkboxInput("show_heat_c", label = "Show heatmap", value = TRUE))),
                                                                                                       
                                                                                                       fluidRow(
                                                                                                         column(3, offset = 2, numericInput("Heat_width_c", label = "Column width (inches)", min = 0.005, max = 1, value = 0.03, step = 0.001)),
                                                                                                         column(3, offset = 0, numericInput("font_col_c", label = "Font PSSC names", value = 2, min = 1)))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(column(6, offset = 1, p("Other options"))),
                                                                                                       
                                                                                                       fluidRow(column(3, offset = 2, numericInput("font_row_c", label = "Font genomes/VGCs IDs ", value = 12, min=1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_font_c", label = "Legend font", value = 20, min=1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_lab_font_c", label = "Legend label font", value = 30, min = 1))
                                                                                                                
                                                                                                       ),
                                                                                                       fluidRow(column(3, offset = 2, numericInput("lgd_h_c", label = "Legend heigth", value = 5, min = 1)),
                                                                                                                column(3, offset = 0, numericInput("lgd_w_c", label = "Legend width", value = 10, min=1))
                                                                                                       ),
                                                                                                       
                                                                                                       fluidRow(h1(" ")),
                                                                                                       fluidRow(
                                                                                                         column(5, offset = 1, actionButton("actOut_pdf_3C", label = "Generate plot")),
                                                                                                         column(3, offset = 0, tags$div(style = "color:green", textOutput("done3C_Plot")))),
                                                                                                       fluidRow(column(10, offset = 1, textOutput("mes_3C_Plot"))),
                                                                                                       fluidRow(column(5, offset = 1, h5("Download results"))),
                                                                                                       fluidRow(column(5, offset = 1, downloadButton("Down_3C_Plot", "clustering PDF"))),
                                                                                                       fluidRow(h1(" "))
                                                                                              ),
                                                                                              fluidRow(h1(" "))
                                                                                       ))
                                                                            )
                                                                     )),
                                                                   fluidRow(h1(" "))
                                                          )),
                                                        fluidRow(h1(" ")),

                                                        #Step 4C --------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(12, offset = 1, h4("Step 4C. CORE PROTEINS"))),
                                                                   fluidRow(
                                                                     column(10, offset=1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(column(10, offset = 1, h5("Calculate core proteins for each VGC, based on PSSCs"))),
                                                                                     fluidRow(
                                                                                       column(5, offset = 1, actionButton("actCore_PSSCs", label = "Start this step ")),
                                                                                       column(5, offset = 0, tags$div(style = "color:green", textOutput("done4C")))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_4C"))),
                                                                                     fluidRow(column(10, offset = 1, h5("Download results"))),
                                                                                     fluidRow(column(10, offset = 1 , downloadButton("Down_4C", "tables and multifasta with core PSSCs"))),
                                                                                     fluidRow(h1(" "))
                                                                            ),
                                                                            fluidRow(h1(" "))
                                                                     )))),
                                                        fluidRow(h1(" ")),

                                                        #Step 5C ---------
                                                        fluidRow(
                                                          tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                                                   fluidRow(column(10, offset = 1, h4("PROTEIN ANNOTATIONS"))),
                                                                   fluidRow(h6(" ")),
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            tags$div(style = "background-color: #eaf2f8; width: 100%; border-radius: 5px;",
                                                                                     fluidRow(h6(" ")),
                                                                                     fluidRow(column(10, offset = 1, h4("Step 5C. Annotate proteins"))),
                                                                                     fluidRow(column(10, offset = 1, radioButtons("Annot_prots_PSSCs", label = "Select proteins to annotate",
                                                                                                                                 choices = c("all proteins and relate them to PCs, PCSs and PSSCs", "core proteins based on PSSCs"),
                                                                                                                                 selected = "all proteins and relate them to PCs, PCSs and PSSCs", inline = TRUE, width = "100%"))),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the InterPro database using InterProScan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CI")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotInterPro_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downInterPro_PSSC", "InterPro results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CI"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the pVOGs database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CpV")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotpVOGs_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downpVOGs_PSSC", "pVOGs results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CpV"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the VOGDB database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CVO")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotVOGDB_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downVOGDB_PSSC", "VOGDB results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CVO"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the PHROG database using hhsearch")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CPH")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotPHROGS_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downPHROGS_PSSC", "PHROG results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CPH"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CE")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam_PSSC", "Efam results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CE"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the Efam-XC database using hmmscan")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CXC")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotEfam-XC_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downEfam-XC_PSSC", "Efam-XC results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CXC"))),
                                                                                     fluidRow(h6(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Query the NCBI database using BLASTP")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CN")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actAnnotNCBI_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downNCBI_PSSC", "NCBI results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CN"))),
                                                                                     fluidRow(h1(" ")),

                                                                                     fluidRow(column(5, offset = 1, h5("Merge annotation tables")),
                                                                                              column(5, offset = 0, tags$div(style = "color:green", textOutput("done5CM")))),
                                                                                     fluidRow(column(5, offset = 1, actionButton("actMergeAnnot_PSSC", label = "Start this step")),
                                                                                              column(5, offset = 1, downloadButton("downMergedAnnots_PSSC", "Merged results"))),
                                                                                     fluidRow(column(10, offset = 1, textOutput("mes_5CM"))),
                                                                                     fluidRow(h1(" "))
                                                                            ))),

                                                                   fluidRow(h1(" "))
                                                          ))
                                                ) ######################################################TAB
                                    ),
                                   
                                   
                                   h6(" "),
                                   fluidRow(
                                     tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 2px",
                                              fluidRow(h6(" ")),
                                              fluidRow(
                                                column(5, offset = 1, checkboxInput("want_email2", label = "Receive an email notification when calculations have finished", value = FALSE, width= "100%")),
                                                column(5, offset = 0, textInput("email_2", label = "User email"))
                                              ),
                                              fluidRow(h6(" ")),
                                              fluidRow(column(7, offset = 2, textOutput("VirClust_cmd")))
                                     ))
                                   
                          )
                        ),
                        h1(" "),
                        ##### Bottom ----
                        fluidRow(
                          column(12, offset = 0,
                                 tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 5px;",
                                          fluidRow(
                                            column(2, offset=1, tags$img(src = "https://www.uni-oldenburg.de/img/orga/f5icbm/kopflogo.png", width = "70px", height = "90px"), tags$img(src = "UniOld.png", width = "70px", height = "70px",  align = "center")),
                                            column(7,
                                                   h1(" "),
                                                   h5(tags$b("    Developer - Cristina Moraru, PhD")),
                                                   p("Senior Scientist, Department of The Biology of Geological Processes"),
                                                   p("Institute for Chemistry and Biology of the Marine Environment"),
                                                   p("Carl-von-Ossietzky –Str. 9 -11, D-26111 Oldenburg, Germany"),
                                                   p(tags$b("Email:"), "liliana.cristina.moraru(     at     )uni-oldenburg.de"),
                                                   h1(" ")
                                            )
                                          )
                                 )
                          )
                        )
                 )
               )),
      #####Download ----
      tabPanel("DOWNLOAD",
               useShinyjs(),
               ##### Header ----
               tags$style(type="text/css", "body {padding-top: 70px;}"),
               fluidRow(
                 column(12, offset = 0,
                        tags$div(style = "text-align:justify;", #background-color: #eaf2f8; width: 100%; border-radius: 10px; border-style: solid; border-color: #c1d5e0; border-width: 1px",
                                 h1(" "),
                                 #fluidRow(column(10, offset=1, h4("The resources on this page are in preparation, and will be released soon."))),
                                 h1(" "),
                                 fluidRow(column(10, offset=1, h3("VirClust v2 web-server"))),
                                 h1(" "),
                                 fluidRow(column(6, offset = 1, downloadButton("Down_manual_web", label = "Manual web-server"))),
                                 h1(" "),
                                 h1(" "),
                                 h1(" "),
                                 fluidRow(column(10, offset=1, h3("VirClust v2 stand-alone"))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_standalone", label = "VirClust v2 stand-alone")),
                                          column(4, offset = 0, downloadButton("Down_manual_stand", label = "Manual stand-alone"))),
                                 h1(" "),
                                 h1(" "),
                                 fluidRow(column(10, offset=1, h3("Annotation databases for VirClust stand-alone"))),
                                 h3(" "),
                                 fluidRow(column(10, offset=1, p("For the annotation of viral genomes, VirClust relies on several databases previously published. 
                                                                 The InterProScan and the BLASTNR database should be installed by the user as described in the manual for the VirClust v2 stand-alone version. 
                                                                 The other databases (Efam, Efam_XC, PHROG, pVOGs and VOGDB) need to be in a format specific for VirClust and they can be downloaded below. 
                                                                 For each database used, please cite the original publications describing the respective databases."))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, h4("Download database ...")),
                                          column(4, offset = 0, h4("Publication to cite ..."))),
                                 h4(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_Efam", label = "Efam")),
                                          column(4, offset = 0, p("Zayed, A.A., Lücking, D., Mohssen, M., Cronin, D., Bolduc, B., Gregory, A.C., Hargreaves, K.R., Piehowski, P.D., White, R.A., Huang, E.L., Adkins, J.N., Roux, S., Moraru, C., and Sullivan, M.B. (2021) 
                                                                  efam: an expanded, metaproteome-supported HMM profile database of viral protein families. Bioinformatics (Oxford, England), doi: 10.1093/bioinformatics/btab451."))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_Efam_XC", label = "Efam_XC")),
                                          column(4, offset = 0, p("Zayed, A.A., Lücking, D., Mohssen, M., Cronin, D., Bolduc, B., Gregory, A.C., Hargreaves, K.R., Piehowski, P.D., White, R.A., Huang, E.L., Adkins, J.N., Roux, S., Moraru, C., and Sullivan, M.B. (2021) 
                                                                  efam: an expanded, metaproteome-supported HMM profile database of viral protein families. Bioinformatics (Oxford, England), doi: 10.1093/bioinformatics/btab451."))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_PHROG", label = "PHROG")),
                                          column(4, offset = 0, p("Terzian, P., Olo Ndela, E., Galiez, C., Lossouarn, J., Pérez Bucio, R.E., Mom, R., Toussaint, A., Petit, M.-A., and Enault, F. (2021) 
                                                                  PHROG: families of prokaryotic virus proteins clustered using remote homology. NAR genomics and bioinformatics, doi: 10.1093/nargab/lqab067."))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_VOG_DB", label = "VOGDB")),
                                          column(4, offset = 0, p("Kiening, M., Ochsenreiter, R., Hellinger, H.-J., Rattei, T., Hofacker, I., and Frishman, D. (2019) 
                                                                  Conserved Secondary Structures in Viral mRNAs. Viruses, doi: 10.3390/v11050401."))),
                                 h1(" "),
                                 fluidRow(column(4, offset = 1, downloadButton("Down_pVOGs", label = "pVOGs")),
                                          column(4, offset = 0, p("Grazziotin, A.L., Koonin, E.V., and Kristensen, D.M. (2017) Prokaryotic virus orthologous groups (pVOGs). 
                                                                  A resource for comparative genomics and protein family annotation. Nucleic acids research, doi: 10.1093/nar/gkw975."))),
                                 h1(" ")
                                 ))),
               h1(" "),
               ##### Bottom ----
               fluidRow(
                 column(12, offset = 0,
                        tags$div(style = "background-color: #ffffff; width: 100%; border-radius: 5px;",
                                 fluidRow(
                                   column(2, offset=1, tags$img(src = "https://www.uni-oldenburg.de/img/orga/f5icbm/kopflogo.png", width = "70px", height = "90px"), tags$img(src = "UniOld.png", width = "70px", height = "70px",  align = "center")),
                                   column(7,
                                          h1(" "),
                                          h5(tags$b("    Developer - Cristina Moraru, PhD")),
                                          p("Senior Scientist, Department of The Biology of Geological Processes"),
                                          p("Institute for Chemistry and Biology of the Marine Environment"),
                                          p("Carl-von-Ossietzky –Str. 9 -11, D-26111 Oldenburg, Germany"),
                                          p(tags$b("Email:"), "liliana.cristina.moraru(     at     )uni-oldenburg.de"),
                                          h1(" ")
                                   )
                                 )
                        )
                 )
               )
               )
    )
  )
)

