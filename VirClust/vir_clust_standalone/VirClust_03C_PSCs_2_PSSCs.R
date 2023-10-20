## ----Librarries------------------------------------------------
suppressPackageStartupMessages(
  {
    library(seqinr, warn.conflicts = FALSE, quietly = FALSE)
    library(magrittr, warn.conflicts = FALSE, quietly = FALSE)
    library(stringr, warn.conflicts = FALSE, quietly = FALSE)
    library(tibble, warn.conflicts = FALSE, quietly = FALSE)
    library(dplyr, warn.conflicts = FALSE, quietly = FALSE)
    library(tidyr, warn.conflicts = FALSE, quietly = FALSE)
    library(readr, warn.conflicts = FALSE, quietly = FALSE)
    library(purrr, warn.conflicts = FALSE, quietly = FALSE)
    library(data.table, warn.conflicts = FALSE, quietly = FALSE)
    library(zip, warn.conflicts = FALSE, quietly = FALSE)
    #library(furrr, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
    #library(future, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
  })


## ----paths---------------------------------------------------------------
input0 <- as.character(commandArgs(trailingOnly = TRUE))
print(input0)

#manual run
# input0 <- ""
# input0[1] <- "projdir=/bioinf/shiny-server/VirClust/data/P983___CLMoraru___TestExecWait"
# input0[2] <- "cpu=20"
# input0[3] <- "prob1_PSC=90"
# input0[4] <- "cov1_PSC=60"
# input0[5] <- "prob2_PSC=90"
# input0[6] <- "cov2_PSC=60"
# input0[7] <- "alig_PSC=0"
# input0[8] <- "functions_path=/bioinf/shiny-server/VirClust/vir_clust_standalone/VirClust_functions.R"
# input0[9] <- "clust_PSC=evalue_log"



in1 <- str_which(input0, "^projdir=")
input <- str_remove(input0[in1], "^projdir=")
rm(in1)

P_D <- paste0(input, "/03C")
if(dir.exists(P_D)){unlink(P_D, recursive = TRUE)}
dir.create(P_D)

cpu_n <- str_which(input0, "^cpu=")
cpu <- str_remove(input0[cpu_n], "^cpu=") %>%
  as.integer()
rm(cpu_n)



#clustering and thresholding params
clust_PSSC_n <- str_which(input0, "^clust_PSSC=")
clust_PSSC <- str_remove(input0[clust_PSSC_n], "^clust_PSSC=")
rm(clust_PSSC_n)

prob1_PSSC_c <- str_which(input0, "^prob1_PSSC=")
prob1_PSSC <- str_remove(input0[prob1_PSSC_c], "^prob1_PSSC=") %>%
  as.integer()
rm(prob1_PSSC_c)

prob2_PSSC_c <- str_which(input0, "^prob2_PSSC=")
prob2_PSSC <- str_remove(input0[prob2_PSSC_c], "^prob2_PSSC=") %>%
  as.integer()
rm(prob2_PSSC_c)

cov1_PSSC_c <- str_which(input0, "^cov1_PSSC=")
cov1_PSSC <- str_remove(input0[cov1_PSSC_c], "^cov1_PSSC=") %>%
  as.integer()
rm(cov1_PSSC_c)

cov2_PSSC_c <- str_which(input0, "^cov2_PSSC=")
cov2_PSSC <- str_remove(input0[cov2_PSSC_c], "^cov2_PSSC=") %>%
  as.integer()
rm(cov2_PSSC_c)

alig_PSSC_c <- str_which(input0, "^alig_PSSC=")
alig_PSSC <- str_remove(input0[alig_PSSC_c], "^alig_PSSC=") %>%
  as.integer()
rm(alig_PSSC_c)



###funtions file
functions_path_c <- str_which(input0, "^functions_path=")
functions_path <- str_remove(input0[functions_path_c], "^functions_path=")
source(functions_path)
rm(functions_path, functions_path_c)

sing <- get_params_simple_fun(input0, param_name = "sing", type = "text")
condaenvpath <- get_params_simple_fun(input0, param_name = "condaenvpath", type = "text")
rm(input0)



## load protDF
protDF_p <- paste0(input, "/03/03_09_genome_protDF_PCs_PSCs.RDS")
rm(input)
prot_DF <- readRDS(protDF_p) %>%
  filter(PSC_ID != "") %>%
  mutate(PSC_ID = as.numeric(PSC_ID))
rm(protDF_p)


### 01. Write Clusters into mfasta files ########################################
st01_D <- paste0(P_D, "/03C_01_PSC_mfasta")
if(dir.exists(st01_D)){unlink(st01_D, recursive = TRUE)}
dir.create(st01_D)

prot_DF_PSC_clust <- prot_DF %>%
  group_by(PSC_ID) %>%
  nest()
rm(prot_DF)
# sprintf variable
d <- nrow(prot_DF_PSC_clust) %>%
  as.character() %>%
  nchar()
d <- paste0("%0", d, "d")

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  mutate(clust_ID = sprintf(d, PSC_ID)) %>%                                     ###make PSC_ID = clust_ID
  mutate(mfasta_p = paste0(st01_D, "/cluster_", clust_ID, ".fasta"))

write.fasta_clustDf_fun <- function(DF, path, clust_ID)
{
  seq_ls <- DF$protein_seq %>%
    as.list()
  
  DF <- DF %>%
    mutate(PSC_prot_ID = paste0("cluster_", clust_ID, "_", protein_ID))
  
  name_ls <- DF$PSC_prot_ID %>%
    as.list()
  
  write.fasta(sequences = seq_ls, names = name_ls, file.out = path, as.string = TRUE)
}

todel <- mapply(write.fasta_clustDf_fun, prot_DF_PSC_clust$data, prot_DF_PSC_clust$mfasta_p, prot_DF_PSC_clust$clust_ID)

rm(d, todel)


### 02. Align with Clustal-Omega ######################################################
st02_D <-paste0(P_D, "/03C_02_aligned_PSCs")
if(dir.exists(st02_D)){unlink(st02_D, recursive = TRUE)}
dir.create(st02_D)
dir.create(paste0(st02_D, "/sdt_err"))
dir.create(paste0(st02_D, "/sdt_out"))
dir.create(paste0(st02_D, "/out"))

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  mutate(alig_mfasta_p = paste0(st02_D, "/out/cluster_", clust_ID, "_aligned.fasta"))

prot_DF_PSC_clust$no_prots <- lapply(X = prot_DF_PSC_clust$data, FUN = nrow) %>% unlist()
malig_DF <- prot_DF_PSC_clust %>%
  filter(no_prots > 1) %>%
  mutate(malig_stdout = paste0(st02_D, "/sdt_out/stdout_cluster_", clust_ID, ".txt")) %>%
  mutate(malig_stderr = paste0(st02_D, "/sdt_err/stderr_cluster_", clust_ID, ".txt"))
  # mutate(muscle_cmd = paste0("muscle -in ", mfasta_p, " -fastaout ", alig_mfasta_p, " -diags -maxiters 10",  # increased to 10
  #                            " > /dev/null 2> /dev/null"))%>%
  #mutate(clustalO_cmd = paste0("clustalo -i ", mfasta_p, " -o ", alig_mfasta_p, " -v --pileup --iter=2 --threads=", cpu,
  #                             " > /dev/null 2> /dev/null"))



for(i in 1:nrow(malig_DF))
{
  malig_DF[[i, "malig_status"]] <- sys::exec_wait(cmd = "clustalo", 
                                                           args = c("-i", malig_DF$mfasta_p[[i]],
                                                                    "-o", malig_DF$alig_mfasta_p[[i]],
                                                                    "-v", "--pileup", "--iter=2",
                                                                    paste0("--threads=", cpu)), 
                                                           std_out = malig_DF$malig_stdout[[i]],
                                                           std_err = malig_DF$malig_stderr[[i]])
}
rm(i)

malig_st <- unique(malig_DF$malig_status)
if(length(malig_st) > 1 | (length(malig_st) == 1 & malig_st != 0))
{
  malig_report_DF <- malig_DF %>%
    filter(malig_status > 0)
  
  saveRDS(malig_report_DF, file = paste0(P_D, "/error_DF_03c_02.RDS"))
  
  print(paste0("Incomplete multiple alignment creation in step 3B_02. From ", nrow(malig_DF), " multiple alignments, ", nrow(malig_report_DF), " were NOT created. Aborting VirClust."))
  
  quit()
}
rm(malig_DF, malig_st)


# make zip archive
outzip_p <- paste0(st02_D, ".zip")
zipr(zipfile = outzip_p, files = st02_D)
rm(outzip_p)

# copy the singletons to the aligned folder
tocopy_DF <- prot_DF_PSC_clust %>%
filter(no_prots == 1)
to_del <- mapply(FUN =file.copy, from = tocopy_DF$mfasta_p, to = tocopy_DF$alig_mfasta_p)
rm(to_del, tocopy_DF)


### 03. Creation of a3m files ##################################################
st03_D <- paste0(P_D, "/03C_03_aligPSCs_a3m")
if(dir.exists(st03_D)){unlink(st03_D, recursive = TRUE)}
dir.create(st03_D)

if(sing == "sing")
  #with hhsuite fom apt-get
  #{com <- "perl /usr/share/hhsuite/scripts/reformat.pl "}else
  #with hhsuite copied from Rhea
{com <- "perl /home/cristinam/bin/hhsuite3/scripts/reformat.pl "}else
  #directly on Rhea
{
  if(sing == "rhea")
  {
    com <- "perl /opt/hhsuite3/scripts/reformat.pl "
  }else
  {
    if(sing == "conda")
    {com = paste0("perl ", condaenvpath, "/scripts/reformat.pl ")} #"perl /home/cmoraru/miniconda3/envs/VirClust/scripts/reformat.pl"
  }
}


prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  mutate(a3m_p = paste0(st03_D, "/cluster_", clust_ID, ".a3m")) %>%
  mutate(reformat_cmd = paste0(com, alig_mfasta_p, " ", a3m_p,
                               " -r 50 > /dev/null 2> /dev/null"))


#plan(multisession, workers = cpu)
todel <- map(.x = prot_DF_PSC_clust$reformat_cmd, .f = system)
#plan(sequential)

rm(todel, com)


### 04. Creation of hmm files ##################################################
st04_D <- paste0(P_D, "/03C_04_PSCs_hmms")
if(dir.exists(st04_D)){unlink(st04_D, recursive = TRUE)}
dir.create(st04_D)
dir.create(paste0(st04_D, "/sdt_err"))
dir.create(paste0(st04_D, "/sdt_out"))
dir.create(paste0(st04_D, "/out"))

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  select(-reformat_cmd) %>%
  mutate(hmm_p = paste0(st04_D, "/out/cluster_", clust_ID, ".hmm")) %>%
  mutate(hmm_name = paste0("cluster_", clust_ID)) %>%
  mutate(hhmake_stdout = paste0(st04_D, "/sdt_out/stdout_cluster_", clust_ID, ".txt")) %>%
  mutate(hhmake_stderr = paste0(st04_D, "/sdt_err/stderr_cluster_", clust_ID, ".txt"))

for(i in 1:nrow(prot_DF_PSC_clust))
{
  prot_DF_PSC_clust[[i, "hhmake_status"]] <- sys::exec_wait(cmd = "hhmake", 
                                                           args = c("-i", prot_DF_PSC_clust$a3m_p[[i]],
                                                                    "-o", prot_DF_PSC_clust$hmm_p[[i]],
                                                                    "-name", prot_DF_PSC_clust$hmm_name[[i]],
                                                                    "-id", "100",
                                                                    "-diff", "1000000"), 
                                                           std_out = prot_DF_PSC_clust$hhmake_stdout[[i]],
                                                           std_err = prot_DF_PSC_clust$hhmake_stderr[[i]])
}
rm(i)


prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  select(-hmm_name, -hhmake_stdout, -hhmake_stderr)

#CLM: in the hhmake command I've modified the MSA filtering parameters so as to keep all sequences; with the default parameters, the most divergent sequences
#are not used to calculate the HMM profiles

hhmake_st <- unique(prot_DF_PSC_clust$hhmake_status)
if(length(hhmake_st) > 1 | (length(hhmake_st) == 1 & hhmake_st != 0))
{
  hhmake_report_DF <- prot_DF_PSC_clust %>%
    filter(hhmake_status > 0)
  
  saveRDS(hhmake_report_DF, file = paste0(P_D, "/error_DF_03C_04.RDS"))
  
  print("Incomplete HMM creation in step 3B_04. From ", nrow(prot_DF_PSC_clust), " HMM profiles, ", nrow(hhmake_report_DF), " were NOT created. Aborting VirClust.")
  
  quit()
}
rm(hhmake_st)

### 05. Make hmm_cluster_DB #################################################
st05_D <- paste0(P_D, "/03C_05_hmmDB")
if(dir.exists(st05_D)){unlink(st05_D, recursive = TRUE)}
dir.create(st05_D)

#system(paste0("/opt/hhsuite3/bin/ffindex_build ", st05_D, "/search_DB_a3m.ffdata ", st05_D, "/search_DB_a3m.ffindex ", st03_D))
if(sing %in% c("sing", "conda"))
{comfin <- "ffindex_build"}else{comfin <- "/opt/hhsuite3/bin/ffindex_build"}

ffindex_build1_st <- sys::exec_wait(cmd = comfin,
                                   args = c(paste0(st05_D, "/search_DB_a3m.ffdata"), 
                                            paste0(st05_D, "/search_DB_a3m.ffindex"),
                                            st03_D), 
                                   std_out = paste0(st05_D, "/stdout_ffindex_build1.txt"),
                                   std_err = paste0(st05_D, "/stderr_ffindex_build1.txt"))
check_exec_st_fun(st = ffindex_build1_st, who = "FFindex for a3m files")
rm(ffindex_build1_st)

#system(paste0("/opt/hhsuite3/bin/ffindex_build ", st05_D, "/search_DB_hhm.ffdata ", st05_D, "/search_DB_hhm.ffindex ", st04_D, "/out"))

ffindex_build2_st <- sys::exec_wait(cmd = comfin, 
                                    args = c(paste0(st05_D, "/search_DB_hhm.ffdata"), 
                                             paste0(st05_D, "/search_DB_hhm.ffindex"),
                                             paste0(st04_D, "/out")), 
                                    std_out = paste0(st05_D, "/stdout_ffindex_build2.txt"), 
                                    std_err = paste0(st05_D, "/stderr_ffindex_build2.txt"))
check_exec_st_fun(st = ffindex_build2_st, who = "FFindex for hmm files")
rm(ffindex_build2_st, comfin)

#system(paste0("cstranslate -A /opt/hhsuite3/data/cs219.lib -D /opt/hhsuite3/data/context_data.lib  -f -x 0.3 -c 4 -I a3m -i ", st05_D, "/search_DB_a3m -o ",
#              st05_D, "/search_DB_cs219"))

if(sing == "yes")
  #hhsuite from apt-gt
  #{csp <- "/usr/lib/hhsuite/data/cs219.lib"
  #conp <- "/usr/lib/hhsuite/data/context_data.lib"}
  #with the hhsuite version from Rhea
{csp <- "/home/cristinam/bin/hhsuite3/data/cs219.lib"
conp <- "/home/cristinam/bin/hhsuite3/data/context_data.lib"}else
  #directly on Rhea
{
  if(sing == "rhea")
  {
    {csp <- "/opt/hhsuite3/data/cs219.lib"
    conp <- "/opt/hhsuite3/data/context_data.lib"}
  }else
  { #in conda env
    if(sing == "conda")
    {
      csp <- paste0(condaenvpath, "/data/cs219.lib")
      conp <- paste0(condaenvpath, "/data/context_data.lib")
    }
  }
}

cstranslate_st <- sys::exec_wait(cmd = "cstranslate", 
                                 args = c("-A", csp,
                                          "-D", conp,
                                          "-f",
                                          "-x", "0.3",
                                          "-c", "4", 
                                          "-I", "a3m",
                                          "-i", paste0(st05_D, "/search_DB_a3m"),
                                          "-o", paste0(st05_D, "/search_DB_cs219")),
                                 std_out = paste0(st05_D, "/stdout_cstranslate.txt"), 
                                 std_err = paste0(st05_D, "/stderr_cstranslate.txt"))

check_exec_st_fun(st = cstranslate_st, who = "Ctranslate")
rm(cstranslate_st, csp, conp)

system(paste0("LC_ALL=C sort ", st05_D, "/search_DB_hhm.ffindex > ", st05_D, "/search_DB_hhm_sort.ffindex"))

# sort1_st <- sys::exec_wait(cmd= "LC_ALL=C sort", 
#                            args = c(#"sort",
#                                     paste0(st05_D, "/search_DB_hhm.ffindex"),
#                                     ">",
#                                     paste0(st05_D, "/search_DB_hhm_sort.ffindex")),
#                            std_out = paste0(st05_D, "/stdout_sort1.txt"), 
#                            std_err = paste0(st05_D, "/stderr_sort1.txt"))
# 
# check_exec_st_fun(st = sort1_st, who = "Sort hmm ffindex")
# rm(sort1_st)

system(paste0("LC_ALL=C sort ", st05_D, "/search_DB_a3m.ffindex > ", st05_D, "/search_DB_a3m_sort.ffindex"))
# sort2_st <- sys::exec_wait(cmd= "LC_ALL=C", 
#                            args = c("sort",
#                                     paste0(st05_D, "/search_DB_a3m.ffindex"),
#                                     ">",
#                                     paste0(st05_D, "/search_DB_a3m_sort.ffindex")),
#                            std_out = paste0(st05_D, "/stdout_sort2.txt"), 
#                            std_err = paste0(st05_D, "/stderr_sort2.txt"))
# check_exec_st_fun(st = sort2_st, who = "Sort a3m index")
# rm(sort2_st)


#below I'm removing and renaming some HMM DB files
system(paste0("rm ", st05_D, "/search_DB_a3m.ffindex"))
system(paste0("rm ", st05_D, "/search_DB_hhm.ffindex"))

system(paste0("mv ", st05_D, "/search_DB_a3m_sort.ffindex ", st05_D, "/search_DB_a3m.ffindex"))
system(paste0("mv ", st05_D, "/search_DB_hhm_sort.ffindex ", st05_D, "/search_DB_hhm.ffindex"))


### 06. Search hmms vs hmm_cluster_DB ##############################################
st06_D <- paste0(P_D, "/03C_06_hhsearch_out")
if(dir.exists(st06_D)){unlink(st06_D, recursive = TRUE)}
dir.create(st06_D)

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  mutate(hhr_p = paste0(st06_D, "/cluster_", clust_ID, ".hrr")) %>%
  mutate(hhsearch_tsv_p = paste0(st06_D, "/cluster_", clust_ID, ".tsv")) %>%
  mutate(hhsearch_log_p = paste0(st06_D, "/cluster_", clust_ID, ".log")) %>%
  mutate(hhsearch_err_p = paste0(st06_D, "/cluster_", clust_ID, ".err")) #%>%
  # mutate(hhsearch_cmd = paste0("/opt/hhsuite3/bin/hhsearch -i ", hmm_p, " -o ", hhr_p, " -blasttab  ", hhsearch_tsv_p,
  #                              " -d ", st05_D, "/search_DB -id 100 -diff 0 -cpu 20 -p 50 -z 1 -Z ", as.character(nrow(prot_DF_PSC_clust) + 600000),
  #                              # -Z sets the number of maximum hits that should appear in the list hrr and log file; it overrides the -p argument if there are more hits >p that Z
  #                              # -z sets the number of minimum hots that should appear in the list hrr and log file, regardless of the -p (even if probab is lower than p, at least z hits would appear)
  #                              " > ", hhsearch_log_p, " 2> ", hhsearch_err_p))


for(i in 1:nrow(prot_DF_PSC_clust))
{
  prot_DF_PSC_clust[[i, "hmmsearch_status"]] <- sys::exec_wait(cmd = "hhsearch", 
                                                             args = c("-i", prot_DF_PSC_clust$hmm_p[[i]],
                                                                      "-o", prot_DF_PSC_clust$hhr_p[[i]],
                                                                      "-blasttab", prot_DF_PSC_clust$hhsearch_tsv_p[[i]],
                                                                      "-d", paste0(st05_D, "/search_DB"),
                                                                      "-id", "100",
                                                                      "-diff", "0",
                                                                      "-cpu", "20",
                                                                      "-p", "50",
                                                                      "-z", "1", 
                                                                      "-Z", as.character(nrow(prot_DF_PSC_clust) + 600000)), 
                                                             std_out = prot_DF_PSC_clust$hhsearch_log_p[[i]],
                                                             std_err = prot_DF_PSC_clust$hhsearch_err_p[[i]])
}
rm(i)


### 07. Filter and write 2nd abc file ##########################################
st07_D <- paste0(P_D, "/03C_07_in_mcl")
if(file.exists(st07_D)){unlink(st07_D, recursive = TRUE)}
dir.create(st07_D)

### #test
#tsv_p <- prot_DF_PSC_clust$hhsearch_log_p[1]
#clust_ID <- prot_DF_PSC_clust$clust_ID[1]

import_hhsearch_res_fun <- function(tsv_p, clust_ID)
{
  if(file.size(tsv_p) > 0)
  {
    score_TB <- read_table2(file = tsv_p , skip = 8, col_names = TRUE, progress = FALSE,
                            col_types = cols(
                              No = col_double(),
                              Hit = col_character(),
                              Prob = col_double(),
                              `E-value` = col_character(),
                              `P-value` = col_character(),
                              Score = col_double(),
                              SS = col_double(),
                              Cols = col_double(),
                              Query = col_character(),
                              HMM = col_character(),
                              Template = col_character(),
                              HMM_1 = col_character()
                            )) %>%
      separate(col = HMM, into = c("del", "s_len2"), sep = "\\(", remove = FALSE) %>%
      replace_na(replace = list(s_len2 = "")) %>%
      mutate(s_len2 = str_remove(s_len2, "\\)")) %>%
      select(-Query, -HMM, -HMM_1, -No, -del) %>%
      mutate(s_len1 = str_remove(Template, "\\(")) %>%
      mutate(s_len1 = str_remove(s_len1, "\\)")) %>% # s_len1 is the length of the subject HMM
      replace_na(replace = list(s_len1 = "")) %>%
      unite(col= s_len, s_len2, s_len1, remove = FALSE, sep = "") %>%
      select(-Template, -s_len2, -s_len1) %>%
      mutate(Cols = as.numeric(Cols), s_len = as.numeric(s_len)) %>%
      mutate(scov = Cols*100/s_len) ##I'm calculating subject coverage;
    
    query_len_TB <- score_TB %>%
      filter(str_detect(Hit, paste0("cluster_", clust_ID, "_prot_")))
    
    score_TB$q_HMM_len <- query_len_TB$s_len[[1]]
    rm(query_len_TB)
    
    score_TB <- score_TB %>%
      mutate(qcov = Cols*100/q_HMM_len) %>% ## I'm calculating the query coverage
      filter((Prob >= prob1_PSSC & scov >= cov1_PSSC & qcov >= cov1_PSSC) | (Prob >= prob2_PSSC & scov >= cov2_PSSC & qcov >=cov2_PSSC & Cols >= alig_PSSC)) %>% #I'm filtering hits
      mutate(query_cluster = paste0("cluster_", clust_ID)) %>%
      mutate(target_cluster = str_remove(Hit, "_prot_\\d*$")) %>%
      rename(evalue = 'E-value', pvalue = 'P-value')
  }else
  {
    score_TB <- tibble(Hit = "none",
                       Prob = 0,
                       evalue = "none",
                       pvalue = "none",
                       Score = 0,
                       SS = 1000,
                       Cols = 0,
                       s_len = 0,
                       scov = 0,
                       q_HMM_len = 0,
                       qcov = 0,
                       query_cluster = paste0("cluster_", clust_ID),
                       target_cluster = "none")
  }
  
  return(score_TB)
}

#print("------------------------------------------------ The following messages regarding parsing errors should be ignored!")
#print("--------------------------------------Start parsing error messages:")
score_TB_ls <- map2(.x = prot_DF_PSC_clust$hhsearch_log_p, .y = prot_DF_PSC_clust$clust_ID, .f =  import_hhsearch_res_fun)
#print("--------------------------------------End parsing error messages.")

score_TB <- bind_rows(score_TB_ls) %>%
  filter(target_cluster != "none" & Hit != "none")
rm(score_TB_ls)

if(clust_PSSC == "evalue")
{
  for_mcl_TB <- score_TB%>%
    select(query_cluster, target_cluster, evalue) ###here I can have multiple rows for the same query-target cluster pairs; should I keep only the one with the best eval????
  # change notation of zeroes and add quotes around everything
  for_mcl_TB$evalue[for_mcl_TB$evalue == "0"] <- "0.0"
  
  for_mcl_TB <- for_mcl_TB %>%
    mutate(query_cluster = paste0('\"', query_cluster, '\"')) %>%
    mutate(target_cluster = paste0('\"', target_cluster, '\"')) %>%
    mutate(evalue = paste0('\"', evalue, '\"'))
  
}

if(clust_PSSC == "evalue_log")
{
  for_mcl_TB <- score_TB %>%
    mutate(evalue_log10 = -log10(as.numeric(evalue))) %>%
    mutate(evalue_log10_capped = if_else(condition = {evalue_log10 > 200}, 
                                         true = 200,
                                         false = evalue_log10)) %>%
    select(query_cluster, target_cluster, evalue_log10_capped)
}

if(clust_PSSC == "bitscore")
{
  for_mcl_TB <- score_TB %>%
    select(query_cluster, target_cluster, Score)
}

if(clust_PSSC == "norm_bitscore")
{
  score_TB$norm_bitscore <- map2(.x = score_TB$query_cluster, .y = score_TB$target_cluster, .f = norm_Score_HMM_fun, DF = score_TB) %>%
    unlist()
  
  for_mcl_TB <- score_TB %>%
    select(query_cluster, target_cluster, norm_bitscore)
}


# write the mcl input as a file
hmm_abc_file <- paste0(st07_D, "/hmm_scores_for_mcl2.abc")
fwrite(for_mcl_TB, hmm_abc_file, quote = FALSE,
       sep = "\t", col.names = FALSE, row.names = FALSE)

rm(score_TB)


### 08. Making PSSCs #######################################################
st08_D <- paste0(P_D, "/03C_08_mcl_out")
if(dir.exists(st08_D)){unlink(st08_D, recursive = TRUE)}
dir.create(st08_D)

mcl_clust_path <- paste0(st08_D, "/mcl_super_superclusters.txt")

#mcl_cmd <- paste0("mcl ", hmm_abc_file, " -I 2 --abc -o ", mcl_clust_path, " 2> ", mcl_log_p)
#system(mcl_cmd)

mcl_status <- sys::exec_wait(cmd = "mcl", 
               args = c(hmm_abc_file, 
                        "-I", "2",
                        "--abc",
                        "-o", mcl_clust_path), 
               std_out = paste0(st08_D, "/mcl_log.txt"),
               std_err = paste0(st08_D, "/mcl_err.txt")
               )
check_exec_st_fun(st=mcl_status, who = "MCL for PSSCs")
rm(hmm_abc_file, mcl_status)


### 17. Add PSSCs to prot_DF_PSC_clust
mcl_super_clust_DF <- read.csv(file = mcl_clust_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rm(mcl_clust_path)

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  mutate(clust_ID = paste0("cluster_", clust_ID))

for(i in 1:nrow(mcl_super_clust_DF))
{
  prot_ls <- mcl_super_clust_DF[i,] %>%
    as.list() %>%
    unname() %>%
    unlist()
  prot_ls <- prot_ls[prot_ls!=""]
  
  prot_DF_PSC_clust[prot_DF_PSC_clust$clust_ID %in% prot_ls, "PSSC_ID"] <- i
  rm(prot_ls)
}
rm(i, mcl_super_clust_DF)

prot_DF_PSC_clust <- prot_DF_PSC_clust %>%
  select(-mfasta_p, -alig_mfasta_p, -a3m_p, -hmm_p, -hhr_p, -hhsearch_tsv_p, -no_prots, -hhsearch_log_p, -hhsearch_err_p) %>%
  unnest(data) %>%
  ungroup() %>%
  select(genome_name, gene_ID, gene_start, gene_end, gene_length, strand, frame, protein_name, protein_ID, PC_ID, PSC_ID, PSSC_ID, protein_length, protein_seq)

rm(st01_D, st02_D, st03_D, st04_D, st05_D, st06_D, st07_D, st08_D)


### 18. Save prot_DF_PSC_clust #############################################################
DF_path <- paste0(P_D, "/03C_09_genome_protDF_PSCs_PSSCs.RDS")
if(file.exists(DF_path)){file.remove(DF_path)}
saveRDS(prot_DF_PSC_clust, DF_path)
rm(DF_path)


tsv_p <- paste0(P_D, "/03C_09_genome_protDF_PSCs_PSSCs.tsv")
write.table(x = prot_DF_PSC_clust, file = tsv_p, sep = "\t", row.names = FALSE, col.names = TRUE)
rm(tsv_p)


print("Step 3C, from PSCs to PSSCs, has finished successfully.")