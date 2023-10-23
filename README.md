# VirClust v2.0

## VirClust is a bioinformatics tool which can be used for:  

- virus clustering  

- protein annotation  

- core protein calculation  

At its core is the grouping of viral proteins into clusters of three different levels:  

- at the first level, proteins are grouped based on their reciprocal BLASTP similarities into protein clusters, or PCs.  

- at the second level, PCs are grouped based on their Hidden Markov Model (HMM) similarities into protein superclusters, or PSCs.  

- at the third level, PSCs are grouped based on their HMM similarities into protein super-superclusters, or PSSC.  

VirClust if available at http://virclust.icbm.de as a web-service or as a singularity package. In addition, the source code of VirClust v2.0 is available in this repository.
This repository contains:
- the source-code for the stand-alone VirClust, written in R (https://github.com/CristinaMoraru/VirClust/tree/main/VirClust/vir_clust_standalone)
- the source-code for the VirClust shiny-app, which provides a grahical user interface for the stand-alone VirClust (https://github.com/CristinaMoraru/VirClust/tree/main/VirClust)
- the manuals (https://github.com/CristinaMoraru/VirClust/tree/main/VirClust/vir_clust_standalone/manuals)
- the YAML file for the VirClust environment, needed when running VirClust from the source-code (https://github.com/CristinaMoraru/VirClust/tree/main/VirClust/user-install_sourcecode/VirClust.yml)


## How to cite VirClust
If you are using any of the VirClust distributions in your work, please cite:  
Moraru, C. (2023) VirClus - A Tool for Hierarchical Clustering, Core Protein Detection and Annotation of (Prokaryotic) Viruses, Viruses 15(4), pp 1007, doi:  https://doi.org/10.3390/v15041007  

## How to install the VirClust source-code from this repository
- download the VirClust repository, e.g by using
  ```bash
    git clone https://github.com/CristinaMoraru/VirClust
  ```
- create a conda environment using the YAML file found in this repo folder: VirClust/ user-install_sourcecode/VirClust.yml
  ```bash
    conda env create -f VirClust.yml
    conda activate VirClust
  ```
- install some R libraries from inside R
  ```bash
    R
  ```
  
  ```R
    options(repos = c(CRAN = "https://cloud.r-project.org/"))
    #Complex heatmap
    if (!require("BiocManager", quietly = TRUE)) 
    install.packages("BiocManager") 
    BiocManager::install("ComplexHeatmap" ) 
    # this.path
    install.packages("this.path")
  ```
- download and install the databases used for annotation  
  VirClust uses the following databases for protein annotations: Efam, Efam-XC, PHROGS, pVOGs, VOGDB, 
  InterPro, or BLAST NR. These databases are not distributed with VirClust standalone. They should be downloaded and 
  installed separately.  
#### InterPro and BLAST NR databases
The InterPro database and InterProScan software should be downloaded and installed from here: https://www.ebi.ac.uk/interpro/download/InterProScan/ (comes together with the InterProScan software). VirClust singularity will connect to your InterProScan installation, as long as you provide the path toward its folder.  
The BLAST NR database should be downloaded and installed as instructed here: https://www.ncbi.nlm.nih.gov/books/NBK569850/ .
#### Efam, Efam-XC, PHROGS, pVOGs and VOGDB databases
The Efam, Efam-XC, PHROGS, pVOGs, and VOGDB databases can be downloaded from the VirClust website (see Download section), in a format compatible with VirClust. After downloading, you should remove them from the .tar.gz archive and place them all in a folder named dbs/, at a location of your choice.  
    
    The folder structure and the names of the subfolders for each database should be as follows:   
    + dbs/
      + Efam/
      + Efam_XC/
      + PHROGS/
      + pVOGs/
      + VOGDB/

## If you can't create the conda env from the YAML file, then create the VirClust environment manually:
```bash
conda create -n VirClust
conda activate VirClust

# install R and its packages
conda install -c conda-forge r-base
conda install -c conda-forge r-stringr #installs also magrittr
#conda install -c conda-forge r-magrittr
conda install -c conda-forge r-dplyr #installs also tibble, tidyselect
conda install -c conda-forge r-tidyr #installs also purrr
conda install -c conda-forge r-ggplot2 
conda install -c conda-forge r-seqinr
conda install -c conda-forge r-readr
conda install -c conda-forge r-data.table
conda install -c conda-forge r-zip
conda install -c conda-forge r-furrr #installs also future
conda install -c conda-forge r-ape
conda install -c conda-forge r-pvclust
conda install -c conda-forge r-dendextend #install also viridis
conda install -c conda-forge r-circlize
conda install -c conda-forge r-sys
conda install -c conda-forge r-DT #also installs knitr
conda install -c conda-forge r-shinyjs
conda install -c conda-forge r-shiny
conda install -c conda-forge r-shinyWidgets
conda install -c conda-forge r-shinythemes
conda install -c edurand r-complexheatmap

# Some R packages must be installed manually, as follows:
R
	options(repos = c(CRAN = "https://cloud.r-project.org/"))
	#install.packages("ComplexHeatmap")
					if (!require("BiocManager", quietly = TRUE))
					    install.packages("BiocManager")
					BiocManager::install("ComplexHeatmap")

	install.packages("this.path")

# install bioinf toools
conda install -c bioconda blast=2.14
conda install -c bioconda metagene_annotator
conda install -c bioconda hmmer
conda install -c bioconda hhsuite
conda install -c bioconda mcl
conda install -c bioconda clustalo
```

## How to run the VirClust source-code from this repository
- use the command
  ```bash
  Rscript path/to/VirClust_MASTER.R sing=conda condaenvpath=path/toward/VirClust/conda/env [...options]
  ```
- for options, see "Parameters for VirClust stand-alone" in the manual found here: https://github.com/CristinaMoraru/VirClust/tree/main/VirClust/vir_clust_standalone/manuals/VirClust-v2_manual_standalone-condadistrib.pdf
- In addition, the options below are mandatory when running the corresponding annotation steps
  ```bash
  interproscan=path/toward/interproscanfolder #when annotating against InterPro Db
  blastdb=path/toward/NRblastdb #when annotating against NR blast db
  databases=path/toward/databasefolder #when annotating against all other DBs
  ```
