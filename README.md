# VirClust v2.0

## VirClust is a bioinformatics tool which can be used for:  

• virus clustering  

• protein annotation  

• core protein calculation  

At its core is the grouping of viral proteins into clusters of three different levels:  

• at the first level, proteins are grouped based on their reciprocal BLASTP similarities into protein clusters, or PCs.  

• at the second level, PCs are grouped based on their Hidden Markov Model (HMM) similarities into protein superclusters, or PSCs.  

• at the third, still experimental level, PSCs are grouped based on their HMM similarities into protein super-superclusters, or PSSC.  

VirClust if available at http://virclust.icbm.de as a web-service or as a singularity package. In addition, the source code of VirClust v2.0 is available in this repository.
This repository contains:
- the source-code for the stand-alone VirClust, written in R
- the source-code for the VirClust shiny-app, which provides a grahical user interface for the stand-alone VirClust
- the manuals
- the YAML file for the VirClust environment, needed when running VirClust from the source-code


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
- downdload and install the databases used for annotation
```R
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  #Complex heatmap
  if (!require("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager") 
  BiocManager::install("ComplexHeatmap" ) 
  # this.path
  install.packages("this.path")
```
- dksjlkjf

