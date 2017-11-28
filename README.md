# FMR1rnaseqCA1

## Data organization

1. UNIXworkflow 
1. data
1. scripts

- UNIXworkflow - runs on The Texas Advanced Computing Center's (TACC) cluster 'Stampede'
- data
	- the files with the 01 and 02 prefix are semi-raw files for starting analysis
	- all other files are results files or meta data
- scripts
	- 06_GO_MWU	- directory
	- 00_cpfiles.sh	- a bash script for copying files into and out of this repo
	- 01_behavior.Rmd - knittable analysis script for wrangling, analyzing, and visualizing avoidance behavior 
	- 01_behavior.md - output of above script
	- 02_RNAseq.Rmd	- knittable analysis of differential gene expression of RNA-seq data with DESeq2, GO-WMU, and WGCNA
	- 02_RNAseq.md	- output of above script
	- 03_wgcna.R	- R script for weighted gene co-expression network analysis
	- 04_ephys.Rmd	- knittable analysis hippocampal electrophysiology to measure synaptic properties
	- 04_ephys.md - output of above script
	- figureoptions.R - my color pallet of choice for this experiment

## Data Availability

- NCBI: The raw sequencing data, the counts matrix, and meta data at https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595
- GitHub: This and more RNA-seq data can be found GitHub at https://github.com/raynamharris/MouseHippocampusRNAseqData

## Citation and re-use 

- For the NCBI data archive, include this inline: (Accession: GSE106595)
- For the GitHub data-only archive Harris 2017 MouseHippocampusRNAseqData Zenodo 10.5281/zenodo.815048
- For the GitHub data, code, and results repository cite Harris 2017
- For the paper: cite the soon to be release bioRxiv paper or my thesis

## Scientific insights

These are the main conclusions from the research




