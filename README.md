# FMR1rnaseqCA1

The GitHub Repository for "FMR1-KO knockdown alters gene expression of some autism disorder risk genes"

## Data organization

1. UNIXworkflow - runs on The Texas Advanced Computing Center's (TACC) cluster 'Stampede'
2. data
	- the files with the 01 and 02 prefix are semi-raw files for starting analysis
	- all other files are results files or meta data
3. scripts
	- 00_cpfiles.sh	- a bash script for copying files into and out of this repo
	- 01_behavior.Rmd - knittable analysis script for wrangling, analyzing, and visualizing avoidance behavior 
	- 01_behavior.md - output of above script
	- 02_RNAseq.Rmd	- knittable analysis of differential gene expression of RNA-seq data with DESeq2, GO-WMU, and WGCNA
	- 02_RNAseq.md	- output of above script
	- 03_wgcna.R	- R script for weighted gene co-expression network analysis
	- 04_ephys.Rmd	- knittable analysis hippocampal electrophysiology to measure synaptic properties
	- 04_ephys.md - output of above script
	- 05_Ceolin.Rmd - a replication of a recently published paper
	- 06_GO_MWU	- directory with data, code, and results for Gene Ontology analysis
	- figureoptions.R - my color pallet of choice for this experiment

## Data Availability

- NCBI: The raw sequencing data, the counts matrix, and meta data at https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595
- GitHub: A collection of RNA-seq datasets listed by GEO accession number https://github.com/raynamharris/MouseHippocampusRNAseqData

## Citation 

- For the NCBI data archive, include this inline: ([Accession: GSE106595](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595))
- For the GitHub data-only archive cite [![DOI](https://zenodo.org/badge/94957366.svg)](https://zenodo.org/badge/latestdoi/94957366)
- For the GitHub data, code, and results repository cite [![DOI](https://zenodo.org/badge/101933073.svg)](https://zenodo.org/badge/latestdoi/101933073)
- For the paper: cite the soon to be release bioRxiv paper or my thesis

## Scientific insights

See [08_results.md](./scripts/08_results.md) for a full write up of the results with figures. 

