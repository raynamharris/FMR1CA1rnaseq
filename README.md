# FMR1rnaseqCA1

The GitHub Repository for "FMR1-KO knockdown alters gene expression of some autism disorder risk genes"

## Repository organization and workflow

1. UNIXworkflow - runs on The Texas Advanced Computing Center's (TACC) cluster 'Stampede'
2. data
	- the files with the 01 and 02 prefix are semi-raw files for starting analysis
	- all other files are results files or meta data
3. scripts - part 1
	- [00_cpfiles.sh](./scripts/00_cpfiles.sh)	- a bash script for copying files into and out of this repo
	- [01_behavior.md](./scripts/01_behavior.md) - output of a knittable analysis script for wrangling the avoidance behavior  data. The output is use for the `Fig*.Rmd` scripts that accompany each figure 
	- [02_RNAseq.md](./scripts/02_RNAseq.md)	- output of an knittable analysis of differential gene expression of RNA-seq data with DESeq2, GO-WMU, and WGCNA
	- [03_wgcna.R](./scripts/03_wgcna.R)	- R script for weighted gene co-expression network analysis
	- [04_ephys.Rmd](./scripts/04_ephys.Rmd) - analysis of hippocampal electrophysiology to measure synaptic properties -- note: must be run from the R command line as knitting doesn't currently work well
	- [05_Ceolin.md](./scripts/05_Ceolin.Rmd) - a **replication** of a recently published paper and a **comparison** to my own data
	- [06_GO_MWU](./scripts/06_GO_MWU)	- directory with data, code, and results for Gene Ontology analysis
	- [figureoptions.R](./scripts/figureoptions.R) - my color pallet of choice for this experiment
4. scripts - part 2
	- Because I had so many behavior figures and accompanying statistics, I created a Rmd script containing the code to make the figure and calculate the statistics. 
	-The data were generated in 01_behavior.Rmd. 
	The markdown files with the statistics and the sub fiures are here: [1](./scripts/Fig1.md), [2](./scripts/Fig2.md), [3](./scripts/Fig3.md), [4](./scripts/Fig4.md), [5](./scripts/Fig5.md), [6](./scripts/Fig6.md), [7](./scripts/Fig7.md).
	
5. results
	- In the R script accompanying this file, [08_results.md](./scripts/08_results.md), I write the results section for my manuscript. 
	- This document is an executable way for me to keep the results section under version control and with newest version of the figure. 
	- I alternate between exporting the result to a `md_document` and a `word_document` for writing my thesis
	- This is an overview of all nine results figures that accompany this research:
	
![](./figures/07_colorblindr/overview-1.png)

## Data Availability

- NCBI: The raw sequencing data, the counts matrix, and meta data at https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595
- GitHub: A collection of RNA-seq datasets listed by GEO accession number https://github.com/raynamharris/MouseHippocampusRNAseqData

## Citation 

- For the NCBI data archive, include this inline: ([Accession: GSE106595](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595))
- For the GitHub data, code, and results repository cite [![DOI](https://zenodo.org/badge/101933073.svg)](https://zenodo.org/badge/latestdoi/101933073)
- For the paper: cite the soon to be released bioRxiv paper or my thesis