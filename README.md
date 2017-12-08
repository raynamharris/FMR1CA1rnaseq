# FMR1CA1rnaseq: The GitHub Repository for "Reproducible approaches for studying behavior and transcription in a mouse model for autism"

## The forthcoming manuscript
This research has been written up as a chapter of my doctoral thesis. It will be modified before submitting to a jounral for peer review and publishing. Stayed tuned for citation details and links.

## Repository organization and workflow

1. [UNIXworkflow](./UNIXworkflow/) - runs on The Texas Advanced Computing Center's (TACC) cluster 'Stampede'
2. [data](./data/)
	- mostly raw data and meta data, some results included
3. [scripts](./scripts/)
	- [00_cpfiles.sh](./scripts/00_cpfiles.sh)	- a bash script for copying files into and out of this repo
	- [01_behavior.md](./scripts/01_behavior.md) - output of a knittable analysis script for wrangling the avoidance behavior  data. The output is use for the `Fig*.Rmd` scripts that accompany each figure 
	- [02_RNAseq.md](./scripts/02_RNAseq.md)	- output of an knittable analysis of differential gene expression of RNA-seq data with DESeq2, GO-WMU, and WGCNA
	- [03_wgcna.R](./scripts/03_wgcna.R)	- R script for weighted gene co-expression network analysis
	- [04_ephys.Rmd](./scripts/04_ephys.Rmd) - analysis of hippocampal electrophysiology to measure synaptic properties -- note: must be run from the R command line as knitting doesn't currently work well
	- [05_Ceolin.md](./scripts/05_Ceolin.Rmd) - a **replication** of a recently published paper and a **comparison** to my own data
	- [06_GO_MWU](./scripts/06_GO_MWU)	- directory with data, code, and results for Gene Ontology analysis
	- Because I had so many behavior figures and accompanying statistics, I created a Rmd script containing the code to make the figure and calculate the statistics. The data were generated in 01_behavior.Rmd. The markdown files with the statistics and the sub fiures are here: **Figures** [1](./scripts/Fig1.md), [2](./scripts/Fig2.md), [3](./scripts/Fig3.md), [4](./scripts/Fig4.md), [5](./scripts/Fig5.md), [6](./scripts/Fig6.md), [7](./scripts/Fig7.md).
	
5. [results](./results/)
	- In the R script accompanying this file, [08_results.md](./scripts/08_results.md), I write the results section for my manuscript. 
	- This document is an executable way for me to keep the results section under version control and with newest version of the figure. 
	- I alternate between exporting the result to a `md_document` and a `word_document` for writing my thesis

## Data Availability

- NCBI: The raw sequencing data, the counts matrix, and meta data at https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595
- GitHub: A collection of RNA-seq datasets listed by GEO accession number https://github.com/raynamharris/MouseHippocampusRNAseqData

## Citation 

- For the NCBI data archive, include this inline: [Accession: GSE106595](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE106595)
- For the GitHub data, code, and results repository cite [![DOI](https://zenodo.org/badge/101933073.svg)](https://zenodo.org/badge/latestdoi/101933073)
- For the paper: stay tuned!!
