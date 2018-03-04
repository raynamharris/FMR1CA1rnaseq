# RNAseq workflow on TACC

## The workflow
* **00_rawdata:** Download the data to scratch on Stampede with `00_gsaf_download.sh`. 
* **01_fastqc:** Evaluate the quality of the reads using the program FastQC.
* **02_kallisto:** Quantify transcript-level expression using Kallisto
* **03_NCBIGEO.md:** Prep for submitting files to NCBI's Gene Expression Omnibus database 