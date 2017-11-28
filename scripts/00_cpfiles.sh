# import raw and cleaned data
# mv 02f_wgcna_RNAseq.R ../../FMR1rnaseqCA1/00_wgcna.R
cp ../../BehavEphyRNAseq/data/rnaseq/fmr1* .
cp ../../BehavEphyRNAseq/data/rnaseq/FMR1MetaData.xls .
cp ../../BehavEphyRNAseq/data/rnaseq/geneids.csv .
cp -r ../../BehavEphyRNAseq/data/rnaseq/JA17009/ .
cp ../../BehavEphyRNAseq/data/behavior/fmr1.csv .

# seeding the folder with files from previous research
#cp ../../IntegrativeProjectWT2015/RmdFiles/functions_behavior.R  .
#cp ../../IntegrativeProjectWT2015/RmdFiles/figureoptions.R .
#cp ../../IntegrativeProjectWT2015/RmdFiles/02a_rnaseq_makedfs.Rmd  .
#cp ../../IntegrativeProjectWT2015/RmdFiles/02a_rnaseq_makedfs.Rmd  .
#cp ../../IntegrativeProjectWT2015/RmdFiles/functions_RNAseq.R .

# output for public repositories

cp ../data/fmr1ColData.csv	../../MouseHippocampusRNAseqData/GSE106595_fmr1ColData.csv
cp ../data/fmr1CountData.csv ../../MouseHippocampusRNAseqData/GSE106595_fmr1CountData.csv