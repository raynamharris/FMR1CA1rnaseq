#!/bin/bash

rm rmh_2022_04_08/*tsv

cp blank_nonCV_C2M2_tables/*tsv rmh_2022_04_08/

Rscript c2m2.R

cp osfstorage-archive/a* rmh_2022_04_08/
cp osfstorage-archive/compound.tsv rmh_2022_04_08/
cp osfstorage-archive/data_type.tsv  rmh_2022_04_08/
cp osfstorage-archive/disease.tsv  rmh_2022_04_08/
cp osfstorage-archive/file_format.tsv  rmh_2022_04_08/
cp osfstorage-archive/gene.tsv  rmh_2022_04_08/
cp osfstorage-archive/ncbi_taxonomy.tsv  rmh_2022_04_08/
cp osfstorage-archive/phenotype*  rmh_2022_04_08/
cp osfstorage-archive/protein.tsv  rmh_2022_04_08/
cp osfstorage-archive/protein_gene.tsv  rmh_2022_04_08/
cp osfstorage-archive/substance.tsv  rmh_2022_04_08/
