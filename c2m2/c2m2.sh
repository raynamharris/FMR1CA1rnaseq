#!/bin/bash

rm rmh_2022_04_08/*tsv
cp blank_nonCV_C2M2_tables/*tsv rmh_2022_04_08/
Rscript c2m2.R