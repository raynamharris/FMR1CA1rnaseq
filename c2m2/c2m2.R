library(tidyverse)

# https://github.com/nih-cfde/published-documentation/wiki/
# submission-prep-script  C2M2-Table-Summary Quickstart

# set working directory to source file location

# download blank files and save in "osfstorage-archive/"

dcc_id = "cfde_registry_dcc:test1"
id_namespace = "tag:raynamharris.com"
id_description = "Testing account for Rayna Harris"
id_name = "raynamharris"
id_abbreviation = "RMH"
contact_email = "rmharris@ucdavis.edu"
contact_name = "Rayna"

project_id_namespace = "https://www.ncbi.nlm.nih.gov/bioproject/"
project_local_id = "PRJNA417316"
project_abbreviation = "FMR1CA1RNASeq"
project_name = "FMR1 CA1 RNA-Seq"
project_description = "Transcriptonal profiling of the CA1 subfield of the hippocampus of Fmr1 knockout mouse"

biosample_id_namespace = "https://www.ncbi.nlm.nih.gov/sra/"
biosample_abbreviation = "SRA"
biosample_name = "SRA"
biosample_description = "Sequence Read Archive"

subject_id_namespace = "https://www.ncbi.nlm.nih.gov/biosample/"
subject_abbreviation = "GEO"
subject_name = "NCBI GEO"
subject_description = "NCBI GEO"

creation_time = "2017-11-06"

outdir = "rmh_2022_04_12/"

getcolnames <- function(file){
  tsvcolnames <- read_tsv(file) %>% 
    colnames()
  return(tsvcolnames)
}

dcc_cols <- getcolnames("osfstorage-archive/dcc.tsv")
dcc_cols

dcc.tsv <- data.frame(id = dcc_id,
                      dcc_name = id_name,
                      dcc_abbreviation = id_abbreviation,
                      dcc_description = id_description,
                      contact_email = contact_email,
                      contact_name = contact_name,
                      dcc_url = id_namespace,
                      project_id_namespace = project_id_namespace,
                      project_local_id = project_local_id)
dcc.tsv

project_cols <- getcolnames("osfstorage-archive/project.tsv")
project_cols

project.tsv <- data.frame(id_namespace = project_id_namespace,
                          local_id = project_local_id,
                          persistent_id = NA,
                          creation_time = creation_time,
                          abbreviation = project_abbreviation,
                          name = project_name,
                          description = project_description)
project.tsv

biosample_cols <- getcolnames("osfstorage-archive/biosample.tsv") 
biosample_cols

biosample.tsv <- data.frame(id_namespace = biosample_id_namespace,
                            local = "SRX",
                            id = 3368300:3368315,
                            project_id_namespace = project_id_namespace,
                            project_local_id = project_local_id, 
                            persistent_id = NA,
                            creation_time = creation_time,
                            assay_type = "OBI:0002571",
                            anatomy = "UBERON:0003881") %>%
  mutate(local_id = paste(local, id, sep = "")) %>%
  select(all_of(biosample_cols))
biosample.tsv

biosample_gene_cols <- getcolnames("osfstorage-archive/biosample_gene.tsv") 
biosample_gene_cols

biosample_gene.tsv <- biosample.tsv %>%
  mutate(gene =  "ENSG00000102081") %>%
  dplyr::rename(biosample_id_namespace = id_namespace,
                biosample_local_id = local_id) %>%
  dplyr::select(all_of(biosample_gene_cols))
biosample_gene.tsv



subject_cols <- getcolnames("osfstorage-archive/subject.tsv") 
subject_cols


subject.tsv <- data.frame(id_namespace = subject_id_namespace,
                          local_id = 7982524:7982547,
                          project_id_namespace = project_id_namespace,
                          project_local_id = project_local_id, 
                          persistent_id = NA,
                          creation_time = creation_time,
                          granularity = "cfde_subject_granularity:0",
                          sex = "cfde_subject_sex:2",
                          ethnicity = NA,
                          age_at_enrollment = NA) %>%
  select(all_of(subject_cols))
subject.tsv



id_namespace_cols <- getcolnames("osfstorage-archive/id_namespace.tsv")
id_namespace_cols

id_namespace.tsv <- data.frame(id = id_namespace,
                               abbreviation = id_abbreviation,
                               name = id_name,
                               description = id_description) %>%
  rbind(., c(project_id_namespace, project_abbreviation, project_name, project_description)) %>%
  rbind(., c(biosample_id_namespace, biosample_abbreviation, biosample_name, biosample_description)) %>%
  rbind(., c(subject_id_namespace, subject_abbreviation, subject_name, subject_description))
id_namespace.tsv


# unused
# granularity: 0" 
# "NCBI:txid10090" 


###################################################################

# save files

savefiles <- function(object){
  print(paste("Writing", substitute(object), "to file", sep = " "))
  write.table(object, file = paste0(outdir, substitute(object), sep = ""),
              sep ="\t", row.names = F, col.names = T, quote = F, na = "")
}

savefiles(dcc.tsv)
savefiles(project.tsv)
savefiles(id_namespace.tsv)


savefiles(biosample.tsv)
savefiles(biosample_gene.tsv)
savefiles(subject.tsv)

## in the terminal

# scp tsv files to FARM  
# follow https://github.com/nih-cfde/published-documentation/wiki/submission-prep-script
# run the curl commands below
# run python prepare_C2M2_submission.py 
# scp autogenerated tsv files from FARM
# activate cfde-env
# CFDE_SUBMIT_SERVICE_INSTANCE=dev
# cfde-submit run rmh_2022_04_11 --dcc-id test1

# curl commands

#curl -L https://osf.io/c67sp/download > prepare_C2M2_submission.py

#mkdir external_CV_reference files
#cd external_CV_reference

#curl -L https://osf.io/aef7g/download > compound.tsv.gz
#curl -L https://osf.io/msfrq/download > doid.version_2021-10-12.obo
#curl -L https://osf.io/nq6u9/download > EDAM.version_1.25.tsv
#curl -L https://osf.io/5sxvt/download > ensembl_genes.tsv
#curl -L https://osf.io/wcvuy/download > ensembl_organism_name_to_ncbi_taxonomy_id.tsv
#curl -L https://osf.io/wcvuy/download > ensembl_organism_name_to_ncbi_taxonomy_id.tsv
#curl -L https://osf.io/3k9e7/download > hp.2022-02-14.obo
#curl -L https://osf.io/rq6au/download > hp.phenotype_to_genes.txt
#curl -L https://osf.io/trewg/download > ncbi_taxonomy.tsv.gz
#curl -L https://osf.io/76jnk/download > OBI.provisional_terms.2022-01-06.tsv
#curl -L https://osf.io/mv4by/download > OBI.version_2021-08-18.obo
#curl -L https://osf.io/gy932/download > substance.tsv.gz
