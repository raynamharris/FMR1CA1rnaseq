library(tidyverse)
library(lubridate)

# https://github.com/nih-cfde/published-documentation/wiki/
# submission-prep-script  C2M2-Table-Summary Quickstart

# set working directory to source file location

my_dcc_id = "cfde_registry_dcc:rmh"
my_id_namespace = "raynamharris.com"
my_project_id_namespace = "raynamharris.com:FRM1"
project_local_id = "FMR1"
outdir = "rmh_2022_04_08/"

getcolnames <- function(file){
  tsvcolnames <- read_tsv(file) %>% 
    colnames()
  return(tsvcolnames)
}


dcc_colnames <- getcolnames("./blank_nonCV_C2M2_tables/dcc.tsv") 
dcc_colnames

dcc.tsv <- data.frame(id = my_dcc_id,
                      dcc_name = "Rayna M Harris",
                      dcc_abbreviation = "RMH",
                      dcc_description = "Testing account for Rayna Harris",
                      contact_email = "rmharris@ucdavis.edu",
                      contact_name = "Rayna",
                      dcc_url = my_id_namespace,
                      project_id_namespace = paste(my_id_namespace, project_local_id,sep = "_"),
                      project_local_id = project_local_id)
dcc.tsv

subject_colnames <- getcolnames("./blank_nonCV_C2M2_tables/subject.tsv") 
subject_colnames

df1 <- subject.tsv <- read_csv("../data/summer2016_edited.csv") %>%
  filter(Genotype %in% c("WT", "FMR1"),
         grepl("122B|122D|118B|118D", Mouse))  %>%
  mutate(local_id = paste(Mouse, Genotype, sep = "_"),
         id_namespace = my_id_namespace,
         project_id_namespace = my_project_id_namespace,
         project_local_id = project_local_id,
         sex = "Male", granularity = 0,
         persistent_id = NA, creation_time =  NA,
         ethnicity = NA, age_at_enrollment = NA ) 
  
subject.tsv <-  df1  %>% select(subject_colnames) 
subject.tsv

biosample_colnames <- getcolnames("blank_nonCV_C2M2_tables/biosample.tsv") 
biosample_colnames

df2 <- read_csv("../data/summer2016forRNAseq.csv") %>%
  filter(Genotype %in% c("WT", "FMR1"),
         grepl("122|118", Mouse)) %>%
  mutate(local_id = paste(Mouse, Genotype, Punch, sep = "_"),
         id_namespace = my_id_namespace,
         project_id_namespace = my_project_id_namespace,
         project_local_id = "FMR1",
         persistent_id = ifelse(Mouse == "16-122B", "GSM2843687",
                         ifelse(Mouse == "16-122D", "GSM2843688",
                         ifelse(Mouse == "16-118B", "GSM2843681",
                         ifelse(Mouse == "16-118D", "GSM2843682", NA)))),
         Ssample = ifelse(Mouse == "16-122B", "S1",
                                ifelse(Mouse == "16-122D", "S2",
                                       ifelse(Mouse == "16-118B", "S16",
                                              ifelse(Mouse == "16-118D", "S17", NA)))),
         creation_time = mdy(Date),
         assay_type = "OBI:0002571",
         anatomy = "UBERON:0003881") 


biosample.tsv <- df2 %>% dplyr::select(biosample_colnames)
biosample.tsv

biosample_from_subject_colnames <- getcolnames("./blank_nonCV_C2M2_tables/biosample_from_subject.tsv") 
biosample_from_subject_colnames

biosample_from_subject.tsv <- full_join(df1, df2,  by = c("Mouse", "Genotype", "Date", "id_namespace"))   %>%
  select(contains(".")) %>%
  mutate("biosample_id_namespace" = paste(project_id_namespace.y, local_id.y, sep = "_"),
         "biosample_local_id"  = local_id.y,
         "subject_id_namespace" = paste(project_id_namespace.x, local_id.x, sep = "_"), 
         "subject_local_id"  = local_id.x, 
         "age_at_sampling" = NA) %>%
  select(biosample_from_subject_colnames)
biosample_from_subject.tsv


file_colnames <- getcolnames("./blank_nonCV_C2M2_tables/file.tsv") 
file_colnames

file.tsv <- df2 %>%
  mutate(filename = paste(persistent_id, "abundance", Mouse, Ssample, "L002.tsv.gz", sep = "_"),
         filename = gsub("-", "_", filename),
         uncompressed_size_in_bytes = NA, sha256 = NA, md5 = NA,
         file_format = "format:3475",
         compression_format = "format:3989", size_in_bytes = 3200,
         mime_type = NA,   bundle_collection_id_namespace = NA ,
         data_type = "data:3495", bundle_collection_local_id = NA,
         dbgap_study_id = NA, 
         analysis_type = assay_type) %>%
  select(file_colnames)
file.tsv

biosample_disease_colnames <- getcolnames("./blank_nonCV_C2M2_tables/biosample_disease.tsv") 
biosample_disease_colnames

biosample_disease.tsv <- biosample_from_subject.tsv %>%
  mutate(association_type = NA,
         disease =  NA) %>%
  select(biosample_disease_colnames)
biosample_disease.tsv


biosample_gene_colnames <- getcolnames("./blank_nonCV_C2M2_tables/biosample_gene.tsv") 
biosample_gene_colnames

biosample_gene.tsv <- biosample.tsv %>%
  mutate(gene = ifelse(grepl("FMR1", local_id), "ENSG00000102081", NA)) %>%
  dplyr::rename(biosample_id_namespace = id_namespace,
                biosample_local_id = local_id) %>%
  dplyr::select(all_of(biosample_gene_colnames))
biosample_gene.tsv






###################################################################

# save files

savefiles <- function(object){
  print(paste("Writing", substitute(object), "to file", sep = " "))
  write.table(object, file = paste0(outdir, substitute(object), sep = ""),
              sep ="\t", row.names = F, col.names = T, quote = F, na = "")
}

savefiles(biosample_disease.tsv)
savefiles(biosample_from_subject.tsv)
savefiles(biosample_gene.tsv)
savefiles(biosample.tsv)
savefiles(file.tsv)
savefiles(subject.tsv)


