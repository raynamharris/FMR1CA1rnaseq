library(tidyverse)
library(lubridate)

# https://github.com/nih-cfde/published-documentation/wiki/
# submission-prep-script  C2M2-Table-Summary Quickstart

# set working directory to source file location

my_dcc_id = "cfde_registry_dcc:test1"
my_id_namespace = "raynamharris.com"
my_project_id_namespace = "raynamharris.com:FRM1"
my_description = "Testing account for Rayna Harris"
my_name = "raynamharris"
my_abbreviation = "RMH"
project_local_id = "PRJNA417316"
outdir = "rmh_2022_04_08/"

getcolnames <- function(file){
  tsvcolnames <- read_tsv(file) %>% 
    colnames()
  return(tsvcolnames)
}

dcc.tsv <- data.frame(id = my_dcc_id,
                      dcc_name = my_name,
                      dcc_abbreviation = my_abbreviation,
                      dcc_description = my_description,
                      contact_email = "rmharris@ucdavis.edu",
                      contact_name = "Rayna",
                      dcc_url = my_id_namespace,
                      project_id_namespace = paste(my_id_namespace, 
                                                   project_local_id,sep = ":"),
                      project_local_id = project_local_id)
dcc.tsv


id_namespace.tsv <- data.frame(id = my_id_namespace,
                               abbreviation = my_abbreviation,
                               name = my_name,
                               description = my_description)
id_namespace.tsv

project_cols <- getcolnames("./blank_nonCV_C2M2_tables/project.tsv")
project_cols


project.tsv <- data.frame(id_namespace = my_id_namespace,
                          local_id = project_local_id,
                          persistent_id = NA,
                          creation_time = NA,
                          abbreviation = my_abbreviation,
                          description = my_description)
project.tsv

subject_cols <- getcolnames("./blank_nonCV_C2M2_tables/subject.tsv") 
subject_cols

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
  
subject.tsv <-  df1  %>% select(subject_cols) 
subject.tsv

biosample_cols <- getcolnames("blank_nonCV_C2M2_tables/biosample.tsv") 
biosample_cols

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


biosample.tsv <- df2 %>% dplyr::select(biosample_cols)
biosample.tsv

biosample_from_subject_cols <- getcolnames("./blank_nonCV_C2M2_tables/biosample_from_subject.tsv") 
biosample_from_subject_cols

biosample_from_subject.tsv <- full_join(df1, df2,  
                                        by = c("Mouse", "Genotype", 
                                               "Date", "id_namespace"))   %>%
  select(contains(".")) %>%
  mutate("biosample_id_namespace" = paste(project_id_namespace.y, local_id.y, sep = "_"),
         "biosample_local_id"  = local_id.y,
         "subject_id_namespace" = paste(project_id_namespace.x, local_id.x, sep = "_"), 
         "subject_local_id"  = local_id.x, 
         "age_at_sampling" = NA) %>%
  select(biosample_from_subject_cols)
biosample_from_subject.tsv


file_cols <- getcolnames("./blank_nonCV_C2M2_tables/file.tsv") 
file_cols

file.tsv <- df2 %>%
  mutate(local_id = paste(local_id, Ssample, sep = "_"),
         filename = paste(persistent_id, "abundance", Mouse, 
                          Ssample, "L002.tsv.gz", sep = "_"),
         filename = gsub("-", "_", filename),
         uncompressed_size_in_bytes = NA, 
         sha256 = ifelse(grepl("S1", local_id), "38df0f90a70c280dfa7f1104229dad546e22599b30d6af92d84f778f27f5343f",
                         ifelse(grepl("S2", local_id), "b6da8abb446150e2f8de6030e9cdc34a926f6fac57168a0582efba621dea6427",
                                ifelse(grepl("S16", local_id), "501af4d7e188888fdff6557fc91c898b857357574480b11ec6712376ddfddce2",
                                       ifelse(grepl("S17", local_id), "e713a19cc47170be2086461b3d506c91acd3d3032da8312d76fc4ba754ddd1fc", NA)))), 
         md5 = NA,
         file_format = "format:3475",
         compression_format = "format:3989", size_in_bytes = 3200,
         mime_type = NA,   bundle_collection_id_namespace = NA ,
         data_type = "data:3495", bundle_collection_local_id = NA,
         dbgap_study_id = NA, 
         analysis_type = assay_type,
         persistent_id = ifelse(grepl("122B", local_id), "SRS2665919",
                                ifelse(grepl("122D", local_id), "SRS2665914",
                                       ifelse(grepl("118B", local_id), "SRS2665911",
                                              ifelse(grepl("118D", local_id), "SRS2665910", 
                                                     NA))))) %>%
  select(file_cols)
file.tsv$filename

df2$Ssample

biosample_disease_cols <- getcolnames("./blank_nonCV_C2M2_tables/biosample_disease.tsv") 
biosample_disease_cols

biosample_disease.tsv <- biosample_from_subject.tsv %>%
  mutate(association_type = NA,
         disease =  NA) %>%
  select(biosample_disease_cols)
biosample_disease.tsv


biosample_gene_cols <- getcolnames("./blank_nonCV_C2M2_tables/biosample_gene.tsv") 
biosample_gene_cols

biosample_gene.tsv <- biosample.tsv %>%
  mutate(gene = ifelse(grepl("FMR1", local_id), "ENSG00000102081", NA)) %>%
  dplyr::rename(biosample_id_namespace = id_namespace,
                biosample_local_id = local_id) %>%
  dplyr::select(all_of(biosample_gene_cols))
biosample_gene.tsv

file_describes_biosample_cols <- getcolnames("blank_nonCV_C2M2_tables/file_describes_biosample.tsv")
file_describes_biosample_cols

file_describes_biosample.tsv <- file.tsv %>%
  rename(file_id_namespace = id_namespace,
         file_local_id = local_id) %>%
  left_join(., biosample.tsv, by = "persistent_id") %>%
  rename(biosample_id_namespace = id_namespace,
         biosample_local_id = local_id) %>%
  select(all_of(file_describes_biosample_cols))
file_describes_biosample.tsv

file_describes_subject_cols <- getcolnames("blank_nonCV_C2M2_tables/file_describes_subject.tsv")
file_describes_subject_cols

filetemp <- file.tsv %>% 
  rename(file_id_namespace = id_namespace,
         file_local_id = local_id) %>%
  select(file_id_namespace, file_local_id) 
filetemp

subjecttemp <- subject.tsv %>%
  rename(subject_id_namespace = id_namespace,
         subject_local_id = local_id) %>%
  select(subject_id_namespace, subject_local_id)
subjecttemp

file_describes_subject.tsv <- cbind(filetemp, subjecttemp)
file_describes_subject.tsv



subject_role_taxonomy_cols <- getcolnames("./blank_nonCV_C2M2_tables/subject_role_taxonomy.tsv")
subject_role_taxonomy_cols

subject_role_taxonomy.tsv <- subject.tsv %>%
  rename(subject_id_namespace = id_namespace, 
         subject_local_id = local_id) %>%
  mutate(role_id = "cfde_subject_role:0") %>%
  mutate(taxonomy_id = "NCBI:txid10090") %>%
  select(all_of(subject_role_taxonomy_cols))
subject_role_taxonomy.tsv

###################################################################

# save files

savefiles <- function(object){
  print(paste("Writing", substitute(object), "to file", sep = " "))
  write.table(object, file = paste0(outdir, substitute(object), sep = ""),
              sep ="\t", row.names = F, col.names = T, quote = F, na = "")
}


savefiles(biosample.tsv)
savefiles(biosample_disease.tsv)
savefiles(biosample_from_subject.tsv)
savefiles(biosample_gene.tsv)

savefiles(dcc.tsv)

savefiles(id_namespace.tsv)

savefiles(file.tsv)
savefiles(file_describes_biosample.tsv)
savefiles(file_describes_subject.tsv)

savefiles(project.tsv)

savefiles(subject.tsv)
savefiles(subject_role_taxonomy.tsv)
