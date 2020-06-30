# Cleaning old muts data

CHIP_muts <- 
  read_csv(paste0(path, "/data/CH_myelosupp_muts_2018_wUNC.csv"))

unique_patient_in_mutation <- as.data.frame(unique(CHIP_muts$Sample)) %>% 
  `colnames<-` (c("patient_id"))
CHIP_muts <- CHIP_muts %>% drop_na("VAF") %>% 
  rename(patient_id = Sample)

CHIP_muts <- left_join(unique_patient_in_mutation, CHIP_muts, by = "patient_id")
rm(unique_patient_in_mutation)
clinical <- clinical %>%
  filter(Cohort == "M4M")
clinical <- clinical %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Controls", "Cases"))) %>% 
  mutate(Case_Control = factor(Case_Control, levels= c("Cases", "Controls"))) %>% 
  mutate(CHIP = factor(CHIP, labels=c("No", "Yes"))) %>% 
  mutate(CHPD= factor(CHPD, labels=c("No", "Yes"))) %>% 
  mutate(Race = factor(Race, labels=c("White", "Other"))) %>% 
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) %>% 
  mutate(Smoking = factor(Smoking, labels=c("Never","Ever"))) %>% 
  mutate(Mets = factor(Mets, labels=c("No", "Yes"))) %>% 
  mutate(Neutro = factor(Neutro, labels=c("No", "Yes"))) %>% 
  mutate(Anemia = factor(Anemia, labels=c("No", "Yes"))) %>% 
  mutate(Thrombo = factor(Thrombo, labels=c("No", "Yes"))) %>% 
  mutate(Prior_chemo = factor(Prior_chemo, labels=c("No", "Yes"))) %>% 
  mutate(Prior_rad = factor(Prior_rad, labels=c("No", "Yes")))

global_data <- full_join(clinical[3:32], CHIP_muts, 
                         by = c("NGS_ID" = "patient_id")) %>% 
  filter(str_detect(NGS_ID, "M4M"))



global_data$Case_Control == global_data$Group
