# Import library
library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(gtsummary)
library(VennDiagram)
library(ggpubr)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#---
clinical <-
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v05.csv"))
CHIP_muts <- 
  read_csv(paste0(path, "/data/cleaned CH_myelosupp_muts_2020_CC.csv"))

#######################################################################################  II  ### Data cleaning----
# 1.1.Clinical----
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
  mutate(Prior_rad = factor(Prior_rad, labels=c("No", "Yes"))) %>%
  filter(Cohort == "M4M")# remove later

# 1.2.Mutations----
unique_patient_in_mutation <- as.data.frame(unique(CHIP_muts$patient_id)) %>% 
  `colnames<-` (c("patient_id"))
CHIP_muts <- CHIP_muts %>% drop_na("DATA")
# CHIP_muts <- dcast(setDT(CHIP_muts), patient_id ~ rowid(patient_id),
#                    value.var = c("CHROM", "POS", "REF", "ALT", "GENE", "VARIANT_C", "VARIANT_P",
#                                  "FUNCTION", "COSMIC", "ESP6500", "VAF", "DEPTH", "INFO",
#                                  "FORMAT", "DATA"))
CHIP_muts <- left_join(unique_patient_in_mutation, CHIP_muts, by = "patient_id")


# Cleaning 
rm(unique_patient_in_mutation)


#######################################################################################  III  ### Data binding----
global_data <- full_join(clinical[3:32], CHIP_muts, 
                          by = c("NGS_ID" = "patient_id")) %>% 
  filter(str_detect(NGS_ID, "M4M"))# remove later


#######################################################################################  III  ### Data mining


# Tables----





# Table 1_Patient Population
table <- clinical %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
         ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/Table 1_Patient Population.pdf"))
# Filtered
table1 <- clinical %>% 
  filter(CHIP == "Yes") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- clinical %>% 
  filter(CHIP == "No") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table <- tbl_merge(list(table1, table2),
          tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/Table 1_Patient Population faceted by CHIP.pdf"))

# CHIP in age, gender, race, ethnicity
table1 <- clinical %>% 
  select(CHIP, Age, Gender, Race, Ethnicity) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- clinical %>% 
  select(CHPD, Age, Gender, Race, Ethnicity) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table <- tbl_merge(list(table1, table2),
          tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in age, gender, race, ethnicity.pdf"))

# CHIP in comorbidity ans mets
table1 <- clinical %>% 
  select(CHIP, Smoking, Mets) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- clinical %>% 
  select(CHPD, Smoking, Mets) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table <- tbl_merge(list(table1, table2),
          tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in comorbidity ans mets.pdf"))

table1 <- clinical %>% 
  select(CHIP, Age, CANCER) %>%
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- clinical %>% 
  select(CHPD, Age, CANCER) %>%
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% 
  simul
table <- tbl_merge(list(table1, table2),
          tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in age and cancer.pdf"))

table <- clinical %>% 
  select(CHIP, CHPD, CANCER) %>%
  tbl_summary(by= CANCER, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in cancer.pdf"))

