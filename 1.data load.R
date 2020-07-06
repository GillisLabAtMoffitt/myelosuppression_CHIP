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
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v05.csv")) %>% 
  rename(old_CHIP = CHIP, old_CHPD = CHPD) %>% 
  select(-X32)

CHIP_muts <- 
  read_csv(paste0(path, "/data/cleaned CH_myelosupp_muts_2020_CC.csv"))

#######################################################################################  II  ### Data cleaning----
# 1.1.Clinical----
clinical <- clinical %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Controls", "Cases"))) %>% 
  mutate(Case_Control = factor(Case_Control, levels= c("Cases", "Controls"))) %>% 
  mutate(old_CHIP = factor(old_CHIP, labels=c("No CHIP", "CHIP"))) %>% 
  mutate(old_CHPD= factor(old_CHPD, labels=c("No CHPD", "CHPD"))) %>% 
  mutate(Race = factor(Race, labels=c("White", "Other"))) %>% 
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) %>% 
  mutate(Smoking = factor(Smoking, labels=c("Never","Ever"))) %>% 
  mutate(Mets = factor(Mets, labels=c("No", "Yes"))) %>% 
  mutate(Neutro = factor(Neutro, labels=c("No", "Yes"))) %>% 
  mutate(Anemia = factor(Anemia, labels=c("No", "Yes"))) %>% 
  mutate(Thrombo = factor(Thrombo, labels=c("No", "Yes"))) %>% 
  mutate(Prior_chemo = factor(Prior_chemo, labels=c("No", "Yes"))) %>% 
  mutate(Prior_rad = factor(Prior_rad, labels=c("No", "Yes"))) %>%
  filter(Cohort == "M4M") # remove later
# Modified CHIP var to fit with the new data
CHIP_muts <- CHIP_muts %>% 
  mutate(CHIP = case_when(
    !is.na(DATA) ~ "CHIP",
    is.na(DATA) ~ "No CHIP"
  ))
  

#######################################################################################  III  ### Data binding----

global_data <- full_join(clinical[3:31], CHIP_muts, 
                          by = c("NGS_ID" = "patient_id")) %>% 
  filter(str_detect(NGS_ID, "M4M")) %>% # remove later
  select("NGS_ID", "Case_Control", "Strata", "old_CHIP", "old_CHPD", "CHIP", everything())

write_csv(global_data, paste0(path, "/Output/global_data.csv"))

