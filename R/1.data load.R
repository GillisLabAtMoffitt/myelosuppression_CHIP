# Import library
library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(gtsummary)
library(VennDiagram)
library(ggpubr)
library(survival)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#---
clinical <-
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v06.csv")) %>% 
  rename(old_CHIP = CHIP, old_CHPD = CHPD) %>% 
  select(-X32)

CHIP_muts1 <- 
  read_csv(paste0(path, "/data/cleaned CH_myelosupp_muts_2020_CC.csv"))
CHIP_muts <- 
  read.delim(paste0(path, "/data/M4M2018_updated 06.24.20_filtered.txt"))

#######################################################################################  II  ### Data cleaning----
# 2.1.Clinical data----
clinical <- clinical %>% 
  mutate(Cases_Controls = Case_Control) %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Cases", "Controls"), levels= c(1, 0))) %>% # 0 = ctrl
  # mutate(Case_Control = factor(Case_Control, labels= c("Controls", "Cases"))) %>% 
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
  mutate(ChangeANC = abs(ChangeANC)) %>% 
  mutate(ChangeHGB = abs(ChangeHGB)) %>% 
  mutate(ChangeWBC = abs(ChangeWBC)) %>% 
  mutate(ChangePLT = abs(ChangePLT)) %>% 
  filter(Cohort == "M4M") # remove later

# 2.2.CHIP data----
CHIP_muts <- full_join(CHIP_muts1 %>% 
                         select(patient_id),
                       CHIP_muts, 
                       by = "patient_id")

# Modified CHIP var to fit with the new data
CHIP_muts <- CHIP_muts %>% 
  mutate(CHIP = case_when(
    is.na(DATA) ~ 0,
    !is.na(DATA) ~ 1
  )) %>% 
  mutate(CHIP = factor(CHIP, labels= c("CHIP", "No CHIP"), levels= c(1, 0)))

#######################################################################################  III  ### Data binding----
# Will bind the 2 data but in 2 different ways 

# 3.1.Bind to have 1 mutation per row, multiple row per patient----
muts_data <- full_join(clinical[3:32], CHIP_muts, 
                          by = c("NGS_ID" = "patient_id")) %>%
  filter(str_detect(NGS_ID, "M4M")) %>% # remove later
  select("NGS_ID", "Case_Control", "Strata", "old_CHIP", "CHIP", everything())
# write_csv(muts_data, paste0(path, "/Output/data output/muts_data.csv"))

# 3.2.Bind to have 1 patient per row, multiple mutation per row----
CHIP_muts1 <- dcast(setDT(CHIP_muts), patient_id ~ rowid(patient_id),
                    value.var = c("GENE", "FUNCTION", "COSMIC", "ESP6500", "VAF", "DEPTH", "CHIP")) %>% 
  select(-CHIP_2, -CHIP_3, -CHIP_4) %>% 
  rename(CHIP = CHIP_1)

global_data <- left_join(clinical[, c(1,3:32)], CHIP_muts1, 
                       by = c("NGS_ID" = "patient_id")) %>% 
  filter(str_detect(NGS_ID, "M4M")) %>% # remove later
  select("Patient", "NGS_ID", "Case_Control", "Strata", "old_CHIP", "CHIP", everything())

# write_csv(global_data, paste0(path, "/Output/data output/global_data.csv"))

rm(CHIP_muts1)
