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
  mutate(old_CHIP = factor(old_CHIP, labels=c("No CHIP", "CHIP"))) %>% 
  mutate(old_CHPD= factor(old_CHPD, labels=c("No CHPD", "CHPD"))) %>% 
  mutate(Race = factor(Race, labels=c("White", "Other"))) %>% 
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) %>% 
  mutate(Smoking = factor(Smoking, labels=c("Never","Ever"))) %>% 
  mutate(mets = Mets) %>% 
  mutate(Mets = factor(Mets, labels=c("No", "Yes"))) %>%
  mutate(neutro = Neutro) %>% 
  mutate(Neutro = factor(Neutro, labels=c("No", "Yes"))) %>%
  mutate(anemia = Anemia) %>% 
  mutate(Anemia = factor(Anemia, labels=c("No", "Yes"))) %>%
  mutate(thrombo = Thrombo) %>% 
  mutate(Thrombo = factor(Thrombo, labels=c("No", "Yes"))) %>%
  mutate(leuko = Leuko) %>% 
  mutate(Leuko = factor(Leuko, labels=c("No", "Yes"))) %>%
  mutate(Prior_chemo = factor(Prior_chemo, labels=c("No", "Yes"))) %>% 
  mutate(Prior_rad = factor(Prior_rad, labels=c("No", "Yes")))

# 2.2.CHIP data----
CHIP_muts <- full_join(CHIP_muts1 %>% 
                         select(patient_id),
                       CHIP_muts, 
                       by = "patient_id")

# Modified CHIP var to fit with the new data
CHIP_muts <- CHIP_muts %>% 
  mutate(CHIP = case_when(
    is.na(DATA) ~ 0,
    !is.na(DATA) ~ 1 # CHIP
  )) %>% 
  mutate(CHIP = factor(CHIP, labels= c("No CHIP", "CHIP"), levels= c(0, 1))) %>% 
  mutate(VAF_10P = case_when(
    VAF >= 0.1 ~ VAF
    )) %>% 
  mutate(VAF_grp = case_when(
    VAF >= 0.1 ~ "VAF >= 10%",
    VAF < 0.1 ~ "VAF < 10%"
  )) %>% 
  mutate(CHIP_DDR = case_when(
    GENE %in% c("TP53", "PPM1D", "CHEK2") ~ "CHIP",
    is.na(GENE) ~ "No CHIP",
    GENE %in% c("ASXL1", "BRCC3", "DNMT3A", "JAK2", "KMT2D", "SF3B1", "SH2B3", "SRSF2", "TET2") ~ "No CHIP"
  ))

#######################################################################################  III  ### Data binding----
# Will bind the 2 data but in 2 different ways 

# 3.1.Bind to have 1 mutation per row, multiple row per patient----
muts_data <- full_join(clinical[2:37], CHIP_muts, 
                          by = c("NGS_ID" = "patient_id")) %>%
  select("NGS_ID", "Cohort", "Case_Control", "Strata", "old_CHIP", "CHIP", "CHIP_DDR", everything())
# write_csv(muts_data, paste0(path, "/Output/data output/muts_data.csv"))

# 3.2.Bind to have 1 patient per row, multiple mutation per row----
CHIP_muts1 <- dcast(setDT(CHIP_muts), patient_id+CHIP ~ rowid(patient_id),
                    value.var = c("GENE", "FUNCTION", "COSMIC", "ESP6500", "VAF", "DEPTH", "VAF_10P", "CHIP_DDR")) %>% 
  mutate(CHIP_VAF_10P = factor(ifelse(
    !is.na(VAF_10P_1) | !is.na(VAF_10P_2) | !is.na(VAF_10P_3) | !is.na(VAF_10P_4) | !is.na(VAF_10P_5) |
    !is.na(VAF_10P_6) | !is.na(VAF_10P_7) | !is.na(VAF_10P_8) | !is.na(VAF_10P_9) | !is.na(VAF_10P_10) |
    !is.na(VAF_10P_11) | !is.na(VAF_10P_12) | !is.na(VAF_10P_13) | !is.na(VAF_10P_14) | !is.na(VAF_10P_15) |
    !is.na(VAF_10P_16), "CHIP", "No CHIP"))
  ) %>% 
  mutate(CHIP_VAF_10P = factor(CHIP_VAF_10P, levels = c("No CHIP", "CHIP"))) %>% 
  mutate(CHIP_DDR1 = factor(ifelse(
    (CHIP_DDR_1  == "CHIP"| CHIP_DDR_2  == "CHIP"| CHIP_DDR_3  == "CHIP"| CHIP_DDR_4  == "CHIP" |
      CHIP_DDR_5  == "CHIP" | CHIP_DDR_6  == "CHIP" | CHIP_DDR_7  == "CHIP" | CHIP_DDR_8  == "CHIP" |
       CHIP_DDR_9  == "CHIP" | CHIP_DDR_10  == "CHIP" | CHIP_DDR_11  == "CHIP" | CHIP_DDR_12  == "CHIP" |
       CHIP_DDR_13  == "CHIP" | CHIP_DDR_14  == "CHIP" | CHIP_DDR_15  == "CHIP" | CHIP_DDR_16 == "CHIP"),
    "CHIP", "No CHIP"))
  ) %>% 
  mutate(CHIP_DDR = coalesce(CHIP_DDR1, "No CHIP")) %>% 
  mutate(CHIP_DDR = factor(CHIP_DDR, levels = c("No CHIP", "CHIP")))

global_data <- left_join(clinical, CHIP_muts1, 
                       by = c("NGS_ID" = "patient_id")) %>%
  select("Patient", "NGS_ID", "Cohort", "Case_Control", "Strata", 
         "old_CHIP", "CHIP", "CHIP_VAF_10P", "CHIP_DDR", everything()) %>% 
  mutate(CHIP = coalesce(CHIP, old_CHIP))
# write_csv(global_data, paste0(path, "/Output/data output/global_data.csv"))

global_M4M <- global_data %>% filter(Cohort == "M4M")

rm(CHIP_muts1)
