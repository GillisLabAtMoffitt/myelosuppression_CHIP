# Import library
library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(gtsummary)
library(VennDiagram)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#---
data <-
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v05.csv"))
CHIP_muts <- 
  read_csv(paste0(path, "/data/all CH_myelosupp_muts_2018_wUNC.csv"))

#######################################################################################  II  ### Data cleaning----
# 1.1.Clinical----
data <- data %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Control", "Case"))) %>% 
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

# 1.2.Mutations----
unique_patient_in_mutation <- as.data.frame(unique(CHIP_muts$patient_id)) %>% 
  `colnames<-` (c("patient_id"))
CHIP_muts <- CHIP_muts %>% drop_na("DATA")
CHIP_muts <- dcast(setDT(CHIP_muts), patient_id ~ rowid(patient_id),
                   value.var = c("CHROM", "POS", "REF", "ALT", "GENE", "VARIANT_C", "VARIANT_P",
                                 "FUNCTION", "COSMIC", "ESP6500", "VAF", "DEPTH", "INFO",
                                 "FORMAT", "DATA"))
CHIP_muts <- left_join(unique_patient_in_mutation, CHIP_muts, by = "patient_id")


# Cleaning 
rm(unique_patient_in_mutation)
#######################################################################################  III  ### Data mining


# Tables----
table <- data %>% 
  select(Case_Control, CHIP, CHPD) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
table <- data %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
table <- data %>% 
  select(Case_Control, CHPD) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
chisq.test(table(data$Case_Control, data$CHIP))
chisq.test(table(data$Case_Control, data$CHIP))$residuals
chisq.test(table(data$Case_Control, data$CHPD))
chisq.test(table(data$Case_Control, data$CHPD))$residuals


# CHIP vs CHPD
venn.diagram(
  x = list(CHIP = c(unique(data[data$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(data[data$CHPD == "Yes",]$NGS_ID))),
  category.names = c("CHIP" , "CHPD"),
  filename = 'Patients exhibiting CHIP vs CHPD.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  main = "Patients exhibiting CHIP vs CHPD",
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FEA873FF"),
  margin = 0.05,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(0, 180),
  cat.dist = c(0.025, -0.08)
)
venn.diagram(
  x = list(CHIP = c(unique(data[data$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(data[data$CHPD == "Yes",]$NGS_ID)),
           CHIPno = c(unique(data[data$CHIP == "No",]$NGS_ID))),
  category.names = c("CHIP" , "CHPD", "none"),
  filename = 'Mutations presented by patients.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  main = "Mutations presented by patients",
  main.pos = c(0.5, .9),
  lwd = 2,
  lty = 'blank',
  fill = c("#CC4678FF", "#F0F921FF", "#0D0887FF"),
  margin = 0.07,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(0, 180, 0),
  cat.dist = c(0.055, -0.07, 0.015),
  cat.cex = .7
  #ext.percent = 2,

)
# Table 1_Patient Population
table <- data %>% 
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
table1 <- data %>% 
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
table2 <- data %>% 
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
table1 <- data %>% 
  select(CHIP, Age, Gender, Race, Ethnicity) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- data %>% 
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
table1 <- data %>% 
  select(CHIP, Smoking, Mets) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- data %>% 
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

table1 <- data %>% 
  select(CHIP, Age, CANCER) %>%
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
table2 <- data %>% 
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

table <- data %>% 
  select(CHIP, CHPD, CANCER) %>%
  tbl_summary(by= CANCER, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in cancer.pdf"))

