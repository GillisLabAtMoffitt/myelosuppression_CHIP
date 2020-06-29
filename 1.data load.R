# Import library
library(tidyverse)
library(ggplot2)
library(viridis)
library(gtsummary)
library(VennDiagram)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#---
data <-
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v05.csv"))
mutation <- 
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v05.csv"))

#######################################################################################  II  ### Load cleaning----
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

#######################################################################################  III  ### Data mining
print(paste("This data have", dim(data)[2], "variables on", dim(data)[1], "patients.",
            sum(str_detect(data$Case_Control,"Case")), "are cases,", sum(str_detect(data$Case_Control,"Control")), "are controls.
            This differs a liitle from the previous presentation in 2018 as we had 44 cases and 87 controls."))

# Cancer repartition
table <- as.data.frame(table(data$CANCER))
colourCount = length(unique(table$Var1))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)

# pdf(paste0(path, "/Output/Cancer repartition.pdf"))
table %>% mutate(Var1 = fct_reorder(Var1, desc(Freq))) %>%
  ggplot(aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Cancer type", values = getPalette) +
  theme_minimal() +
  coord_polar("y", start=0, direction=-1) +
  labs(x="", y="", title="Cancer type Repartition")
# dev.off()

# Age
# pdf(paste0(path, "/Output/Age repartition.pdf"))
qplot(x =Age, data=data, fill=..count.., geom="histogram")+
  scale_fill_viridis_c(
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1,
  option = "D",
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
) +
  theme_minimal() +
  labs(x="Age", y="Number of Patient", title="Age Repartition")
# dev.off()

# pdf(paste0(path, "/Output/Age.pdf"))
ggplot(data = data, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=25)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# pdf(paste0(path, "/Output/Gender.pdf"))
ggplot(data = data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# pdf(paste0(path, "/Output/Gender2.pdf"))
ggplot(data = data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CANCER)
# dev.off()

# CHIP prevalence
# pdf(paste0(path, "/Output/CHIP prevalence.pdf"))
data %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
         ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(data$Case_Control, data$CHIP))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

# CHPD prevalence
# pdf(paste0(path, "/Output/CHPD prevalence.pdf"))
data %>% group_by(Case_Control,CHPD) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHPD))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHPD") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(data$Case_Control, data$CHPD))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

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

