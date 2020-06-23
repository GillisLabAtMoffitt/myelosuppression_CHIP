# Import library
library(tidyverse)
library(ggplot2)
library(viridis)
library(gtsummary)
library(VennDiagram)

#######################################################################################  I  ### Load data
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#-----------------------------------------------------------------------------------------------------------------
data <-
  read_csv(paste0(path, "/CHIP_CRASH_data_for_stats_v05.csv"))


#######################################################################################  II  ### Load cleaning
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

#######################################################################################  III  ### Load mining
print(paste("This data have", dim(data)[2], "variables on", dim(data)[1], "patients.",
            sum(str_detect(data$Case_Control,"Case")), "are cases,", sum(str_detect(data$Case_Control,"Control")), "are controls.
            This differs a liitle from the previous presentation in 2018 as we had 44 cases and 87 controls."))

# Cancer repartition
table <- as.data.frame(table(data$CANCER))
colourCount = length(unique(table$Var1))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)

table %>% mutate(Var1 = fct_reorder(Var1, desc(Freq))) %>%
  ggplot(aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Cancer type", values = getPalette) +
  theme_minimal() +
  coord_polar("y", start=0, direction=-1) +
  labs(x="", y="", title="Cancer type Repartition")

# Age
p <- qplot(x =Age, data=data, fill=..count.., geom="histogram") 
p + scale_fill_viridis_c(
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

ggplot(data = data, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=25)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(data = data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(data = data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CANCER)

# CHIP prevalence
data %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
         ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))

# CHPD prevalence
data %>% group_by(Case_Control,CHPD) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHPD))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHPD") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))

# CHIP vs CHPD
d <- data %>% 
  mutate(CHIPid = case_when(
    CHIP == "Yes" ~ .$NGS_ID
  ))

venn.diagram(
  x = list(data$NGS_ID %>% filter(CHIP == "Yes"),
           data$NGS_ID),
  category.names = c("CHIP" , "CHPD"),
  filename = 'Patient .png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FEA873FF"),
  margin = 0.05,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-20, 160),
  cat.dist = c(0.055, 0.055),
  #ext.percent = 2,
  rotation.degree = -90
  
)






