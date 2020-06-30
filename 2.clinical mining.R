############################################################################################## I ### Clinical mining----
print(paste("This data have", dim(clinical)[2], "variables on", dim(clinical)[1], "patients.",
            sum(str_detect(clinical$Case_Control,"Case")), "are cases,", sum(str_detect(clinical$Case_Control,"Control")), "are controls.
            This differs a little from the previous presentation in 2018 as we had 44 cases and 87 controls."))

############################################################################################## I ### CHIP----

# 1.1 CHIP prevalence----
# pdf(paste0(path, "/Output/CHIP prevalence.pdf"))
clinical %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(clinical$Case_Control, clinical$CHIP))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

# CHPD prevalence
# pdf(paste0(path, "/Output/CHPD prevalence.pdf"))
clinical %>% group_by(Case_Control,CHPD) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHPD))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHPD") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(clinical$Case_Control, clinical$CHPD))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

table <- clinical %>% 
  select(Case_Control, CHIP, CHPD) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
table <- clinical %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
table <- clinical %>% 
  select(Case_Control, CHPD) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
chisq.test(table(clinical$Case_Control, clinical$CHIP))
chisq.test(table(clinical$Case_Control, clinical$CHIP))$residuals
chisq.test(table(clinical$Case_Control, clinical$CHPD))
chisq.test(table(clinical$Case_Control, clinical$CHPD))$residuals

# CHIP vs CHPD
venn.diagram(
  x = list(CHIP = c(unique(clinical[clinical$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(clinical[clinical$CHPD == "Yes",]$NGS_ID))),
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
  x = list(CHIP = c(unique(clinical[clinical$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(clinical[clinical$CHPD == "Yes",]$NGS_ID)),
           CHIPno = c(unique(clinical[clinical$CHIP == "No",]$NGS_ID))),
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
venn.diagram(
  x = list(CHIP = c(unique(clinical[clinical$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(clinical[clinical$CHPD == "Yes",]$NGS_ID)),
           Cases = c(unique(clinical[clinical$Case_Control == "Cases",]$NGS_ID)),
           Controls = c(unique(clinical[clinical$Case_Control == "Controls",]$NGS_ID))),
  category.names = c("CHIP" , "CHPD", "Cases", "Controls"),
  filename = 'Mutations presented by cases_control.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  main = "Mutations presented by cases_control",
  main.pos = c(0.5, .9),
  lwd = 2,
  lty = 'blank',
  fill = cividis(n=4),
  margin = 0.07,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180, 0),
  # cat.dist = c(0.055, -0.07, 0.015),
  cat.cex = .7
  #ext.percent = 2,
  
)
venn.diagram(
  x = list(CHIP = c(unique(clinical[clinical$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(clinical[clinical$CHPD == "Yes",]$NGS_ID)),
           Cases = c(unique(clinical[clinical$Case_Control == "Cases",]$NGS_ID))),
  category.names = c("CHIP" , "CHPD", "Cases"),
  filename = 'Mutations presented by cases.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  main = "Mutations presented by cases",
  main.pos = c(0.5, .9),
  lwd = 2,
  lty = 'blank',
  fill = cividis(n=3),
  margin = 0.07,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180, 0),
  # cat.dist = c(0.055, -0.07, 0.015),
  cat.cex = .7
  #ext.percent = 2,
  
)
venn.diagram(
  x = list(CHIP = c(unique(clinical[clinical$CHIP == "Yes",]$NGS_ID)),
           CHPD = c(unique(clinical[clinical$CHPD == "Yes",]$NGS_ID)),
           Controls = c(unique(clinical[clinical$Case_Control == "Controls",]$NGS_ID))),
  category.names = c("CHIP" , "CHPD", "Controls"),
  filename = 'Mutations presented by control.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  main = "Mutations presented by control",
  main.pos = c(0.5, .9),
  lwd = 2,
  lty = 'blank',
  fill = cividis(n=3),
  margin = 0.07,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180, 0),
  # cat.dist = c(0.055, -0.07, 0.015),
  cat.cex = .7
  #ext.percent = 2,
  
)

############################################################################################## II ### Mutation mining----
# Allelic Variant----

ggplot(global_data, aes(x=Case_Control, y= VAF)) +
  geom_jitter(position = position_jitter(0.25), size = 1.2) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.4
  )


global_data.summary <- global_data %>%
  group_by(Case_Control) %>% 
  filter(!is.na(VAF)) %>% 
  summarise(
    sd = sd(VAF, na.rm = TRUE),
    VAF = mean(VAF, na.rm = TRUE),
    median = median(VAF, na.rm = TRUE),
    min = min(VAF, na.rm = TRUE),
    max = max(VAF, na.rm = TRUE)
  )

ggplot(global_data %>% filter(!is.na(Case_Control)), aes(x=Case_Control, y= VAF, color=Case_Control)) +
  geom_jitter(
    position = position_jitter(0.2), color = "darkgray"
  ) + 
  theme_minimal()+
  geom_pointrange(
    aes(ymin = median-sd, ymax = median+sd),
    data = global_data.summary
  )+
  stat_compare_means(label.x = 1.15, label.y = .52)

table <- global_data %>% 
  group_by(Case_Control) %>% 
  filter(!is.na(VAF)) %>% 
  select(Case_Control, VAF) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
gt::gtsave(table, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/VAF in Case_Control.pdf"))

ggplot(global_data, aes(x=Case_Control, y= VAF))+
  geom_dotplot(
    binaxis = "y", stackdir = "center",
    color = "lightgray"
  ) + 
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult=1), 
    geom = "pointrange", color = "red"
  )

# Barplot
ggplot(global_data %>% filter(!is.na(Case_Control)), aes(x=Case_Control, y=VAF, fill=Case_Control)) +
  geom_bar(stat = "summary_bin", fun = mean)+
  geom_errorbar(aes(ymin = min(VAF), ymax = max(VAF)))

ggplot(global_data.summary, aes(x=Case_Control, y=median, fill=Case_Control)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = median, ymax = median+sd, color=Case_Control))+
  theme_minimal()

# Genes----
# Number mutations vs Case # Attention will count multiple if happens more than once in same patient
global_data %>% filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

global_data %>% filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(GENE, -percent), y=percent, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

global_data %>% filter(!is.na(Case_Control)) %>% 
  select(Case_Control, GENE) %>% 
  tbl_summary(by=Case_Control) %>% 
  add_p() %>%
  add_n()

# Number/% of Patients with CHIP # Count 1 type of mutation per patient
global_data %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

global_data %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(GENE, -percent), y=percent, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

global_data %>% filter(!is.na(Case_Control)) %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  select(Case_Control, GENE) %>% 
  tbl_summary(by=Case_Control) %>% 
  add_p() %>%
  add_n()

# CHIP vs GENE----
global_data %>%
  select(CHIP, GENE) %>% 
  tbl_summary(by=CHIP) %>% 
  add_p() %>%
  add_n()

global_data %>% 
  filter(!is.na(GENE)) %>% group_by(CHIP,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(GENE, -percent), y=percent, fill = CHIP)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

global_data %>% 
  filter(!is.na(GENE)) %>% group_by(CHIP,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = CHIP)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

tbl <- as.data.frame(table(global_data$FUNCTION, global_data$GENE)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
# write.csv(tbl, paste0(path, "/Output/Fonction type per gene.csv"))














############################################################################################## III ### Clinical mining----

# Cancer repartition pie ----
table <- as.data.frame(table(clinical$CANCER))
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
qplot(x =Age, data=clinical, fill=..count.., geom="histogram")+
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

# pdf(paste0(path, "/Output/Age.pdf")) # Age per cancer
ggplot(data = clinical, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=25)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# pdf(paste0(path, "/Output/Gender.pdf"))
ggplot(data = clinical, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# pdf(paste0(path, "/Output/Gender2.pdf"))
ggplot(data = clinical, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CANCER)
# dev.off()











