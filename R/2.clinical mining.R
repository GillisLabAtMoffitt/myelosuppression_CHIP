writeLines(paste("M4M+UNC data have", dim(global_data)[2], "variables on", dim(global_data)[1], "patients.",
            sum(str_detect(global_data$Case_Control,"Cases")), "are cases,", 
            sum(str_detect(global_data$Case_Control,"Controls")), "are controls.
            In 2018 as we had 44 cases and 87 controls."))

writeLines(paste("M4M data have", dim(global_M4M)[2], "variables on", dim(global_M4M)[1], "patients.",
                 sum(str_detect(global_M4M$Case_Control,"Cases")), "are cases,", 
                 sum(str_detect(global_M4M$Case_Control,"Controls")), "are controls."))

############################################################################################## I ### CHIP----

# 1.1 CHIP prevalence----
# jpeg(paste0(path, "/Output/CHIP prevalence.jpeg"))
global_M4M %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP in M4M data") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_M4M$Case_Control, global_M4M$CHIP))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()
# tbl <-
global_M4M %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= Case_Control) %>% 
  add_p() %>% add_overall() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 0.5,
#            paste0(
#              path,
#              "/Output/sumtable CHIP in Case_Control in M4M.pdf"))

# CHPD prevalence
# pdf(paste0(path, "/Output/CHPD prevalence.pdf"))
# global_data %>% group_by(Case_Control,CHPD) %>% 
#   summarise(count=n()) %>% 
#   mutate(perc=(count/sum(count)*100)
#   ) %>% 
#   ggplot(aes(x=Case_Control, y= perc, fill=CHPD))+
#   geom_bar(stat="identity") +
#   labs(x = "", y = "percent", title = "Prevalence of CHPD") +
#   theme_minimal()+
#   geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
#   geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
#   annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_data$Case_Control, global_data$CHPD))[3]),
#            color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

global_data %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP in M4M+UNC data") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_data$Case_Control, global_data$CHIP))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)

# tbl <- 
  global_data %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= Case_Control) %>% 
  add_p() %>% add_overall() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 0.5,
#            paste0(
#              path,
#              "/Output/sumtable CHIP in Case_Control in M4M-UNC.pdf"))
# tbl <- 
  global_data %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= CHIP) %>% 
  add_p() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 0.5, 
#            paste0(
#              path, 
#              "/Output/sumtable Case_Control in CHIP.pdf"))
# tbl <- global_data %>% 
#   select(Case_Control, CHPD) %>% 
#   tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/CHIP and CHPD in Case_Control.pdf"))
chisq.test(table(global_data$Case_Control, global_data$CHIP))
chisq.test(table(global_data$Case_Control, global_data$CHIP))$residuals
# chisq.test(table(global_data$Case_Control, global_data$CHPD))
# chisq.test(table(global_data$Case_Control, global_data$CHPD))$residuals

# CHIP vs CHPD
# venn.diagram(
#   x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
#            CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID))),
#   category.names = c("CHIP" , "CHPD"),
#   filename = 'Patients exhibiting CHIP vs CHPD.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 700 , 
#   width = 700 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   main = "Patients exhibiting CHIP vs CHPD",
#   lwd = 2,
#   lty = 'blank',
#   fill = c("darkgrey", "#FEA873FF"),
#   margin = 0.05,
#   
#   # Numbers
#   cex = .6,
#   fontface = "bold",
#   fontfamily = "sans",
#   cat.pos = c(0, 180),
#   cat.dist = c(0.025, -0.08)
# )
# venn.diagram(
#   x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
#            CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
#            CHIPno = c(unique(global_data[global_data$CHIP == "No CHIP",]$NGS_ID))),
#   category.names = c("CHIP" , "CHPD", "none"),
#   filename = 'Mutations presented by patients.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 700 , 
#   width = 700 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   main = "Mutations presented by patients",
#   main.pos = c(0.5, .9),
#   lwd = 2,
#   lty = 'blank',
#   fill = c("#CC4678FF", "#F0F921FF", "#0D0887FF"),
#   margin = 0.07,
#   
#   # Numbers
#   cex = .5,
#   fontface = "bold",
#   fontfamily = "sans",
#   cat.pos = c(0, 180, 0),
#   cat.dist = c(0.055, -0.07, 0.015),
#   cat.cex = .7
#   #ext.percent = 2,
# )
# venn.diagram(
#   x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
#            CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
#            Cases = c(unique(global_data[global_data$Case_Control == "Cases",]$NGS_ID)),
#            Controls = c(unique(global_data[global_data$Case_Control == "Controls",]$NGS_ID))),
#   category.names = c("CHIP" , "CHPD", "Cases", "Controls"),
#   filename = 'Mutations presented by cases_control.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 700 , 
#   width = 700 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   main = "Mutations presented by cases_control",
#   main.pos = c(0.5, .9),
#   lwd = 2,
#   lty = 'blank',
#   fill = cividis(n=4),
#   margin = 0.07,
#   
#   # Numbers
#   cex = .5,
#   fontface = "bold",
#   fontfamily = "sans",
#   # cat.pos = c(0, 180, 0),
#   # cat.dist = c(0.055, -0.07, 0.015),
#   cat.cex = .7
#   #ext.percent = 2,
#   
# )
# venn.diagram(
#   x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
#            CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
#            Cases = c(unique(global_data[global_data$Case_Control == "Cases",]$NGS_ID))),
#   category.names = c("CHIP" , "CHPD", "Cases"),
#   filename = 'Mutations presented by cases.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 700 , 
#   width = 700 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   main = "Mutations presented by cases",
#   main.pos = c(0.5, .9),
#   lwd = 2,
#   lty = 'blank',
#   fill = cividis(n=3),
#   margin = 0.07,
#   
#   # Numbers
#   cex = .5,
#   fontface = "bold",
#   fontfamily = "sans",
#   # cat.pos = c(0, 180, 0),
#   # cat.dist = c(0.055, -0.07, 0.015),
#   cat.cex = .7
#   #ext.percent = 2,
#   
# )
# venn.diagram(
#   x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
#            CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
#            Controls = c(unique(global_data[global_data$Case_Control == "Controls",]$NGS_ID))),
#   category.names = c("CHIP" , "CHPD", "Controls"),
#   filename = 'Mutations presented by control.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 700 , 
#   width = 700 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   main = "Mutations presented by control",
#   main.pos = c(0.5, .9),
#   lwd = 2,
#   lty = 'blank',
#   fill = cividis(n=3),
#   margin = 0.07,
#   
#   # Numbers
#   cex = .5,
#   fontface = "bold",
#   fontfamily = "sans",
#   # cat.pos = c(0, 180, 0),
#   # cat.dist = c(0.055, -0.07, 0.015),
#   cat.cex = .7
#   #ext.percent = 2,
#   
# )
venn.diagram(
  x = list(Cases = c(unique(global_data[global_data$Case_Control == "Cases",]$NGS_ID)),
           Controls = c(unique(global_data[global_data$Case_Control == "Controls",]$NGS_ID)),
           CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID))),
  category.names = c("Cases", "Controls", "CHIP"),
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
  main.cex = .5,
  main.pos = c(0.5, .9),
  lwd = 2,
  lty = 'blank',
  fill = cividis(n=3),
  margin = 0.15,

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

muts_data.summary <- muts_data %>%
  group_by(Case_Control) %>% 
  filter(!is.na(VAF)) %>% 
  summarise(
    sd = sd(VAF, na.rm = TRUE),
    VAF = mean(VAF, na.rm = TRUE),
    median = median(VAF, na.rm = TRUE),
    min = min(VAF, na.rm = TRUE),
    max = max(VAF, na.rm = TRUE),
    mean = mean(VAF, na.rm = TRUE)
  )
# write_csv(muts_data.summary, paste0(path, "/Output/VAF mutation sum table.csv"))
# jpeg(paste0(path, "/Output/VAF in Case_Control.jpeg"))
ggplot(muts_data %>% filter(!is.na(Case_Control)), aes(x=Case_Control, y= VAF, color=Case_Control)) +
  geom_jitter(
    position = position_jitter(0.2), color = "lightgray"
  ) + 
  theme_minimal()+
  geom_pointrange(
    aes(ymin = median-sd, ymax = median+sd),
    data = muts_data.summary
  )+
  stat_compare_means(label.x = 1.16, label.y = .4)
# dev.off()

# tbl <-
  muts_data %>% 
  group_by(Case_Control) %>% 
  filter(!is.na(VAF)) %>% 
  select(Case_Control, VAF) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = .5,
#            paste0(
#              path,
#              "/Output/sum VAF in Case_Control.pdf"))


# Barplot
# jpeg(paste0(path, "/Output/barplot VAF in Case_Control.jpeg"))
ggplot(muts_data.summary, aes(x=Case_Control, y=median, fill=Case_Control)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = median, ymax = median+sd, color=Case_Control))+
  theme_minimal()
# dev.off()

# VAF 10%----VAF is defined as the number of alternative reads divided by the total read depth
muts_data.summ <- muts_data %>%
  group_by(Case_Control) %>% 
  filter(!is.na(VAF_10P)) %>% 
  summarise(
    sd = sd(VAF_10P, na.rm = TRUE),
    VAF_10P = mean(VAF_10P, na.rm = TRUE),
    median = median(VAF_10P, na.rm = TRUE),
    min = min(VAF_10P, na.rm = TRUE),
    max = max(VAF_10P, na.rm = TRUE),
    mean = mean(VAF_10P, na.rm = TRUE)
  )
# jpeg(paste0(path, "/Output/VAF10% in Case_Control.jpeg"))
ggplot(muts_data %>% filter(!is.na(Case_Control)), aes(x=Case_Control, y= VAF_10P, color=Case_Control)) +
  geom_jitter(
    position = position_jitter(0.2), color = "lightgray"
  ) + 
  theme_minimal()+
  geom_pointrange(
    aes(ymin = median-sd, ymax = median+sd),
    data = muts_data.summ
  )+
  stat_compare_means(label.x = 1.16, label.y = .4)
# dev.off()
# tbl <- 
muts_data %>% 
  group_by(Case_Control) %>% 
  filter(!is.na(VAF_10P)) %>% 
  select(Case_Control, VAF_10P) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = .5,
#            paste0(
#              path,
#              "/Output/sum VAF10% in Case_Control.pdf"))

# Prevalence when CHIP for 10%----
global_M4M %>% group_by(Case_Control,CHIP_VAF_10P) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP_VAF_10P))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_M4M$Case_Control, global_M4M$CHIP_VAF_10P))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# tbl <- 
global_M4M %>% 
  select(Case_Control, CHIP_VAF_10P) %>% 
  tbl_summary(by= Case_Control) %>% 
  add_p() %>% add_overall() %>% as_gt
# gt::gtsave(tbl, expand = 1, zoom = .5,
#            paste0(
#              path,
#              "/Output/sumtable CHIP Prevalence in Case_Control for VAF10%.pdf"))


# Genes----
# Number mutations vs Case # Attention will count multiple if happens more than once in same patient
muts_data %>% filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

# jpeg(paste0(path, "/Output/barplot all GENE in Case_Control.jpeg"))
muts_data %>% filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(GENE, percent), y=percent, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()
# dev.off()

# tbl <- 
  muts_data %>% filter(!is.na(Case_Control)) %>% 
  select(Case_Control, GENE) %>% 
  tbl_summary(by=Case_Control) %>% 
  add_p() %>%
  add_n() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable GENE in Case_Control.pdf"))

# Number/% of Patients with CHIP # Count 1 type of mutation per patient
muts_data %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

# jpeg(paste0(path, "/Output/new barplot GENE per patient in Case_Control.jpeg"), width = 800, height = 500)
muts_data %>% filter(Cohort == "M4M") %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  filter(!is.na(GENE)) %>% group_by(Case_Control,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent= case_when(
    Case_Control == "Cases" ~ count/(sum(str_detect(global_M4M$Case_Control,"Cases")))*100,
    Case_Control == "Controls" ~ count/(sum(str_detect(global_M4M$Case_Control,"Controls")))*100
  )

  ) %>% 
  ggplot(aes(x=reorder(GENE, -percent), y=percent, fill = Case_Control)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"))+
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0))+
  labs(x = "Genes")
# dev.off()

# tbl <- 
muts_data %>% filter(!is.na(Case_Control)) %>% filter(Cohort == "M4M") %>% 
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  mutate(GENE = case_when(
    is.na(GENE) ~ "No mutation",
    TRUE ~ GENE
  )) %>% 
  select(Case_Control, GENE) %>% 
  tbl_summary(by=Case_Control, percent = "column") %>% 
  add_p() %>%
  add_n() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable GENE per patient in Case_Control.pdf"))


# CHIP vs GENE----
muts_data %>%
  distinct(NGS_ID, GENE, .keep_all = TRUE) %>% 
  select(CHIP, GENE) %>% 
  tbl_summary(by=CHIP) %>% 
  add_p() %>%
  add_n()

muts_data %>% 
  filter(!is.na(GENE)) %>% group_by(CHIP,GENE) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(GENE, -percent), y=percent, fill = CHIP)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

muts_data %>% 
  filter(!is.na(GENE)) %>% group_by(CHIP,GENE) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=reorder(GENE, -count), y=count, fill = CHIP)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
  coord_flip()

#tbl <- 
  as.data.frame(table(muts_data$FUNCTION, muts_data$GENE)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
# write.csv(tbl, paste0(path, "/Output/Fonction type per gene.csv"))


############################################################################################## III ### Clinical mining----

# Cancer repartition pie ----
tbl <- as.data.frame(table(global_M4M$CANCER))
colourCount = length(unique(tbl$Var1))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)
# jpeg(paste0(path, "/Output/Cancer repartition.jpeg"))
tbl %>% mutate(Var1 = fct_reorder(Var1, desc(Freq))) %>%
  ggplot(aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Cancer type", values = getPalette) +
  theme_minimal() +
  coord_polar("y", start=0, direction=-1) +
  labs(x="", y="", title="Cancer type Repartition in M4M")
# dev.off()

tbl <- as.data.frame(table(global_M4M$CHIP,global_M4M$CANCER))
# jpeg(paste0(path, "/Output/Cancer repartition vs CHIP.jpeg"))
tbl %>% mutate(Var2 = fct_reorder(Var2, Freq)) %>%
  ggplot(aes(x=Var2, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width = .75, position = position_dodge(preserve = "single")) +
  scale_fill_discrete(name = "CHIP") +
  geom_text(aes(y = Freq + .5 , label=Freq), position = position_dodge(width = 0.9)) +
  theme_minimal() +
  coord_flip()+
  labs(x="Cancer Type", y="Number of Patient", title="CHIP in Cancer type Repartition")
# dev.off()

global_M4M %>% group_by(CANCER,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=reorder(CANCER, perc), y=perc, fill=CHIP)) +
  geom_bar(stat="identity", width = .75, position = position_dodge(preserve = "single")) +
  scale_fill_discrete(name = "CHIP") +
  theme_minimal() +
  coord_flip()

# jpeg(paste0(path, "/Output/Cancer repartition vs CHIP percent whole pop.jpeg"))
global_M4M %>% group_by(CANCER,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/NROW(muts_data$CANCER)*100)
  ) %>% 
  ggplot(aes(x=reorder(CANCER, percent), y=percent, fill=CHIP)) +
  geom_bar(stat="identity", width = .75, position = position_dodge(preserve = "single")) +
  scale_fill_discrete(name = "CHIP") +
  theme_minimal() +
  coord_flip()
# dev.off()

# tbl <- 
global_M4M %>% 
  select(CHIP, CANCER) %>% 
  tbl_summary(by=CHIP) %>% 
  add_p %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable Cancer repartition vs CHIP percent whole pop.pdf"))

# Age
# pdf(paste0(path, "/Output/Age repartition.pdf"))
qplot(x =Age, data=global_M4M, fill=..count.., geom="histogram")+
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

# pdf(paste0(path, "/Output/Age repartition.pdf"))
global_M4M %>% 
  ggplot(aes(x=Age)) +
  geom_freqpoly()
# dev.off()

# jpeg(paste0(path, "/Output/Age.jpeg")) # Age per cancer
ggplot(data = global_M4M, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=22)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()
# jpeg(paste0(path, "/Output/Age vs CHIP.jpeg")) # Age per cancer
ggplot(data = global_M4M, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=31)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(rows = vars(CHIP))
# dev.off()

# pdf(paste0(path, "/Output/Gender.pdf"))
ggplot(data = global_M4M, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(data = global_M4M, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Race.pdf"))
ggplot(data = global_M4M, aes(x=Race, y=Age), fill=Race) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Ethnicity.pdf"))
ggplot(data = global_M4M, aes(x=Ethnicity, y=Age), fill=Ethnicity) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Ethnicity", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Smoking.pdf"))
ggplot(data = global_M4M, aes(x=Smoking, y=Age), fill=Smoking) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Smoking", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

tbl1 <- global_M4M %>% filter(CHIP == "CHIP") %>% 
  select(Age, Race) %>% 
  tbl_summary(by=Race, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Race")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_M4M %>% filter(CHIP == "No CHIP") %>% 
  select(Age, Race) %>% 
  tbl_summary(by=Race, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Race")) %>% 
  add_p() %>% add_overall()
# tblm1 <- 
  tbl_merge(list(tbl1, tbl2),
          tab_spanner = c("**CHIP**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels() %>% as_gt()
# gt::gtsave(tblm1, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable Age per race in CHIP.pdf"))

tbl1 <- global_M4M %>% filter(CHIP == "CHIP") %>% 
  select(Age, Ethnicity) %>% 
  tbl_summary(by=Ethnicity, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Ethnicity")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_M4M %>% filter(CHIP == "No CHIP") %>% 
  select(Age, Ethnicity) %>% 
  tbl_summary(by=Ethnicity, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Ethnicity")) %>% 
  add_p() %>% add_overall()
# tblm2 <- 
  tbl_merge(list(tbl1, tbl2),
         tab_spanner = c("**CHIP**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels() %>% as_gt()
# gt::gtsave(tblm2, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable Age per ethnicity in CHIP.pdf"))

tbl1 <- global_M4M %>%filter(CHIP == "CHIP") %>% 
  select(Age, Smoking) %>% 
  tbl_summary(by=Smoking, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Smoking")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_M4M %>%filter(CHIP == "No CHIP") %>% 
  select(Age, Smoking) %>% 
  tbl_summary(by=Smoking, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Smoking")) %>% 
  add_p() %>% add_overall()
# tblm3 <- 
  tbl_merge(list(tbl1, tbl2),
        tab_spanner = c("**CHIPs**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels() %>% as_gt()
# gt::gtsave(tblm3, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable Age per smoking in CHIP.pdf"))

# jpeg(paste0(path, "/Output/Metastasis.jpeg"))
ggplot(data = global_M4M, aes(x=Mets, y=Age), fill=Mets) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Mets", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()
global_M4M %>%
  select(Age, Mets) %>% 
  tbl_summary(by=Mets, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall()

global_M4M %>% 
  group_by(CHIP, Case_Control,Mets) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y=percent, fill=Mets)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  facet_grid(. ~ CHIP)+
  scale_y_continuous(expand = c(0, 0))+
  theme(strip.background = element_rect(colour="darkgrey", fill="white",
                                        size=1.5, linetype="solid"))

# tbl <- 
  global_M4M %>%
  select(CHIP, Mets) %>% 
  tbl_summary(by=CHIP) %>% 
  add_p() %>% add_overall() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/sumtable Metastasis in CHIP.pdf"))

# Neutro, Anemia, Thrombo, Leuko----
global_M4M %>% 
  select(CHIP,
         Neutro, Anemia, Thrombo, Leuko
  ) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()

# Table 1_Patient Population----
# Case_Control
# tbl <- 
    global_M4M %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/Table 1_Patient Population.pdf"))

# CHIP
# tbl <- 
  global_M4M %>% 
  select(CHIP, Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/Table 2_CHIP vs no CHIP.pdf"))
  
# Combined----
tbl1 <- global_M4M %>% 
  filter(CHIP == "CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_M4M %>% 
  filter(CHIP == "No CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
# tbl <- 
  tbl_merge(list(tbl1, tbl2),
                 tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1, 
#            paste0(
#              path, 
#              "/Output/Table 2_Patient Population faceted by CHIP.pdf"))

# without the unknown
tbl1 <- 
  global_M4M %>% 
  filter(CHIP == "CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_M4M %>% 
  filter(CHIP == "No CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
# tbl <- 
tbl_merge(list(tbl1, tbl2),
          tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()

tbl1 <- 
  muts_data %>% 
  filter(CHIP == "CHIP") %>% 
  select(Case_Control, VAF
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
tbl2 <- 
  muts_data %>% 
  filter(CHIP == "No CHIP") %>% 
  select(Case_Control, VAF
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
# tbl <- 
tbl_merge(list(tbl1, tbl2),
          tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 1,
#            paste0(
#              path,
#              "/Output/Table VAF faceted by CHIP.pdf"))

# CHIP in age, gender, race, ethnicity
# tbl1 <- global_M4M %>% 
#   select(CHIP, Age, Gender, Race, Ethnicity) %>% 
#   tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl2 <- global_M4M %>% 
#   select(CHPD, Age, Gender, Race, Ethnicity) %>% 
#   tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl <- tbl_merge(list(tbl1, tbl2),
#                    tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/CHIP_CHPD in age, gender, race, ethnicity.pdf"))

# CHIP in comorbidity ans mets
# tbl1 <- global_M4M %>% 
#   select(CHIP, Smoking, Mets) %>% 
#   tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl2 <- global_M4M %>% 
#   select(CHPD, Smoking, Mets) %>% 
#   tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl <- tbl_merge(list(tbl1, tbl2),
#                    tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/CHIP_CHPD in comorbidity ans mets.pdf"))

# tbl1 <- global_M4M %>% 
#   select(CHIP, Age, CANCER) %>%
#   tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl2 <- global_M4M %>% 
#   select(CHPD, Age, CANCER) %>%
#   tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl <- tbl_merge(list(tbl1, tbl2),
#                    tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/CHIP_CHPD in age and cancer.pdf"))

# tbl <- global_M4M %>% 
#   select(CHIP, CHPD, CANCER) %>%
#   tbl_summary(by= CANCER, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n() %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2, 
#            paste0(
#              path, 
#              "/Output/CHIP_CHPD in cancer.pdf"))



# Table CHIP facet Cases without the unknown---
tbl1 <- 
  global_M4M %>% 
  filter(Case_Control == "Cases") %>% 
  select(CHIP, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_M4M %>% 
  filter(Case_Control == "Controls") %>% 
  select(CHIP, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})",
              missing = "no") %>% 
  add_p() %>%
  add_n()
# tbl <- 
tbl <- tbl_merge(list(tbl1, tbl2),
          tab_spanner = c("**Cases**", "**Controls**"))  %>% as_gt()
# gt::gtsave(tbl, expand = 1, zoom = 2,
#            paste0(
#              path,
#              "/Output/Table 3_Patient Population faceted by CaseControl.pdf"))



