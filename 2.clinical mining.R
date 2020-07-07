############################################################################################## I ### global_data mining----
print(paste("This data have", dim(global_data)[2], "variables on", dim(global_data)[1], "patients.",
            sum(str_detect(global_data$Case_Control,"Cases")), "are cases,", sum(str_detect(global_data$Case_Control,"Controls")), "are controls.
            In 2018 as we had 44 cases and 87 controls."))

############################################################################################## I ### CHIP----

# 1.1 CHIP prevalence----
# pdf(paste0(path, "/Output/CHIP prevalence.pdf"))
global_data %>% group_by(Case_Control,CHIP) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHIP))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHIP") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_data$Case_Control, global_data$CHIP))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

# CHPD prevalence
# pdf(paste0(path, "/Output/CHPD prevalence.pdf"))
global_data %>% group_by(Case_Control,CHPD) %>% 
  summarise(count=n()) %>% 
  mutate(perc=(count/sum(count)*100)
  ) %>% 
  ggplot(aes(x=Case_Control, y= perc, fill=CHPD))+
  geom_bar(stat="identity") +
  labs(x = "", y = "percent", title = "Prevalence of CHPD") +
  theme_minimal()+
  geom_text(aes(label = round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0("n=", count)), size = 3, position = position_stack(vjust = 0.25))+
  annotate("text", x = 0, y = 105, label = paste0("p=",chisq.test(table(global_data$Case_Control, global_data$CHPD))[3]),
           color = "black", size = 6, hjust = 0, vjust = 1)
# dev.off()

tbl <- global_data %>% 
  select(Case_Control, CHIP, CHPD) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
tbl <- global_data %>% 
  select(Case_Control, CHIP) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
tbl <- global_data %>% 
  select(Case_Control, CHPD) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP and CHPD in Case_Control.pdf"))
chisq.test(table(global_data$Case_Control, global_data$CHIP))
chisq.test(table(global_data$Case_Control, global_data$CHIP))$residuals
chisq.test(table(global_data$Case_Control, global_data$CHPD))
chisq.test(table(global_data$Case_Control, global_data$CHPD))$residuals

# CHIP vs CHPD
venn.diagram(
  x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
           CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID))),
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
  x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
           CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
           CHIPno = c(unique(global_data[global_data$CHIP == "No CHIP",]$NGS_ID))),
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
  x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
           CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
           Cases = c(unique(global_data[global_data$Case_Control == "Cases",]$NGS_ID)),
           Controls = c(unique(global_data[global_data$Case_Control == "Controls",]$NGS_ID))),
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
  x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
           CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
           Cases = c(unique(global_data[global_data$Case_Control == "Cases",]$NGS_ID))),
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
  x = list(CHIP = c(unique(global_data[global_data$CHIP == "CHIP",]$NGS_ID)),
           CHPD = c(unique(global_data[global_data$CHPD == "CHPD",]$NGS_ID)),
           Controls = c(unique(global_data[global_data$Case_Control == "Controls",]$NGS_ID))),
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
    position = position_jitter(0.2), color = "gray"
  ) + 
  theme_minimal()+
  geom_pointrange(
    aes(ymin = median-sd, ymax = median+sd),
    data = global_data.summary
  )+
  stat_compare_means(label.x = 1.15, label.y = .52)

tbl <- global_data %>% 
  group_by(Case_Control) %>% 
  filter(!is.na(VAF)) %>% 
  select(Case_Control, VAF) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/VAF in Case_Control.pdf"))

# ggplot(global_data, aes(x=Case_Control, y= VAF))+
#   geom_dotplot(
#     binaxis = "y", stackdir = "center",
#     color = "lightgray"
#   ) + 
#   stat_summary(
#     fun.data = "mean_sdl", fun.args = list(mult=1), 
#     geom = "pointrange", color = "red"
#   )

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

as.data.frame(table(global_data$FUNCTION, global_data$GENE)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
# write.csv(tbl, paste0(path, "/Output/Fonction type per gene.csv"))


############################################################################################## III ### Clinical mining----

# Cancer repartition pie ----
tbl <- as.data.frame(table(global_data$CANCER))
colourCount = length(unique(tbl$Var1))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)
# pdf(paste0(path, "/Output/Cancer repartition.pdf"))
tbl %>% mutate(Var1 = fct_reorder(Var1, desc(Freq))) %>%
  ggplot(aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Cancer type", values = getPalette) +
  theme_minimal() +
  coord_polar("y", start=0, direction=-1) +
  labs(x="", y="", title="Cancer type Repartition")
# dev.off()

tbl <- as.data.frame(table(global_data$CHIP,global_data$CANCER))
tbl %>% mutate(Var2 = fct_reorder(Var2, desc(Freq))) %>%
  ggplot(aes(x=Var2, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width = .75, position = position_dodge(preserve = "single")) +
  scale_fill_discrete(name = "CHIP") +
  theme_minimal() +
  coord_flip()+
  labs(x="Cancer Type", y="Number of Patient", title="CHIP in Cancer type Repartition")

# Age
# pdf(paste0(path, "/Output/Age repartition.pdf"))
qplot(x =Age, data=global_data, fill=..count.., geom="histogram")+
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
ggplot(data = global_data, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=22)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()
ggplot(data = global_data, aes(x=CANCER, y=Age), fill=CANCER) +
  geom_boxplot(color= magma(n=32)) +
  theme_minimal() +
  labs(x="Cancer type", y="Age", title="Age repartition per cancer") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(rows = vars(CHIP))

# pdf(paste0(path, "/Output/Gender.pdf"))
ggplot(data = global_data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Race.pdf"))
ggplot(data = global_data, aes(x=Race, y=Age), fill=Race) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Ethnicity.pdf"))
ggplot(data = global_data, aes(x=Ethnicity, y=Age), fill=Ethnicity) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Ethnicity", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

# pdf(paste0(path, "/Output/Smoking.pdf"))
ggplot(data = global_data, aes(x=Smoking, y=Age), fill=Smoking) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Smoking", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()

tbl1 <- global_data %>% filter(CHIP == "CHIP") %>% 
  select(Age, Race) %>% 
  tbl_summary(by=Race, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Race")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_data %>% filter(CHIP == "No CHIP") %>% 
  select(Age, Race) %>% 
  tbl_summary(by=Race, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Race")) %>% 
  add_p() %>% add_overall()
tblm1 <- tbl_merge(list(tbl1, tbl2),
          tab_spanner = c("**CHIP**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels()

tbl1 <- global_data %>% filter(CHIP == "CHIP") %>% 
  select(Age, Ethnicity) %>% 
  tbl_summary(by=Ethnicity, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Ethnicity")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_data %>% filter(CHIP == "No CHIP") %>% 
  select(Age, Ethnicity) %>% 
  tbl_summary(by=Ethnicity, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Ethnicity")) %>% 
  add_p() %>% add_overall()
tblm2 <- tbl_merge(list(tbl1, tbl2),
         tab_spanner = c("**CHIPe**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels()

tbl1 <- global_data %>%filter(CHIP == "CHIP") %>% 
  select(Age, Smoking) %>% 
  tbl_summary(by=Smoking, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Smoking")) %>% 
  add_p() %>% add_overall()
tbl2 <- global_data %>%filter(CHIP == "No CHIP") %>% 
  select(Age, Smoking) %>% 
  tbl_summary(by=Smoking, statistic = all_continuous() ~ "{median} ({sd})", label = list(Age ~ "Age per Smoking")) %>% 
  add_p() %>% add_overall()
tblm3 <- tbl_merge(list(tbl1, tbl2),
        tab_spanner = c("**CHIPs**", "**no CHIP**")) %>%
  bold_labels() %>%
  italicize_levels()
tbl <- tbl_stack(list(tblm1, tblm2, tblm3))

# pdf(paste0(path, "/Output/Metastasis.pdf"))
ggplot(data = global_data, aes(x=Mets, y=Age), fill=Mets) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Mets", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CHIP)
# dev.off()
global_data %>%
  select(Age, Mets) %>% 
  tbl_summary(by=Mets, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>% add_overall()

global_data %>%
  select(CHIP, Mets) %>% 
  tbl_summary(by=CHIP) %>% 
  add_p() %>% add_overall()


# Table 1_Patient Population----
# Case_Control
tbl <- global_data %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/Table 1_Patient Population.pdf"))

# CHIP
tbl <- global_data %>% 
  select(CHIP, Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/Table 2_CHIP vs no CHIP.pdf"))
# Combined
tbl1 <- global_data %>% 
  filter(CHIP == "CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_data %>% 
  filter(CHIP == "No CHIP") %>% 
  select(Case_Control, Age, Gender, Race, Ethnicity, Smoking, Mets, 
         BaseANC, BaseHGB, BasePLT, BaseWBC,
         ChangeANC, ChangeHGB, ChangePLT, ChangeWBC, 
         Prior_chemo, Prior_rad, 
         MAX2, MAX2heme
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl <- tbl_merge(list(tbl1, tbl2),
                 tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/Table 1_Patient Population faceted by CHIP.pdf"))


# CHIP in age, gender, race, ethnicity
tbl1 <- global_data %>% 
  select(CHIP, Age, Gender, Race, Ethnicity) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_data %>% 
  select(CHPD, Age, Gender, Race, Ethnicity) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl <- tbl_merge(list(tbl1, tbl2),
                   tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in age, gender, race, ethnicity.pdf"))

# CHIP in comorbidity ans mets
tbl1 <- global_data %>% 
  select(CHIP, Smoking, Mets) %>% 
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_data %>% 
  select(CHPD, Smoking, Mets) %>% 
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl <- tbl_merge(list(tbl1, tbl2),
                   tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in comorbidity ans mets.pdf"))

tbl1 <- global_data %>% 
  select(CHIP, Age, CANCER) %>%
  tbl_summary(by= CHIP, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl2 <- global_data %>% 
  select(CHPD, Age, CANCER) %>%
  tbl_summary(by= CHPD, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()
tbl <- tbl_merge(list(tbl1, tbl2),
                   tab_spanner = c("**CHIP**", "**CHPD**"))  %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in age and cancer.pdf"))

tbl <- global_data %>% 
  select(CHIP, CHPD, CANCER) %>%
  tbl_summary(by= CANCER, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n() %>% as_gt()
gt::gtsave(tbl, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Output/CHIP_CHPD in cancer.pdf"))







