############################################################################################## I ### Clinical mining----
print(paste("This data have", dim(data)[2], "variables on", dim(data)[1], "patients.",
            sum(str_detect(data$Case_Control,"Case")), "are cases,", sum(str_detect(data$Case_Control,"Control")), "are controls.
            This differs a liitle from the previous presentation in 2018 as we had 44 cases and 87 controls."))

# Cancer repartition pie
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

# pdf(paste0(path, "/Output/Age.pdf")) # Age per cancer
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

# CHIP prevalence----
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





