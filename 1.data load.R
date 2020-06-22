# Import library
library(tidyverse)
library(ggplot2)
library(viridis)

#######################################################################################  I  ### Load data
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#-----------------------------------------------------------------------------------------------------------------
data <-
  read_csv(paste0(path, "/CHIP_CRASH_data_for_stats_v05.csv"))


#######################################################################################  II  ### Load mining
data <- data %>% 
  mutate_at(("Race"), ~ case_when(
    . == 0 ~ "Black",
    . == 1 ~ "White",
    TRUE ~ NA_character_
  )) %>% 
  mutate_at(("Gender"), ~ case_when(
    . == 0 ~ "Male",
    . == 1 ~ "Female",
    TRUE ~ NA_character_
  ))

# Cancer repartition
table <- as.data.frame(table(data$CANCER))
colourCount = length(unique(table$Var1))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)

ggplot(table, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = getPalette) +
  theme_minimal() +
  coord_polar("y", start=0, direction=-1)

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
  labs(x="?", y="Age", title="Age repartition") +
  geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(data = data, aes(x=Gender, y=Age), fill=Gender) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Gender", y="Age", title="Age repartition") +
  coord_flip() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ CANCER)


