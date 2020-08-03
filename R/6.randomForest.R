library(randomForest)

table(global_M4M$Case_Control)
which(is.na(global_M4M$CANCER))

# Imput
G_M4M <- global_M4M %>% select("NGS_ID", "Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                               "Smoking", "CANCER", "Mets", 
                               "BaseANC", "BaseHGB", "BasePLT", "BaseWBC", "ChangeANC", "ChangeHGB", "ChangePLT", "ChangeWBC",
                               "MAX2", "MAX2heme",
                               "Neutro", "Anemia", "Thrombo", "Leuko", 
                               "Prior_chemo", "Prior_rad")
Amelia::missmap(as.data.frame(G_M4M))

G_M4M <- G_M4M %>% filter(!is.na(BaseANC&ChangeANC))
Amelia::missmap(as.data.frame(G_M4M))

library(mice)
# md.pattern(G_M4M)
# set.seed(2020)
# M4M_imputed <- rfImpute(global_M4M[18:25], data= G_M4M, iter= 6, y=global_M4M[4:17])
# 

colnames(global_M4M)

# install.packages("missForest")
# library(missForest)
# G_M4M_imp <- missForest(G_M4M )


# RandomForest---
model <- randomForest(Case_Control ~ CHIP, data= G_M4M, proximity=TRUE)
model
# 33.59% of the samples were not correctly classified by the ramdomF
# 44 CAses are classified Controls-> Bad

model <- randomForest(Case_Control ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC, data= G_M4M, proximity=TRUE)
model

model <- randomForest(Case_Control ~ BaseANC+BaseHGB+BasePLT+BaseWBC, data= G_M4M, proximity=TRUE)
model

model <- randomForest(Case_Control ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+MAX2heme, data= G_M4M, proximity=TRUE)
model

G_M4M_CHIP <- G_M4M %>% filter(CHIP == "CHIP")
G_M4M_NoCHIP <- G_M4M %>% filter(CHIP == "No CHIP")

model <- randomForest(Case_Control ~ BaseANC+BaseHGB+BasePLT+BaseWBC, data= G_M4M_CHIP, proximity=TRUE)
model

model <- randomForest(Case_Control ~ BaseANC+BaseHGB+BasePLT+BaseWBC, data= G_M4M_NoCHIP, proximity=TRUE)
model




