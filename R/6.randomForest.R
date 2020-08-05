library(randomForest)

table(global_M4M$Case_Control)
which(is.na(global_M4M$CANCER))

# Imput
G_M4M <- global_M4M %>% select("NGS_ID", "Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                               "Smoking", "CANCER", "Mets", 
                               "BaseANC", "BaseHGB", "BasePLT", "BaseWBC",
                               
                                
                               "Prior_chemo", "Prior_rad")
Amelia::missmap(as.data.frame(G_M4M))

G_M4M <- G_M4M %>% filter(!is.na(BaseANC&MAX2heme))
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


model <- randomForest(Case_Control ~ ., data= G_M4M, proximity=TRUE)
model



# Mingxiang----
data = read.csv('CHIP_CRASH_data_for_stats_v07_randomforest_2.csv',stringsAsFactors = F,header=T)
features = data[,c(-1,-3,-4)]
features$Ethnicity[features$Ethnicity==''] = 'unknown'
features = features[,c(-8,-(16:23))]
for(i in 11:15){
  features[is.na(features[,i]),i] = median(features[,i],na.rm=T)
}
data.all = data.frame(features,cc=as.factor(data$Case_Control==1))
library(dplyr)
data.all = data.all %>% mutate_if(is.character, as.factor)
for( i in 1:ncol(data.all))
  cat(class(data.all[,i]),'\n')

## random Forest
library(randomForest)
dat = data.all
realidx = which(as.logical(dat$cc))
resimp = reserr = c()
for(i in 1:1000){
  subidx = sort(c(sample(seq_len(nrow(dat))[-realidx],length(realidx)),realidx))
  subrf = randomForest(cc ~ ., data=dat, subset = subidx, na.action=na.pass, ntree=100)
  plot(subrf,ylim=c(0,1))
  resimp = cbind(resimp,importance(subrf,type=2))
  reserr = cbind(reserr,subrf$confusion[,3])
  if(i %% 100 ==0) cat(i,'\n')
}


for(i in 1:1000){
  subidx = sort(c(sample(seq_len(nrow(dat))[-realidx],length(realidx)),realidx))
  subrf = randomForest(cc ~ ., data=dat, subset = subidx, na.action=na.pass, ntree=100)
  plot(subrf)
  resimp = cbind(resimp,importance(subrf,type=2))
  reserr = cbind(reserr,subrf$confusion[,3])
  if(i %% 100 ==0) cat(i,'\n')
}


# Prep data----
G_M4M <- global_M4M %>% select("Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                               "Smoking", "Mets",
                               "Prior_chemo", "Prior_rad",
                               "BaseANC", "BaseHGB", "BasePLT", "BaseWBC",
                               "MAX2heme")
Amelia::missmap(as.data.frame(G_M4M))
# Imputation
for(i in 11:15){
  G_M4M[is.na(G_M4M[,i]),i] = median(G_M4M[,i],na.rm=T)
}
class(G_M4M$BaseWBC)


