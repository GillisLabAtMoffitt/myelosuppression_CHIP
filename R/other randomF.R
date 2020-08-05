path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#---
clinical <-
  read_csv(paste0(path, "/data/CHIP_CRASH_data_for_stats_v06.csv")) %>% 
  rename(old_CHIP = CHIP, old_CHPD = CHPD) %>% 
  select(-X32)

CHIP_muts1 <- 
  read_csv(paste0(path, "/data/cleaned CH_myelosupp_muts_2020_CC.csv"))
CHIP_muts <- 
  read.delim(paste0(path, "/data/M4M2018_updated 06.24.20_filtered.txt"))

#######################################################################################  II  ### Data cleaning----
# 2.1.Clinical data----
clinical <- data.frame(clinical)
for(i in 15:18){
  clinical[is.na(clinical[,i]),i] = median(clinical[,i],na.rm=TRUE)
}

clinical$Ethnicity[is.na(clinical$Ethnicity)] = 'unknown'

clinical <- clinical %>% 
  mutate(Cases_Controls = Case_Control) %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Cases", "Controls"), levels= c(1, 0))) %>% # 0 = ctrl
  mutate(old_CHIP = factor(old_CHIP, labels=c("No CHIP", "CHIP"))) %>% 
  mutate(old_CHPD= factor(old_CHPD, labels=c("No CHPD", "CHPD")))




# 2.2.CHIP data----
CHIP_muts <- full_join(CHIP_muts1 %>% 
                         select(patient_id),
                       CHIP_muts, 
                       by = "patient_id")

# Modified CHIP var to fit with the new data
CHIP_muts <- CHIP_muts %>% 
  mutate(CHIP = case_when(
    is.na(DATA) ~ 0,
    !is.na(DATA) ~ 1 # CHIP
  )) %>% 
  mutate(CHIP = factor(CHIP)) %>% 
  mutate(VAF_10P = case_when(
    VAF >= 0.1 ~ VAF
  )) %>% 
  mutate(VAF_grp = case_when(
    VAF >= 0.1 ~ "VAF >= 10%",
    VAF < 0.1 ~ "VAF < 10%"
  )) %>% 
  mutate(CHIP_DDR = case_when(
    GENE %in% c("TP53", "PPM1D", "CHEK2") ~ "CHIP",
    is.na(GENE) ~ "No CHIP",
    GENE %in% c("ASXL1", "BRCC3", "DNMT3A", "JAK2", "KMT2D", "SF3B1", "SH2B3", "SRSF2", "TET2") ~ "No CHIP"
  ))

#######################################################################################  III  ### Data binding----
# Will bind the 2 data but in 2 different ways 

# 3.1.Bind to have 1 mutation per row, multiple row per patient----
muts_data <- full_join(clinical, CHIP_muts, 
                       by = c("NGS_ID" = "patient_id")) %>%
  select("NGS_ID", "Cohort", "Case_Control", "Strata", "old_CHIP", "CHIP", "CHIP_DDR", everything())
# write_csv(muts_data, paste0(path, "/Output/data output/muts_data.csv"))

# 3.2.Bind to have 1 patient per row, multiple mutation per row----
CHIP_muts1 <- dcast(setDT(CHIP_muts), patient_id+CHIP ~ rowid(patient_id),
                    value.var = c("GENE", "FUNCTION", "COSMIC", "ESP6500", "VAF", "DEPTH", "VAF_10P", "CHIP_DDR")) %>% 
  mutate(CHIP_VAF_10P = factor(ifelse(
    !is.na(VAF_10P_1) | !is.na(VAF_10P_2) | !is.na(VAF_10P_3) | !is.na(VAF_10P_4) | !is.na(VAF_10P_5) |
      !is.na(VAF_10P_6) | !is.na(VAF_10P_7) | !is.na(VAF_10P_8) | !is.na(VAF_10P_9) | !is.na(VAF_10P_10) |
      !is.na(VAF_10P_11) | !is.na(VAF_10P_12) | !is.na(VAF_10P_13) | !is.na(VAF_10P_14) | !is.na(VAF_10P_15) |
      !is.na(VAF_10P_16), "CHIP", "No CHIP"))
  ) %>% 
  mutate(CHIP_VAF_10P = factor(CHIP_VAF_10P, levels = c("No CHIP", "CHIP"))) %>% 
  mutate(CHIP_DDR1 = factor(ifelse(
    (CHIP_DDR_1  == "CHIP"| CHIP_DDR_2  == "CHIP"| CHIP_DDR_3  == "CHIP"| CHIP_DDR_4  == "CHIP" |
       CHIP_DDR_5  == "CHIP" | CHIP_DDR_6  == "CHIP" | CHIP_DDR_7  == "CHIP" | CHIP_DDR_8  == "CHIP" |
       CHIP_DDR_9  == "CHIP" | CHIP_DDR_10  == "CHIP" | CHIP_DDR_11  == "CHIP" | CHIP_DDR_12  == "CHIP" |
       CHIP_DDR_13  == "CHIP" | CHIP_DDR_14  == "CHIP" | CHIP_DDR_15  == "CHIP" | CHIP_DDR_16 == "CHIP"),
    "CHIP", "No CHIP"))
  ) %>% 
  mutate(CHIP_DDR = coalesce(CHIP_DDR1, "No CHIP")) %>% 
  mutate(CHIP_DDR = factor(CHIP_DDR, levels = c("No CHIP", "CHIP")))

global_data <- full_join(clinical, CHIP_muts, 
                         by = c("NGS_ID" = "patient_id")) %>%
  select("Patient", "NGS_ID", "Cohort", "Case_Control", "Strata", 
         "old_CHIP", "CHIP", "CHIP_DDR", everything()) %>% 
  mutate(CHIP = coalesce(CHIP, old_CHIP)) %>% 
  drop_na("Case_Control")

global_data <- global_data %>% select("Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                              "Smoking", "Mets",
                              "Prior_chemo", "Prior_rad",
                              "BaseANC", "BaseHGB", "BasePLT", "BaseWBC", "VAF", "GENE")

global_data <- global_data %>%
# mutate(Base_ANC_grp = case_when(
#   BaseANC >= median(rf.data$BaseANC, na.rm = TRUE) ~ "ANC_high",
#   BaseANC < median(rf.data$BaseANC, na.rm = TRUE) ~ "ANC_low"
# )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-BaseANC) %>% 
  drop_na(c("VAF", "GENE"))
str(global_data)

# RF----
rf <- randomForest(Case_Control ~ ., data=global_data,ntree=2000,importance=TRUE)
print(rf)
rf.feat <- rf$importance

######################################################################
######################################################################
# Plotting Classification Trees with the plot.rpart and rattle pckages


cat("Getting Features...")
feats <- rf.feat[order(rf.feat[,"MeanDecreaseGini"],decreasing=TRUE),"MeanDecreaseGini"]
write.csv(rf.feat,"output/RF_features_gini_scores.csv")
png("output/RF_features_gini_plot.png",width=600,height=600,units='px')
plot(feats,type="l",ylab="Mean Decrease Gini Score",xlab="Genes")
abline(h=.2,col="red") #cutoff
feats <- feats[feats>.2]
abline(v=length(feats),col="red")
graphics.off()

probe.feats <- names(feats)

tree.data <- global_data[,probe.feats]
tree.data <- cbind(as.character(global_data$Case_Control),tree.data)
colnames(tree.data)[1] <- "Response"
tree.data <- data.frame(tree.data)
head(tree.data)

# Make big tree
form <- as.formula(Response ~ .)

# Leave one out cross-validation LOOCV
library(rpart)
set.seed(1485)
leave.out <- sample(1:nrow(tree.data),replace=FALSE)
loocv.results <- NULL
for(i in 1:nrow(tree.data)){
  train <- tree.data[-leave.out[i],]
  test <- tree.data[leave.out[i],]
  sample.id <- rownames(tree.data)[leave.out[i]]
  all.tree <- rpart(form,train,control=rpart.control(minsplit=2,minbucket=1))
  
  #   cat("Pruning Tree...")
  #   prune.val <- all.tree$cptable[which.min(all.tree$cptable[,"xerror"])[1],"CP"]
  #   train.pfit <- prune(all.tree,cp=prune.val)
  #   cat("Done.\n")
  
  #don't prune
  train.pfit <- all.tree
  
  
  #cat("Plotting Tree...")
  #fancyRpartPlot(all.tree,type=1,extra=2) 
  #pdf(paste0("Decision_Tree_CVfold-",fold,"_trainData_",format(Sys.Date(),"%d%b%Y"),".pdf"),width=18,height=12)
  #fancyRpartPlot_DR(all.tree)
  #graphics.off()
  #cat("Done.\n")
  
  cat("Predicting using test data for calls...LOOCV:",i,"/",length(leave.out),"\n")
  pred.tree <- predict(train.pfit,test)
  calls <- colnames(pred.tree)[apply(pred.tree,1,which.max)]
  features <- unlist(unique(train.pfit$frame[1]))
  features <- paste(features[grep("<leaf>",features,invert=TRUE)],collapse=";")
  
  temp <- c(i,sample.id,calls,as.character(test[["Response"]]),features)
  #calls <- cbind(rownames(pred.tree),calls)
  #actual <- phenotype
  #calls <- cbind(calls,actual)
  loocv.results <- rbind(loocv.results,temp)
}
colnames(loocv.results) <- c("iteration","sample_id_left_out","calls","actual","features")
head(loocv.results)

#Calculate percent accuracy
score <- apply(loocv.results,1,function(x) if(x[3]==x[4]){return(1)}else{return(0)})
loocv.results <- cbind(loocv.results,score)
perc.correct <- sum(score)/length(score)

for(i in unique(loocv.results[,"calls"])){
  print(i)
  print(sum(as.numeric(loocv.results[which(loocv.results[,"calls"]==i),"score"]))/length(which(loocv.results[,"calls"]==i)))
}

actual <- loocv.results[,"actual"]
cv.call.mat <- matrix(0,nrow=length(unique(actual)),ncol=length(unique(actual)))
colnames(cv.call.mat) <- unique(actual)
rownames(cv.call.mat) <- unique(actual)

cat("Calculing Model Performance...") 
for(i in 1:nrow(loocv.results)){
  act <- loocv.results[i,"actual"]
  pred <- loocv.results[i,"calls"]
  cv.call.mat[pred,act] <- cv.call.mat[pred,act]+1
}


###   create final tree based on loocv features

final.feats <- unique(unlist(strsplit(loocv.results[,"features"],split=";")))
final.tree.data <- tree.data[c("Response",final.feats)]
final.tree <- rpart(form,final.tree.data,control=rpart.control(minsplit=2,minbucket=1))

# cat("Pruning Tree...")
# prune.val <- final.tree$cptable[which.min(final.tree$cptable[,"xerror"])[1],"CP"]
# final.pfit <- prune(final.tree,cp=prune.val)
# cat("Done.\n")

# don't prune
final.pfit <- final.tree


#cat("Plotting Tree...")
#fancyRpartPlot(all.tree,type=1,extra=2) 
pdf(paste0("output/Decision_Tree_ALLData.pdf"),width=18,height=12)
fancyRpartPlot(final.pfit)
graphics.off()
#cat("Done.\n")

cat("Predicting back on data for calls...")
pred.tree <- predict(final.pfit,final.tree.data)
calls <- colnames(pred.tree)[apply(pred.tree,1,which.max)]
calls <- cbind(rownames(pred.tree),calls)
actual <- as.character(final.tree.data[,"Response"])
calls <- cbind(calls,actual)
score <- apply(calls,1,function(x) if(x[2]==x[3]){return(1)}else{return(0)})
calls <- cbind(calls,score)
perc.correct <- sum(score)/length(score)
cat("Done.\n")

final.features <- unlist(unique(final.pfit$frame[1]))
final.features <- paste(final.features[grep("<leaf>",final.features,invert=TRUE)],collapse=";")


final.call.mat <- matrix(0,nrow=length(unique(actual)),ncol=length(unique(actual)))
colnames(final.call.mat) <- unique(actual)
rownames(final.call.mat) <- unique(actual)
cat("Calculing Model Performance...") 
for(i in 1:nrow(calls)){
  act <- calls[i,"actual"]
  pred <- calls[i,"calls"]
  final.call.mat[pred,act] <- final.call.mat[pred,act]+1
}


summary.file <- NULL
#calculate summary statistics
for(resp in unique(colnames(cv.call.mat))){
  
  j <- which(rownames(cv.call.mat)==resp)
  tp <- cv.call.mat[j,j]
  fp <- sum(cv.call.mat[j,-j])
  tn <- sum(cv.call.mat[-j,-j])
  fn <- sum(cv.call.mat[-j,j])
  
  sens <- round(tp/(tp+fn),digits = 3)
  spec <- round(tn/(tn+fp), digits = 3)
  ba <- mean(c(sens,spec))
  OR <- (tp/fp)/(fn/tn)
  OR.se <- sqrt((1/cv.call.mat[1,1])+(1/cv.call.mat[1,2])+(1/cv.call.mat[2,1])+(1/cv.call.mat[2,2]))
  OR.ci.l <- exp(log(OR)-(1.96*OR.se))
  OR.ci.h <- exp(log(OR)+(1.96*OR.se))
  RR <- (tp/(tp+fp)) / (fn/(tn+fn))
  TPR <- sens
  FPR <- 1-spec
  PPV <- tp/(tp+fp)
  NPV <- tn/(tn+fn)
  p.val <- fisher.test(matrix(c(tp,fp,tn,fn),nrow=2))[[1]]
  temp.out <- c("LOOCV",resp,sens,spec,ba,OR,OR.ci.l,OR.ci.h,RR,TPR,FPR,PPV,NPV,p.val,features)
  summary.file <- rbind(summary.file,temp.out)
} 
for(resp in unique(colnames(final.call.mat))){ 
  j <- which(rownames(final.call.mat)==resp)
  tp <- final.call.mat[j,j]
  fp <- sum(final.call.mat[j,-j])
  tn <- sum(final.call.mat[-j,-j])
  fn <- sum(final.call.mat[-j,j])
  
  test.n <- "NA"#nrow(test.tree.data)
  train.n <-nrow(tree.data)
  n.cancer.train <- length(which(tree.data[,"Response"]==resp))
  n.cancer.test <- "NA"#length(which(test.tree.data[,"Class"]==j))
  sens <- round(tp/(tp+fn),digits = 3)
  spec <- round(tn/(tn+fp), digits = 3)
  ba <- mean(c(sens,spec))
  OR <- (tp/fp)/(fn/tn)
  OR.se <- sqrt((1/final.call.mat[1,1])+(1/final.call.mat[1,2])+(1/final.call.mat[2,1])+(1/final.call.mat[2,2]))
  OR.ci.l <- exp(log(OR)-(1.96*OR.se))
  OR.ci.h <- exp(log(OR)+(1.96*OR.se))
  RR <- (tp/(tp+fp)) / (fn/(tn+fn))
  TPR <- sens
  FPR <- 1-spec
  PPV <- tp/(tp+fp)
  NPV <- tn/(tn+fn)
  p.val <- fisher.test(matrix(c(tp,fp,tn,fn),nrow=2))[[1]]
  temp.out <- c("ALL",resp,sens,spec,ba,OR,OR.ci.l,OR.ci.h,RR,TPR,FPR,PPV,NPV,p.val,final.features)
  summary.file <- rbind(summary.file,temp.out)
}

#out.path <- "Data/Predictive_Model/DecisionTree/ALL_AML/noCNV/"
write.csv(calls,"output/Decision_Tree_Calls_ALL_Data.csv",row.names=FALSE)
write.csv(loocv.results,"output/Decision_Tree_Calls_LOOCV_Data.csv",row.names=FALSE)


## Counts for LOOCV and ALL
write("#rows=predicted;cols=actual","output/Decision_Tree_CallCounts.csv",sep="")
write("LOOCV","output/Decision_Tree_CallCounts.csv",sep="",append = TRUE)
cv.call.mat.out <- cbind(rownames(cv.call.mat),cv.call.mat)
write.table(cv.call.mat.out,"output/Decision_Tree_CallCounts.csv",sep=",",append=TRUE,row.names=FALSE)
write("\nAll Data","output/Decision_Tree_CallCounts.csv",sep="",append = TRUE)
final.call.mat.out <- cbind(rownames(final.call.mat),final.call.mat)
write.table(final.call.mat.out,"output/Decision_Tree_CallCounts.csv",sep=",",append=TRUE,row.names=FALSE)

##write out summary file
colnames(summary.file) <-c("Model","Response","sens","spec","ba","OR","OR_95ci_l","OR_95ci_h","RR","TPR","FPR","PPV","NPV","Fisher_p","features")
write.csv(summary.file,"output/Decision_Tree_SummaryResults.csv",row.names=FALSE)


