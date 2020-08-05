rf.data = read.csv('CHIP_CRASH_data_for_stats_v07_randomforest_2.csv',stringsAsFactors = F,header=T)
# calculate random forest to get variable importance
rf.data <- data.frame(rf.data)
head(rf.data)
rf.data <- rf.data %>% select("Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                         "Smoking", "Mets",
                         "Prior_chemo", "Prior_rad",
                         "BaseANC", "BaseHGB", "BasePLT", "BaseWBC"# ,
                         # "MAX2heme"
                         )
rf.data$Ethnicity[rf.data$Ethnicity==''] = 'unknown'

rf.data <- rf.data %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Cases", "Controls"), levels= c(1, 0)))
rf.data = rf.data %>% mutate_if(is.character, as.factor)

for(i in 11:14){
  rf.data[is.na(rf.data[,i]),i] = median(rf.data[,i],na.rm=T)
}

set.seed(1485)
# rf <- randomForest(rf.data, as.factor(rf.data$Case_Control),ntree=2000,importance=TRUE)#,sampsize=c(samp.size,samp.size))
rf <- randomForest(Case_Control ~ ., data=rf.data,ntree=2000,importance=TRUE)
print(rf)
rf.feat <- rf$importance

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

tree.data <- rf.data[,probe.feats]
tree.data <- cbind(as.character(rf.data$Case_Control),tree.data)
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




# For Baseline prdiction----
rf.data = read.csv('CHIP_CRASH_data_for_stats_v07_randomforest_2.csv',stringsAsFactors = F,header=T)
# calculate random forest to get variable importance
rf.data <- data.frame(rf.data)
head(rf.data)
rf.data$BaseANC
median(rf.data$BaseANC, na.rm = TRUE)
rf.data <- rf.data %>% select("Case_Control", "CHIP", "Age", "Gender", "Race", "Ethnicity", 
                              "Smoking", "Mets",
                              "Prior_chemo", "Prior_rad",
                              "BaseANC", "VAF"# ,
                              # "MAX2heme"
)
for(i in 11:14){
  rf.data[is.na(rf.data[,i]),i] = median(rf.data[,i],na.rm=T)
}

rf.data$Ethnicity[rf.data$Ethnicity==''] = 'unknown'

rf.data <- rf.data %>% 
  mutate(Case_Control = factor(Case_Control, labels = c("Cases", "Controls"), levels= c(1, 0))) %>% 
  mutate(Base_ANC_grp = case_when(
    BaseANC >= median(rf.data$BaseANC, na.rm = TRUE) ~ "ANC_high",
    BaseANC < median(rf.data$BaseANC, na.rm = TRUE) ~ "ANC_low"
  )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-BaseANC)



set.seed(1485)
# rf <- randomForest(rf.data, as.factor(rf.data$Case_Control),ntree=2000,importance=TRUE)#,sampsize=c(samp.size,samp.size))
rf <- randomForest(Base_ANC_grp ~ ., data=rf.data,ntree=2000,importance=TRUE)
print(rf)
rf.feat <- rf$importance

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

tree.data <- rf.data[,probe.feats]
tree.data <- cbind(as.character(rf.data$Base_ANC_grp),tree.data)
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
