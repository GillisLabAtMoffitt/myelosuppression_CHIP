names(global_data)
summary(global_data)
# head(subset(global_data, select = 'CANCER'))
# tablecancer <-table (global_data$CANCER)
# prop.table(tablecancer)

#####Descriptive Stats by CHIP status
##Race
table(global_data$CHIP, global_data$Race)
CHIPrace <- table(global_data$CHIP, global_data$Race)
prop.table(CHIPrace)
# summary(CHIPrace)
fisher.test(table (global_data$CHIP, global_data$Race))

##Gender
table(global_data$CHIP, global_data$Gender)
CHIPsex <- table(global_data$CHIP, global_data$Gender)
prop.table(CHIPsex)
summary(CHIPsex)
# fisher.test(table (global_data$CHIP, global_data$Gender))

##Smoking
table(global_data$CHIP, global_data$Smoking)
CHIPsmok <- table(global_data$CHIP, global_data$Smoking)
prop.table(CHIPsmok)
summary(CHIPsmok)
# fisher.test(table (global_data$CHIP, global_data$Smoking))

##Cancer type
table(global_data$CHIP, global_data$CANCER)
CHIPtype <- table(global_data$CHIP, global_data$CANCER)
prop.table(CHIPtype)
summary(CHIPtype)
fisher.test(table (global_data$CHIP, global_data$CANCER))

##Chemo
table(global_data$CHIP, global_data$Prior_chemo)
CHIPchemo <- table(global_data$CHIP, global_data$Prior_chemo)
prop.table(CHIPchemo)
summary(CHIPchemo)
# fisher.test(table (global_data$CHIP, global_data$Prior_chemo))

#Radiation
table(global_data$CHIP, global_data$Prior_rad)
CHIPrad <- table(global_data$CHIP, global_data$Prior_rad)
prop.table(CHIPrad)
summary(CHIPrad)
# fisher.test(table (global_data$CHIP, global_data$Prior_rad))

##Age (independent 2-group Mann-Whitney U Test)
library(psych)
describeBy(global_data$Age_at_chemo_start, group = global_data$CHIP, digits=3 )
wilcox.test(Age_at_chemo_start~CHIP, global_data = global_data)




#####CONTINGENCY TABLES AND FET/ChiSq
##CHIP status and Metastases (p=0.046)
table(global_data$CHIP, global_data$Met_at_chemo_start)
CHIPmet <- table(global_data$CHIP, global_data$Met_at_chemo_start)
prop.table(CHIPmet)
summary(CHIPmet)
fisher.test(table (global_data$CHIP, global_data$Met_at_chemo_start))
# chisq.test(table (global_data$CHIP, global_data$Met_at_chemo_start), correct = F) 
  
##Prior chemo and Met (associated, p=0.029)
# describeBy(global_data$Prior_chemo, group=global_data$Prior_chemo)
table(global_data$Prior_chemo, global_data$Met_at_chemo_start)
chemo_met <- table(global_data$Prior_chemo, global_data$Met_at_chemo_start)
prop.table(chemo_met)
summary(chemo_met)
fisher.test( table (global_data$Prior_chemo, global_data$Met_at_chemo_start))
# chisq.test(table (global_data$Prior_chemo, global_data$Met_at_chemo_start), correct = F)

##Prior rad and Met (not associated)
table(global_data$Prior_rad, global_data$Met_at_chemo_start)
# fisher.test( table (global_data$Prior_rad, global_data$Met_at_chemo_start))
chisq.test(table (global_data$Prior_rad, global_data$Met_at_chemo_start), correct = F)

##Smoking and Met (not associated)
table(global_data$Smoking, global_data$Met_at_chemo_start)
# fisher.test( table (global_data$Smoking, global_data$Met_at_chemo_start))
chisq.test(table (global_data$Smoking, global_data$Met_at_chemo_start), correct = F)

##Gender and Met (not associated)
table(global_data$Gender, global_data$Met_at_chemo_start)
# fisher.test( table (global_data$Gender, global_data$Met_at_chemo_start))
chisq.test(table (global_data$Gender, global_data$Met_at_chemo_start), correct = F)

##Race and Met (not associated)
table(global_data$Race, global_data$Met_at_chemo_start)
fisher.test( table (global_data$Race, global_data$Met_at_chemo_start))
# chisq.test(table (global_data$Race, global_data$Met_at_chemo_start), correct = F)

##Age (independent 2-group Mann-Whitney U Test)
describeBy(global_data$Age_at_chemo_start, group = global_data$Met_at_chemo_start, digits=3 )
wilcox.test(Age_at_chemo_start~Met_at_chemo_start, global_data = global_data)

##Cancer type
table(global_data$Met_at_chemo_start, global_data$CANCER)
Mettype <- table(global_data$Met_at_chemo_start, global_data$CANCER)
prop.table(Mettype)
summary(Mettype)
fisher.test(table (global_data$Met_at_chemo_start, global_data$CANCER))


#####LOGISTIC REGRESSION (http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/)
##CHIP and Mets - https://cfss.uchicago.edu/stat003_logistic_regression.html
CHIP_met <-glm(Met_at_chemo_start ~ CHIP, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met)
coef(CHIP_met)

##CHIP and Mets, chemo hx, age, sex, race
CHIP_met_adj <-glm(Met_at_chemo_start ~ CHIP + Age_at_chemo_start + Gender +Race + Prior_chemo, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met_adj)
coef(CHIP_met_adj)
# summary(CHIP_met_adj$coef)

##CHIP and Mets, chemo hx, rad hx, age, sex, smoking, race
CHIP_met_adj2 <-glm(Met_at_chemo_start ~ CHIP + Age_at_chemo_start + Race + Gender + Prior_chemo + Prior_rad + Smoking, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met_adj2)

##CHIP and Mets, chemo hx
CHIP_met_adj3 <-glm(Met_at_chemo_start ~ CHIP + Prior_chemo, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met_adj3)

##CHIP and Mets, chemo hx - INTERACTION
CHIP_met_adj4 <-glm(Met_at_chemo_start ~ CHIP + Prior_chemo + CHIP:Prior_chemo, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met_adj4)

##CHIP and Mets, chemo hx, age
CHIP_met_adj5 <-glm(Met_at_chemo_start ~ CHIP + Prior_chemo + Age_at_chemo_start, global_data = global_data, family = binomial(link='logit'))
summary(CHIP_met_adj5)

##To get ORs and Forest Plot
exp(cbind(OR = coef(CHIP_met), confint(CHIP_met)))
exp(cbind(OR = coef(CHIP_met_adj), confint(CHIP_met_adj)))
exp(cbind(OR = coef(CHIP_met_adj2), confint(CHIP_met_adj2)))
exp(cbind(OR = coef(CHIP_met_adj3), confint(CHIP_met_adj3)))
adjOR <- exp(cbind(OR = coef(CHIP_met_adj), confint(CHIP_met_adj)))

odds.ratio(x, alpha = 0.05)


library(forestplot)
yi<-c(1.84,1.04,0.86,1.19, 2.62)
lower<-c(0.88,0.97,0.40,0.31,1.04)
upper<-c(3.88,1.11,1.83,6.00,6.61)
Variable<-c("CHIP","Age","Sex (Male)","Race (White)","Prior Chemo (yes)")
studi2<-c("1.84 [0.88-3.88]","1.04 [0.97-1.11]","0.86 [0.40-1.83]","1.19 [0.31-6.00]","2.62 [1.04-6.61]")
forestplot(Variable,yi,lower,upper, xlim=c(-1, 2), alim=c(0,1), xlab="Odds Ratio", refline=1, main=expression("Covariates - Met dz"))
x=mean(yi)
y=sd(yi)
se=y/sqrt(length(yi))
lim1=x-y
lim2=x+y
forestplot(Variable, yi, lower, upper, is.summary = FALSE,
           clip = c(0.3, 5),boxsize=0.1, xlab = "Variable",zero=1,xticks=c(0.1,1,2,3,4,5,6,7),
           fn.ci_norm="fpDrawCircleCI")

#Code From https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html
library(forestplot)
CHIP_met_adj_OR <-
  structure(list(
    mean = c(NA, 1.84, 1.04, 0.86, 1.19, 2.62),
    lower = c(NA, 0.88, 0.97, 0.40, 0.31, 1.04),
    upper = c(NA, 3.88, 1.11, 1.83, 6.00, 6.61)),
    .Names = c("mean", "lower", "upper"),
    row.names = c(NA, -4L),
    class = "global_data.frame")
tabletext <-cbind(
  c("Variable", "CHIP","Age","Sex (Male)","Race (White)","Prior Chemo (yes)"),
  c("Exposure+", "39%", "77 yo", "29%","30%", "48%"),
  c("Exposure-", "24%", "76 yo", "30%","25%", "26%"),
  c("OR","1.84","1.04","0.86","1.19", "2.62"))

forestplot(tabletext,
           CHIP_met_adj_OR,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,5)),
           clip=c(0.3,7), 
           xlog=TRUE, 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))

##CHIP and prior chemo only
CHIP_met_adj_OR <-
  structure(list(
    mean = c(NA, 1.91, 2.45),
    lower = c(NA, 0.95, 1.00),
    upper = c(NA, 3.85, 5.97)),
    .Names = c("mean", "lower", "upper"),
    row.names = c(NA, -3L),
    class = "global_data.frame")
tabletext <-cbind(
  c("Variable", "CHIP","Prior Chemo (yes)"),
  c("Exposure+", "39%", "48%"),
  c("Exposure-", "24%", "26%"),
  c("OR","1.91","2.45"))

forestplot(tabletext,
           CHIP_met_adj_OR,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,2)),
           clip=c(0.5,7), 
           xlog=TRUE, 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))





# #ChiSquare for model fit (likelihood ratio test, https://stats.idre.ucla.edu/r/dae/logit-regression/)
# with(CHIP_met_adj, null.deviance - deviance) #ChiSq value
# with(CHIP_met_adj, df.null - df.residual) #df
# with(CHIP_met_adj, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #p-value

##ANOVA to compare model performance (https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/)
anova(CHIP_met, test="Chisq")
anova(CHIP_met_adj, test="Chisq")
anova(CHIP_met_adj2, test="Chisq")
anova(CHIP_met_adj3, test="Chisq")
anova(CHIP_met,CHIP_met_adj, test="Chisq")
anova(CHIP_met,CHIP_met_adj2, test="Chisq")
anova(CHIP_met,CHIP_met_adj3, test="Chisq")
anova(CHIP_met,CHIP_met_adj4, test="Chisq")
anova(CHIP_met,CHIP_met_adj5, test="Chisq")

##McFadden R2 index - model fit
library(pscl)
pR2(CHIP_met)
pR2(CHIP_met_adj)
pR2(CHIP_met_adj2)
pR2(CHIP_met_adj3)

#Model accuracy
library(tidyverse)
library(modelr)
library(broom)
CHIP_accuracy <- augment(CHIP_met, type.predict = "response") %>%
  mutate(.pred = as.numeric(.fitted > .5))

mean (CHIP_accuracy$Met_at_chemo_start != CHIP_accuracy$.pred, na.rm = TRUE)

#####29.6% of the time the predictions based on CHIP status were incorrect

adj_accuracy <- augment(CHIP_met_adj, type.predict = "response") %>%
  mutate(.pred = as.numeric(.fitted > .5))

mean (adj_accuracy$Met_at_chemo_start != adj_accuracy$.pred, na.rm = TRUE)

#####27.2% of the time the predictions based on CHIP+other factors were incorrect

# adj_accuracy2 <- augment(CHIP_met_adj2, type.predict = "response") %>%
#   mutate(.pred = as.numeric(.fitted > .5))
# 
# mean (adj_accuracy2$Met_at_chemo_start != adj_accuracy2$.pred, na.rm = TRUE)
# 
# #####27.2% of the time the predictions based on CHIP+other factors were incorrect
# 
# adj_accuracy3 <- augment(CHIP_met_adj3, type.predict = "response") %>%
#   mutate(.pred = as.numeric(.fitted > .5))
# 
# mean (adj_accuracy3$Met_at_chemo_start != adj_accuracy3$.pred, na.rm = TRUE)
