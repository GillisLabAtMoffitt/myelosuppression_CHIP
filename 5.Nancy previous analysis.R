####Conditional logistic regression: Myelosupp and CHIP only
res.clogit1 <- clogit(C_C ~ CHIP + strata(Strata), data = global_data, id=NGS_ID)
summ.clogit1 <- summary(res.clogit1)
summ.clogit1
exp(cbind(coef(res.clogit1), confint(res.clogit1)))

##Conditional: Myelosupp~CHIP, age, gender, race, MAX2
res.clogit2 <- clogit(C_C ~ CHIP + Age + Gender + Race + MAX2heme + strata(Strata), data = global_data)
summ.clogit2 <- summary(res.clogit2)
summ.clogit2
exp(cbind(coef(res.clogit2), confint(res.clogit2)))

##Conditional: Myelosupp~CHIP, all variables
res.clogit3 <- clogit(C_C ~ CHIP + Age + Gender + Race + Smoking + Mets + BaseANC + BaseHGB + BasePLT + BaseWBC + Prior_chemo + Prior_rad + MAX2heme + strata(Strata), data = global_data)
summ.clogit3 <- summary(res.clogit3)
summ.clogit3
exp(cbind(coef(res.clogit3), confint(res.clogit3)))

##Conditional: Myelosupp~CHIP, clinical variables -- in Moffitt job talk
res.clogit4 <- clogit(C_C ~ CHIP + Age + Mets + BaseANC + BaseHGB + BasePLT + BaseWBC + Prior_chemo + Prior_rad + MAX2heme + strata(Strata), data = global_data)
summ.clogit4 <- summary(res.clogit4)
summ.clogit4
exp(cbind(coef(res.clogit4), confint(res.clogit4)))

##Conditional: Myelosupp~CHIP, clinical variables -- in MSK job talk
res.clogit4 <- clogit(C_C ~ CHIP + Age + Gender + BaseANC + BaseHGB + BasePLT + BaseWBC + Prior_chemo + Prior_rad + MAX2heme + strata(Strata), data = global_data)
summ.clogit4 <- summary(res.clogit4)
summ.clogit4
exp(cbind(coef(res.clogit4), confint(res.clogit4)))

##Conditional: Myelosupp~CHIP, baseline labs, MAX2
res.clogit5 <- clogit(C_C ~ CHIP + BaseANC + BaseHGB + BasePLT + BaseWBC + MAX2heme + strata(Strata), data = global_data)
summ.clogit5 <- summary(res.clogit5)
summ.clogit5
exp(cbind(coef(res.clogit5), confint(res.clogit5)))



##Age by Case_Control 
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(Age, na.rm = TRUE),
    IQR = IQR(Age, na.rm = TRUE)
  )

res <- wilcox.test(Age ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Sex by Case_Control
table(global_data$Case_Control, global_data$Gender)
Case_Controlsex <- table(global_data$Case_Control, global_data$Gender)
summary(Case_Controlsex)
#fisher.test(table (global_data$Case_Control, global_data$Gender))

##Race by Case_Control
table(global_data$Case_Control, global_data$Race)
Case_Controlrace <- table(global_data$Case_Control, global_data$Race)
# summary(Case_Controlrace)
fisher.test(table (global_data$Case_Control, global_data$Race))

##Smoking by Case_Control 
table(global_data$Case_Control, global_data$Smoking)
Case_ControlSmoking <- table(global_data$Case_Control, global_data$Smoking)
summary(Case_ControlSmoking)
# fisher.test(table (global_data$Case_Control, global_data$Smoking))

##Metastasis by Case_Control
table(global_data$Case_Control, global_data$Mets)
Case_ControlMets <- table(global_data$Case_Control, global_data$Mets)
summary(Case_ControlMets)
# fisher.test(table (global_data$Case_Control, global_data$Mets))

##Prior Chemotherapy by Case_Control
table(global_data$Case_Control, global_data$Prior_chemo)
Case_Controlchemo <- table(global_data$Case_Control, global_data$Prior_chemo)
summary(Case_Controlchemo)
# fisher.test(table (global_data$Case_Control, global_data$Prior_chemo))

##Prior Radiation by Case_Control
table(global_data$Case_Control, global_data$Prior_rad)
Case_Controlrad <- table(global_data$Case_Control, global_data$Prior_rad)
summary(Case_Controlrad)
# fisher.test(table (global_data$Case_Control, global_data$Prior_rad))

##MAX2-heme by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(MAX2heme, na.rm = TRUE),
    IQR = IQR(MAX2heme, na.rm = TRUE)
  )
res <- wilcox.test(MAX2heme ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline ANC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseANC, na.rm = TRUE),
    IQR = IQR(BaseANC, na.rm = TRUE)
  )
res <- wilcox.test(BaseANC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline HGB by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseHGB, na.rm = TRUE),
    IQR = IQR(BaseHGB, na.rm = TRUE)
  )
res <- wilcox.test(BaseHGB ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline PLT by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BasePLT, na.rm = TRUE),
    IQR = IQR(BasePLT, na.rm = TRUE)
  )
res <- wilcox.test(BasePLT ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline WBC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseWBC, na.rm = TRUE),
    IQR = IQR(BaseWBC, na.rm = TRUE)
  )
res <- wilcox.test(BaseWBC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in ANC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeANC, na.rm = TRUE),
    IQR = IQR(ChangeANC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeANC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in HGB by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeHGB, na.rm = TRUE),
    IQR = IQR(ChangeHGB, na.rm = TRUE)
  )
res <- wilcox.test(ChangeHGB ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in PLT by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangePLT, na.rm = TRUE),
    IQR = IQR(ChangePLT, na.rm = TRUE)
  )
res <- wilcox.test(ChangePLT ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in WBC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeWBC, na.rm = TRUE),
    IQR = IQR(ChangeWBC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeWBC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Neutopenia by Case_Control
table(global_data$Case_Control, global_data$Neutro)
Case_ControlNeutro <- table(global_data$Case_Control, global_data$Neutro)
summary(Case_ControlNeutro)
# fisher.test(table (global_data$Case_Control, global_data$Neutro))

##Anemia by Case_Control
table(global_data$Case_Control, global_data$Anemia)
Case_ControlAnemia <- table(global_data$Case_Control, global_data$Anemia)
# summary(Case_ControlAnemia)
fisher.test(table (global_data$Case_Control, global_data$Anemia))

##Thrombocytopenia by Case_Control
table(global_data$Case_Control, global_data$Thrombo)
Case_ControlThrombo <- table(global_data$Case_Control, global_data$Thrombo)
# summary(Case_ControlThrombo)
fisher.test(table (global_data$Case_Control, global_data$Thrombo))

##Leukopenia by Case_Control
table(global_data$Case_Control, global_data$Leuko)
Case_ControlLeuko <- table(global_data$Case_Control, global_data$Leuko)
summary(Case_ControlLeuko)
# fisher.test(table (global_data$Case_Control, global_data$Leuko))





#### global_data only - By CHIP status----


##Age by CHIP 
global_data %>% group_by(CHIP) %>%
  summarise(
    count = n(),
    median = median(Age, na.rm = TRUE),
    IQR = IQR(Age, na.rm = TRUE)
  )

res <- wilcox.test(Age ~ CHIP, data = global_data,
                   exact = FALSE)
res

##Sex by CHIP
table(global_data$CHIP, global_data$Gender)
CHIPsex <- table(global_data$CHIP, global_data$Gender)
summary(CHIPsex)
#fisher.test(table (global_data$CHIP, global_data$Gender))

##Race by CHIP
table(global_data$CHIP, global_data$Race)
CHIPrace <- table(global_data$CHIP, global_data$Race)
# summary(CHIPrace)
fisher.test(table (global_data$CHIP, global_data$Race))

##Smoking by CHIP 
table(global_data$CHIP, global_data$Smoking)
CHIPSmoking <- table(global_data$CHIP, global_data$Smoking)
summary(CHIPSmoking)
# fisher.test(table (global_data$CHIP, global_data$Smoking))


##Metastasis by CHIP
table(global_data$CHIP, global_data$Mets)
CHIPMets <- table(global_data$CHIP, global_data$Mets)
summary(CHIPMets)
# fisher.test(table (global_data$CHIP, global_data$Mets))

##Prior Chemotherapy by CHIP
table(global_data$CHIP, global_data$Prior_chemo)
CHIPchemo <- table(global_data$CHIP, global_data$Prior_chemo)
summary(CHIPchemo)
# fisher.test(table (global_data$CHIP, global_data$Prior_chemo))

##Prior Radiation by CHIP
table(global_data$CHIP, global_data$Prior_rad)
CHIPrad <- table(global_data$CHIP, global_data$Prior_rad)
summary(CHIPrad)
# fisher.test(table (global_data$CHIP, global_data$Prior_rad))


##Baseline ANC by CHIP
global_data %>% group_by(CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseANC, na.rm = TRUE),
    IQR = IQR(BaseANC, na.rm = TRUE)
  )
res <- wilcox.test(BaseANC ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Baseline HGB by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseHGB, na.rm = TRUE),
    IQR = IQR(BaseHGB, na.rm = TRUE)
  )
res <- wilcox.test(BaseHGB ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Baseline PLT by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BasePLT, na.rm = TRUE),
    IQR = IQR(BasePLT, na.rm = TRUE)
  )
res <- wilcox.test(BasePLT ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Baseline WBC by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseWBC, na.rm = TRUE),
    IQR = IQR(BaseWBC, na.rm = TRUE)
  )
res <- wilcox.test(BaseWBC ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Change in ANC by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeANC, na.rm = TRUE),
    IQR = IQR(ChangeANC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeANC ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Change in HGB by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeHGB, na.rm = TRUE),
    IQR = IQR(ChangeHGB, na.rm = TRUE)
  )
res <- wilcox.test(ChangeHGB ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Change in PLT by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangePLT, na.rm = TRUE),
    IQR = IQR(ChangePLT, na.rm = TRUE)
  )
res <- wilcox.test(ChangePLT ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Change in WBC by CHIP
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeWBC, na.rm = TRUE),
    IQR = IQR(ChangeWBC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeWBC ~ CHIP, data = global_data,
                   exact = FALSE)
res


##Neutopenia by CHIP
table(global_data$CHIP, global_data$Neutro)
CHIPNeutro <- table(global_data$CHIP, global_data$Neutro)
summary(CHIPNeutro)
# fisher.test(table (global_data$CHIP, global_data$Neutro))


##Anemia by CHIP
table(global_data$CHIP, global_data$Anemia)
CHIPAnemia <- table(global_data$CHIP, global_data$Anemia)
# summary(CHIPAnemia)
fisher.test(table (global_data$CHIP, global_data$Anemia))


##Thrombocytopenia by CHIP
table(global_data$CHIP, global_data$Thrombo)
CHIPThrombo <- table(global_data$CHIP, global_data$Thrombo)
# summary(CHIPThrombo)
fisher.test(table (global_data$CHIP, global_data$Thrombo))

##Leukopenia by CHIP
table(global_data$CHIP, global_data$Leuko)
CHIPLeuko <- table(global_data$CHIP, global_data$Leuko)
summary(CHIPLeuko)
# fisher.test(table (global_data$CHIP, global_data$Leuko))




####Combined M4M and UNC cohort----

#Association between myelopsuppression and CHIP 
table(global_data$Case_Control, global_data$CHIP)
CHIPmyelo <- table(global_data$Case_Control, global_data$CHIP)
summary(CHIPmyelo)
fisher.test(table (global_data$Case_Control, global_data$CHIP))

res.logit1b <- glm(Case_Control ~ CHIP, data = global_data, family = "binomial")
summary(res.logit1b)$coef
exp(cbind(coef(res.logit1b), confint(res.logit1b)))

##Adjusted - myelosuppression and CHIP -- Moffitt job talk
CHIP_myelo_adj <-glm(Case_Control ~ CHIP + Age + Mets + BaseANC + BaseHGB + BasePLT + BaseWBC + Prior_chemo + Prior_rad + MAX2heme, data = global_data, family = binomial(link='logit'))
summary(CHIP_myelo_adj)
exp(cbind(coef(CHIP_myelo_adj), confint(CHIP_myelo_adj)))

##Adjusted - myelosuppression and CHIP -- MSK job talk
CHIP_myelo_adj <-glm(Case_Control ~ CHIP + Age + Gender + BaseANC + BaseHGB + BasePLT + BaseWBC + Prior_chemo + Prior_rad + MAX2heme, data = global_data, family = binomial(link='logit'))
summary(CHIP_myelo_adj)
exp(cbind(coef(CHIP_myelo_adj), confint(CHIP_myelo_adj)))

#Difference in baseline ANC between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseANC, na.rm = TRUE),
    IQR = IQR(BaseWBC, na.rm = TRUE) # WBC or ANC----
  )

library(ggpubr)
ggboxplot(global_data, x = "CHIP", y = "BaseANC", 
          color = "CHIP", palette = c("#00AFBB", "#E7B800"),
          ylab = "BaseANC", xlab = "CHIP status")

res <- wilcox.test(BaseANC ~ CHIP, data = global_data,
                   exact = FALSE)
res

#Difference in baseline HGB between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseHGB, na.rm = TRUE),
    IQR = IQR(BaseHGB, na.rm = TRUE)
  )

res <- wilcox.test(BaseHGB ~ CHIP, data = global_data,
                   exact = FALSE)
res

#Difference in baseline PLT between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BasePLT, na.rm = TRUE),
    IQR = IQR(BasePLT, na.rm = TRUE)
  )

ggboxplot(global_data, x = "CHIP", y = "BasePLT",
          color = "CHIP", palette = c("#00AFBB", "#E7B800"),
          ylab = "BaseANC", xlab = "CHIP status")

res <- wilcox.test(BasePLT ~ CHIP, data = global_data,
                   exact = FALSE)
res

#Difference in baseline WBC between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(BaseWBC, na.rm = TRUE),
    IQR = IQR(BaseWBC, na.rm = TRUE)
  )

res <- wilcox.test(BaseWBC ~ CHIP, data = global_data,
                   exact = FALSE)
res

#Difference in change in ANC between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeANC, na.rm = TRUE),
    IQR = IQR(ChangeANC, na.rm = TRUE)
  )

res <- wilcox.test(ChangeANC ~ CHIP, data = global_data,
                   exact = FALSE)
res

# #Difference in change in ANC between CHPD+ and CHPD-
# library(dplyr)
# group_by(global_data, CHPD) %>%
#   summarise(
#     count = n(),
#     median = median(ChangeANC, na.rm = TRUE),
#     IQR = IQR(ChangeANC, na.rm = TRUE)
#   )
# 
# res <- wilcox.test(ChangeANC ~ CHPD, data = global_data,
#                    exact = FALSE)
# res

#Difference in change in HGB between CHIP+ and CHIP-
library(dplyr)
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeHGB, na.rm = TRUE),
    IQR = IQR(ChangeHGB, na.rm = TRUE)
  )

res <- wilcox.test(ChangeHGB ~ CHIP, data = global_data,
                   exact = FALSE)
res

# #Difference in change in HGB between CHPD+ and CHPD-
# group_by(global_data, CHPD) %>%
#   summarise(
#     count = n(),
#     median = median(ChangeHGB, na.rm = TRUE),
#     IQR = IQR(ChangeHGB, na.rm = TRUE)
#   )
# 
# res <- wilcox.test(ChangeHGB ~ CHPD, data = global_data,
#                    exact = FALSE)
# res

#Difference in change in PLT between CHIP+ and CHIP-
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangePLT, na.rm = TRUE),
    IQR = IQR(ChangePLT, na.rm = TRUE)
  )

res <- wilcox.test(ChangePLT ~ CHIP, data = global_data,
                   exact = FALSE)
res

# #Difference in change in PLT between CHPD+ and CHPD-
# group_by(global_data, CHPD) %>%
#   summarise(
#     count = n(),
#     median = median(ChangePLT, na.rm = TRUE),
#     IQR = IQR(ChangePLT, na.rm = TRUE)
#   )
# 
# res <- wilcox.test(ChangePLT ~ CHPD, data = global_data,
#                    exact = FALSE)
# res

#Difference in change in WBC between CHIP+ and CHIP-
library(dplyr)
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(ChangeWBC, na.rm = TRUE),
    IQR = IQR(ChangeWBC, na.rm = TRUE)
  )

res <- wilcox.test(ChangeWBC ~ CHIP, data = global_data,
                   exact = FALSE)
res

# #Difference in change in WBC between CHPD+ and CHPD-
# library(dplyr)
# group_by(global_data, CHPD) %>%
#   summarise(
#     count = n(),
#     median = median(ChangeWBC, na.rm = TRUE),
#     IQR = IQR(ChangeWBC, na.rm = TRUE)
#   )
# 
# res <- wilcox.test(ChangeWBC ~ CHPD, data = global_data,
#                    exact = FALSE)
# res

#Difference in AGE between CHIP+ and CHIP-
library(dplyr)
group_by(global_data, CHIP) %>%
  summarise(
    count = n(),
    median = median(Age, na.rm = TRUE),
    IQR = IQR(Age, na.rm = TRUE)
  )

res <- wilcox.test(Age ~ CHIP, data = global_data,
                   exact = FALSE)
res

# #Difference in change in PLT between CHPD+ and CHPD-
# library(dplyr)
# group_by(global_data, CHPD) %>%
#   summarise(
#     count = n(),
#     median = median(Age, na.rm = TRUE),
#     IQR = IQR(Age, na.rm = TRUE)
#   )
# 
# res <- wilcox.test(Age ~ CHPD, data = global_data,
#                    exact = FALSE)
# res

#####Fisher's Exact Tests/Chi-sq for Categorical Variables

##Neutropenia
table(global_data$CHIP, global_data$Neutro)
CHIPneutro <- table(global_data$CHIP, global_data$Neutro)
summary(CHIPneutro)
#fisher.test(table (global_data$CHIP, global_data$Neutro))

##Anemia
table(global_data$CHIP, global_data$Anemia)
CHIPanemia <- table(global_data$CHIP, global_data$Anemia)
fisher.test(table (global_data$CHIP, global_data$Anemia))

##Thrombocytopenia
table(global_data$CHIP, global_data$Thrombo)
CHIPthrombo <- table(global_data$CHIP, global_data$Thrombo)
fisher.test(table (global_data$CHIP, global_data$Thrombo))

##Leukopenia
table(global_data$CHIP, global_data$Leuko)
CHIPleuko <- table(global_data$CHIP, global_data$Leuko)
summary(CHIPleuko)
#fisher.test(table (global_data$CHIP, global_data$Leuko))

##Metastasis
table(global_data$CHIP, global_data$Mets)
CHIPmet <- table(global_data$CHIP, global_data$Mets)
summary(CHIPmet)
# fisher.test(table (global_data$CHIP, global_data$Met))

res.logit1c <- glm(CHIP ~ Mets, data = global_data, family = "binomial")
summary(res.logit1c)$coef
exp(cbind(coef(res.logit1c), confint(res.logit1c)))

##Adjusted - CHIP and mets
CHIP_myelo_adj <-glm(Mets ~ CHIP + Age + Smoking + Prior_chemo + Prior_rad, data = global_data, family = binomial(link='logit'))
summary(CHIP_myelo_adj)
exp(cbind(coef(CHIP_myelo_adj), confint(CHIP_myelo_adj)))


##Gender  
table(global_data$CHIP, global_data$Gender)
CHIPGender <- table(global_data$CHIP, global_data$Gender)
summary(CHIPGender)
# fisher.test(table (global_data$CHIP, global_data$Gender))

##Race  
table(global_data$CHIP, global_data$Race)
CHIPRace <- table(global_data$CHIP, global_data$Race)
# summary(CHIPRace)
fisher.test(table (global_data$CHIP, global_data$Race))

##Smoking  
table(global_data$CHIP, global_data$Smoking)
CHIPSmoking <- table(global_data$CHIP, global_data$Smoking)
summary(CHIPSmoking)
# fisher.test(table (global_data$CHIP, global_data$Smoking))

##Prior Chemo
table(global_data$CHIP, global_data$Prior_chemo)
CHIPchemo <- table(global_data$CHIP, global_data$Prior_chemo)
summary(CHIPchemo)
# fisher.test(table (global_data$CHIP, global_data$Prior_chemo))

##Prior Radiation
table(global_data$CHIP, global_data$Prior_rad)
CHIPrad <- table(global_data$CHIP, global_data$Prior_rad)
summary(CHIPrad)
# fisher.test(table (global_data$CHIP, global_data$Prior_rad))


####Compare baseline b/t Case_Control - MCC + UNC


global_data$Case_Control <- factor (global_data$Case_Control, labels=c("Control", "Case"))

##Age by Case_Control 
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(Age, na.rm = TRUE),
    IQR = IQR(Age, na.rm = TRUE)
  )

res <- wilcox.test(Age ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Sex by Case_Control
table(global_data$Case_Control, global_data$Gender)
Case_Controlsex <- table(global_data$Case_Control, global_data$Gender)
summary(Case_Controlsex)
#fisher.test(table (global_data$Case_Control, global_data$Gender))

##Race by Case_Control
table(global_data$Case_Control, global_data$Race)
Case_Controlrace <- table(global_data$Case_Control, global_data$Race)
# summary(Case_Controlrace)
fisher.test(table (global_data$Case_Control, global_data$Race))

##Smoking by Case_Control 
table(global_data$Case_Control, global_data$Smoking)
Case_ControlSmoking <- table(global_data$Case_Control, global_data$Smoking)
summary(Case_ControlSmoking)
# fisher.test(table (global_data$Case_Control, global_data$Smoking))

##Metastasis by Case_Control
table(global_data$Case_Control, global_data$Mets)
Case_ControlMets <- table(global_data$Case_Control, global_data$Mets)
summary(Case_ControlMets)
# fisher.test(table (global_data$Case_Control, global_data$Mets))

##Prior Chemotherapy by Case_Control
table(global_data$Case_Control, global_data$Prior_chemo)
Case_Controlchemo <- table(global_data$Case_Control, global_data$Prior_chemo)
summary(Case_Controlchemo)
# fisher.test(table (global_data$Case_Control, global_data$Prior_chemo))

##Prior Radiation by Case_Control
table(global_data$Case_Control, global_data$Prior_rad)
Case_Controlrad <- table(global_data$Case_Control, global_data$Prior_rad)
summary(Case_Controlrad)
# fisher.test(table (global_data$Case_Control, global_data$Prior_rad))

##MAX2-heme by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(MAX2heme, na.rm = TRUE),
    IQR = IQR(MAX2heme, na.rm = TRUE)
  )
res <- wilcox.test(MAX2heme ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline ANC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseANC, na.rm = TRUE),
    IQR = IQR(BaseANC, na.rm = TRUE)
  )
res <- wilcox.test(BaseANC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline HGB by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseHGB, na.rm = TRUE),
    IQR = IQR(BaseHGB, na.rm = TRUE)
  )
res <- wilcox.test(BaseHGB ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline PLT by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BasePLT, na.rm = TRUE),
    IQR = IQR(BasePLT, na.rm = TRUE)
  )
res <- wilcox.test(BasePLT ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Baseline WBC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(BaseWBC, na.rm = TRUE),
    IQR = IQR(BaseWBC, na.rm = TRUE)
  )
res <- wilcox.test(BaseWBC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in ANC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeANC, na.rm = TRUE),
    IQR = IQR(ChangeANC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeANC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in HGB by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeHGB, na.rm = TRUE),
    IQR = IQR(ChangeHGB, na.rm = TRUE)
  )
res <- wilcox.test(ChangeHGB ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in PLT by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangePLT, na.rm = TRUE),
    IQR = IQR(ChangePLT, na.rm = TRUE)
  )
res <- wilcox.test(ChangePLT ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Change in WBC by Case_Control
group_by(global_data, Case_Control) %>%
  summarise(
    count = n(),
    median = median(ChangeWBC, na.rm = TRUE),
    IQR = IQR(ChangeWBC, na.rm = TRUE)
  )
res <- wilcox.test(ChangeWBC ~ Case_Control, data = global_data,
                   exact = FALSE)
res

##Neutopenia by Case_Control
table(global_data$Case_Control, global_data$Neutro)
Case_ControlNeutro <- table(global_data$Case_Control, global_data$Neutro)
summary(Case_ControlNeutro)
# fisher.test(table (global_data$Case_Control, global_data$Neutro))

##Anemia by Case_Control
table(global_data$Case_Control, global_data$Anemia)
Case_ControlAnemia <- table(global_data$Case_Control, global_data$Anemia)
# summary(Case_ControlAnemia)
fisher.test(table (global_data$Case_Control, global_data$Anemia))

##Thrombocytopenia by Case_Control
table(global_data$Case_Control, global_data$Thrombo)
Case_ControlThrombo <- table(global_data$Case_Control, global_data$Thrombo)
# summary(Case_ControlThrombo)
fisher.test(table (global_data$Case_Control, global_data$Thrombo))

##Leukopenia by Case_Control
table(global_data$Case_Control, global_data$Leuko)
Case_ControlLeuko <- table(global_data$Case_Control, global_data$Leuko)
summary(Case_ControlLeuko)
# fisher.test(table (global_data$Case_Control, global_data$Leuko))

