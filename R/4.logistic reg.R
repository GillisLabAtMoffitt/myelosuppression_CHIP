# Logistic regression----

# What is the most correlated with myelosupp?----
# Neutro, Anemia, Thrombo, Leuko?
summary(table(global_data$Case_Control, global_data$Neutro))
summary(table(global_data$Case_Control, global_data$Anemia))
summary(table(global_data$Case_Control, global_data$Thrombo))
summary(table(global_data$Case_Control, global_data$Leuko))

global_data %>% 
  select(Case_Control,
         Neutro, Anemia, Thrombo, Leuko
  ) %>% 
  tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
  add_p() %>%
  add_n()

# Case_Control predicted using Neutro+Anemia+Thrombo+Leuko
logistic <- glm(Cases_Controls ~ Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control predicted using CBC?----
# Case_Control predicted using Base, Change
logistic <- glm(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Case_Control predicted using Base
logistic <- glm(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Case_Control predicted using Change
logistic <- glm(Cases_Controls ~ ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control predicted using CHIP+----
# CHIP+Base, Change
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# CHIP+Base
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# CHIP+Change
logistic <- glm(Cases_Controls ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)


logistic <- glm(Cases_Controls ~ BaseANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ CHIP+BaseANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)

# Neutro predicted using CHIP----
# Base, Change
logistic <- glm(Neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Base
logistic <- glm(Neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Change
logistic <- glm(Neutro ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Leuko predicted using CHIP----
# Base, Change
logistic <- glm(Leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Base
logistic <- glm(Leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Change
logistic <- glm(Leuko ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Anemia predicted using CHIP----
# Base, Change
logistic <- glm(Anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Base
logistic <- glm(Anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Change
logistic <- glm(Anemia ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Thrombo predicted using CHIP----
# Base, Change
logistic <- glm(Thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Base
logistic <- glm(Thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)
# Change
logistic <- glm(Thrombo ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control Case_Control predicted using CHIP+----
# All var
logistic <- glm(Cases_Controls ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+Neutro+Anemia+Thrombo+Prior_chemo+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ Age+Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ Race+Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ Smoking+Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

logistic <- glm(Cases_Controls ~ Mets+Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)


logistic <- glm(Cases_Controls ~ Race+Smoking+Mets+Neutro+Anemia+Thrombo,
                data = global_data, family = "binomial")
summary(logistic)

# Rad----
logistic <- glm(Cases_Controls ~ Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ Race+Smoking+Mets+Neutro+Anemia+Thrombo+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)
# Chemo----
logistic <- glm(Cases_Controls ~ Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ Race+Smoking+Mets+Neutro+Anemia+Thrombo+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)





logistic <- glm(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC+MAX2heme,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)


##Adjusted - CHIP and mets
CHIP_myelo_adj <-glm(Mets ~ CHIP + Age + Smoking + Prior_chemo + Prior_rad, data = global_data, family = binomial(link='logit'))
summary(CHIP_myelo_adj)
exp(cbind(coef(CHIP_myelo_adj), confint(CHIP_myelo_adj)))

# Predict mets ~ adjusted by CHIP
logistic <- glm(Mets ~ CHIP+BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)







