# Linear and Conditionnal Logistic regression----

# 1.Simple check----

# 1.1.Case_Control predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(Cases_Controls ~ Neutro+Anemia+Thrombo+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Case_Control, global_data$Neutro))
summary(table(global_data$Case_Control, global_data$Anemia))
summary(table(global_data$Case_Control, global_data$Thrombo))
summary(table(global_data$Case_Control, global_data$Leuko))
# 1.2.Case_Control predicted using CBC----
# Case_Control predicted using Base
logistic <- clogit(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)



logistic <- clogit(Cases_Controls ~ Age+BaseANC+BaseHGB+BasePLT+BaseWBC+MAX2heme + strata(Strata),
                   data = global_M4M)
summary(logistic)
logistic <- clogit(Cases_Controls ~ Age+BaseANC+BaseHGB+BasePLT+BaseWBC+MAX2heme+Prior_rad + strata(Strata),
                   data = global_M4M)
summary(logistic)




# Case_Control predicted using Change
logistic <- clogit(Cases_Controls ~ ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control predicted using Base + Change
logistic <- clogit(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 1.3.Case_Control predicted using Age----
logistic <- clogit(Cases_Controls ~ Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Age,
                data = global_data, family = "binomial")
summary(logistic)

# 1.4.Case_Control predicted using Gender----
logistic <- clogit(Cases_Controls ~ Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 1.5.Case_Control predicted using Race----
logistic <- clogit(Cases_Controls ~ Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Race,
                data = global_data, family = "binomial")
summary(logistic)

# 1.6.Case_Control predicted using Ethnicity----
logistic <- clogit(Cases_Controls ~ Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 1.7.Case_Control predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(Cases_Controls ~ Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 1.8.Case_Control predicted using Smoking----
logistic <- clogit(Cases_Controls ~ Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 1.9.Case_Control predicted using Mets----
logistic <- clogit(Cases_Controls ~ Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 1.10.Case_Control predicted using Smoking+Mets----
logistic <- clogit(Cases_Controls ~ Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 1.11.Case_Control predicted using CHIP----
logistic <- clogit(Cases_Controls ~ CHIP + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP,
                data = global_data, family = "binomial")
summary(logistic)

# 1.13.Case_Control predicted using Prior_rad----
logistic <- clogit(Cases_Controls ~ Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 1.14.Case_Control predicted using Prior_chemo----
logistic <- clogit(Cases_Controls ~ Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 1.15.Case_Control predicted using Prior_rad+Prior_chemo----
logistic <- clogit(Cases_Controls ~ Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)





# 2.Case_Control predicted using CHIP +----
# 2.1.Case_Control predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(Cases_Controls ~ CHIP+Neutro+Anemia+Thrombo+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Case_Control, global_data$CHIP, global_data$Neutro))
summary(table(global_data$Case_Control, global_data$CHIP, global_data$Anemia))
summary(table(global_data$Case_Control, global_data$CHIP, global_data$Thrombo))
summary(table(global_data$Case_Control, global_data$CHIP, global_data$Leuko))
# 2.2.Case_Control predicted using CBC----
# Case_Control predicted using Base
logistic <- clogit(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control predicted using Change
logistic <- clogit(Cases_Controls ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Case_Control predicted using Base + Change
logistic <- clogit(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 2.3.Case_Control predicted using Age----
logistic <- clogit(Cases_Controls ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 2.4.Case_Control predicted using Gender----
logistic <- clogit(Cases_Controls ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 2.5.Case_Control predicted using Race----
logistic <- clogit(Cases_Controls ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 2.6.Case_Control predicted using Ethnicity----
logistic <- clogit(Cases_Controls ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 2.7.Case_Control predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(Cases_Controls ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 2.8.Case_Control predicted using Smoking----
logistic <- clogit(Cases_Controls ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 2.9.Case_Control predicted using Mets----
logistic <- clogit(Cases_Controls ~ CHIP+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 2.10.Case_Control predicted using Smoking+Mets----
logistic <- clogit(Cases_Controls ~ CHIP+Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 2.11.Case_Control predicted using Prior_rad----
logistic <- clogit(Cases_Controls ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 2.12.Case_Control predicted using Prior_chemo----
logistic <- clogit(Cases_Controls ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 2.13.Case_Control predicted using Prior_rad+Prior_chemo----
logistic <- clogit(Cases_Controls ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 2.14.All var----
logistic <- clogit(Cases_Controls ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                  Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)




# 3.More complexe models----
# 3.1.Case_Control predicted CHIP +----


# CHIP+CBC+, Age increase p
logistic <- clogit(Cases_Controls ~ CHIP+Age+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# CHIP+CBC+, Age+Race increse p even if better then Age alone
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# CHIP+CBC+, Age+Race+Ethnicity
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+Ethnicity+
                     BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+Ethnicity+
                  BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# CHIP+CBC+, Age+Race+Ethnicity+Smoking
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+
                     BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+
                  BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# CHIP+CBC+, Age+Race+Ethnicity+Smoking+Mets
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+
                     BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+
                  BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# CHIP+CBC+, Age+Race+Ethnicity+Smoking+Mets+Prior_rad
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+Prior_rad+
                     BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+Prior_rad+
                  BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
# CHIP+CBC+, Age+Race+Ethnicity+Smoking+Mets+Prior_rad+Prior_chemo
logistic <- clogit(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+Prior_rad+Prior_chemo+
                     BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                   data = global_data)
summary(logistic)
logistic <- glm(Cases_Controls ~ CHIP+Age+Race+Ethnicity+Smoking+Mets+Prior_rad+Prior_chemo+
                  BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)










# 4.Neutro predicted using CHIP +----
# 4.1.Neutro predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(neutro ~ CHIP+Anemia+Thrombo+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Neutro, global_data$CHIP, global_data$Anemia))
summary(table(global_data$Neutro, global_data$CHIP, global_data$Thrombo))
summary(table(global_data$Neutro, global_data$CHIP, global_data$Leuko))
# 4.2.Neutro predicted using CBC----
# Neutro predicted using Base
logistic <- clogit(neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Neutro predicted using Change
logistic <- clogit(neutro ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Neutro predicted using Base + Change
logistic <- clogit(neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 4.3.Neutro predicted using Age----
logistic <- clogit(neutro ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 4.4.Neutro predicted using Gender----
logistic <- clogit(neutro ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 4.5.Neutro predicted using Race----
logistic <- clogit(neutro ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 4.6.Neutro predicted using Ethnicity----
logistic <- clogit(neutro ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 4.7.Neutro predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(neutro ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 4.8.Neutro predicted using Smoking----
logistic <- clogit(neutro ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 4.9.Neutro predicted using Mets----
logistic <- clogit(neutro ~ CHIP+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 4.10.Neutro predicted using Smoking+Mets----
logistic <- clogit(neutro ~ CHIP+Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 4.11.Neutro predicted using Prior_rad----
logistic <- clogit(neutro ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 4.12.Neutro predicted using Prior_chemo----
logistic <- clogit(neutro ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 4.13.Neutro predicted using Prior_rad+Prior_chemo----
logistic <- clogit(neutro ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 4.14.All var----
logistic <- clogit(neutro ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(neutro ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                  Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)


# 5.Anemia predicted using CHIP +----
# 5.1.Anemia predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(anemia ~ CHIP+Neutro+Thrombo+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Anemia, global_data$CHIP, global_data$Neutro))
summary(table(global_data$Anemia, global_data$CHIP, global_data$Thrombo))
summary(table(global_data$Anemia, global_data$CHIP, global_data$Leuko))
# 5.2.Anemia predicted using CBC----
# Anemia predicted using Base
logistic <- clogit(anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Anemia predicted using Change
logistic <- clogit(anemia ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Anemia predicted using Base + Change
logistic <- clogit(anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 5.3.Anemia predicted using Age----
logistic <- clogit(anemia ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 5.4.Anemia predicted using Gender----
logistic <- clogit(anemia ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 5.5.Anemia predicted using Race----
logistic <- clogit(anemia ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 5.6.Anemia predicted using Ethnicity----
logistic <- clogit(anemia ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 5.7.Anemia predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(anemia ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 5.8.Anemia predicted using Smoking----
logistic <- clogit(anemia ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 5.9.Anemia predicted using Mets----
logistic <- clogit(anemia ~ CHIP+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 5.10.Anemia predicted using Smoking+Mets----
logistic <- clogit(anemia ~ CHIP+Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 5.11.Anemia predicted using Prior_rad----
logistic <- clogit(anemia ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 5.12.Anemia predicted using Prior_chemo----
logistic <- clogit(anemia ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 5.13.Anemia predicted using Prior_rad+Prior_chemo----
logistic <- clogit(anemia ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 5.14.All var----
logistic <- clogit(anemia ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(anemia ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                  Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)


# 6.Thrombo predicted using CHIP +----
# 6.1.Neutropredicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(thrombo ~ CHIP+Neutro+Anemia+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Thrombo, global_data$CHIP, global_data$Neutro))
summary(table(global_data$Thrombo, global_data$CHIP, global_data$Anemia))
summary(table(global_data$Thrombo, global_data$CHIP, global_data$Leuko))
# 6.2.Thrombo predicted using CBC----
# Thrombo predicted using Base
logistic <- clogit(thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Thrombo predicted using Change
logistic <- clogit(thrombo ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Thrombo predicted using Base + Change
logistic <- clogit(thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 6.3.Thrombo predicted using Age----
logistic <- clogit(thrombo ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 6.4.Thrombo predicted using Gender----
logistic <- clogit(thrombo ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 6.5.Thrombo predicted using Race----
logistic <- clogit(thrombo ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 6.6.Thrombo predicted using Ethnicity----
logistic <- clogit(thrombo ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 6.7.Thrombo predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(thrombo ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 6.8.Thrombo predicted using Smoking----
logistic <- clogit(thrombo ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 6.9.Thrombo predicted using Mets----
logistic <- clogit(thrombo ~ CHIP+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 6.10.Thrombo predicted using Smoking+Mets----
logistic <- clogit(thrombo ~ CHIP+Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 6.11.Thrombo predicted using Prior_rad----
logistic <- clogit(thrombo ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 6.12.Thrombo predicted using Prior_chemo----
logistic <- clogit(thrombo ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 6.13.Thrombo predicted using Prior_rad+Prior_chemo----
logistic <- clogit(thrombo ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 6.14.All var----
logistic <- clogit(thrombo ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(thrombo ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                  Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)


# 7.Leuko predicted using CHIP +----
# 7.1.Leuko predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(leuko ~ CHIP+Neutro+Thrombo+Anemia + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$Leuko, global_data$CHIP, global_data$Neutro))
summary(table(global_data$Leuko, global_data$CHIP, global_data$Anemia))
summary(table(global_data$Leuko, global_data$CHIP, global_data$Thrombo))
# 7.2.Leuko predicted using CBC----
# Leuko predicted using Base
logistic <- clogit(leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Leuko predicted using Change
logistic <- clogit(leuko ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Leuko predicted using Base + Change
logistic <- clogit(leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 7.3.Leuko predicted using Age----
logistic <- clogit(leuko ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 7.4.Leuko predicted using Gender----
logistic <- clogit(leuko ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 7.5.Leuko predicted using Race----
logistic <- clogit(leuko ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 7.6.Leuko predicted using Ethnicity----
logistic <- clogit(leuko ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 7.7.Leuko predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(leuko ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 7.8.Leuko predicted using Smoking----
logistic <- clogit(leuko ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 7.9.Leuko predicted using Mets----
logistic <- clogit(leuko ~ CHIP+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 7.10.Leuko predicted using Smoking+Mets----
logistic <- clogit(leuko ~ CHIP+Smoking+Mets + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Smoking+Mets,
                data = global_data, family = "binomial")
summary(logistic)

# 7.11.Leuko predicted using Prior_rad----
logistic <- clogit(leuko ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 7.12.Leuko predicted using Prior_chemo----
logistic <- clogit(leuko ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 7.13.Leuko predicted using Prior_rad+Prior_chemo----
logistic <- clogit(leuko ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 7.14.All var----
logistic <- clogit(leuko ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(leuko ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+Mets+
                     Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)



# 8.Mets predicted using CHIP +----
# 8.1.Mets predicted using Neutro+Anemia+Thrombo+Leuko----
logistic <- clogit(mets ~ CHIP+Neutro+Thrombo+Anemia+Leuko + strata(Strata),
                   data = global_data)
summary(logistic)
summary(table(global_data$mets, global_data$CHIP, global_data$Neutro))
summary(table(global_data$mets, global_data$CHIP, global_data$Anemia))
summary(table(global_data$mets, global_data$CHIP, global_data$Thrombo))
summary(table(global_data$mets, global_data$CHIP, global_data$Leuko))
# 8.2.Mets predicted using CBC----
# Mets predicted using Base
logistic <- clogit(mets ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Mets predicted using Change
logistic <- clogit(mets ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# Mets predicted using Base + Change
logistic <- clogit(mets ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC + strata(Strata),
                   data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)

# 8.3.Mets predicted using Age----
logistic <- clogit(mets ~ CHIP+Age + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Age,
                data = global_data, family = "binomial")
summary(logistic)

# 8.4.Mets predicted using Gender----
logistic <- clogit(mets ~ CHIP+Gender + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Gender,
                data = global_data, family = "binomial")
summary(logistic)

# 8.5.Mets predicted using Race----
logistic <- clogit(mets ~ CHIP+Race + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Race,
                data = global_data, family = "binomial")
summary(logistic)

# 8.6.Mets predicted using Ethnicity----
logistic <- clogit(mets ~ CHIP+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 8.7.Mets predicted using Age+Gender+Race+Ethnicity----
logistic <- clogit(mets ~ CHIP+Age+Gender+Race+Ethnicity + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Age+Gender+Race+Ethnicity,
                data = global_data, family = "binomial")
summary(logistic)

# 8.8.Mets predicted using Smoking----
logistic <- clogit(mets ~ CHIP+Smoking + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Smoking,
                data = global_data, family = "binomial")
summary(logistic)

# 8.9.Mets predicted using Prior_rad----
logistic <- clogit(mets ~ CHIP+Prior_rad + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)

# 8.10.Mets predicted using Prior_chemo----
logistic <- clogit(mets ~ CHIP+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 8.11.Mets predicted using Prior_rad+Prior_chemo----
logistic <- clogit(mets ~ CHIP+Prior_rad+Prior_chemo + strata(Strata), data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Prior_rad+Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

# 8.12.All var----
logistic <- clogit(mets ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+
                     Prior_chemo+Prior_rad, data = global_data)
summary(logistic)
logistic <- glm(mets ~ CHIP+Age+Gender+Race+Ethnicity+Smoking+
                  Prior_chemo+Prior_rad, data = global_data, family = "binomial")
summary(logistic)


