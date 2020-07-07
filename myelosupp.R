# Blood


ggplot(global_data, aes(x=Case_Control, y=ChangeANC, fill=CHIP))+
  geom_bar(stat = "summary_bin")

min(global_data$ChangeANC, na.rm = TRUE)



library(survival)

global_data$Case_Control <- factor (global_data$Case_Control, labels=c(0, 1))



####Conditional logistic regression: Myelosupp and CHIP only
res.clogit1 <- clogit(Case_Control ~ old_CHIP + strata(Strata), data = global_data, id=NGS_ID)
summ.clogit1 <- summary(res.clogit1)
summ.clogit1
exp(cbind(coef(res.clogit1), confint(res.clogit1)))
