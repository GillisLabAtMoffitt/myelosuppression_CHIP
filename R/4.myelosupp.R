
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

logistic <- glm(C_C ~ Neutro+Anemia+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(C_C ~ Neutro+Thrombo+Leuko,
                data = global_data, family = "binomial")
summary(logistic)

# CBC?
logistic <- glm(C_C ~ CHIP+BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC+MAX2heme,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(C_C ~ BaseANC+BaseHGB+BasePLT+BaseWBC+ChangeANC+ChangeHGB+ChangePLT+ChangeWBC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(C_C ~ BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(C_C ~ CHIP+BaseANC+ChangeANC,
                data = global_data, family = "binomial")
summary(logistic)


global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                  "BaseHGB", "ChangeHGB",
                                                  "BasePLT", "ChangePLT",
                                                  "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=Case_Control, y=value))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)

# Is it better if separate with CHIP
df2 <- data.frame(CHIP = rep("CHIP",4), blood_subset = factor(c("BaseANC", "ChangeANC",
                                                                "BaseHGB", "ChangeHGB",
                                                                "BasePLT", "ChangePLT",
                                                                "BaseWBC", "ChangeWBC")),
                  value = c(10,10,16,NA, NA, NA, 10,10), Case_Control = rep("Cases",4))
global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                  "BaseHGB", "ChangeHGB",
                                                  "BasePLT", "ChangePLT",
                                                  "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_boxplot()+
  geom_point(data = df2, aes(x = CHIP, y = value, fill=Case_Control), colour = "white", alpha=0) +
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(aes(group = CHIP), size= 3)


# tbl1 <- global_data %>% 
#   filter(CHIP == "CHIP") %>% 
#   select(Case_Control,
#          BaseANC
#   ) %>% 
#   tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl2 <- global_data %>% 
#   filter(CHIP == "No CHIP") %>% 
#   select(Case_Control,
#          BaseANC
#   ) %>% 
#   tbl_summary(by= Case_Control, statistic = all_continuous() ~ "{median} ({sd})") %>% 
#   add_p() %>%
#   add_n()
# tbl_merge(list(tbl1, tbl2),
#                  tab_spanner = c("**CHIP**", "**no CHIP**"))  %>% as_gt()
# 
p1 <- global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                        "BaseHGB", "ChangeHGB",
                                                        "BasePLT", "ChangePLT",
                                                        "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=Case_Control, y=value))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)
df2 <- data.frame(CHIP = rep("CHIP",4), blood_subset = factor(c("BaseANC", "ChangeANC",
                                                                "BaseHGB", "ChangeHGB",
                                                                "BasePLT", "ChangePLT",
                                                                "BaseWBC", "ChangeWBC")),
                  value = c(10,10,16,NA, NA, NA, 10,10), Case_Control = rep("Cases",4))
p2 <- global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                        "BaseHGB", "ChangeHGB",
                                                        "BasePLT", "ChangePLT",
                                                        "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_boxplot()+
  geom_point(data = df2, aes(x = CHIP, y = value, fill=Case_Control), colour = "white", alpha=0) +
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)
gridExtra::grid.arrange(p1, p2, ncol = 2, widths= c(4,6))

# What if we compare Case_Control
global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                  "BaseHGB", "ChangeHGB",
                                                  "BasePLT", "ChangePLT",
                                                  "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=Case_Control, y=value, fill=CHIP))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)

global_data %>% gather("blood_subset", "value", c("ChangeANC","ChangeHGB","ChangePLT","ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_bar(stat = "summary_bin")+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = Case_Control), label.y= 0, size= 3)
min(global_data$ChangeANC, na.rm = TRUE)
max(global_data$ChangeANC, na.rm = TRUE)
mean(global_data$ChangeANC, na.rm = TRUE)

global_data %>% gather("blood_subset", "value", c("BaseANC","BaseHGB","BasePLT","BaseWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_bar(stat = "summary_bin")+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = Case_Control), label.y= 0, size= 3)+
  stat_compare_means(label.y= 10, size= 3)






# Logistic regression----
# Case prediction by CHIP

# All var
logistic <- glm(C_C ~ CHIP+Gender+Race+Ethnicity+Smoking+Mets+Neutro+Anemia+Thrombo+Prior_chemo+Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)



logistic <- glm(C_C ~ Prior_rad,
                data = global_data, family = "binomial")
summary(logistic)
logistic <- glm(C_C ~ Prior_chemo,
                data = global_data, family = "binomial")
summary(logistic)

##Adjusted - CHIP and mets
CHIP_myelo_adj <-glm(Mets ~ CHIP + Age + Smoking + Prior_chemo + Prior_rad, data = global_data, family = binomial(link='logit'))
summary(CHIP_myelo_adj)
exp(cbind(coef(CHIP_myelo_adj), confint(CHIP_myelo_adj)))



# Strata----
global_data %>% 
  ggpaired(x = "Case_Control", y = "BaseANC",
           id = "Strata",
              color = "Case_Control", 
              line.color = "gray", line.size = 0.4,
              facet.by = "CHIP", short.panel.labs = FALSE) +
  stat_compare_means(label = "p.format", paired = TRUE)












