# Blood


# boxplot
ggplot(global_data, aes(x=CHIP, y=BaseANC, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=BaseHGB, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=BasePLT, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=BaseWBC, fill=Case_Control))+
  geom_boxplot()


ggplot(global_data, aes(x=CHIP, y=ChangeANC, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=ChangeHGB, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=ChangePLT, fill=Case_Control))+
  geom_boxplot()
ggplot(global_data, aes(x=CHIP, y=ChangeWBC, fill=Case_Control))+
  geom_boxplot()

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

df2 <- data.frame(CHIP = rep("CHIP",4), blood_subset = factor(c("BaseANC", "ChangeANC",
                                                         "BaseHGB", "ChangeHGB",
                                                         "BasePLT", "ChangePLT",
                                                         "BaseWBC", "ChangeWBC")),
                  value = c(9.7,9.7,16,NA, NA, NA, 10,10), Case_Control = rep("Cases",4))
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
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = Case_Control))

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
  #geom_point(data = df2, aes(x = CHIP, y = value, fill=Case_Control), colour = "white", alpha=0) +
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = CHIP))

global_data %>% gather("blood_subset", "value", c("ChangeANC","ChangeHGB","ChangePLT","ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_bar(stat = "summary_bin")+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = CHIP), method = "anova", label.y= 0)
min(global_data$ChangeANC, na.rm = TRUE)
max(global_data$ChangeANC, na.rm = TRUE)
mean(global_data$ChangeANC, na.rm = TRUE)

global_data %>% gather("blood_subset", "value", c("BaseANC","BaseHGB","BasePLT","BaseWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  ggplot(aes(x=CHIP, y=value, fill=Case_Control))+
  geom_bar(stat = "summary_bin")+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)+
  stat_compare_means(aes(group = CHIP), method = "t.test", label.y= 0)+
  stat_compare_means(label.y= 5)






# Logistic regression----
# Case prediction by CHIP






# Strata----
global_data %>% 
  ggpaired(x = "Case_Control", y = "BaseANC",
           id = "Strata",
              color = "Case_Control", 
              line.color = "gray", line.size = 0.4,
              facet.by = "CHIP", short.panel.labs = FALSE) +
  stat_compare_means(label = "p.format", paired = TRUE)












