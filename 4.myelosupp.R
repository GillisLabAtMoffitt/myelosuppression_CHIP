# Blood


ggplot(global_data, aes(x=Case_Control, y=ChangeANC, fill=CHIP))+
  geom_bar(stat = "summary_bin")

min(global_data$ChangeANC, na.rm = TRUE)

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

df2 <- data.frame(CHIP = rep("CHIP",4), blood_subset = factor(c("BaseANC", "ChangeANC",
                                                         "BaseHGB", "ChangeHGB",
                                                         "BasePLT", "ChangePLT",
                                                         "BaseWBC", "ChangeWBC")),
                  value = c(9.7,9.7,16,16, 450, 100, 10,10), Case_Control = rep("Cases",4))
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
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot") + 
  # geom_boxplot()+
  geom_point(data = df2, aes(x = CHIP, y = value, fill=Case_Control), colour = "white", alpha=0) +
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2)














