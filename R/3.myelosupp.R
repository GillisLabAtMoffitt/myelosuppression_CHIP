# Look at CBC in Case_Control
global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                  "BaseHGB", "ChangeHGB",
                                                  "BasePLT", "ChangePLT",
                                                  "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=Case_Control, y=abs(value)))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)

# Is it better if separate with CHIP
global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                  "BaseHGB", "ChangeHGB",
                                                  "BasePLT", "ChangePLT",
                                                  "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=CHIP, y=abs(value), fill=Case_Control))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(aes(group = CHIP), size= 3)

p1 <- global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                        "BaseHGB", "ChangeHGB",
                                                        "BasePLT", "ChangePLT",
                                                        "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=Case_Control, y=abs(value)))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)
p2 <- global_data %>% gather("blood_subset", "value", c("BaseANC", "ChangeANC",
                                                        "BaseHGB", "ChangeHGB",
                                                        "BasePLT", "ChangePLT",
                                                        "BaseWBC", "ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  mutate(blood_subset= factor(blood_subset, levels = c("BaseANC", "ChangeANC",
                                                       "BaseHGB", "ChangeHGB",
                                                       "BasePLT", "ChangePLT",
                                                       "BaseWBC", "ChangeWBC"))) %>% 
  ggplot(aes(x=CHIP, y=abs(value), fill=Case_Control))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(aes(label = paste0("p = ", ..p.format..)), size= 3)
gridExtra::grid.arrange(p1, p2, ncol = 2, widths= c(4,6),
                        bottom = "Kruskal-Wallis")









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
  ggplot(aes(x=Case_Control, y=abs(value), fill=CHIP))+
  geom_boxplot()+
  facet_wrap(. ~ blood_subset, scales = "free",  ncol=2, strip.position = "right")+
  stat_compare_means(size= 3)

global_data %>% gather("blood_subset", "value", c("ChangeANC","ChangeHGB","ChangePLT","ChangeWBC")) %>% 
  select("NGS_ID", "blood_subset", "value", "CHIP", "Case_Control") %>% 
  ggplot(aes(x=CHIP, y=abs(value), fill=Case_Control))+
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


# Strata----
global_data %>% 
  ggpaired(x = "Case_Control", y = "BaseANC",
           id = "Strata",
              color = "Case_Control", 
              line.color = "gray", line.size = 0.4,
              facet.by = "CHIP", short.panel.labs = FALSE) +
  stat_compare_means(label = "p.format", paired = TRUE)












