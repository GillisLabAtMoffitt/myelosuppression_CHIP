# Blood


ggplot(global_data, aes(x=Case_Control, y=ChangeANC, fill=CHIP))+
  geom_bar(stat = "summary_bin")




min(global_data$ChangeANC, na.rm = TRUE)
