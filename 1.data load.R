# Import library
library(tidyverse)


#######################################################################################  I  ### Load data
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_myelosuppression_M4M 2018")
#-----------------------------------------------------------------------------------------------------------------
data <-
  read_csv(paste0(path, "/CHIP_CRASH_data_for_stats_v05.csv"))


#######################################################################################  II  ### Load mining


