# =====================================================
# Task: 2016WK44 NSV summary
# C:\0 My documents\Documents\Memjet\10 Integration\
#     90 Task force\00 Memjet\2016WK44 NSV summary
# =====================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)

NSVCD <- read.csv("c:/9_R_raw_temp/0_useful/2016WK42_NSVCD_all.csv")
NSV_score <- read.csv("c:/9_R_raw_temp/0_useful/2016WK42_NSVscore_all.csv")
NSV_Die_summary <- read.csv("c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS.csv")
DieBinCAll <- read.csv("c:/9_R_raw_temp/0_useful/2016WK42_VOQC_Die.csv")

CPData_NSV <- fread("c:/9_R_raw_temp/CPraw/CPrawData_die_NSV.csv")

CPData_NSV_Rs <- CPData_NSV %>% 
                  select(V2,V48,V49,V50,V51,V57,V58)

names(CPData_NSV_Rs) <- c("DieID","RS_left","RS_Mid","RS_right",
                          "RS_slop","RS_mean","RS_sd")

NSV_Die_summary_Rs <- left_join(NSV_Die_summary,CPData_NSV_Rs, by="DieID") %>% 
                      mutate(RS_LRbias=as.numeric(RS_left) - as.numeric(RS_right)) %>% 
                      filter(RS_left != "NULL")
write.csv(NSV_Die_summary_Rs,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS_Rs.csv")


NSV_Die_summary_Rs_Bin <- NSV_Die_summary_Rs %>% 
                          filter(Bin.code=="P" | Bin.code == "L")

write.csv(NSV_Die_summary_Rs_Bin,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS_Rs_BinPL.csv")




#==================================================================

NSV_Die_summary_2016 <- NSV_Die_summary %>% 
                        filter(TSMC_FO_year == "2016")

NSV_Die_summary_LotGroup <- NSV_Die_summary %>% 
                            filter(LotGroup == "CHT" | LotGroup == "CHP" |LotGroup == "CHX")
write.csv(NSV_Die_summary_LotGroup,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS_LotGroup.csv")

NSV_Die_summary_5Lot <- NSV_Die_summary %>% 
  filter(LotID == "CGF445" | LotID == "CGN398" |LotID == "CHK526"|
           LotID == "CHK529" |LotID == "CHT290")
write.csv(NSV_Die_summary_5Lot,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS_5Lot.csv")




