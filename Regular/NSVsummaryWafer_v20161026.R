# ========================================================================
# author: Hank Hsu
# date: 2016/10/26
# version: Rev01.20161026
# The following code is to summarize the "Wafer" informaiton NSVCD, NSVscore, VOQC...  
# The raw data is from: NSVCD, NSV 
# =========================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)

# combine all NSV Wafer pass rate data
NSV_wafer_UG <- read.csv("c:/9_R_raw_temp/NSVRawData/NSV_summary_UG_WK33.csv")
NSV_wafer_XT <- read.csv("c:/9_R_raw_temp/NSVRawData/NSV_summary_XT_WK41.csv")

NSV_Wafer <- rbind(NSV_wafer_XT,NSV_wafer_UG) %>% 
                    mutate(SlotID=str_sub(WaferID,-2,-1),
                           EvenOdd=str_sub(WaferID,-2,-1))

NSV_Wafer$EvenOdd <- as.numeric(NSV_Wafer$EvenOdd)
NSV_Wafer$EvenOdd <- NSV_Wafer$EvenOdd %% 2
NSV_Wafer$EvenOdd[NSV_Wafer$EvenOdd == "1"] = "Odd" 
NSV_Wafer$EvenOdd[NSV_Wafer$EvenOdd == "0"] = "Even" 

write.csv(NSV_Wafer,"c:/9_R_raw_temp/0_useful/2016WK42_NSV_wafer_all.csv")
rm(NSV_wafer_UG,NSV_wafer_XT)

NSV60_Wafer <- NSV_Wafer %>% 
                filter(Total.scanned.dies>30 & Total.scanned.dies <65)
write.csv(NSV60_Wafer,"c:/9_R_raw_temp/0_useful/2016WK42_NSV60_wafer_all.csv")


# import the VOQC_wafer data
VOQC_Wafer <- read.csv("c:/9_R_raw_temp/0_useful/WaferSlot_Summary_20161026.csv") %>% 
                select(waferbatch,waferID,Total.Dies,VOQC.Pass,VOQC.Fail,
                       VOQC.Yield.,Pass.ratio.,Ramping.R.ratio.,Ramping.L.ratio.,
                       Dark.Band.ratio.,White.Band.ratio.,Streak.ratio.,Others.ratio.,
                       Center,X2nd,N.S,X4.Corner)
names(VOQC_Wafer) <- c("LotID","WaferID","TotalDies","VOQCPassDie","VOQCFailDie",
                       "VOQCYield","DieBin_P","Failrate_R","Failrate_L",
                       "Failrate_D","Failrate_W","Failrate_S","Failrate_O",
                       "VOQCYield_C","VOQCYield_2nd","VOQCYield_NS","VOQCYield_4C")

VOQC_Wafer<- VOQC_Wafer %>% 
              mutate(DieBin_Fail=(Failrate_R+Failrate_L+Failrate_D+
                                    Failrate_W+Failrate_S+Failrate_O)) %>% 
              mutate(Check=(DieBin_P+DieBin_Fail))

write.csv(VOQC_Wafer,"c:/9_R_raw_temp/0_useful/2016WK42_VOQC_Wafer.csv")

VOQC_Wafer_strange <- VOQC_Wafer %>% 
                      filter(Check < 0.9)
write.csv(VOQC_Wafer_strange,"c:/9_R_raw_temp/0_useful/2016WK42_VOQC_Wafer_strange.csv",
          row.names = FALSE)


