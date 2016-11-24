# ========================================================================
# author: Hank Hsu
# date: 2016/10/26
# version: Rev01.20161026
# The following code is to summarize the "Die" informaiton NSVCD, NSVscore, VOQC...  
# The raw data is from: NSVCD, NSV 
# =========================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)

# combine all statical NSVCD raw data
NSVCD_UG <- read.csv("c:/9_R_raw_temp/NSVCD/UGSI/All_summary_WK37.csv")
NSVCD_XT_1 <- read.csv("c:/9_R_raw_temp/NSVCD/Xintec/All_summary_WK37.csv")
NSVCD_XT_2 <- read.csv("c:/9_R_raw_temp/NSVCD/Xintec/All_summary_WK42.csv")
NSVCD_XT <- rbind(NSVCD_XT_1,NSVCD_XT_2)
NSVCD <- rbind(NSVCD_XT,NSVCD_UG)
write.csv(NSVCD,"c:/9_R_raw_temp/0_useful/2016WK42_NSVCD_all.csv")
rm(NSVCD_XT_1,NSVCD_XT_2,NSVCD_UG,NSVCD_XT)

# combine all NSV score data
NSV_score_UG <- read.csv("c:/9_R_raw_temp/NSVRawData/NSV_score_UG_WK33.csv")
NSV_score_XT <- read.csv("c:/9_R_raw_temp/NSVRawData/NSV_score_XT_WK41.csv")
NSV_score_temp <- rbind(NSV_score_XT,NSV_score_UG) %>% 
                    select(-contains("NSVDate")) %>% 
                    select(-VOQC_DieBin)
names(NSV_score_temp) <- c("LotID","WaferID","ReticleID","DiePos",
                      "DieID","RampNSV_status","RampNSV_value","ReticleNSV_status",
                      "ReticleNSV_value","TSMC_FO_date","TSMC_FO_year","TSMC_FO_month",
                      "TSMC_FO_week","TSMC_FO_yyWK")
NSV_score <- NSV_score_temp %>%
            mutate(RampNSV_value_abs= abs(RampNSV_value),
                   ReticleNSV_value_abs= abs(ReticleNSV_value)) %>% 
            mutate(DiePos.temp= paste ("0",DiePos,sep="")) %>%
            mutate(RetiDiePos=paste("RD",str_sub(DiePos.temp,-2,-1),sep="")) %>% 
            select(-DiePos.temp)
write.csv(NSV_score,"c:/9_R_raw_temp/0_useful/2016WK42_NSVscore_all.csv")
rm(NSV_score_UG,NSV_score_XT,NSV_score_temp)

NSV_score_short <- NSV_score %>%
                    select(DieID,RetiDiePos,RampNSV_status,RampNSV_value_abs,
                           ReticleNSV_status,ReticleNSV_value_abs,TSMC_FO_year,
                           TSMC_FO_month,TSMC_FO_yyWK)

TSMC_FO <- NSV_score_short %>% 
            mutate(WaferID=str_sub(DieID,1,9)) %>% 
            select(WaferID, TSMC_FO_yyWK) %>% 
            distinct(WaferID,.keep_all = TRUE)
                              

# import VOQC die bin data
DieBinCAll <- read.csv("c:/9_R_raw_temp/0_useful/DieBinCAll_20161026.csv") %>% 
              mutate(DieID= paste(WaferID,"_R",Reticle,"_D",Die.in.Reticle,sep="")) %>% 
              mutate(PHDiePos.temp= paste ("0",Die.on.PH,sep="")) %>%
              mutate(PHDiePos=paste("PHD",str_sub(PHDiePos.temp,-2,-1),sep="")) %>% 
              mutate(RetiDiePos.temp= paste ("0",Die.in.Reticle,sep="")) %>%
              mutate(RetiDiePos=paste("RD",str_sub(RetiDiePos.temp,-2,-1),sep="")) %>% 
              select(DieID,PHID,Bin.code,VOQC.Result,PHDiePos) %>% 
              mutate(LotID=str_sub(DieID,1,6)) %>% 
              mutate(WaferID=str_sub(DieID,1,9)) %>% 
              mutate(WaferGroup =str_sub(DieID,1,3)) %>% 
              left_join(TSMC_FO,by = "WaferID") %>%
              mutate(NSVscan = ifelse(!is.na(TSMC_FO_yyWK), "Yes","No"))

write.csv(DieBinCAll,"c:/9_R_raw_temp/0_useful/2016WK42_VOQC_Die.csv")

DieBinCAll_short <- DieBinCAll %>% 
                    select(-LotID,-WaferID,-WaferGroup,-TSMC_FO_yyWK,-NSVscan)
                    
# Combine data: NSVCD, NSV_score, VOQC die bin
NSV_Die_summary <- left_join(NSVCD, NSV_score_short, by="DieID") %>% 
                    left_join(DieBinCAll_short,by="DieID") %>% 
                    filter(Min_m!="FailToScan") %>% 
                    filter(RampNSV_status!="Failed To Scan") %>% 
                    mutate(LotGroup=str_sub(LotID,1,3)) %>% 
                    mutate(SlotID=str_sub(WaferID,-2,-1)) %>% 
                    mutate(EvenOdd= as.numeric(SlotID)) %>% 
                    mutate(EvenOdd=ifelse(EvenOdd %% 2 == "0", "Even","Odd"))

write.csv(NSV_Die_summary,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS.csv")
