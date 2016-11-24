# ====================================================================
# author: Hank Hsu
# the first edition date: 2016/11/22
# the last modification date: 2016/11/22
# version: Rev01.20161122
# The following code is for the NSV rig criteria v2 and v3 comparison
# =====================================================================
library(dplyr)
library(tidyr) 
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)
library(stringr)

# read NSV v2, v3 die result
NSV_scoreV3_temp <- read.csv("c:/9_R_raw_temp/0_useful/Results_20161121_NSV criteria v2,v3.csv")
names(NSV_scoreV3_temp) <- c("WaferID","Reticle","ReticleDiePos","RampNSV_status","RampNSV_value",
                      "ReticleNSV_statusV2","ReticleNSV_valueV2","ReticleNSV_statusV3",
                      "ReticleNSV_valueV3")
NSV_scoreV3 <- NSV_scoreV3_temp %>% 
              mutate(DieID=paste(WaferID,"_R",Reticle,"_D",ReticleDiePos,sep="")) %>% 
              mutate(RampNSV_value_abs= abs(RampNSV_value)) %>% 
              mutate(ReticleNSV_valueV2_abs= abs(ReticleNSV_valueV2)) %>% 
              mutate(ReticleNSV_valueV3_abs= abs(ReticleNSV_valueV3)) %>% 
              mutate(DiePos.temp= paste ("0",ReticleDiePos,sep="")) %>%
              mutate(RetiDiePos=paste("RD",str_sub(DiePos.temp,-2,-1),sep="")) %>% 
              mutate(LotID= str_sub(WaferID,1,6)) %>% 
              mutate(LotGroup= str_sub(WaferID,1,3)) %>% 
              select(LotGroup,LotID,WaferID,DieID,RetiDiePos,
                     RampNSV_status,RampNSV_value,RampNSV_value_abs,
                     ReticleNSV_statusV2,ReticleNSV_valueV2,ReticleNSV_valueV2_abs,
                     ReticleNSV_statusV3,ReticleNSV_valueV3,ReticleNSV_valueV3_abs)
rm(NSV_scoreV3_temp)

# import origianl NSV_score file to get TSMC_FO_date
NSV_scoreV2 <- read.csv("c:/9_R_raw_temp/0_useful/2016WK46_NSVscore_all.csv")
TSMC_FO <- NSV_scoreV2 %>% 
            select(WaferID, TSMC_FO_yyWK,TSMC_FO_year,TSMC_FO_month) %>% 
            distinct(WaferID,.keep_all = TRUE)

# import VOQC die bin data
DieBinCAll <- read.csv("c:/9_R_raw_temp/0_useful/DieBinCAll_20161122.csv") %>% 
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
DieBinCAll_short <- DieBinCAll %>% 
                    select(-WaferID,-LotID,-WaferGroup)
NSV_scoreV3_VOQC <- left_join(NSV_scoreV3,DieBinCAll_short,by="DieID") %>% 
                    filter(!is.na(Bin.code))
                    
write.csv(NSV_scoreV3_VOQC,"c:/9_R_raw_temp/0_useful/2016WK48_Die_NSVV3_VOQC.csv")





