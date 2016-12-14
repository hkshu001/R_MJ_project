# =======================================================================
# author: Hank Hsu
# the first edition date: 2016/12/02
# the last modification date: 2016/12/02
# version: Rev01.20161202
# The following code is for merge all information by die level:
# DieID, PHID, WaferID, LotID...etc.
# =======================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)
library(stringr)
library(lubridate)

# Read necessary files ================================================================
WaferPos <- read.csv("c:/9_R_raw_temp/00_useful/WaferPositionList.csv")

# Import XT standard NSV score to get TSMC_FO =========================================
NSV_score_XT_temp <- read_excel("C:/9_R_raw_temp/30_NSV/Xintec NSV scanning result of W48_rev1.xlsx",
                                sheet="Results") %>% 
                      select(-contains("NSVDate")) %>% 
                      select(-VOQC_DieBin) %>% 
                      mutate(NSVsite="Xintec")

names(NSV_score_XT_temp) <- c("LotID","WaferID","Reticle","DieInReticle",
                              "DieID","RampNSV_status","RampNSV_value","ReticleNSV_status",
                              "ReticleNSV_value","TSMC_FO_date","TSMC_FO_year","TSMC_FO_month",
                              "TSMC_FO_week","TSMC_FO_yyWK","NSVsite")

NSV_score_XT <- NSV_score_XT_temp %>%
                mutate(RampNSV_value_abs= abs(RampNSV_value),
                       ReticleNSV_value_abs= abs(ReticleNSV_value)) %>% 
                mutate(DieInReticle_temp= paste ("0",DieInReticle,sep="")) %>%
                mutate(DieRetiPos=paste("RD",str_sub(DieInReticle_temp,-2,-1),sep="")) %>% 
                mutate(ReticleNo=paste("R",Reticle,sep="")) %>%
                left_join(WaferPos, by="ReticleNo") %>% 
                select(-DieInReticle_temp)

TSMC_FO <- NSV_score_XT %>% 
        select(WaferID, TSMC_FO_yyWK) %>% 
        distinct(WaferID,.keep_all = TRUE)

rm(NSV_score_XT_temp,NSV_score_XT)

# Import NSV v2, v3 die result================================================================
NSV_scoreV3_temp <- fread("c:/9_R_raw_temp/30_NSV/Results_20161121_NSV criteria v2,v3.csv")
names(NSV_scoreV3_temp) <- c("WaferID","Reticle","DieInReticle","RampNSV_status","RampNSV_value",
                             "ReticleNSV_statusV2","ReticleNSV_valueV2","ReticleNSV_statusV3",
                             "ReticleNSV_valueV3")
NSV_scoreV3_XT <- NSV_scoreV3_temp %>% 
                  mutate(DieID= paste(WaferID,"_R",Reticle,"_D",DieInReticle,sep="")) %>% 
                  mutate(RampNSV_value_abs= abs(RampNSV_value)) %>% 
                  mutate(ReticleNSV_valueV2_abs= abs(ReticleNSV_valueV2)) %>% 
                  mutate(ReticleNSV_valueV3_abs= abs(ReticleNSV_valueV3)) %>% 
                  mutate(DieRetiPos_temp= paste("0",DieInReticle,sep="")) %>%
                  mutate(DieRetiPos=paste("RD",str_sub(DieRetiPos_temp,-2,-1),sep="")) %>% 
                  mutate(LotID= str_sub(WaferID,1,6)) %>% 
                  mutate(LotGroup= str_sub(WaferID,1,3)) %>% 
                  select(LotGroup,LotID,WaferID,DieID,DieRetiPos,
                         RampNSV_status,RampNSV_value,RampNSV_value_abs,
                         ReticleNSV_statusV2,ReticleNSV_valueV2,ReticleNSV_valueV2_abs,
                         ReticleNSV_statusV3,ReticleNSV_valueV3,ReticleNSV_valueV3_abs) %>% 
                  mutate(ReticleNo= str_sub(DieID,11,13)) %>% 
                  left_join(WaferPos, by="ReticleNo") %>% 
                  left_join(TSMC_FO, by="WaferID")

rm(NSV_scoreV3_temp)


# Import CP MEMS RS data =============================================================
CP_MEMS_temp <- fread("c:/9_R_raw_temp/30_SQL_CP/20161121_CPMEMS.csv", header = FALSE)

CP_MEMS_temp2 <- CP_MEMS_temp %>% 
                  select(V2,V9,V10,V11,V12,V13,V19,V20)
names(CP_MEMS_temp2) <- c("DieID","CPresult","Rs_L","Rs_mid","Rs_R","RS_slop","RS_mean","RS_sd")
CP_MEMS <- CP_MEMS_temp2 %>% 
            mutate(RsBias_Lm= as.numeric(Rs_L)-as.numeric(Rs_mid)) %>% 
            mutate(RsBias_LR = as.numeric(Rs_L) - as.numeric(Rs_R)) %>% 
            mutate(RsBias_mR = as.numeric(Rs_mid) - as.numeric(Rs_R))
rm(CP_MEMS_temp,CP_MEMS_temp2)  

# Import VOQC DieBin =============================================================
DieBinCAll_temp <- fread("c:/9_R_raw_temp/40_VOQC/DieBinCAll_20161205.csv")
names(DieBinCAll_temp) <- c("PHID","WaferID","VOQCDate","VOQC_OPID","CPTEQ","DieonPH",
                       "Reticle","DieInReticle","BinDate","Bin_OPID","BinEQ",
                       "BinCode","VOQCresult","VOQCfailCode","LotID","TSMC_FOdate")  
DieBinCAll <- DieBinCAll_temp %>%  
              mutate(DieID= paste(WaferID,"_R",Reticle,"_D",DieInReticle,sep="")) %>% 
              mutate(DiePHPos_temp= paste("0",DieonPH,sep="")) %>%
              mutate(DiePHPos= paste("PHD",str_sub(DiePHPos_temp,-2,-1),sep="")) %>% 
              mutate(DieRetiPos_temp= paste("0",DieInReticle,sep="")) %>%
              mutate(DieRetiPos=paste("RD",str_sub(DieRetiPos_temp,-2,-1),sep="")) %>%
              mutate(ReticleNo=paste("R",Reticle,sep="")) %>% 
              select(DieID,PHID,BinCode,VOQCresult,DiePHPos,DieRetiPos,CPTEQ,Bin_OPID,ReticleNo) %>% 
              mutate(LotID=str_sub(DieID,1,6)) %>% 
              mutate(WaferID=str_sub(DieID,1,9)) %>% 
              mutate(WaferGroup =str_sub(DieID,1,3)) %>% 
              left_join(TSMC_FO,by = "WaferID") %>% 
              left_join(WaferPos, by ="ReticleNo")


rm(DieBinCAll_temp)
DieBinCAll_s <- DieBinCAll %>% 
                select(DieID,BinCode,PHID,DiePHPos)

# Combine data "NSV data" as the main body=====================================================

NSV_score_CP_VOQC <- NSV_scoreV3_XT %>% 
                      left_join(CP_MEMS, by="DieID") %>% 
                      left_join(DieBinCAll_s, By="DieID") %>% 
                      filter(!is.na(BinCode)) %>% 
                      filter(RampNSV_status != "Failed To Scan") %>% 
                      mutate(SlotID= str_sub(WaferID,-2,-1))

write.csv(NSV_score_CP_VOQC,"c:/9_R_raw_temp/00_useful/2016WK49_RampNSV correlation check.csv")

# Data sorting for Cup3-4 only ==============================================================

Cup3_4Only <- NSV_score_CP_VOQC %>% 
              filter(TSMC_FO_yyWK>="16WK37") 
write.csv(Cup3_4Only,"c:/9_R_raw_temp/00_useful/2016WK49_RampNSV correlation check_3-4.csv")

# Combine data "VOQC die bin" as the main body =============================================

VOQCBin_CP <- DieBinCAll %>% 
              select(WaferGroup,LotID,WaferID,DieID,BinCode,PHID,DieRetiPos,
                     DiePHPos,ReticleNo,WaferPos,TSMC_FO_yyWK) %>%
              left_join(CP_MEMS, by="DieID")

VOQCBin_CP_cup3_4 <- VOQCBin_CP %>% 
                      filter(TSMC_FO_yyWK>="16WK37") %>% 
                      group_by(BinCode,ReticleNo)


# Diagram chart plot ==========================================================

pl <- ggplot(data=VOQCBin_CP_cup3_4, mapping=aes(x=ReticleNo, y=RS_sd))


pl + geom_jitter(aes(color=BinCode)) +
      facet_grid(DieRetiPos~.)

df <- VOQCBin_CP_cup3_4 %>% 
      summarise(count=n()) %>% 
      filter(count <50)







# Import MQ_VOQC_info=============================================================
MQ_PH_VOQC<- read.csv("c:/9_R_raw_temp/10_MQ_DieInfo/[Hank]LookUp_VOQC_information_150101To161127.csv")
VOQCPHID <- MQ_PH_VOQC %>%
                select(Serial.Number,Designation,Wafer.ID,Wafer.Part.Number,
                       D0,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10)
names(VOQCPHID) <- c("PHID","ProdType","WaferID","WaferPartNum",
                        "D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")

# Import SQL_PHEQ list============================================================
PHEQ_temp <- read.csv("C:/9_R_raw_temp/40_SQL_PHEQ/2016VOQCPH.csv") 
PHEQ <- PHEQ_temp %>% 
        select(mtlMaterialLotID,psgStartTime,psgPersonID,prqProductionRequestID,
               psdProcessSegmentID,eqpEquipmentID,eqpDescription,pscProcessSegmentClassID) 
names(PHEQ) <- c("PHID","StartTime","OPID","ProdRequestID","ProcessSegID",
                 "EQID","EQDescrip","Step")
rm(PHEQ_temp)


# combine VOQCPHID into PHEQ=========================================================
PHEQDie <- PHEQ %>% left_join(VOQCPHID, by= "PHID")
PHEQDie_L <- PHEQDie %>% 
              gather(key= PHDiePos.temp, value= Die.temp, 12:22) %>%
              separate(PHDiePos.temp, c("temp1","temp2"),sep="D",remove= FALSE) %>%
              mutate(PHDiePos.temp2= paste ("0",temp2,sep="")) %>%
              mutate(PHDiePos=paste("PHD",str_sub(PHDiePos.temp2,-2,-1),sep="")) %>%
              separate(Die.temp, c("temp3","temp4"),sep="D",remove= FALSE) %>%
              mutate(DiePos.temp= paste ("0",temp4,sep="")) %>%
              mutate(RetiDiePos=paste("RD",str_sub(DiePos.temp,-2,-1),sep="")) %>%
              mutate(DieID= paste(WaferID,"_",Die.temp,sep="")) %>%
              select(-contains("temp"))  





# combine all necessary info.==========================================================

PHEQDie_sum <- PHEQDie_L %>% 
                left_join(TSMC_FO, by="WaferID") %>% 
                left_join(DieBinCAll_s, by="DieID") %>% 
                mutate(LotGroup= str_sub(WaferID,1,3)) 
                
PHEQDie_sum2 <- PHEQDie_sum %>% 
                mutate(LotID= str_sub(WaferID,1,6)) %>% 
                mutate(SlotID= str_sub(WaferID,8,9)) %>% 
                mutate(ReticleNo = str_sub(DieID,11,13)) %>% 
                left_join(WaferPos, by="ReticleNo")
              
write.csv(PHEQDie_sum2, "c:/9_R_raw_temp/00_useful/2016WK49PHEQDie_sum.csv")


# Plot diagram ==========================================================

P_data <- PHEQDie_sum2 %>% 
          filter(Step=="segCalibrationPrintTest") %>%
          mutate(StartDate= str_sub(as.Date(StartTime),6,10)) %>% 
          filter(!is.na(Bin.code)) %>%
          group_by(EQID) %>% 
          group_by(Bin.code)


P_data2 <- PHEQDie_sum2 %>% 
            filter(Step=="segDieAttach") %>%
            mutate(StartDate= str_sub(as.Date(StartTime),6,10)) %>% 
            filter(!is.na(Bin.code)) %>%
            group_by(EQID) %>% 
            group_by(Bin.code)


write.csv(P_data,"c:/9_R_raw_temp/00_useful/2016WK49PHEQDie_sum_DA.csv",row.names=F)


ggplot(P_data,aes(x=StartDate,fill=Bin.code)) +
  geom_bar(position="fill")


