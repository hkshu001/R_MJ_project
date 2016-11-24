# =======================================================================
# author: Hank Hsu
# the first edition date: 2016/11/28
# the last modification date: 2016/11/28
# version: Rev01.20161128
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

# import die info. base on DA step from SQL 
SQL_Die_DA <- read.csv("C:/9_R_raw_temp/DieInfo/SQL_Die_Wafer die information (DA).csv", header = F)
SQL_Die_DA[1,1] <- "C3N003"
names(SQL_Die_DA) <- c("LotID","WaferID","DieID","DAEQID","ProType","DAdate","DAsegID")

SQL_PH_DA <- read.csv("c:/9_R_raw_temp/DieInfo/SQL_PH_Wafer information (DA).csv",header=F) 
SQL_PH_DA <- SQL_PH_DA %>% select(V5,V3,V4)
names(SQL_PH_DA) <-c("WaferID","PHID","DAEQID")

MQ_PH_VOQC<- read.csv("c:/9_R_raw_temp/DieInfo/[Hank]LookUp_VOQC_information_150101To161127.csv")
PHDieIDList <- MQ_PH_VOQC %>%
              select(Serial.Number,Designation,Wafer.ID,Wafer.Part.Number,
                     D0,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10) %>%
              gather(key= PHDiePos.temp, value= Die.temp, 5:15) %>%
              # to generate PH Die position column from PHD00 to PHD10 
              separate(PHDiePos.temp, c("temp1","temp2"),sep="D",remove= FALSE) %>%
              mutate(PHDiePos.temp2= paste ("0",temp2,sep="")) %>%
              mutate(PHDiePos=paste("PHD",str_sub(PHDiePos.temp2,-2,-1),sep="")) %>%
              # to generate Reticle Die position column from RD01 to RD11
              separate(Die.temp, c("temp3","temp4"),sep="D",remove= FALSE) %>%
              mutate(DiePos.temp= paste ("0",temp4,sep="")) %>%
              mutate(RetiDiePos=paste("RD",str_sub(DiePos.temp,-2,-1),sep="")) %>%
              # to generate complete DieID column
              mutate(DieID= paste(Wafer.ID,"_",Die.temp,sep="")) %>%
              select(-contains("temp")) # to remove all column named with"temp" %>% 
              select(DieID,Serial.Number,PHDiePos,RetiDiePos)
#===20161130============================================
PHDieIDList_DA_CPT <- MQ_PH_VOQC %>%
                select(Serial.Number,Designation,Wafer.ID,Wafer.Part.Number,
                       D0,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,
                       Die.Attach.Start.Time,Die.Attach.Equipment.ID,Die.Attach.Process.ID,
                       VOQC.Test.Result..Final.,VOQC.Start.Time..Final.,
                       VOQC.Process.Segment.ID..Final.,VOQC.Chart.CPT.EquipmentID..Final.,
                       Description) %>%
                gather(key= PHDiePos.temp, value= Die.temp, 5:15) %>%
                # to generate PH Die position column from PHD00 to PHD10 
                separate(PHDiePos.temp, c("temp1","temp2"),sep="D",remove= FALSE) %>%
                mutate(PHDiePos.temp2= paste ("0",temp2,sep="")) %>%
                mutate(PHDiePos=paste("PHD",str_sub(PHDiePos.temp2,-2,-1),sep="")) %>%
                # to generate Reticle Die position column from RD01 to RD11
                separate(Die.temp, c("temp3","temp4"),sep="D",remove= FALSE) %>%
                mutate(DiePos.temp= paste ("0",temp4,sep="")) %>%
                mutate(RetiDiePos=paste("RD",str_sub(DiePos.temp,-2,-1),sep="")) %>%
                # to generate complete DieID column
                mutate(DieID= paste(Wafer.ID,"_",Die.temp,sep="")) %>%
                select(-contains("temp")) # to remove all column named with"temp"
              
names(PHDieIDList_DA_CPT) = c("PHID","Production","WaferID","WaferPartNum",
                              "DAstarttime","DAEQID","DAsegID","VOQCresult",
                              "VOQCstarttime","VOQCseqID","VOQCEQID","ProdDescription",
                              "PHDiePos","RetiDiePos","DieID")
write.csv(PHDieIDList_DA_CPT,"c:/9_R_raw_temp/DieInfo/PHDieList_DA_CPT.csv")              


DieBinCAll <- read.csv("c:/9_R_raw_temp/0_useful/DieBinCAll_20161124.csv") %>% 
            mutate(DieID= paste(WaferID,"_R",Reticle,"_D",Die.in.Reticle,sep="")) %>% 
            mutate(PHDiePos.temp= paste ("0",Die.on.PH,sep="")) %>%
            mutate(PHDiePos=paste("PHD",str_sub(PHDiePos.temp,-2,-1),sep="")) %>% 
            mutate(RetiDiePos.temp= paste ("0",Die.in.Reticle,sep="")) %>%
            mutate(RetiDiePos=paste("RD",str_sub(RetiDiePos.temp,-2,-1),sep="")) %>% 
            select(DieID,PHID,Bin.code,VOQC.Result,PHDiePos,
                   CPT.Machine,Bin.Operator.ID) %>% 
            mutate(LotID=str_sub(DieID,1,6)) %>% 
            mutate(WaferID=str_sub(DieID,1,9)) %>% 
            mutate(WaferGroup =str_sub(DieID,1,3)) %>% 
            left_join(TSMC_FO,by = "WaferID") %>%
            mutate(NSVscan = ifelse(!is.na(TSMC_FO_yyWK), "Yes","No"))
DieBinCAll_s <- DieBinCAll %>% 
                select(DieID,Bin.code,CPT.Machine,Bin.Operator.ID,TSMC_FO_yyWK,
                       TSMC_FO_year,TSMC_FO_month)

PHDieIDList_DA_CPT2 <- PHDieIDList_DA_CPT %>%
                        mutate(DAtime= str_sub(DAstarttime,1,10)) %>% 
                        filter(DAtime >= "2016-07-01") %>% 
                        left_join(DieBinCAll_s, by="DieID")


write.csv(PHDieIDList_DA_CPT2,"c:/9_R_raw_temp/DieInfo/PHDieList_DA_CPT_2016.csv")   



              
#========================================================
              
SQL_material <- read.csv("c:/9_R_raw_temp/DieInfo/All material information_20150101to20161127.csv",header = F)
M1.levels<-levels(SQL_material$V1)
levels(SQL_material$V1) <-c(M1.levels,"C3N024-12_R38_D5")
SQL_material[1,1] <- "C3N024-12_R38_D5"
rm(M1.levels)
names(SQL_material) <- c("MtlMaterialLotID","EquipmentID","mtlDesignation","psdDescription",
                         "psgPersonID","psgStartTime","psdProcessSegmentID","pscProcessSegmentClassID",
                         "ConsumedMlcMaterialClassID")


assyPrintHead <- SQL_material %>% 
          filter(ConsumedMlcMaterialClassID=="assyPrintHead") %>% 
          filter(pscProcessSegmentClassID != "segDieAttach")


