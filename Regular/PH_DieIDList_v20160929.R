# author: Hank Hsu
# date: 2016/09/29
# version: Rev01.20160929
# The following code is for getting a short table on WaferID, DieID, PHIDs
# The raw data is from SVN: "32-01-10 Tools/04-get full report on cartridges...."
# The batch code is:
# "python cartridge_report.py -mMP2 -b2016-06-25 -e2016-09-23 -iPAM -iVOQC -o VOQC.csv"

library(dplyr)
library(tidyr)
library(stringr)

VOQC.raw<- read.csv("c:/9_R_raw_temp/0_useful/[Hank]LookUp_VOQC_information_20160925.csv")
DieIDList <- VOQC.raw %>%
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
          select(-contains("temp")) # to remove all column named with"temp"

names(DieIDList) <- c("PHID","Type","WaferID","WaferPartID","PHDiePos","RetiDiePo","DieID")
write.csv(DieIDList,"c:/9_R_raw_temp/0_useful/PH_DieIDList_20160925.csv")
