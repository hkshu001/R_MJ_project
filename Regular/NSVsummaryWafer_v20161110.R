# ========================================================================
# author: Hank Hsu
# date: 2016/10/26
# version: Rev01.20161110
# The following code is to summarize the "Wafer" informaiton NSVCD, NSVscore, VOQC...  
# The raw data is from: NSVCD, NSV 
# =========================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)

# combine all NSV Wafer pass rate data
# import UGSI NSV data from excel file
NSV_Wafer_UG_temp <- read_excel("c:/9_R_raw_temp/NSVRawData/UGSI NSV scanning result of WK33.xlsx",
                           sheet= "Summary")
names(NSV_Wafer_UG_temp) <- c("LotID","WaferID","NSVDate","TSMC_FO_date",
                              "TotalScannedDies","FailedToScan",
                              "RampNSV60die_Fail","RampNSV60die_Pass","RampNSV60die_Passedratio",
                              "RampNSVD1D11_Fail","RampNSVD1D11_Pass","RampNSVD1D11_Passedratio",
                              "RetiNSV60die_Fail","RetiNSV60die_Pass","RetiNSV60die_Passedratio",
                              "RetiNSVD06_Fail","RetiNSVD06_Pass","RetiNSVD06_Passedratio",
                              "human_DarkCorner","human_ReticleNSV")

NSV_Wafer_UG <- NSV_Wafer_UG_temp[-1:-2,-21:-22] %>% 
                mutate(NSVSite="UGSI")
#import Xintec NSV data from excel file
NSV_Wafer_XT_temp <- read_excel("c:/9_R_raw_temp/NSVRawData/Xintec NSV scanning result of W45_rev1.xlsx",
                                sheet= "Summary")
names(NSV_Wafer_XT_temp) <- c("LotID","WaferID","NSVDate","TSMC_FO_date",
                              "TotalScannedDies","FailedToScan",
                              "RampNSV60die_Fail","RampNSV60die_Pass","RampNSV60die_Passedratio",
                              "RampNSVD1D11_Fail","RampNSVD1D11_Pass","RampNSVD1D11_Passedratio",
                              "RetiNSV60die_Fail","RetiNSV60die_Pass","RetiNSV60die_Passedratio",
                              "RetiNSVD06_Fail","RetiNSVD06_Pass","RetiNSVD06_Passedratio",
                              "human_DarkCorner","human_ReticleNSV")
NSV_Wafer_XT <- NSV_Wafer_XT_temp[-1:-2,-21:-22] %>% 
                mutate(NSVSite="Xintec")
# combind Xintec and UGSI NSV wafer level data
NSV_Wafer <- rbind(NSV_Wafer_XT,NSV_Wafer_UG) %>% 
             mutate(SlotID=str_sub(WaferID,-2,-1)) %>% 
             mutate(EvenOdd= as.numeric(SlotID)) %>%
             mutate(EvenOdd=ifelse(EvenOdd %% 2 == "0", "Even","Odd"))

write.csv(NSV_Wafer,"c:/9_R_raw_temp/0_useful/2016WK46_wafer_NSV.csv")
rm(NSV_Wafer_UG,NSV_Wafer_XT,NSV_Wafer_UG_temp,NSV_Wafer_XT_temp)

NSV60_Wafer <- NSV_Wafer %>% 
                filter(TotalScannedDies>30 & TotalScannedDies <65)
write.csv(NSV60_Wafer,"c:/9_R_raw_temp/0_useful/2016WK46_wafer_NSV60.csv")


# import the VOQC_wafer data
VOQC_Wafer_temp <- read.csv("c:/9_R_raw_temp/0_useful/WaferSlot_Summary_20161114.csv") %>% 
                select(waferbatch,waferID,Total.Dies,VOQC.Pass,VOQC.Fail,
                       VOQC.Yield.,Pass.ratio.,Ramping.R.ratio.,Ramping.L.ratio.,
                       Dark.Band.ratio.,White.Band.ratio.,Streak.ratio.,Others.ratio.,
                       Center,X2nd,N.S,X4.Corner)
names(VOQC_Wafer_temp) <- c("LotID","WaferID","TotalDies","VOQCPassDie","VOQCFailDie",
                       "VOQCYield","DieBin_P","Failrate_R","Failrate_L",
                       "Failrate_D","Failrate_W","Failrate_S","Failrate_O",
                       "VOQCYield_C","VOQCYield_2nd","VOQCYield_NS","VOQCYield_4C")

VOQC_Wafer<- VOQC_Wafer_temp %>% 
              mutate(DieBin_Fail=(Failrate_R+Failrate_L+Failrate_D+
                                    Failrate_W+Failrate_S+Failrate_O)) %>% 
              mutate(DieBinSum=(DieBin_P+DieBin_Fail)) %>% 
              mutate(DieBinCheck= ifelse(DieBinSum>0.95, "Normal","Strange"))

write.csv(VOQC_Wafer,"c:/9_R_raw_temp/0_useful/2016WK46_wafer_VOQC.csv")
rm(VOQC_Wafer_temp)

NSV60_VOQC <- left_join(NSV60_Wafer,VOQC_Wafer, by="WaferID")
write.csv(NSV60_VOQC,"c:/9_R_raw_temp/0_useful/2016WK46_wafer_NSV60_VOQC.csv")

