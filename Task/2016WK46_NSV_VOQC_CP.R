# =================================================================
# Task: 2016WK46 NSV summary
# Path: C:\0 My documents\Documents\Memjet\10 Integration\
#     90 Task force\00 Memjet\2016WK46 NSV summary
# ===================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)

Die_NSVCD <- read.csv("c:/9_R_raw_temp/0_useful/2016WK46_NSVCD_all.csv")
Die_NSVScore <- read.csv("c:/9_R_raw_temp/0_useful/2016WK46_NSVscore_all.csv")
DieBinCAll <- read.csv("c:/9_R_raw_temp/0_useful/2016WK46_VOQC_Die.csv")
Die_NSV_VOQC <- read.csv("c:/9_R_raw_temp/0_useful/2016WK46_Die_NSVsummary_rmFS.csv")


CPData_NSV <- fread("c:/9_R_raw_temp/CPraw/CPrawData_die_NSV.csv")

# only select Rs column from CP data
CPData_NSV_Rs <- CPData_NSV %>% 
                  select(V2,V48,V49,V50,V51,V57,V58)
names(CPData_NSV_Rs) <- c("DieID","RS_left","RS_Mid","RS_right",
                          "RS_slop","RS_mean","RS_sd")

names(CPData_NSV) <- c("WaferID",
                       "DieID",
                       "mtlDesignation",
                       "psdDescription",
                       "psgPersonID",
                       "psgStartTime",
                       "psgEndTime",
                       "psdProcessSegmentID",
                       "psgTestResult",
                       "Wirebond - Voltage drop across Vss protection diode on rst_n pad(mV)",
                       "Wirebond - Voltage drop across Vss protection on clk_p pad(mV)",
                       "Wirebond - Voltage drop across Vss protection on clk_n pad(mV)",
                       "Wirebond - Voltage drop across Vss protection on dat_p pad(mV)",
                       "Wirebond - Voltage drop across Vss protection on dat_n pad(mV)",
                       "Wirebond - Voltage drop across Vss protection(mV)",
                       "Gross current draw - Idd(mV)",
                       "Gross current draw - Ipos(mV)",
                       "Die Version - Design ID",
                       "Die Version - CMOS Version",
                       "Die Version - MEMS Version",
                       "Vector - Softmisc - Functional test of registers, error flags etc.(errors)",
                       "Parametric - Do - Low drive test at 4mA(mV)",
                       "Parametric - Do - High drive test at 4mA(mV)",
                       "Parametric - Do - Tristate leakage test(uA)",
                       "Number of RESET test errors(errors)",
                       "Parametric - Reset - Leakage test(uA)",
                       "Parametric - LVDS - Clock and data levels test(uA)",
                       "Parametric - LVDS - DataP leakage test(uA)",
                       "Parametric - LVDS - ClkP leakage test(uA)",
                       "Parametric - LVDS - DataN leakage test(uA)",
                       "Parametric - LVDS - ClkN leakage test(uA)",
                       "Vector - RingOsc - Ring oscillator frequency(MHz)",
                       "Vector - ColSR - Column shift register test.  Write data then read back(errors)",
                       "Vector - TDCFifo - TDC FIFO memory test.  Write data then read back(errors)",
                       "Vector - RowRd - Test core rows.  Write data then read back(errors)",
                       "Vector - CoreLatch - Test core latches for stuck at faults(errors)",
                       "Scan Chain - Mainscan - Main scan chain test(errors)",
                       "Scan Chain - Datamux - Datamux scan chain test(errors)",
                       "Static current - Vector - Die receives idles, slow clk(errors)",
                       "Static current draw - Idd(mA)",
                       "Static current draw - Ipos(mA)",
                       "Dynamic current - Vector - Die receives normal clk while most 3.3V logic is exercised(errors)",
                       "Dynamic current draw - Idd(mA)",
                       "Dynamic current draw - Ipos(mA)",
                       "Leakage current - Vector errors(errors)",
                       "Leakage current - Iddq - Leakage(uA)",
                       "Leakage current - Ipsoq - Leakage(uA)",
                       "MEMS - Mean resistance for actuators in left region(Ohms)",
                       "MEMS - Mean resistance for actuators in mid region(Ohms)",
                       "MEMS - Mean resistance for actuators in right region(Ohms)",
                       "MEMS - abs(left mean - right mean) / overall mean",
                       "MEMS - Number of actuators whose resistance further than res_deviation from the mean(actuators)",
                       "MEMS - Number of actuators more than x% below the mean(actuators)",
                       "MEMS - Number of actuators more than x% above the mean(actuators)",
                       "MEMS - Number of dead actuators too close together(actuators)",
                       "MEMS - Number of dead actuators too near the edge of the chip(actuators)",
                       "MEMS - Mean resistance for all actuators(Ohms)",
                       "MEMS - Standard deviation of resistance, all actuators where R is between res_abs_min(0) and res_abs_max(0)(Ohms)",
                       "Number of contact failures",
                       "Number of overcurrent failures",
                       "Number of Iddq leakage failures",
                       "Number of CMOS Core failures",
                       "Number of other CMOS failures",
                       "Number of resistance end to end differences",
                       "Number of excess resistance standard deviation",
                       "Number of die with too many dead nozzles",
                       "Number of other resistance failures",
                       "Insufficient percentage of devices passed CMOS",
                       "Insufficient percentage of devices passed MEMS",
                       "rnk")

Die_NSV_VOQC_Rs <- left_join(Die_NSV_VOQC,CPData_NSV_Rs, by="DieID") %>% 
                      mutate(RS_LRbias=as.numeric(RS_left) - as.numeric(RS_right)) %>% 
                      filter(RS_left != "NULL")
write.csv(Die_NSV_VOQC_Rs,"c:/9_R_raw_temp/0_useful/2016WK46_Die_NSV_VOQC_Rs.csv")

Die_NSV_VOQC_CP <- left_join(Die_NSV_VOQC,CPData_NSV, by="DieID")
write.csv(Die_NSV_VOQC_CP,"c:/9_R_raw_temp/0_useful/2016WK46_Die_NSV_VOQC_CP.csv")



write.csv(NSV_Die_summary_Rs_Bin,"c:/9_R_raw_temp/0_useful/2016WK42_Die_NSVsummary_rmFS_Rs_BinPL.csv")



