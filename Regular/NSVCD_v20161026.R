# ====================================================================
# author: Hank Hsu
# the first edition date: 2016/09/13
# the last modification date: 2016/10/26
# version: Rev01.20161026
# The following code is to calcuate each die statical NSVCD from NSV raw data
# Notice: the raw data folder should have three level folder strcture
# For example: Rawpath/LotID/waferID/*.csv
# =====================================================================

# Define raw data folder & output folder path
Rawpath <- "C:/9_R_raw_temp/NSVRawData/newWK46_1106/"
Outputpath <- "C:/9_R_raw_temp/NSVCD/newWK46_1106/"
NSVSite <- "Xintec"
LotList <- list.files(path=Rawpath) #get lot list in the raw data folder
ItemNames <- c("NSVSite","LotID","WaferID","DieID"
               ,"Min_m","1st Qu_m","Median_m","Mean_m","3rd Qu_m","Max_m", "Std Dev_m","IQR_m"
               ,"Min_M","1st Qu_M","Median_M","Mean_M","3rd Qu_M","Max_M", "Std Dev_M","IQR_M"
               ,"Min_A","1st Qu_A","Median_A","Mean_A","3rd Qu_A","Max_A", "Std Dev_A","IQR_A"
               ,"ScanDate")
result_all <- data.frame(colnames(ItemNames)) #creat an empty for fill later

for (Lot in LotList)
{
  Lotpath <- paste(Rawpath,Lot,"/",sep="")
  WaferList <- list.files(path=Lotpath) #get wafer list in the lot folder
  result_lot <- data.frame(colnames(ItemNames)) #creat an empty for fill later 
  
  for (Wafer in WaferList)
  {
    if (file.size(paste(Lotpath,Wafer,sep=""))!=0) next #skip non-folder files
    Waferpath <- paste(Lotpath,Wafer,"/",sep="")
    NSVRawFiles <- list.files(path=Waferpath, pattern="*.csv") #get NSV raw data list
    result <- data.frame(colnames(c(ItemNames))) #creat an empty for fill later  
    
    for (file in NSVRawFiles)
    {
      DotPos <- which(strsplit(file, "")[[1]]==".") #get "." position from file name
      DieID <- substr(file, DotPos[1]+1, DotPos[2]-1) #extract DieID from file name
      WaferID <- substr(file, DotPos[1]+1, DotPos[1]+9) #extract WaferID from file name
      LotID <- substr(file, DotPos[1]+1, DotPos[1]+6) #extract WaferID from file name
      ScanDate <- substr(file, DotPos[2]+1, DotPos[2]+8) #extract ScanDate from file name
      
      if (file.size(paste(Waferpath,file,sep="")) > 1500) {
        
        NSVData <- read.csv(paste(Waferpath,file,sep=""),header = TRUE) #input raw data
        x <- NSVData$Nozzle.Minor.Diameter..um. #set "x" as the Minor nozzle CD data input variable
        y <- NSVData$Nozzle.Major.Diameter..um. #set "y" as the Major nozzle CD data input variable
        z <- (x/2)*(y/2)*pi #set "z" as the area of nozzle input variable
        NewDatarow <- data.frame(t(c(NSVSite,LotID,WaferID,DieID
                                     ,summary(x),sqrt(var(x)),IQR(x)
                                     ,summary(y),sqrt(var(y)),IQR(y)
                                     ,summary(z),sqrt(var(z)),IQR(z)
                                     ,ScanDate)))
        names(NewDatarow) <- names(result) #to solve match.names error (still don't understand)
      }else{
        NewDatarow <- data.frame(t(c(NSVSite,LotID,WaferID,DieID
                                     ,"FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan"
                                     ,"FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan"
                                     ,"FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan","FailToScan"
                                     ,ScanDate)))
        names(NewDatarow) <- names(result) #to solve match.names error (still don't understand)
      }
      result <- rbind(result,NewDatarow)
    }  
    names(result)=ItemNames
    write.csv(result,file= paste(Outputpath,WaferID,"_summary.csv",sep="")) #output Wafer Summary
    result_lot <- rbind(result_lot,result)
    rm(result)
  }
  names(result_lot)=ItemNames
  write.csv(result_lot,file= paste(Outputpath,LotID,"_summary.csv",sep="")) #output Lot Summary
  result_all <- rbind(result_all,result_lot)
  rm(result_lot)  
}
names(result_all)=ItemNames
write.csv(result_all,file= paste(Outputpath,"All_summary.csv",sep="")) #output All Summary

print("Complete")
