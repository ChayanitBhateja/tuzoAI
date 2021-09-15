# Demand forecasting simulation for Pernod Ricard USA

# =======================================================================================================
# Revision History
# v0  19.Aug.2013 gdalvi  Initial Exploratory script
# =======================================================================================================

rm(list=ls())

USER = "Chayanit"

if(USER == "Gautam")
{
  setwd("D:/_STADIUM/Tuzo V0")
  ioPathName = "D:/_STADIUM/Tuzo V0"
}
if(USER == "Chayanit")
{
  ioPathName = paste0(getwd(),"/myCodes/datasets")
}
source("Utils.R")


DataFileSuffix = 228

#PriceMatrix = read.csv(paste0(getwd(),"/myCodes/datasets/Price_",DataFileSuffix,".csv"))
#PriceMatrix
#VolumeMatrix = ReadFile(paste0("Volume_", DataFileSuffix, ".csv"), ioPathName)
master.data <- read.csv(paste0(ioPathName,"/tuzoUPCDataset.csv"))

head(master.data)


NUM_UPC = length(unique(master.data$UPC))
NUM_WEEKS = length(unique(master.data$week))

# WeekStatus = ReadFile("WeekStatus.csv", ioPathName) 
# PriceStatus = ReadFile("PriceStatus.csv", ioPathName) 
# Weeks window 

OutData = data.frame()

MIN_CMP_WEEKS = 3

PRC_CHG_WINDOW = 12
MIN_VOL_ET = 1

START_WK = PRC_CHG_WINDOW + 1
END_WK = NUM_WEEKS - PRC_CHG_WINDOW

StartTime0 = proc.time()
loopTime0 = proc.time()

LOG_ITERS = 1000

TEST_UPC <- 28947644927

subsetted.data <- master.data[master.data$UPC == TEST_UPC,]

elasticity(subsetted.data)

elasticity <- function(upc.data)
{
  NUM_WEEKS = length(unique(upc.data$week))
  
  # WeekStatus = ReadFile("WeekStatus.csv", ioPathName) 
  # PriceStatus = ReadFile("PriceStatus.csv", ioPathName) 
  # Weeks window 
  
  OutData = data.frame()
  
  MIN_CMP_WEEKS = 3
  
  PRC_CHG_WINDOW = 12
  MIN_VOL_ET = 1
  
  START_WK = PRC_CHG_WINDOW + 1
  END_WK = NUM_WEEKS - PRC_CHG_WINDOW
  
  currUPC = unique(upc.data$UPC)
  
  UPC_Prices = as.numeric(upc.data$price)
  
  UPC_Prices[is.na(UPC_Prices)] = 0
  
  UPC_Volumes = as.numeric(upc.data$volume)
  UPC_Volumes[is.na(UPC_Volumes)] = 0
  
  # Determine number of unique price points
  
  unique_prices = unique(UPC_Prices)  
  
  num_prices = length(unique_prices)  
  if(min(unique_prices) == 0) num_prices = num_prices-1
  
  
  if(num_prices <= 1) next
  
  # Determine price pulse windows - in every 26 week, 0 = Normal Price, 1 = Promotion price
  
  for(weekID in START_WK:END_WK)
  {    
    # Identify price change     
    currPrice = UPC_Prices[weekID]
    
    if(currPrice == 0) next
    
    prevPrice = UPC_Prices[weekID-1]
    
    if(prevPrice == 0 | currPrice == prevPrice) next     
    
    # Check if previous 4 and next 4 prices are same     
    str_wk = (weekID-PRC_CHG_WINDOW)
    end_wk = (weekID+PRC_CHG_WINDOW-1)    
    
    before_prices = UPC_Prices[str_wk:(weekID-1)]
    after_prices = UPC_Prices[weekID:end_wk]
    
    before_flag = as.numeric(UPC_Prices[str_wk:(weekID-1)] == prevPrice)
    #before_flag = before_flag * as.numeric(UPC_Volumes[str_wk:(weekID-1)] > 0)
    num_before_wks = sum(before_flag)
    
    after_flag = as.numeric(UPC_Prices[weekID:end_wk] == currPrice)
    #after_flag = after_flag*as.numeric(UPC_Volumes[weekID:end_wk] > 0)
    num_after_wks = sum(after_flag)
    
    # Ignore UPC if number of weeks with constant price before/after is less than MIN_CMP_WEEKS
    if(num_before_wks < MIN_CMP_WEEKS | num_after_wks < MIN_CMP_WEEKS) next
    
    before_vol = before_flag*UPC_Volumes[str_wk:(weekID-1)]  
    after_vol = after_flag*UPC_Volumes[weekID:end_wk]
    
    # Ignore UPC is min volume is less than MIN_VOL_ET
    ## if(min(before_vol) < MIN_VOL_ET & min(after_vol) < MIN_VOL_ET) next
    # browser("UPC Found")
    
    avg_before_vol = sum(before_vol)/num_before_wks
    avg_after_vol = sum(after_vol)/num_after_wks
    
    bf_test<-as.data.frame(t(before_vol))
    colnames(bf_test) <- paste0("bf_wk_vol ",-rev(1:PRC_CHG_WINDOW))
    
    af_test<-as.data.frame(t(after_vol))
    colnames(af_test) <- paste0("af_wk_vol ",0:(PRC_CHG_WINDOW-1))
    
    bf_test_prc<-as.data.frame(t(before_prices))
    colnames(bf_test_prc) <- paste0("bf_wk_prc ",-rev(1:PRC_CHG_WINDOW))
    
    af_test_prc<-as.data.frame(t(after_prices))
    colnames(af_test_prc) <- paste0("af_wk_prc ",0:(PRC_CHG_WINDOW-1))
     
    # Store Data for valid instances for measuring elasticity     
    pc_info = data.frame(currUPC, weekID, num_before_wks, prevPrice, avg_before_vol, num_after_wks, currPrice, avg_after_vol)
    
    pc_info<- merge(pc_info,bf_test)
    pc_info<- merge(pc_info,af_test)
    
    pc_info<- merge(pc_info,bf_test_prc)
    pc_info<- merge(pc_info,af_test_prc)
    
    OutData = if(dim(OutData)[1] == 0) pc_info else rbind(OutData, pc_info)              
  } # End iterating over all weeks for one UPC
  
} # End iterating over all UPC



OutData

#write.csv(OutData, paste0("Elasticity_PRC_CHG_WINDOW_",PRC_CHG_WINDOW,".csv"), row.names=F)
write.csv(OutData,"Elasticity.csv", row.names=F)

EndTime0 = proc.time()
ET0 = round(EndTime0 - StartTime0,2)
print(paste0("Processed ", NUM_UPC, " UPCs in ", ET0[3], " secs"))
