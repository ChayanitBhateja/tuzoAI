# Demand forecasting simulation for Pernod Ricard USA

# =======================================================================================================
# Revision History
# v0  19.Aug.2013 gdalvi  Initial Exploratory script
# =======================================================================================================

rm(list=ls())

getwd()

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

UMG_Final = read.csv(paste0(ioPathName,'/tuzoUMGDataset.csv'))

UMG_Final$activity.date <- as.Date(UMG_Final$activity.date,"%Y-%m-%d")

head(UMG_Final)

tail(UMG_Final)

summary(UMG_Final)

subset.data <- UMG_Final[UMG_Final$UPC == 11105962223,]

head(subset.data)

tail(subset.data)

umg_analysis <- function(subset.data)
{
  MINWEEK = 2061
  MAXWEEK = 2274
  NUM_WEEKS = MAXWEEK - MINWEEK + 1
  
  # Define short/medium/long term horizon
  
  RollWin_S = 4
  interval = 1/(1+RollWin_S)
  Wts_S = 1-seq(interval, 1-interval, interval)
  
  RollWin_M = 8
  interval = 1/(1+RollWin_M)
  Wts_M = 1-seq(interval, 1-interval, interval)
  
  RollWin_L = 13
  interval = 1/(1+RollWin_L)
  Wts_L = 1-seq(interval, 1-interval, interval)
  
  NUM_INIT_WEEKS = 8
  
  # Weeks window 
  VOL_THRESHOLD = 0.25
  
  HORIZON = 52
  
  PRICE_WINDOW = 26
  PRICE_THRESHOLD = 1
  
  #NUM_UPC = dim(UMG_Final)[1]
  NUM_UPC = 1
  
  # Identify the price and volume changes
  
  RevenueMatrix = as.matrix(subset.data[(subset.data$week >= MINWEEK) & (subset.data$week <= MAXWEEK),c('UPC','week','revenue')])
  head(RevenueMatrix)
  tail(RevenueMatrix)
  #RevenueMatrix <- as.matrix(UMG_Final[])
  
  dim(RevenueMatrix)
  
  rownames(RevenueMatrix) = unique(subset.data$week)
  
  VolumeMatrix = as.matrix(subset.data[(subset.data$week >= MINWEEK) & (subset.data$week <= MAXWEEK),c('UPC','week','volume')])
  head(VolumeMatrix)
  rownames(VolumeMatrix) = unique(subset.data$week)
  
  
  CatalogueVolume = as.matrix(subset.data)
  rownames(CatalogueVolume) = unique(subset.data$week)
  
  
  PriceMatrix <- as.matrix(subset.data$revenue / subset.data$volume)
  rownames(PriceMatrix) = unique(subset.data$week)
  head(subset.data, 13)
  
  PriceMatrix[is.na(PriceMatrix)] <- 0
  PriceMatrix<- t(PriceMatrix)
  
  #PriceMatrixOrig = RevenueMatrix/pmax(VolumeMatrix,1)
  #PriceMatrix = PriceMatrixOrig
  #rownames(PriceMatrix) = unique(subset.data$UPC)
  #PriceMatrix
  # Determine Prices and price changes - short term pulsing or longer term change
  Roll_Vol_Prev = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Vol_Prev) = unique(subset.data$UPC)
  
  Roll_Vol_Next = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Vol_Next) = unique(subset.data$UPC)
  
  Roll_Vol_Around = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Vol_Around) = unique(subset.data$UPC)
  
  Roll_Vol_Smoothed = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Vol_Smoothed) = unique(subset.data$UPC)
  
  Roll_Price_Prev = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Price_Prev) = unique(subset.data$UPC)
  
  Roll_Price_Next = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(Roll_Price_Next) = unique(subset.data$UPC)
  
  InitialVolume = matrix(0, NUM_UPC, NUM_INIT_WEEKS)
  #rownames(InitialVolume) = unique(subset.data$UPC)
  colnames(InitialVolume) = paste0("W", seq(1, NUM_INIT_WEEKS))
  browser('for Matrix Debugging..')
  WeekStatus = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(WeekStatus) = UMG_Final$UPC
  
  PriceStatus = matrix(0, NUM_UPC, NUM_WEEKS)
  #rownames(PriceStatus) = UMG_Final$UPC
  
  FirstWeeks = matrix(0, NUM_UPC, 2) # First Week, FCAT
  #rownames(FirstWeeks) = UMG_Final$UPC
  colnames(FirstWeeks) = c("FA_WeekID", "FCAT_WeekID")
  
  First_Vol_Week = 0
  
  Baseline = matrix(0, NUM_UPC, 3) # Volume, Revenue, Price, STD-DEV
  #rownames(Baseline) = UMG_Final$UPC
  colnames(Baseline) = c("Volume", "Revenue", "Price")
  
  library(readxl)
  
  datesData <- read_excel(paste0(getwd(),'/myCodes/datasets/UMG Dates.xlsx'))
  
  head(datesData)
  
  names(datesData) <- c('Sales_Week','Start_Week')
  
  datesData$Start_Week <- as.Date(datesData$Start_Week,"%Y-%m-%d")
  
  class(datesData$Start_Week)
  
  #subset.data$activity.date <- as.Date(subset.data$activity.date,"%Y-%m-%d")
  
  subset.data$activity.date[1] > datesData$Start_Week[1]
  
  class(subset.data$activity.date)
  
  MIN_WEEK_DATE <- datesData$Start_Week[1]
  
  MAX_WEEK_DATE <- datesData$Start_Week[len(datesData$Start_Week)]
  
  createWeekCohort <- function(album){
    for(index in 1:(NUM_WEEKS-1)){
      print(album)
      if((album >= datesData$Start_Week[index]) && (album < datesData$Start_Week[index + 1])){
          return(datesData$Sales_Week[index])
      } 
      if(album < MIN_WEEK_DATE){
        return(datesData$Sales_Week[1])
      }
      if(album > MAX_WEEK_DATE){
        return(datesData$Sales_Week[NUM_WEEKS])
      }
    }
  }
  
  
  subset.data$fa.week.id <- sapply(subset.data$activity.date, FUN=createWeekCohort)
  
  head(subset.data)

StartTime0 = proc.time()


  if(upc %% 100 == 0)
  {
    print(paste0("Processing UPC # ", upc))
    break;
  }
  currUPC = unique(subset.data$UPC)  

  FCAT_WeekID = max(1,subset.data$fa.week.id - MINWEEK+1 + 26) #-> 2172 - 2061+1 + 26
  FA_WeekID = max(1,subset.data$fa.week.id - MINWEEK+1) #-> 2172 - 2061+1
  
  FirstWeeks[1,1] = FA_WeekID
  FirstWeeks[1,2] = FCAT_WeekID
  
  if(FA_WeekID >= NUM_WEEKS) next
  
  # Set prices for weeks with no sales to the previous or next weeks
  # Forward Pass
  createWeekID <- function(record){
    return(record - MINWEEK+1)
  }
  
  subset.data$week.id <- sapply(subset.data$week, createWeekID)
  
  for(weekID in FA_WeekID:NUM_WEEKS)
  {
     if((First_Vol_Week == 0 )&& (subset.data[subset.data$week.id == weekID, 'volume'] > 0)) First_Vol_Week = weekID
     
     if(PriceMatrix[1,weekID] == 0)
     {
        if(weekID > FA_WeekID) PriceMatrix[1,weekID] = PriceMatrix[1,weekID-1]
      }
  }
  
  if(First_Vol_Week == 0) First_Vol_Week = FA_WeekID
  # Backward Pass - start only from first volume week until the first activity week
  for(weekID in First_Vol_Week:FA_WeekID)
  {
     if(PriceMatrix[1, weekID] == 0)
     {
        PriceMatrix[1,weekID] = PriceMatrix[1,weekID+1]
     }
  }
  
  # Determine price pulse windows - in every 26 week, 0 = Normal Price, 1 = Promotion price
 
  # Iterate over all weeks to generate data
  BaselineWeeks = 0
  LatestPriceSet = F

# Here for the week.... 
  for(weekID in  NUM_WEEKS:FA_WeekID)
  {   
    #browser('for Matrix Debugging..')
     CatWeek = T
     
     if(is.na(FCAT_WeekID) == T)
     {
        CatWeek = F
     } else if(weekID < FCAT_WeekID)
     {
        CatWeek = F
     } 
     if(CatWeek == F) next
     
     CatalogueVolume[weekID,'volume'] = VolumeMatrix[weekID, 'volume']
     
     isBaselinePrice = F
     
     CurrPrice = PriceMatrix[1, weekID]
     CurrVolume = VolumeMatrix[weekID,'volume']     
     
     StartWeek = max(1,weekID-RollWin_M)    
     interval_Prev = weekID - StartWeek
     
    if(interval_Prev > 0)
    {
       #Roll_Vol_Prev[upc, weekID] = sum(VolumeMatrix[upc,StartWeek:(weekID-1)] * Wts_M[1:interval_Prev])/sum(Wts_M[1:interval_Prev])
       Roll_Vol_Prev[1, weekID] = sum(VolumeMatrix[StartWeek:(weekID-1),'volume'])/interval_Prev
       Roll_Price_Prev[1, weekID] = sum(PriceMatrix[1,StartWeek:(weekID-1)])/interval_Prev
       
       if(IsWithin(CurrPrice, Roll_Price_Prev[1, weekID], PRICE_THRESHOLD, F)) isBaselinePrice = T
    }
    
    EndWeek = min(NUM_WEEKS,weekID+RollWin_M) 
    interval_Next = EndWeek - weekID
    if(interval_Next > 0)
    {
      #Roll_Vol_Next[upc, weekID] = sum(VolumeMatrix[upc,(weekID+1):EndWeek] * Wts_M[1:interval_Next])/sum(Wts_M[1:interval_Next])
       Roll_Vol_Next[1, weekID] = sum(VolumeMatrix[(weekID+1):EndWeek,'volume'])/interval_Prev
       Roll_Price_Next[1] = sum(PriceMatrix[1,(weekID+1):EndWeek])/interval_Next
      
      if(IsWithin(CurrPrice, Roll_Price_Next[1, weekID], PRICE_THRESHOLD, F)) isBaselinePrice = T
    }
     
    if(interval_Prev < 1 & interval_Next < 1) isBaselinePrice = T
 
    PriceStatus[1, weekID] = if(isBaselinePrice == T) 1 else -1
        
     NextVols = 0;  PrevVols = 0; div_wts = 0
     
    StartWeek = max(1,weekID-RollWin_S)
    interval_Prev = weekID - StartWeek  
    if(interval_Prev > 0)
    {
       PrevVols = sum(VolumeMatrix[StartWeek:(weekID-1),'volume'] * Wts_S[1:interval_Prev])
       div_wts = sum(Wts_S[1:interval_Prev])
    }

    EndWeek = min(NUM_WEEKS,weekID+RollWin_S)
    interval_Next = EndWeek - weekID    
     if(interval_Next > 0)
     {
       NextVols = sum(VolumeMatrix[(weekID+1):EndWeek,'volume'] * Wts_S[1:interval_Next])
       div_wts =  div_wts + sum(Wts_S[1:interval_Next])
     }

    if(div_wts > 0) Roll_Vol_Around[1, weekID] = (NextVols + PrevVols)/div_wts
    Roll_Vol_Smoothed[1, weekID] = (VolumeMatrix[weekID,'volume'] + NextVols + PrevVols)/(div_wts+1)
    
    # Mark Weeks as valid (1) or out-of-scope (0) or Excluded (-)

   if(IsWithin(CurrVolume, Roll_Vol_Around[1,weekID], VOL_THRESHOLD, mult = T)) 
   {
     WeekStatus[1, weekID] = 1
   } else # Now check if the current data is form is previous or current 
   {
      if(IsWithin(CurrVolume, Roll_Vol_Prev[1,weekID], VOL_THRESHOLD, mult = T) | 
            IsWithin(CurrVolume, Roll_Vol_Prev[1,weekID], VOL_THRESHOLD, mult = T)) 
      {
         WeekStatus[1, weekID] = 1
      } else WeekStatus[1, weekID] = -1         
   }
    
    # Generating data for initial legs    
    initWeekID = weekID - First_Vol_Week[1] + 1
   
    if(initWeekID <= NUM_INIT_WEEKS)
    {
      InitialVolume[1, initWeekID] = CurrVolume    
    }
     
    # browser("Here")
     
    if(WeekStatus[1, weekID] == 1 & PriceStatus[1,weekID] == 1)
    {
      BaselineWeeks = BaselineWeeks + 1
      
      Baseline[1, 1] = Baseline[1, 1] + VolumeMatrix[weekID,'volume']
      Baseline[1, 2] = Baseline[1, 2] + RevenueMatrix[weekID,'revenue']
      
      if(LatestPriceSet == F & PriceMatrix[1,weekID] > 0) Baseline[1,3] = PriceMatrix[1, weekID]
    }
    

  } # End iterating over all weeks for one UPC
  
  if(BaselineWeeks > 0)
  {
    Baseline[1, 1] = Baseline[1, 1]/BaselineWeeks
    Baseline[1, 2] = Baseline[1, 2]/BaselineWeeks
   }
  
} # End iterating over all UPC

umg_analysis(subset.data)

EndTime0 = proc.time()
ET0 = round(EndTime0 - StartTime0,2)
print(paste0("Processed ", NUM_UPC, " UPCs in ", ET0[3], " secs"))

# Select all data released before 1 year before start data
# Create a dataframe of new computed values

init_base_data = cbind(Baseline, InitialVolume)
write.csv(init_base_data, "Base_Init_Vol.csv")


write.csv(PriceStatus, "PriceStatus.csv")
write.csv(WeekStatus, "WeekStatus.csv")
write.csv(PriceMatrix, "PriceMatrix.csv")

selcols = c("UPC", "PROJECT_ID", "CLEAN_R2_PRODUCT_CONFIG", "R2_STATUS", "GENRE_TYPE",  "EXPLICIT",                
           "LONGEST_TRACK_LENGTH", "PRODUCT_DURATION", "TOTAL_DISCS", "TOTAL_TRACKS", "SALES_UNITS", "REVENUE")                         
UMG_SegData = UMG_Final[, selcols]

UMG_SegData$SalesVelocity = init_base_data[,"Volume"]
UMG_SegData$BasePrice = init_base_data[,"Price"]
UMG_SegData$RevPerWeek = init_base_data[,"Revenue"]

write.csv(UMG_SegData, "UMG_SegData.csv")
