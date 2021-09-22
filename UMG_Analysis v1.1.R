# Demand forecasting simulation for Pernod Ricard USA

# =======================================================================================================
# Revision History
# v0  19.Aug.2013 gdalvi  Initial Exploratory script
# =======================================================================================================

rm(list=ls())

setwd("~/work/Simplilearn/Fiverr/tuzo")

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

head(UMG_Final)

UMG_Final$activity.date <- as.Date(UMG_Final$activity.date,"%Y-%m-%d")

multiple.upc.data <- UMG_Final[(UMG_Final$UPC == 8810255023) | (UMG_Final$UPC == 11105962223) | (UMG_Final$UPC == 13431491620),]

subset.data = multiple.upc.data[multiple.upc.data$UPC == 8810255023,]

unique(multiple.upc.data$UPC)


Start_Week = as.Date(c("2009-05-25","2009-06-01","2009-06-08","2009-06-15","2009-06-22","2009-06-29","2009-07-06","2009-07-13","2009-07-20","2009-07-27","2009-08-03","2009-08-10","2009-08-17","2009-08-24","2009-08-31","2009-09-07","2009-09-14","2009-09-21","2009-09-28","2009-10-05","2009-10-12","2009-10-19","2009-10-26","2009-11-02","2009-11-09","2009-11-16","2009-11-23","2009-11-30","2009-12-07","2009-12-14","2009-12-21","2009-12-28","2010-01-04","2010-01-11","2010-01-18","2010-01-25","2010-02-01","2010-02-08","2010-02-15","2010-02-22","2010-03-01","2010-03-08","2010-03-15","2010-03-22","2010-03-29","2010-04-05","2010-04-12","2010-04-19","2010-04-26","2010-05-03","2010-05-10","2010-05-17","2010-05-24","2010-05-31","2010-06-07","2010-06-14","2010-06-21","2010-06-28","2010-07-05","2010-07-12","2010-07-19","2010-07-26","2010-08-02","2010-08-09","2010-08-16","2010-08-23","2010-08-30","2010-09-06","2010-09-13","2010-09-20","2010-09-27","2010-10-04","2010-10-11","2010-10-18","2010-10-25","2010-11-01","2010-11-08","2010-11-15","2010-11-22","2010-11-29","2010-12-06","2010-12-13","2010-12-20","2010-12-27","2011-01-03","2011-01-10","2011-01-17","2011-01-24","2011-01-31","2011-02-07","2011-02-14","2011-02-21","2011-02-28","2011-03-07","2011-03-14","2011-03-21","2011-03-28","2011-04-04","2011-04-11","2011-04-18","2011-04-25","2011-05-02","2011-05-09","2011-05-16","2011-05-23","2011-05-30","2011-06-06","2011-06-13","2011-06-20","2011-06-27","2011-07-04","2011-07-11","2011-07-18","2011-07-25","2011-08-01","2011-08-08","2011-08-15","2011-08-22","2011-08-29","2011-09-05","2011-09-12","2011-09-19","2011-09-26","2011-10-03","2011-10-10","2011-10-17","2011-10-24","2011-10-31","2011-11-07","2011-11-14","2011-11-21","2011-11-28","2011-12-05","2011-12-12","2011-12-19","2011-12-26","2012-01-02","2012-01-09","2012-01-16","2012-01-23","2012-01-30","2012-02-06","2012-02-13","2012-02-20","2012-02-27","2012-03-05","2012-03-12","2012-03-19","2012-03-26","2012-04-02","2012-04-09","2012-04-16","2012-04-23","2012-04-30","2012-05-07","2012-05-14","2012-05-21","2012-05-28","2012-06-04","2012-06-11","2012-06-18","2012-06-25","2012-07-02","2012-07-09","2012-07-16","2012-07-23","2012-07-30","2012-08-06","2012-08-13","2012-08-20","2012-08-27","2012-09-03","2012-09-10","2012-09-17","2012-09-24","2012-10-01","2012-10-08","2012-10-15","2012-10-22","2012-10-29","2012-11-05","2012-11-12","2012-11-19","2012-11-26","2012-12-03","2012-12-10","2012-12-17","2012-12-24","2012-12-31","2013-01-07","2013-01-14","2013-01-21","2013-01-28","2013-02-04","2013-02-11","2013-02-18","2013-02-25","2013-03-04","2013-03-11","2013-03-18","2013-03-25","2013-04-01","2013-04-08","2013-04-15","2013-04-22","2013-04-29","2013-05-06","2013-05-13","2013-05-20","2013-05-27","2013-06-03","2013-06-10","2013-06-17","2013-06-24"))

Sales_Week <- c(2061,2062,2063,2064,2065,2066,2067,2068,2069,2070,2071,2072,2073,2074,2075,2076,2077,2078,2079,2080,2081,2082,2083,2084,2085,2086,2087,2088,2089,2090,2091,2092,2093,2094,2095,2096,2097,2098,2099,2100,2101,2102,2103,2104,2105,2106,2107,2108,2109,2110,2111,2112,2113,2114,2115,2116,2117,2118,2119,2120,2121,2122,2123,2124,2125,2126,2127,2128,2129,2130,2131,2132,2133,2134,2135,2136,2137,2138,2139,2140,2141,2142,2143,2144,2145,2146,2147,2148,2149,2150,2151,2152,2153,2154,2155,2156,2157,2158,2159,2160,2161,2162,2163,2164,2165,2166,2167,2168,2169,2170,2171,2172,2173,2174,2175,2176,2177,2178,2179,2180,2181,2182,2183,2184,2185,2186,2187,2188,2189,2190,2191,2192,2193,2194,2195,2196,2197,2198,2199,2200,2201,2202,2203,2204,2205,2206,2207,2208,2209,2210,2211,2212,2213,2214,2215,2216,2217,2218,2219,2220,2221,2222,2223,2224,2225,2226,2227,2228,2229,2230,2231,2232,2233,2234,2235,2236,2237,2238,2239,2240,2241,2242,2243,2244,2245,2246,2247,2248,2249,2250,2251,2252,2253,2254,2255,2256,2257,2258,2259,2260,2261,2262,2263,2264,2265,2266,2267,2268,2269,2270,2271,2272,2273,2274)

datesData <- data.frame(Sales_Week = Sales_Week, Start_Week = Start_Week, stringsAsFactors = FALSE)

define.constants <- function(){
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
  
  constants <- list(MINWEEK = MINWEEK,
                    MAXWEEK = MAXWEEK,
                    NUM_WEEKS = NUM_WEEKS,
                    RollWin_S = RollWin_S,
                    Wts_S = Wts_S,
                    RollWin_M = RollWin_M,
                    Wts_M = Wts_M,
                    RollWin_L = RollWin_L,
                    Wts_L = Wts_L,
                    NUM_INIT_WEEKS = NUM_INIT_WEEKS,
                    VOL_THRESHOLD = VOL_THRESHOLD,
                    HORIZON = HORIZON,
                    PRICE_WINDOW = PRICE_WINDOW,
                    PRICE_THRESHOLD = PRICE_THRESHOLD)
  
  return(constants)
}

#Function to process subset of each upc for all weeks 
process.upc <- function(subset.data, index){
  constants <- define.constants()
  RevenueMatrix = as.matrix(subset.data[c('UPC','week','revenue')])
  #RevenueMatrix <- as.matrix(UMG_Final[])
  rownames(RevenueMatrix) = unique(subset.data$week)
  
  VolumeMatrix = as.matrix(subset.data[c('UPC','week','volume')])
  head(VolumeMatrix)
  rownames(VolumeMatrix) = unique(subset.data$week)
  
  
  CatalogueVolume = as.matrix(subset.data)
  rownames(CatalogueVolume) = unique(subset.data$week)
  
  
  PriceMatrix <- as.matrix(subset.data$revenue / subset.data$volume)
  rownames(PriceMatrix) = unique(subset.data$week)
  head(subset.data, 13)
  
  PriceMatrix[is.na(PriceMatrix)] <- 0
  PriceMatrix<- t(PriceMatrix)
  
  # Determine Prices and price changes - short term pulsing or longer term change
  Roll_Vol_Prev = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Vol_Prev) = unique(subset.data$UPC)
  
  Roll_Vol_Next = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Vol_Next) = unique(subset.data$UPC)
  
  Roll_Vol_Around = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Vol_Around) = unique(subset.data$UPC)
  
  Roll_Vol_Smoothed = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Vol_Smoothed) = unique(subset.data$UPC)
  
  Roll_Price_Prev = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Price_Prev) = unique(subset.data$UPC)
  
  Roll_Price_Next = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(Roll_Price_Next) = unique(subset.data$UPC)
  
  InitialVolume = matrix(0, 1, constants$NUM_INIT_WEEKS)
  colnames(InitialVolume) = paste0("W", seq(1, constants$NUM_INIT_WEEKS))
  
  WeekStatus = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(WeekStatus) = UMG_Final$UPC
  
  PriceStatus = matrix(0, 1, constants$NUM_WEEKS)
  #rownames(PriceStatus) = UMG_Final$UPC
  
  FirstWeeks = matrix(0, 1, 2) # First Week, FCAT
  #rownames(FirstWeeks) = UMG_Final$UPC
  colnames(FirstWeeks) = c("FA_WeekID", "FCAT_WeekID")
  
  First_Vol_Week = 0
  
  Baseline = matrix(0, 1, 3) # Volume, Revenue, Price, STD-DEV
  #rownames(Baseline) = UMG_Final$UPC
  colnames(Baseline) = c("Volume", "Revenue", "Price")
  
  currUPC = unique(subset.data$UPC)  
  
  FCAT_WeekID = max(1,subset.data$fa.week.id - constants$MINWEEK+1 + 26) #-> 2172 - 2061+1 + 26
  FA_WeekID = max(1,subset.data$fa.week.id - constants$MINWEEK+1) #-> 2172 - 2061+1
  
  FirstWeeks[1,1] = FA_WeekID
  FirstWeeks[1,2] = FCAT_WeekID
  
  if(FA_WeekID >= constants$NUM_WEEKS) next
  
  # Set prices for weeks with no sales to the previous or next weeks
  # Forward Pass
  createWeekID <- function(record){
    return(record - constants$MINWEEK+1)
  }
  
  subset.data$week.id <- sapply(subset.data$week, createWeekID)
  
  for(weekID in FA_WeekID:constants$NUM_WEEKS)
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
  for(weekID in  constants$NUM_WEEKS:FA_WeekID)
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
    
    StartWeek = max(1,weekID-constants$RollWin_M)    
    interval_Prev = weekID - StartWeek
    
    if(interval_Prev > 0)
    {
      #Roll_Vol_Prev[upc, weekID] = sum(VolumeMatrix[upc,StartWeek:(weekID-1)] * constants$Wts_M[1:interval_Prev])/sum(constants$Wts_M[1:interval_Prev])
      Roll_Vol_Prev[1, weekID] = sum(VolumeMatrix[StartWeek:(weekID-1),'volume'])/interval_Prev
      Roll_Price_Prev[1, weekID] = sum(PriceMatrix[1,StartWeek:(weekID-1)])/interval_Prev
      
      if(IsWithin(CurrPrice, Roll_Price_Prev[1, weekID], constants$PRICE_THRESHOLD, F)) isBaselinePrice = T
    }
    
    EndWeek = min(constants$NUM_WEEKS,weekID+constants$RollWin_M) 
    interval_Next = EndWeek - weekID
    if(interval_Next > 0)
    {
      #Roll_Vol_Next[upc, weekID] = sum(VolumeMatrix[upc,(weekID+1):EndWeek] * constants$Wts_M[1:interval_Next])/sum(constants$Wts_M[1:interval_Next])
      Roll_Vol_Next[1, weekID] = sum(VolumeMatrix[(weekID+1):EndWeek,'volume'])/interval_Prev
      Roll_Price_Next[1] = sum(PriceMatrix[1,(weekID+1):EndWeek])/interval_Next
      
      if(IsWithin(CurrPrice, Roll_Price_Next[1, weekID], constants$PRICE_THRESHOLD, F)) isBaselinePrice = T
    }
    
    if(interval_Prev < 1 & interval_Next < 1) isBaselinePrice = T
    
    PriceStatus[1, weekID] = if(isBaselinePrice == T) 1 else -1
    
    NextVols = 0;  PrevVols = 0; div_wts = 0
    
    StartWeek = max(1,weekID-constants$RollWin_S)
    interval_Prev = weekID - StartWeek  
    if(interval_Prev > 0)
    {
      PrevVols = sum(VolumeMatrix[StartWeek:(weekID-1),'volume'] * constants$Wts_S[1:interval_Prev])
      div_wts = sum(constants$Wts_S[1:interval_Prev])
    }
    
    EndWeek = min(constants$NUM_WEEKS,weekID+constants$RollWin_S)
    interval_Next = EndWeek - weekID    
    if(interval_Next > 0)
    {
      NextVols = sum(VolumeMatrix[(weekID+1):EndWeek,'volume'] * constants$Wts_S[1:interval_Next])
      div_wts =  div_wts + sum(constants$Wts_S[1:interval_Next])
    }
    
    if(div_wts > 0) Roll_Vol_Around[1, weekID] = (NextVols + PrevVols)/div_wts
    Roll_Vol_Smoothed[1, weekID] = (VolumeMatrix[weekID,'volume'] + NextVols + PrevVols)/(div_wts+1)
    
    # Mark Weeks as valid (1) or out-of-scope (0) or Excluded (-)
    
    if(IsWithin(CurrVolume, Roll_Vol_Around[1,weekID], constants$VOL_THRESHOLD, mult = T)) 
    {
      WeekStatus[1, weekID] = 1
    } else # Now check if the current data is form is previous or current 
    {
      if(IsWithin(CurrVolume, Roll_Vol_Prev[1,weekID], constants$VOL_THRESHOLD, mult = T) | 
         IsWithin(CurrVolume, Roll_Vol_Prev[1,weekID], constants$VOL_THRESHOLD, mult = T)) 
      {
        WeekStatus[1, weekID] = 1
      } else WeekStatus[1, weekID] = -1         
    }
    
    # Generating data for initial legs    
    initWeekID = weekID - First_Vol_Week[1] + 1
    
    if(initWeekID <= constants$NUM_INIT_WEEKS)
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
  status.matrix <- list(PriceMatrix = PriceMatrix, PriceStatus = PriceStatus, WeekStatus = WeekStatus, Baseline = Baseline, InitialVolume = InitialVolume)
  return(status.matrix)
}

#Analysis starts here...
umg_analysis <- function(multiple.upc.data, datesData)
{
  constants <- define.constants()
  UPC = unique(multiple.upc.data$UPC)
  NUM_UPC = len(UPC)
  
  PriceStatusAll <-  matrix()
  WeekStatusAll <-  matrix()
  PriceMatrixAll <- matrix()
  #Preprocessing stage...


  multiple.upc.data$activity.date <- as.Date(multiple.upc.data$activity.date,"%Y-%m-%d")

  MIN_WEEK_DATE <- datesData$Start_Week[1]
  
  MAX_WEEK_DATE <- datesData$Start_Week[len(datesData$Start_Week)]
  
  createWeekCohort <- function(album){
    for(index in 1:(constants$NUM_WEEKS-1)){
      if((album >= datesData$Start_Week[index]) && (album < datesData$Start_Week[index + 1])){
          return(datesData$Sales_Week[index])
      } 
      if(album < MIN_WEEK_DATE){
        return(datesData$Sales_Week[1])
      }
      if(album > MAX_WEEK_DATE){
        return(datesData$Sales_Week[constants$NUM_WEEKS])
      }
    }
  }
  
  multiple.upc.data$fa.week.id <- sapply(multiple.upc.data$activity.date, FUN=createWeekCohort)

  #index <- 1
  # Identify the price and volume changes
  for(index in 1:NUM_UPC){
    subset.data = multiple.upc.data[multiple.upc.data$UPC == UPC[index],]
    
    status.matrix <- process.upc(subset.data,index);
    
    if(index == 1){
      print('inside first iteration')
      PriceMatrixAll <-  t(status.matrix$PriceMatrix)
      WeekStatusAll <-  t(status.matrix$WeekStatus)
      PriceStatusAll <- t(status.matrix$PriceStatus)
    }
    else if(index > 1){
      print(paste0('inside',index,'iteration'))
      PriceMatrixAll <- rbind(PriceMatrixAll, t(status.matrix$PriceMatrix))
      WeekStatusAll <- rbind(WeekStatusAll, t(status.matrix$WeekStatus))
      PriceStatusAll <- rbind(PriceStatusAll, t(status.matrix$PriceStatus))
    }
    
    Baseline <- status.matrix$Baseline
    
    InitialVolume <- status.matrix$InitialVolume
  } # ----- Looping ends here for all UPCs
  init_base_data = cbind(Baseline, InitialVolume)
  values <- list(PriceStatusAll, WeekStatusAll, PriceMatrixAll, init_base_data)
  names(values) <- c('PriceStatus','WeekStatus','PriceMatrix','InitBaseData')
  return(values)
} # Analysis Ends here...

values = umg_analysis(multiple.upc.data, datesData)

names(values)
# Select all data released before 1 year before start data
# Create a dataframe of new computed values

#init_base_data = cbind(Baseline, InitialVolume)
write.csv(values$InitBaseData, "Base_Init_Vol.csv")


write.csv(values$PriceStatus, "PriceStatus.csv")
write.csv(values$WeekStatus, "WeekStatus.csv")
write.csv(values$PriceMatrix, "PriceMatrix.csv")

selcols = c("UPC", "PROJECT_ID", "CLEAN_R2_PRODUCT_CONFIG", "R2_STATUS", "GENRE_TYPE",  "EXPLICIT",                
           "LONGEST_TRACK_LENGTH", "PRODUCT_DURATION", "TOTAL_DISCS", "TOTAL_TRACKS", "SALES_UNITS", "REVENUE")                         
UMG_SegData = UMG_Final[, selcols]
#UMG_SegData = 
UMG_SegData$SalesVelocity = init_base_data[,"Volume"]
UMG_SegData$BasePrice = init_base_data[,"Price"]
UMG_SegData$RevPerWeek = init_base_data[,"Revenue"]

write.csv(UMG_SegData, "UMG_SegData.csv")


