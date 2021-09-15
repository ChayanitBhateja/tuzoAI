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

master.data <- read.csv(paste0(getwd(),"/myCodes/datasets/tuzoUPCDataset.csv"))

head(master.data)

master.data[order(master.data$UPC),]

head(master.data)

formatWeek <- function(week){
  return(substr(week,2,nchar(week)))
}

master.data$week <- sapply(master.data$week,formatWeek)
master.data$week <- as.integer(master.data$week)

head(master.data, 21)

#PriceMatrixDF = read.csv(paste0(getwd(),"/myCodes/datasets/Price_",DataFileSuffix,".csv"))
#PriceMatrix = as.matrix(PriceMatrixDF[2:dim(PriceMatrixDF)[2]])
#print(PriceMatrix)
#PriceMatrix[is.na(PriceMatrix)] = 0

#VolumeMatrixDF = read.csv(paste0(getwd(),"/myCodes/datasets/Volume_",DataFileSuffix,".csv"))
#VolumeMatrix = as.matrix(VolumeMatrixDF[2:dim(VolumeMatrixDF)[2]])
#VolumeMatrix[is.na(VolumeMatrix)] = 0

# UPC = PriceMatrixDF$UPC

UPC = unique(master.data$UPC)

# NUM_UPC = dim(PriceMatrix)[1]
#NUM_UPC = length(UPC)

#NUM_WEEKS = length(unique(master.data$week))

Unique_Prices = matrix(0,1,NUM_WEEKS+3)

#FinalPrices = PriceMatrix
#print(FinalPrices)
StartTime0 = proc.time()

master.data[(master.data$UPC == 28947644927) & (master.data$week == 146),'price']


priceValidation <- function(upc.data)
{
  #browser('testing this function')
  
  #NUM_UPC = length(UPC)
  
  #NUM_UPC
  
  NUM_WEEKS = length(unique(master.data$week))
  
  # Determine price pulse windows - in every 26 week, 0 = Normal Price, 1 = Promotion price
  
  FA_Week = 0
  numFixedPrices = 0
  
  for(weekID in 1:NUM_WEEKS)
  {    
    # Identify price change     
    currPrice <- upc.data[data$week == weekID,'price']
    currVol <- upc.data[data$week == weekID,'volume']
    if(weekID < 5){
      print(paste0("currPrice: ", currPrice, "CurrVol: ",currVol))
    }
    if(currVol > 0)      
    {
      if(FA_Week == 0) FA_Week = weekID
      latestPrice = currPrice
    } else if(FA_Week > 0) # No volume so price will also be absent
    {
      upc.data[upc.data$week == weekID,'price'] = latestPrice
      numFixedPrices = numFixedPrices + 1
    }
  } # End iterating over all weeks for one UPC
  
  # Determine number of unique price points
  
  #unique_prices = sort(unique(FinalPrices[1,]))  
  unique_prices <- sort(unique(upc.data$price))
  print(paste0('uniquePrices: ',unique_prices[1:5]))
  
  num_prices = length(unique_prices)
  print(paste0('num_prices: ',num_prices[1:5]))
  if(unique_prices[1] == 0) num_prices = num_prices-1
  
  Unique_Prices[1,1] = FA_Week
  Unique_Prices[1,2] = numFixedPrices
  Unique_Prices[1,3] = num_prices
  
  Unique_Prices[1,4:(4+num_prices-1)] = unique_prices[unique_prices > 0]
  
} # End iterating over all UPC

write.csv(data.frame(data), paste0("Price_", DataFileSuffix, "_Fixed_UPC_",upc,".csv"), row.names=F)
write.csv(data.frame(Unique_Prices), paste0("UniquePRC_", DataFileSuffix,"_UPC_",upc, ".csv"), row.names=F)

#preparing Subsetted Data for price validation...
subsetted.data <- master.data[master.data$UPC == 28947644927,]
priceValidation(subsetted.data)

EndTime0 = proc.time()
ET0 = round(EndTime0 - StartTime0,2)
print(paste0("Processed ", NUM_UPC, " UPCs in ", ET0[3], " secs"))
