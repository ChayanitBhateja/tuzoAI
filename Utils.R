# Demand forecasting simulation for Pernod Ricard USA

# =======================================================================================================
# Revision History
# v0  19.Aug.2013 gdalvi  Initial Exploratory scrip
# =======================================================================================================

library(data.table)
library(reshape)
library(date)
library(sqldf)
library(plyr)

len <- function(X)
{
  return(length(X))
}

size <- function(X)
{
  num = dim(X)
  if(is.null(num)) num = length(X)
  return(X)  
}

bt <- function()
{
  browserText()
}

tb <- function()
{
  traceback()
}

sortunique <- function(x)
{
  sort(unique(x))
}

IsBetween <- function(value, LB, UB)
{
   is_between = if(value >= LB & value <= UB) T else F
   return(is_between)
}

IsWithin <- function(value, target, tolerence, mult = F)
{
   if(mult == F)  is_within = if(value  >= target - tolerence & value <= target + tolerence) T else F
   if(mult == T)  is_within = if(value  >= target*(1-tolerence) & value <= target*(1+tolerence)) T else F
   
   return(is_within)
}

ReadFile <- function(filename, folder)
{
  print(paste0("Reading the file: ", filename, " from directory: ", folder))
  
  StartTime0 = proc.time()
  DF = read.csv(filename, header = T, as.is = T);
  EndTime0 = proc.time()
  ET0 = round(EndTime0 - StartTime0,2)
  print(paste0("File read in ", ET0[3], " secs"))
  
  return(DF)
}
WriteFile <- function(filename, folder)
{
  print(paste0("Writing the file: ", filename, " from directory: ", folder))
  
  StartTime0 = proc.time()
  setwd(folder)
  ##write.csv(filename,paste0(filename,".csv"), row.names=F)
  write.table(filename,file=paste0(filename,".csv"), sep = ",", row.names=FALSE)
  EndTime0 = proc.time()
  ET0 = round(EndTime0 - StartTime0,2)
  print(paste0("File write in ", ET0[3], " secs"))
  
  return(0)
}


RemoveSpecialChars<- function(coldata)
{
  temp = gsub("\\$","", coldata) # Remove dollar sign
  temp = gsub(",", "", temp) # Remove any commas 
  temp = gsub("%", "", temp) # Remove any commas 
  
  temp = gsub("\\)","", temp) # Remove ) sign
  temp = gsub("\\(","-", temp) # Replace ( with - sign
  temp = gsub(" ", "", temp) # Remove any blanks
  
  temp[temp==""] = "0"
  num = as.numeric(temp)
  
  return(temp)
}

SummarizeData <- function(MasterData, OutfileName, cols_to_clean = NULL)
{
  ColNames = colnames(MasterData) # Updated list of columns after dropping unwanted ones
  
  write(paste0("Dimensions: ", dim(MasterData)), file = OutfileName, append = T)
  write(paste0("Modes & Classes for: ", dim(MasterData)[2], " colums"), file = OutfileName, append = T)

  for(c in 1:length(ColNames))
  {
    numUniques = sum(is.na(unique(MasterData[c]))==F)
    write(paste(ColNames[c], mode(MasterData[[c]]), class(MasterData[[c]]), numUniques, sep = ","), file = OutfileName, append = T)
  }
  
# TODO Revive  
#   if(is.null(cols_to_clean) == T)
#   {
#     print("Fixing columns in Master Data...")
#     cols_to_clean = unlist(strsplit(cols_to_clean, " "))
#     
#     for(c in 1:length(cols_to_clean))
#     {
#       col = cols_to_clean[c]
#       MasterData[[col]] = RemoveSpecialChars(MasterData[[col]])
#     }
#   }
  
  
  print(paste0("Computing Statistics..."))
  
  # Get sum of all numeric rows to identify columns that have numeric issues like NA, NaN etc
  
  probs = seq(0,1,0.25)
  quantCols = as.character(c(probs*100))
  numQuantiles = length(quantCols)
  
  cnames = "ColName, numNA, numNaN, numUniques, Avg, STDEV "
  for(z in 1:numQuantiles) 
  {
    cnames = paste(cnames, quantCols[z], sep=",")
  }
  write(cnames, file = OutfileName, append = T) 
  
  
  for(cId in 1:length(ColNames))
  {
    colname = ColNames[cId]
    currCol = MasterData[[colname]]
    
    if(is.numeric(currCol) == T)
    {
      Avg = mean(currCol, na.rm = T)
      STDev = sd(currCol, na.rm = T)
      
      quant = quantile(currCol, probs, na.rm=T)
      numNA = sum(is.na(currCol) == T)
      numNaN = sum(is.nan(currCol) == T)
      numUniques = length(unique(currCol))
      
      options(warn = -1)
      
      Col_Names = (cId == 1)
      
      df = data.frame( colname, numNA, numNaN, numUniques, Avg, STDev)
      
      offset = dim(df)[2]
      
      for(z in 1:numQuantiles) df[1,(offset+z)] = quant[z]
      
      write.table(df, file = OutfileName, sep = ",", append = T, col.names = F, row.names = F)   
      options(warn = 1)
    }
  }
}


# $ "": tpty_payment  copay	sales	acq_cost_a	adj_host_fill_q	tot_base_cost
# numeric: awp
# ()% : awp_rate
# 
# tp = AllData$awp_rate
# 
# xa = grep ("[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]", tp, value=T)
# length(xa)# 
# xa = grep ("[]*+?[^{|(\\#%&~_/<=>'!,:;`\")}@]", tp, value=T)
# length(xa)
# 
# tt = tp[1:100]
# tt2 = gsub("\\$","",tt)
# tt2[tt2==""] = "0.00"
# tt3 = as.numeric(tt2)
# 
# tt2 = gsub("\\$","",tp)
# tt2[tt2==""] = "0.00"
# tt3 = as.numeric(tt2)
