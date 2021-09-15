#Function to calculate price changes and promotion periods based on weekly sales data

price_analysis <- function(mypath){
  
  #Reading data
  data_df <- read.csv(mypath, header = TRUE)
  
  
  #Calculating UnitPrice: UnitPrice = Revenue/Volume
  data_df$UnitPrice <- data_df$Revenue/data_df$Volume
  data_df$UnitPrice <- signif(data_df$UnitPrice, 2)
  
  
  #Calculating percentage of price change and remarks
  orginalprice <- data_df$WeekNum[1]
  perc_dec <- 0
  perc_inc <- 0
  data_df$Remarks <- NA
  data_df$Remarks[1] <- 'Original price'
  
  for (row in 2:nrow(data_df)){
    if (data_df$UnitPrice[row] > orginalprice){
      perc_inc <- signif(100*(data_df$UnitPrice[row] - orginalprice)/orginalprice, 2)
      cat('Price increased by', perc_inc, '% in week', row, '\n')
      orginalprice <- data_df$UnitPrice[row]
      data_df$Remarks[row] <- 'Price Increased'
    }
    else if (data_df$UnitPrice[row] < orginalprice){
      perc_dec <- signif(100*(orginalprice - data_df$UnitPrice[row])/orginalprice, 2)
      cat('Price decreased by', perc_dec, '% in week', row, '\n')
      
      #Remarks for promotions
      if (perc_dec >= 47 && perc_dec <= 53)
        data_df$Remarks[row] <- "50% off"
      else if (perc_dec >= 31 && perc_dec <= 36)
        data_df$Remarks[row] <- "Buy 2 Get 1 Free"
      else if (perc_dec >= 22 && perc_dec <= 27)
        data_df$Remarks[row] <- "50% off on 2nd Item"
      else if (perc_dec >= 7 && perc_dec <= 12)
        data_df$Remarks[row] <- "10% off"
      else data_df$Remarks[row] <- 'Other promotions'
    }
    else data_df$Remarks[row] <- 'No change in price'
  }
  return(data_df)
}

#Function calling
finaldata <- price_analysis(paste0(getwd(),"/myCodes/createdDatasets/Price_Analysis.csv"))

View(finaldata)
