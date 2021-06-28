library(tidyverse)
library(dplyr)

#Read data
sp500 <- read_csv(
  "data/assignment_3_sp500_constituents_with_daily_mdata.csv", col_types = cols()
)

ff3f <- read_csv(
  "data/F-F_Research_Data_Factors_daily.csv", col_types = cols()
)

#Adjust date format. Build capm based on 2019 data
ff3f$date <- as.Date(as.character(ff3f$datenum), format='%Y%m%d')
ff3f$`Mkt-RF` <- ff3f$`Mkt-RF`/100
ff3f$RF <- ff3f$RF/100


#Calculate a beta for each company
market_model   <- data.frame(cusip = unique(sp500$cusip), beta =NA)
ff3f_2019      <- ff3f[ff3f$datenum > 20181231 & ff3f$datenum <20200101,]

for(i in 1:length(market_model$cusip)){
  cusip            <- market_model$cusip[i]
  curr.data        <- sp500[sp500$cusip %in% cusip, c("date", "ret")]
  capm_data        <- left_join(ff3f_2019, curr.data, by=c("date"))
  capm_data$excess <- capm_data$ret - capm_data$RF
  
  #If there are less than 100 observations do not calculate beta
  if(sum(is.na(capm_data$ret))>99){
    next
  }
  
  #Get beta
  model <- lm(excess ~ `Mkt-RF` , data = capm_data)
  market_model$beta[i] <- model$coefficients[[2]]
}
market_model <- market_model[!is.na(market_model$beta),]


#Calculate the expected returns for the year 2020 for each company
AR           <- sp500[,c("cusip", "permno", "date", "ret")]
AR           <- AR[AR$date > as.Date('2019-12-31') & AR$date  < as.Date('2021-01-01'),]
AR           <- AR[AR$cusip %in% market_model$cusip,]
AR$ex_return <- NA
AR$beta      <- NA
AR$mkt       <- NA

for(i in 1:length(AR$cusip)){
  cusip <- AR$cusip[i]
  beta  <- market_model$beta[market_model$cusip %in% cusip]
  mkt   <- ff3f$`Mkt-RF`[ff3f$date == AR$date[i]]
  rf    <- ff3f$RF[ff3f$date == AR$date[i]]
  
  #Calculate expected returns based on market model
  AR$beta[i]        <- beta
  AR$ex_return[i] <- rf + mkt*beta
  AR$mkt[i] <- mkt

  
}


save(AR, file ="data/abnormal_returns.RDA")

  