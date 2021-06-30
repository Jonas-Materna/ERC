library(tidyverse)
library(dplyr)

#Read and prep data
eps <- read_csv(
  "data/assignment_3_ibes_eps_analyst_estimates.csv", col_types = cols()
)
#Keep only observations from 2020
eps <- eps[eps$anndats_act > as.Date("2019-12-31") & eps$anndats_act < as.Date("2021-01-01"),]


#If there are multiple estimates for a quarter I take the most recent
data  <-eps %>%
          group_by(cusip, anndats_act) %>%
          filter(statpers == max(statpers)) %>%
          ungroup()


#Keep the needed variables an calculate unexpected returns
data     <- data[, c("cname", "cusip","medest","actual","anndats_act", "fpedats", "statpers", "stdev", "numest")]
data$UE  <- data$actual-data$medest
data$CAR <- NA
data$CAR_naive <- NA
data$beta <- NA

load("data/abnormal_returns.Rda")


#Calculate the CAR for each quarter
for(i in 1:length(data$cname)){
  cusip <- data$cusip[i]
  currdata <- data[data$cusip %in% cusip,]
  currret <- AR[AR$cusip %in% cusip,]
  
  start_window <- data$anndats_act[i]-3
  end_window   <- data$anndats_act[i]+3
  window       <- start_window:end_window
  
  CAR <- NA
  if(length(currret$ex_return[currret$date %in% window])>0){
    CAR       <- sum(currret$ex_return[currret$date %in% window])
    CAR_naive <- sum(currret$ret[currret$date %in% window]
                     - currret$mkt[currret$date %in% window])
  }


  data$beta[i] <- currret$beta[1]
  data$CAR[i]  <- CAR
  data$CAR_naive[i]  <- CAR_naive
  
}



#Delete outliers
data <- data[data$UE>quantile(data$UE, 0.025) & data$UE<quantile(data$UE, 0.975),]
data <- data[data$CAR_naive>quantile(data$CAR_naive, 0.025) & data$CAR_naive<quantile(data$CAR_naive, 0.975),]

#Estimate ERC in the cross-section
lm(CAR_naive ~ UE, data = data)


#Get firm size and leverage
cmp <- read.csv("data/assignment_3_cmp_fundamentals.csv")
cmp <- cmp[cmp$fyear ==2020,]
data$at <- NA
data$dt <- NA
data$ceq <- NA
for(i in 1:length(data$cname)){
  curname <- data$cname[i]
  
  #Fuzzy match
  rownu <- NA
  rownum <- agrep(curname, cmp$conm, max.distance = 0.1, costs = NULL,
                  ignore.case = FALSE, value = FALSE, fixed = TRUE,
                  useBytes = FALSE)[1]
  if(!is.na(rownum)){
    data$at[i] <- cmp$at[rownum]
    data$dt[i] <- cmp$dt[rownum]
    data$ceq[i]<- cmp$ceq[rownum]
  }
}




#Run regressions
leverage<-data%>%
  mutate(quantile = ntile(leverage, 5))
q1_leverage <- lm(CAR_naive ~ UE, data = leverage[leverage$quantile==1,])$coefficients[[2]]
q2_leverage <- lm(CAR_naive ~ UE, data = leverage[leverage$quantile==2,])$coefficients[[2]]
q3_leverage <- lm(CAR_naive ~ UE, data = leverage[leverage$quantile==3,])$coefficients[[2]]
q4_leverage <- lm(CAR_naive ~ UE, data = leverage[leverage$quantile==4,])$coefficients[[2]]
q5_leverage <- lm(CAR_naive ~ UE, data = leverage[leverage$quantile==5,])$coefficients[[2]]

size<-data%>%
  mutate(quantile = ntile(at, 5))
q1_size <- lm(CAR_naive ~ UE, data = size[size$quantile==1,])$coefficients[[2]]
q2_size <- lm(CAR_naive ~ UE, data = size[size$quantile==2,])$coefficients[[2]]
q3_size <- lm(CAR_naive ~ UE, data = size[size$quantile==3,])$coefficients[[2]]
q4_size <- lm(CAR_naive ~ UE, data = size[size$quantile==4,])$coefficients[[2]]
q5_size <- lm(CAR_naive ~ UE, data = size[size$quantile==5,])$coefficients[[2]]


beta<-data%>%
  mutate(quantile = ntile(beta, 5))
q1_beta <- lm(CAR_naive ~ UE, data = beta[beta$quantile==1,])$coefficients[[2]]
q2_beta <- lm(CAR_naive ~ UE, data = beta[beta$quantile==2,])$coefficients[[2]]
q3_beta <- lm(CAR_naive ~ UE, data = beta[beta$quantile==3,])$coefficients[[2]]
q4_beta <- lm(CAR_naive ~ UE, data = beta[beta$quantile==4,])$coefficients[[2]]
q5_beta <- lm(CAR_naive ~ UE, data = beta[beta$quantile==5,])$coefficients[[2]]


noise<-data%>%
  mutate(quantile = ntile(stdev, 5))
q1_noise <- lm(CAR ~ UE, data = noise[noise$quantile==1,])$coefficients[[2]]
q2_noise <- lm(CAR ~ UE, data = noise[noise$quantile==2,])$coefficients[[2]]
q3_noise <- lm(CAR ~ UE, data = noise[noise$quantile==3,])$coefficients[[2]]
q4_noise <- lm(CAR ~ UE, data = noise[noise$quantile==4,])$coefficients[[2]]
q5_noise <- lm(CAR ~ UE, data = noise[noise$quantile==5,])$coefficients[[2]]

coverage<-data%>%
  mutate(quantile = ntile(numest, 5))
q1_coverage <- lm(CAR ~ UE, data = coverage[coverage$quantile==1,])$coefficients[[2]]
q2_coverage <- lm(CAR ~ UE, data = coverage[coverage$quantile==2,])$coefficients[[2]]
q3_coverage <- lm(CAR ~ UE, data = coverage[coverage$quantile==3,])$coefficients[[2]]
q4_coverage <- lm(CAR ~ UE, data = coverage[coverage$quantile==4,])$coefficients[[2]]
q5_coverage <- lm(CAR ~ UE, data = coverage[coverage$quantile==5,])$coefficients[[2]]

#Make a plot
plot(data$CAR_naive, data$UE,
     xlab = "CAR", ylab = "UE",)
abline(lm(UE ~ CAR_naive, data = leverage[leverage$quantile==1,]), col = "blue")
abline(lm(UE ~ CAR_naive, data = leverage[leverage$quantile==5,]), col = "red")
