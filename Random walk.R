#Random walk model

#A time series model used for forecasting stock prices, step taken each day, previous day's price is used here.
#based on the current price.
#Pt = Pt-1 + drift + error term

library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(dplyr)

stock_prices <- getSymbols("ASIANPAINT.NS",from="2020-01-01",to="2024-10-20",auto.assign = FALSE)
stock_prices <- Cl(stock_prices) #closing prices
#calculate simple returns
stock_returns <- Return.calculate(stock_prices,method="discrete")
stock_returns <- na.omit(stock_returns)


#using log method for calculation
#calculate simple returns
stock_returns_log <- Return.calculate(stock_prices,method="log")
stock_returns_log <- na.omit(stock_returns)


set.seed(123) #for reproducability

#mean and std dev of simple returnd
mu <- mean(stock_returns)
sigma <- StdDev(stock_returns)

#generate random walk model
n <- 100 #forecast for 100 days
random_walk <- numeric(n) #Initializing a place to keep the variable (vector)

#TIMESERIES models are stochastic in nature. They cannot be sorted. Order of occurence is very important.
#GEOMETRIC RW MODEL
random_walk[1] <- as.numeric(last(stock_prices)) #start from the last known price
last(stock_prices)
for (i in 2:n) {
  random_walk[i] <- random_walk[i-1] * (1+ mu + sigma * rnorm(1))
  
}

#Projections done
#To make tables with future dates and map these findings

future_dates <- seq(as.Date("2024-10-21"), by = "days", length.out =n)
class(future_dates)

random_walk_df <- data.frame(Date = future_dates, Price = random_walk)

#cbind use 
# cc <- as.data.frame(future_dates)
# class(cc)
# cc$random <- random_walk
# cc[1]

ggplot(random_walk_df,aes(x = Date, y = Price))+
  geom_line(color ="blue")+
  ggtitle("Random walk MOdel simulation for ASIANPAINT.NS stock")+
  xlab("Date")+ylab("Prices")






# RW MODEL WITH DRIFT
#Model is Yt = Yt-1 + mu + Sigma*rnorm()

stock_prices <- getSymbols("ASIANPAINT.NS",from="2020-01-01",to="2024-10-20",auto.assign = FALSE)
stock_prices <- Cl(stock_prices) #closing prices
#calculate simple returns
stock_returns <- Return.calculate(stock_prices,method="discrete")
stock_returns <- na.omit(stock_returns)


set.seed(123) #for reproducability

#mean and std dev of simple returnd
mu <- mean(stock_returns)
sigma <- StdDev(stock_returns)

#generate random walk model
n <- 100 #forecast for 100 days
random_walk <- numeric(n) #Initializing a place to keep the variable (vector)
random_walk[1] <- as.numeric(last(stock_prices)) #start from the last known price
#last(stock_prices)
for (i in 2:n) {
  random_walk[i] <- random_walk[i-1]+ mu + sigma * rnorm(1) #RWM with Drift as mean
}
#Projections done
#To make tables with future dates and map these findings

future_dates <- seq(as.Date("2024-10-21"), by = "days", length.out =n)
class(future_dates)

random_walk_df <- data.frame(Date = future_dates, Price = random_walk)


ggplot(random_walk_df,aes(x = Date, y = Price))+
  geom_line(color ="blue")+
  ggtitle("RW with Drift simulation for ASIANPAINT.NS stock")+
  xlab("Date")+ylab("Prices")
