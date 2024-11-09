getwd()
setwd("D:/R prog/")
library(readxl)

fivesec <- read_excel("D:/R prog/FIVE_SECURITIES.xlsx", sheet = 1, range = "A2:F494")
library(fPortfolio)
library("timeSeries")
library("quantmod")
library("dplyr")
library(PerformanceAnalytics)
library(ggplot2)
library(readxl)
library(zoo)
sec_prices <- data.frame(fivesec)
head(sec_prices)
class(sec_prices)                         
sec_prices$dt

row.names(sec_prices)<-sec_prices$dt
head(sec_prices)
head(sec_prices[1:2])

sec_prices1 <- sec_prices[c(2:6)]
# sec_prices2 <- as.zoo(sec_prices1)
sec_prices3 <- as.timeSeries(sec_prices1)
portfolio1 <- Return.calculate(sec_prices3,method = 'discrete')
#discrete = arithmetic mean 
head(portfolio1)
#one way to omit NA
portfolio1<-na.omit(portfolio1)
portfolio1
#second way to omit NA 
portfolio1<-portfolio1[-1,]
head(portfolio1)
#to remove a range 
# head(portfolio1[-c(1:5)])
#calculating efficient frontier
efficient_frontier <- portfolioFrontier(portfolio1,'setRiskFreeRate<-'(portfolioSpec(),.05/252),constraints ="longonly")

#efficient frontier 1

plot(efficient_frontier,c(1,3,8))

#covariance of portfolio
cov(portfolio1)
#return mean
mean(portfolio1)
#min variance portfolio
min_var_portfolio <- minvariancePortfolio(portfolio1,portfolioSpec(),constraints = "longonly")
#getting weights 
wts_min_var_pf <-getWeights(min_var_portfolio)

#max return 
optimum_portfolio <-tangencyPortfolio(portfolio1,`setRiskFreeRate<-`(portfolioSpec(),.05/252),constraints = "longonly")
##getting weights 
wts_opt_pf <- getWeights(optimum_portfolio)

#getting data from yahoo finance portal 
#live data internet connection is required 
#quant mod is for extraction of data from yahoo finance 

icici<-getSymbols("ICICIBANK.NS",from = "2022-12-15", to ="2023-12-14",auto.assign = FALSE)
bajaj<-getSymbols("BAJAJ-AUTO.NS",from = "2022-12-15", to ="2023-12-14",auto.assign = FALSE)
bpcl<-getSymbols("BPCL.NS",from = "2022-12-15", to ="2023-12-14",auto.assign = FALSE)
asianpaints<-getSymbols("ASIANPAINT.NS",from = "2022-12-15", to ="2023-12-14",auto.assign = FALSE)
nestle<-getSymbols("NESTLEIND.NS",from = "2022-12-15", to ="2023-12-14",auto.assign = FALSE)

head(icici)
class(icici)

#we need only adjusted close 

icici <- Ad(icici)
head(icici)
bajaj <- Ad(bajaj)
bpcl <- Ad(bpcl)
asianpaints <- Ad(asianpaints)
nestle <- Ad(nestle)

#if we have to merge and there is NA for some entries we can use, LOCF: last occurance carried forward 

icici <- na.locf((icici))
bajaj <- na.locf((bajaj))
asianpaints<-na.locf((asianpaints))
bpcl <- na.locf((bpcl))
nestle <- na.locf((nestle))
(dim(nestle))

library(tidyverse)
five_stks <- merge.xts(icici,bajaj,asianpaints,bpcl,nestle)
head(five_stks)
class(five_stks)

#need to convert to data frame so that we can have date also alongside
five_stks_df <- as.data.frame(five_stks)
tail(five_stks_df)

#to calculate returns form the data frame 
five_stks_ret <- Return.calculate(five_stks,method = "discrete")
# to omit na 
five_stks_ret <- five_stks_ret[-1,]
head(five_stks_ret)
class(five_stks_ret)

# write as data frame to convert to excel
library(writexl)
write_xlsx(five_stks_df,"five_stocks.xlsx")

#to calculate the return portfolio 
five_stks_ret_p <- Return.portfolio(five_stks_ret,weights=c(0.2,0.2,.2,.2,.2),geometric = FALSE)
class(five_stks_ret_p)
five_stks_ret_p <- as.timeSeries(five_stks_ret_p)
#to get average return of a daily basis 
mean(five_stks_ret_p)


#efficient frontier 2

class(five_stks_ret_p)
efficient_frontier <- portfolioFrontier(five_stks_ret_p,'setRiskFreeRate<-'(portfolioSpec(),.05/252),constraints ="longOnly")
plot(efficient_frontier,c(1,2,3,4,7,8))

#to find the variance covariance matrix
var_cov <- cov(five_stks_ret)
sd(five_stks_ret_p) #portfolio std dev
five_stks_df$dt <- row.names(five_stks_df)
head(five_stks_df)
row.names(five_stks_df)
mean(five_stks_ret_p)
###OVER
# to get help 
?Return.portfolio()




#############
# to import multiple tickers at once #

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
symbols =c("ICICIBANK.NS","BAJAJ-AUTO.NS","BPCL.NS","ASIANPAINT.NS","NESTLEIND.NS")
#extracting share price data for all the tickers together using a loop
#from and to dates are required
#lapply makes the function applicable to each ticker in symbols repetitively 
#pportfolio has stock data for all tickers in symbols object

portfolio_stk <- lapply(symbols,function(X){
  getSymbols(X,from="2022-06-01",to="2024-05-31",auto.assign = FALSE)
  
})

head(portfolio_stk)
class(portfolio_stk)
#converting the list datastructure into a data frame 
portfolio_stk_df <- as.data.frame(portfolio_stk)
class(portfolio_stk_df)
#we need only adjusted returns 
portfolio_stk_df<-Ad(portfolio_stk_df)
head(portfolio_stk_df)
class(portfolio_stk_df)
#converted to a timeseries 
portfolio_stk_ts<-as.timeSeries(portfolio_stk_df)
portfolio_stk_ret<-Return.calculate(portfolio_stk_ts)
head(portfolio_stk_ret)
portfolio_stk_ret<-na.omit(portfolio_stk_ret)
portfolio_stk_ret
class(portfolio_stk_ret)
#converting to dataframe 
pf_stk_ret_df <- as.data.frame(portfolio_stk_ret)

portfolio_stk_ret <-na.omit(portfolio_stk_ret)
portfolio_stk_ret_pf <- Return.portfolio(portfolio_stk_ret,c(.2,.2,.2,.2,.2),geometric = FALSE)
head(portfolio_stk_ret_pf)
mean(portfolio_stk_ret_pf)
var_cov1<-cov(portfolio_stk_ret)
sd(portfolio_stk_ret_pf)

###bulk loading is over 

library(writexl)
write_xlsx(pf_stk_ret_df,"portfolio_return.xlsx")
pf_stk_ret_df$dt <- row.names(pf_stk_ret_df)
# how to change the order of the columns 

#efficient frontier 3
efficient_frontier<- portfolioFrontier(portfolio_stk_ret,'setRiskFreeRate<-' (portfolioSpec(),.05/252),constraints ="longOnly")
plot(efficient_frontier,c(1,2,3,7,8))



# NOTES 
# To make efficient frontiers we need data in time series format 
# To write in excel we need to write in data frame format 


