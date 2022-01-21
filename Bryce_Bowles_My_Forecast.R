library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)

#month_year - have to make sure youve got a csv with dates column in 01/01/2020 format (month_year)
#core_deposits

#read in data provided (training data)
TSFC <- read_csv("Core_Deposits.csv")
glimpse(TSFC)
dates <- as.Date(TSFC$month_year, 
             format= "%m/%d/%Y")

#read in the hold out data (validation data)
#"Core_Deposits_Validate.csv" contains just the holdout data
TSFCV <- read_csv("Core_Deposits_Validate.csv")
glimpse(TSFCV)
dates <- as.Date(TSFCV$month_year, 
                 format= "%m/%d/%Y")

#convert them to time series formats
training__data <- ts(TSFC$core_deposits, frequency=12, start=c(1988,11), end=c(1992,09))
validation__data <- ts(TSFCV$core_deposits, frequency=12, start=c(1992,09), end=c(1993,08))
all__data <- ts(TSFC$core_deposits, frequency=12, start=c(1992,09)) #no end because all data is in "Core_Deposits.csv" including hold out

#to view the data
plot.ts(training__data)
plot.ts(validation__data)
plot.ts(all__data)

#taking the LN
log_training_data <- log(training__data)
plot.ts(log_training_data)

#Naive model
naive_mod <- naive(training__data, h = 12)
summary(naive_mod)
plot(naive_mod)

#Simple Exponential Smoothing 
se_model <- ses(training__data, h = 12)
summary(se_model)
plot(se_model)

#Holts ES
holt_model <- holt(training__data, h = 12)
summary(holt_model)
plot(holt_model)
print(holt_model)

#ARIMA
arima_model <- auto.arima(training__data)
summary(arima_model)
