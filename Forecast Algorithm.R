# Importing data from excel.
library(readxl)
Long_Beach_CA_Weather_History_LONG_BEACH_AIRPORT_STATION_33_82_N_118_09_W_Daily_Data_Jan_2022_Jan_2023 <- read_excel("C:/Users/ASUS/Desktop/Long Beach, CA Weather History - LONG BEACH AIRPORT STATION - 33.82 °N, 118.09 °W - Daily Data Jan 2022 - Jan 2023.xlsx")
View(Long_Beach_CA_Weather_History_LONG_BEACH_AIRPORT_STATION_33_82_N_118_09_W_Daily_Data_Jan_2022_Jan_2023)

# Creating a data frame in R.
dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
temps <- Long_Beach_CA_Weather_History_LONG_BEACH_AIRPORT_STATION_33_82_N_118_09_W_Daily_Data_Jan_2022_Jan_2023$AvgTemp
df <- data.frame(date = dates, temp = temps)
df

# Creating time series data.
library(tseries)
ts_temps <- ts(df$temp, start = as.Date("2022-01-01"), frequency=365)
ts_temps

# Create a time series plot
plot(ts_temps, main = "Daily Temperature Data", xlab = "Date", ylab = "Temperature")

library(forecast)
Pacf(ts_temps, y=c(1,1))
# Shows no seasonality. No recurrent spikes.  

Acf(ts_temps, y=c(1,1))
# Shows no seasonality but shows trend. Dying down spikes.

library(tseries)
adf.test(ts_temps)
# P-value is 0.553 > 0.05,  which shows that series is not stationary. Therefore, trend component is present.
ndiffs(ts_temps)
acf(diff(ts_temps,1), lag.max = 365)
plot(diff(ts_temps,1))
adf.test(diff(ts_temps,1))
# P-value is 0.01 < 0.05,  which shows that series is stationary after first difference.
nsdiffs(ts_temps)
# No seasonal difference, therefore no seasonal component.

library(uroot) 
#ch.test(ts_temps)
#Error - Error in seq_len(n - i) : argument must be coercible to non-negative integer

library(trend)
library(extraDistr)
smk.test(ts_temps)


# Test for irregular fluctuations.
library(tseries)
ljung_box <- Box.test(ts_temps, lag = 10, type = "Ljung-Box")
print(ljung_box)
# P-value is less than 0.05 suggesting the presence of irregular fluctuations. This confirms visual analysis of the plot.

#Q1 end---------------------------------------------------------------------------------
#Data -
# Not stationary, stationary at first difference 
# Not seasonal 
# Trend is present 
# Irregular fluctuations are present

#Models to use - 
#1. Moving average - it works by smoothing out the fluctuations in the data over a period of time to reveal the underlying trend. 
#2. Simple exponential smoothing - basic form of exponential smoothing that can be used for data with a trend but no seasonality. It involves assigning exponentially decreasing weights to past observations and extrapolating the trend into the future.
#3. Holt's method - extends simple exponential smoothing by adding a second component to model the trend. It can be used when the trend in the data is linear.

#Model 1 - 
ma3_temps <- ma(ts_temps,3) 
plot(ts_temps, main = "Daily Temperature Data", xlab = "Date", ylab = "Temperature")
lines(ma3_temps,col=2,lwd=2)
ma7_temps <- ma(ts_temps,7) 
lines(ma7_temps,col=3,lwd=2)
ma10_temps <- ma(ts_temps,10) 
lines(ma10_temps,col=4,lwd=2)
legend('topleft',c('Actual','ma3','ma7','ma10'),col=1:4,lwd=1,cex=0.5)

#Model 2 -
ses_temps <- ses(ts_temps, initial = 'simple')
# plot(ts_temps, main = "Daily Temperature Data", xlab = "Date", ylab = "Temperature")
lines(ses_temps$fitted,col=5,lwd=2)
# summary(ses_temps)


#Model 3 - 
holt_temps <- holt(ts_temps, initial = 'simple')
# plot(ts_temps, main = "Daily Temperature Data", xlab = "Date", ylab = "Temperature")
lines(holt_temps$fitted,col=6,lwd=2)
summary(holt_temps)
# forecast_holt <- forecast(holt_temps)
# forecast_holt
# plot(forecast_holt, main = "Holt's Method Forecast")
# lines(ts_temps, col = "blue")
legend('topleft',c('Actual','ma3','ma7','ma10','ses','Holts'),col=1:6,lwd=1,cex=0.5)


#Q2 end---------------------------------------------------------------------------------
#Splitting the dataset into train and test
training_temps<-ts_temps[1:(length(ts_temps)*0.8)]
training_temps

testing_temps<-ts_temps[(length(ts_temps)*0.8+1):(length(ts_temps))]
testing_temps

#Model 1
#ttr ma model
library(TTR)
ma3_training_temps_ttr<-SMA(training_temps, 3)
accuracy(forecast(ma3_training_temps_ttr,h=length(testing_temps)), testing_temps)
ma3_training_temps_ttr
forecast(ma3_training_temps_ttr,h=length(testing_temps))

summary(ma3_training_temps_ttr)
autoplot(forecast(ma3_training_temps_ttr))

#Model 2
ses_training_temps<-ses(training_temps, initial="simple", h=length(testing_temps))
ses_training_temps

summary(ses_training_temps)
accuracy(ses_training_temps, testing_temps)
autoplot(ses_training_temps)

#Model 3
holt_training_temps <- holt(training_temps, initial = "simple",  h=length(testing_temps))
holt_training_temps

summary(holt_training_temps)
accuracy(holt_training_temps, testing_temps)
autoplot(holt_training_temps)

#Q4 end---------------------------------------------------------------------------------
tsdisplay(diff(ts_temps))
ndiffs(ts_temps)
nsdiffs(ts_temps)

adf.test(ts_temps)
kpss.test(ts_temps)

library(uroot)
#ch.test(ts_temps)

#Q5 end---------------------------------------------------------------------------------
tsdisplay(diff(ts_temps,1))

auto.arima(ts_temps, trace=TRUE)
#ARIMA(1,1,1)
tstemps_arima111<-arima(ts_temps, order=c(1,1,1))
summary(tstemps_arima111)

#ARIMA(1,1,2)
tstemps_arima112<-arima(ts_temps, order=c(1,1,2))
summary(tstemps_arima112)

checkresiduals(tstemps_arima111)
checkresiduals(tstemps_arima112)

# Install the lmtest package
#install.packages("lmtest")

# Load the lmtest package
library(lmtest)
coeftest(tstemps_arima111)
coeftest(tstemps_arima112)

accuracy(tstemps_arima111)
accuracy(tstemps_arima112)
#Q6 end---------------------------------------------------------------------------------

forecast_tstemps<-forecast(tstemps_arima112, h=30)

plot(forecast_tstemps, main="Forecast next 30 days temperature", xlab="Day", ylab="Temperature(F)")
lines(fitted(tstemps_arima112), col=2, lwd=2)
legend("topleft", c("Actual","Fitted","Forecast"), col=c("black","red","blue"), lwd=2, cex=0.6)
forecast_tstemps

#-----
#Appendix
forecast(ma3_training_temps_ttr,h=10)
forecast(ses_training_temps, h = 10)
forecast(holt_training_temps, h = 10)
forecast_tstemps_111<-forecast(tstemps_arima111, h=10)
forecast_tstemps_111
forecast_tstemps_112<-forecast(tstemps_arima112, h=10)
forecast_tstemps_112
