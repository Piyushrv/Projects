library(ggplot2)
library(forecast)
library(tseries)
bajaj1<-read.csv(file.choose(),header = T)

str(bajaj1$Date)

View(bajaj1)

##converting dteday into date coz in time series date is imp fro forecast

bajaj1$Date<-as.Date(bajaj1$Date)
                     

#1] Ploting cnt(count) vs Date


# daily data is converted in to monthly data. using scale_x_date
ggplot(bajaj1,aes(Date,WAP))+geom_line()+scale_x_date("month")+ylab("Daily Weighted Average Price")
#2] Examining the data if it contains any outliers 


#3] Cleaning the data
summary(bajaj1$WAP)
plot(bajaj1$WAP)

#converting data into time series object this step has to done
wap_ts1<-ts(bajaj1[,c('WAP')])

str(wap_ts1)
plot(wap_ts1)
## tsclean() this command removes all the outliers and null values

bajaj1$clean_wap<-tsclean(wap_ts1)

str(bajaj1$clean_wap)
## Now plotting cleaned data with date

ggplot(bajaj1,aes(Date,clean_wap))+geom_line()+scale_x_date("month")+ylab("Cleaned WAP Data")

#4] Now cretaing Moving Average for weekly and monthly coz to smooth the data 
bajaj1$wap_ma<-ma(bajaj1$clean_wap,order = 7)# using order 7 for creating MA for weekly

bajaj1$wap_ma30<-ma(bajaj1$clean_wap,order = 30)# using order 30 for creating MA for Monthly

#now checking whether the smoothig has done properly i.e original, weekly, monthly MAS
ggplot()+geom_line(data= bajaj1,aes(x=Date,y=clean_wap,colour="counts"))+
  geom_line(data= bajaj1,aes(x=Date,y=wap_ma,colour="Weekly"))+
  geom_line(data= bajaj1,aes(x=Date,y=wap_ma30,colour="Monthly"))

#5] Decomposing the data in to TSCE 
# Checking trend , Seasonality, Cyclyic And Error

avg_ma1<- ts(na.omit(bajaj1$wap_ma),frequency = 30)
decom1<-stl(avg_ma1,s.window = "periodic")

## Ploting the decomposed data

plot(decom1)
## ggplot gives error coz its the stl data after decomposing the data converts into stats
# in plot cyclyic graph is not availble for cylyic, to get cyclic data ,the data should be around 3-5 years

#6] Making Data Stationary
# ARIMA + ADF(Augmented Dickey-Fuller Test(it is used as a statistical for stationarity of data))
# Now doing stationary test using ADF
#adf test known as Unit root test and Stationary test
# if p value is less than alpha then reject the null hypothesis 
adf.test(avg_ma1,alternative = "stationary")
#now check through the graphs using ACF and PACF
acf(avg_ma1)
pacf(avg_ma1)
#now we will differenciate the data to make it satationary
#before going differenciate we had to remove the seasonality
deseasonal_wap1<-seasadj(decom1)
plot(deseasonal_wap1)#only seasonality is removed

avg1_d1<-diff(deseasonal_wap1,differences = 1)
plot(avg1_d1)

#now checking whether the data is stationary after d=1
# again doing ADF test
adf.test(avg1_d1,alternative = "stationary")

#lets check through acf graph
acf(avg1_d1)


aic<-matrix(NA,5,5)

for(p in 0:4)
{
  for(q in 0:4)
  {
    apq<-arima(avg1_d1,order = c(p,1,q))
    aic.pq<-apq$aic
    aic[p+1,q+1]<-aic.pq
  }
}

aic
min(aic)


#8] Fitting the model
# now using auto arima for building model
fitwap1<-auto.arima(deseasonal_wap1,seasonal = F)
fitwap1
#9] Evaluate and Iterate
## we cant trust the model 
# so we will plot residuals

#10] Now Forecasting
fcast1<-forecast(fitwap1,h=90)# h = 90 means next upcoming 30 day according to the data
plot(fcast1)

# hold-out method 

hold1<-window(ts(deseasonal_wap1),start=2000)
fit_no_hold1<-arima(ts(deseasonal_wap1[-c(2000:2106)]),order = c(2,2,3))
fcast_no_hold1<-forecast(fit_no_hold1,h=300)
plot(fcast_no_hold1)
lines(ts(deseasonal_wap1))
fcast_no_hold1

#bring back the seasonality

fit_seasonal1<-auto.arima(deseasonal_wap1,seasonal = T)
fit_seasonal_fcast1<-forecast(fit_seasonal1,h=30)
plot(fit_seasonal_fcast1)
