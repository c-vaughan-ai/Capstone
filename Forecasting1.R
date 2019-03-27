##Predictive Model

#Install Packages
install.packages("forecast")
library("forecast")
install.packages("tseries")
library("tseries")

#Convert to time series
total11.ts.women <- as.ts(total11$`Women Unemployment Rate`)

#Decompose
women_UR = ts(na.omit(total11$`Women Unemployment Rate`), frequency=30)
decomp = stl(women_UR, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


#Stationary Test
adf.test(total11$`Women Unemployment Rate`, alternative = "stationary")

#Autocorrelation
Acf(total11$`Women Unemployment Rate`, main= 'ACF for Differenced Series')

Pacf(total11$`Women Unemployment Rate`, main='PACF for Differenced Series')
????
  
#Arima
model = auto.arima(total11$`Women Unemployment Rate`, allowdrift = TRUE, D=1)
##model = Arima(total11$`Women Unemployment Rate`, order = c(4,0,2))
model_resid = residuals(model)
Box.test(model_resid, lag=10, type = "Ljung-Box")
forecast_women_UR = forecast(model, h=12)
plot(forecast_women_UR)

##Forecacst
total20 <- read.csv('total11.csv')
total20.train <- total20[1:514,]
total20.test <- total20[515:550, ]
#Women
 WUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$Women.Unemployment.Rate)
 WUR.ar.model.tr <- auto.arima(WUR.ts.tr, allowdrift = TRUE)
 WUR.AR.forecast.tr <- forecast(WUR.ar.model.tr, h=36)
 plot(WUR.AR.forecast.tr, main = "Female Unemployment Rate Forecast")
 accuracy(WUR.AR.forecast.tr, x=total20.test$Women.Unemployment.Rate)
 #Men
 MUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$men.Unemployment.Rate)
 MUR.ar.model.tr <- auto.arima(MUR.ts.tr, allowdrift = TRUE)
 MUR.AR.forecast.tr <- forecast(MUR.ar.model.tr, h=36)
 plot(MUR.AR.forecast.tr, main = "Male Unemployment Rate Forecast")
 accuracy(MUR.AR.forecast.tr, x=total20.test$men.Unemployment.Rate)
 #Black
 BUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$Black.Unemployment.Rate)
 BUR.ar.model.tr <- auto.arima(BUR.ts.tr, allowdrift = TRUE)
 BUR.AR.forecast.tr <- forecast(BUR.ar.model.tr, h=36)
 plot(BUR.AR.forecast.tr,main = "Black Unemployment Rate Forecast")
 accuracy(BUR.AR.forecast.tr, x=total20.test$Black.Unemployment.Rate)
 #Hispanic
 HUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$Hispanic.Unemployment.Rate)
 HUR.ar.model.tr <- auto.arima(HUR.ts.tr, allowdrift = TRUE)
 HUR.AR.forecast.tr <- forecast(HUR.ar.model.tr, h=36)
 plot(HUR.AR.forecast.tr,main = "Hispanic Unemployment Rate Forecast")
 accuracy(HUR.AR.forecast.tr, x=total20.test$Hispanic.Unemployment.Rate)
 #White
 WHUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$White.Unemployment.Rate)
 WHUR.ar.model.tr <- auto.arima(WHUR.ts.tr, allowdrift = TRUE)
 WHUR.AR.forecast.tr <- forecast(WHUR.ar.model.tr, h=36)
 plot(WHUR.AR.forecast.tr, main = "White Unemployment Rate Forecast")
 accuracy(WHUR.AR.forecast.tr, x=total20.test$White.Unemployment.Rate)
 #Asian
 AUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$Asian.Unemployment.Rate)
 AUR.ar.model.tr <- auto.arima(AUR.ts.tr, allowdrift = TRUE)
 AUR.AR.forecast.tr <- forecast(AUR.ar.model.tr, h=36)
 plot(AUR.AR.forecast.tr, main = "Asian Unemployment Rate Forecast")
 accuracy(AUR.AR.forecast.tr, x=total20.test$Asian.Unemployment.Rate)
 #16-19
 sixteenUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X16.19.Unemployment.Rate)
 sixteenUR.ar.model.tr <- auto.arima(sixteenUR.ts.tr, allowdrift = TRUE)
 sixteenUR.AR.forecast.tr <- forecast(sixteenUR.ar.model.tr, h=36)
 plot(sixteenUR.AR.forecast.tr, main = "16-19 Unemployment Rate Forecast")
 accuracy(sixteenUR.AR.forecast.tr, x=total20.test$X16.19.Unemployment.Rate)
 #20-24
 twentyUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X20.24.Unemployment.Rate)
 twentyUR.ar.model.tr <- auto.arima(twentyUR.ts.tr, allowdrift = TRUE)
 twentyUR.AR.forecast.tr <- forecast(twentyUR.ar.model.tr, h=36)
 plot(twentyUR.AR.forecast.tr, main = "20-24 Unemployment Rate Forecast")
 accuracy(twentyUR.AR.forecast.tr, x=total20.test$X20.24.Unemployment.Rate)
 #25-34
 twentyfiveUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X25.34.Unemployment.Rate)
 twentyfiveUR.ar.model.tr <- auto.arima(twentyfiveUR.ts.tr, allowdrift = TRUE)
 twentyfiveUR.AR.forecast.tr <- forecast(twentyfiveUR.ar.model.tr, h=36)
 plot(twentyfiveUR.AR.forecast.tr, main = "25-34 Unemployment Rate Forecast")
 accuracy(twentyfiveUR.AR.forecast.tr, x=total20.test$X25.34.Unemployment.Rate)
 #35-44
 thirtyfiveUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X35.34.Unemployment.Rate)
 thirtyfiveUR.ar.model.tr <- auto.arima(thirtyfiveUR.ts.tr, allowdrift = TRUE)
 thirtyfiveUR.AR.forecast.tr <- forecast(thirtyfiveUR.ar.model.tr, h=36)
 plot(thirtyfiveUR.AR.forecast.tr, main = "35-44 Unemployment Rate Forecast")
 accuracy(thirtyfiveUR.AR.forecast.tr, x=total20.test$X35.34.Unemployment.Rate)
 
 #45-54
 fortyfiveUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X45.54.Unemployment.Rate)
 fortyfiveUR.ar.model.tr <- auto.arima(fortyfiveUR.ts.tr, allowdrift = TRUE)
 fortyfiveUR.AR.forecast.tr <- forecast(fortyfiveUR.ar.model.tr, h=36)
 plot(fortyfiveUR.AR.forecast.tr, main = "45-54 Unemployment Rate Forecast")
 accuracy(fortyfiveUR.AR.forecast.tr, x=total20.test$X45.54.Unemployment.Rate)
 
 #55&Over
 fiftyfiveUR.ts.tr <- ts(start=c(1973,4), frequency=12, data=total20.train$X55.Over.Unemployment.Rate)
 fiftyfiveUR.ar.model.tr <- auto.arima(fiftyfiveUR.ts.tr, allowdrift = TRUE)
 fiftyfiveUR.AR.forecast.tr <- forecast(fiftyfiveUR.ar.model.tr, h=36)
 plot(fiftyfiveUR.AR.forecast.tr, main = "55 and Over Unemployment Rate Forecast")
 accuracy(fiftyfiveUR.AR.forecast.tr, x=total20.test$X55.Over.Unemployment.Rate)

 
##Recession
#Ch.9 Dynamic regression model
