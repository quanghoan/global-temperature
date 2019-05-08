library("forecast")
data = read.csv("/home/harry/R/staSpat/land-ocean.csv",header=TRUE)
plot(data$globtemp)
global_temp = ts(data$globtemp, start = data$year[1],frequency = 1)
time = as.numeric(time(global_temp))

#sort globtemp column to see which years are the highest/lowest temperature
data[with(data, order(data$globtemp, data$year)),]

# calculate mean, standard deviation and variance of global temperature
mean(global_temp)    #mean ~ 0.026
var(global_temp)     #variance ~ 0.11
sd(global_temp)      #standard deviation ~ 0.332

ylab = "Temperature Anomaly (C)"
#lowess smothing from given data
plot(global_temp, ylab=ylab)
abline(h = 0, col = "red", lty = "dashed")
lowess = ts(data$lowess,start = data$year[1])
lines(lowess,col="blue")
legend("topleft",legend = c("Annual mean","Lowess smoothing"),lwd=1,lty=1,col=c("black","blue"))

#calculate lowess with span = 0.08, which indicates to the given lowess smothing data
plot(global_temp)
lines(lowess(global_temp,f=0.08),col="blue")

#Polynomial fitting to the global average annual mean
#degree = 1 it's the linear trend
#raw=FALSE means orthongonal polynomial of 3rd order
fit1 = lm(global_temp~poly(time,degree=1,raw=FALSE))
fit3 = lm(global_temp~poly(time,degree=3,raw=FALSE))
lines(time,predict(fit1),col='blue',lwd=2)
lines(time,predict(fit3),col='green',lwd=2)
legend("bottomright",legend = c("1st order","3rd order"),lwd=2,lty=1,col=c("blue","green"))

#density
hist(global_temp,prob=TRUE)
lines(density(global_temp), col="blue")

#auto-regression model
#diff_globtemp <- diff(global_temp) - mean(diff(global_temp))
#plot(diff_globtemp)

#Differencing Global Temperature to eliminate the trend 
plot(diff(global_temp))
mean(diff(global_temp)) # drift estimate = 0.008
acf(diff(global_temp))
pacf(diff(global_temp))

#Forecast time series using arima(p,d,q)
xlab = "Forecast temperature for next 10 years"
fitted_model = auto.arima(global_temp)
fcast = forecast(fitted_model,10, level=c(80,95))
plot(fcast, xlab = xlab)
