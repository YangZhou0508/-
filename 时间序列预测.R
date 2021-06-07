library(readxl)
library(fpp2)

####Q1####
setwd("D:/IESEG/Ô¤²â") # Specify you own working directory here.
data <- read_excel("DataSets2021.xlsx", sheet="Bankrupt")
Bankrupt <- ts(data$Bankrupt, frequency = 12, start = 2000)
Bankrupt
plot(Bankrupt)

# time series plot
plot(Bankrupt, main=" Bankrupt", xlab="Year", 
     ylab=" Bankrupt", type="l")

#(P)ACF plot
tsdisplay(Bankrupt)

# seasonal plot
seasonplot(Bankrupt, year.labels = TRUE,year.labels.left=TRUE,col=rainbow(20))

#seasonal subseries plot
monthplot(Bankrupt)

Bankrupt_train <- window(Bankrupt, end = c(2017,12))
plot(Bankrupt_train,ylab = 'Bankrupt' , xlab = 'Month')
Bankrupt_train
Bankrupt_test <- window(Bankrupt, start = c(2018,1))
plot(Bankrupt_test,ylab = 'RetailSalesVolume' , xlab = 'Month')
Bankrupt_test
h <- length(Bankrupt_test)

####2####
#Box-Cox transformations
n1 <- snaive(Bankrupt_train, h=h) 
plot(Bankrupt, ylab="Bankrupt", xlab="Month")
lines(n1$mean,col=4)
legend("topleft", lty=1, col=c(4,2,6), legend=c("Seasonal Naive method"))

accuracy(n1, Bankrupt_test)[,c(2,3,5,6)]
#RMSE = 235.50829 
checkresiduals(n1)
res <- residuals(n1)
res <- na.omit(res)
tsdisplay(res)



BoxCox.lambda(Bankrupt_train)
plot(BoxCox(Bankrupt_train,lambda=-0.296707))
n2 <- snaive(Bankrupt_train, lambda=-0.296707,h=h)
plot(n2, ylab="Bankrupt", xlab="Month")
lines(n2$mean,col=4)
legend("topleft", lty=1, col=c(4), legend=c("Seasonal Naive method"))

accuracy(n2, Bankrupt_test)[,c(2,3,5,6)]
#RMSE=235.50829
checkresiduals(n2)

#Calendar adjustments
plot(Bankrupt_train, main="Monthly Bankrupt",
     ylab="Bankrupt",xlab="Years")
plot(Bankrupt_train/monthdays(Bankrupt_train), main="Average Bankrupt per day", ylab="Bankrupt", xlab="Years")

plot(Bankrupt_train)
Bankrupt_train <- Bankrupt_train/monthdays(Bankrupt_train)
Bankrupt_test <- Bankrupt_test/monthdays(Bankrupt_test)
plot(Bankrupt_train)

n2 <- snaive(Bankrupt_train,h=h)
plot(n2, ylab="Bankrupt", xlab="Month")
lines(n2$mean,col=4)
legend("topleft", lty=1, col=c(4), legend=c("Seasonal Naive method"))

accuracy(n2, Bankrupt_test)[,c(2,3,5,6)]
#RMSE = 7.700692 
checkresiduals(n2)

####3####
snaive_fit <- snaive(Bankrupt_train, lambda=-0.2727187,h=h)
plot(snaive_fit)
accuracy(snaive_fit, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(snaive_fit)
#snaive : RMSE = 7.700692  

####4####
#t.window and s.window
stl_fit <- stl(Bankrupt_train, t.window=15,s.window="periodic")
plot(stl_fit)
fcast <- forecast(stl_fit, h=h)
plot(fcast)
accuracy(fcast, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fcast)
#t.window=15 , RMSE = 6.996758 

#t.window and s.window
stl_fit <- stl(Bankrupt_train, t.window=5,s.window="periodic")
plot(stl_fit)
fcast <- forecast(stl_fit, h=h)
plot(fcast)
accuracy(fcast, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fcast)
#t.window=5 , RMSE = 6.960799  



#rwf
stl_fit2 <- stl(Bankrupt_train, t.window=5,s.window="periodic")
eeadj1 <- seasadj(stl_fit2)
plot(rwf(eeadj1), xlab="")
stl_fit2<- stl(eeadj1, t.window=5,s.window="periodic")
fcast2 <- forecast(stl_fit2, method="rwdrift", h=h)
plot(fcast2)
accuracy(fcast2, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fcast2)
#t.window=5 , RMSE = 7.698283    




#ets
stl_fit3 <- stl(Bankrupt_train, t.window=5,s.window="periodic")
eeadj2 <- seasadj(stl_fit3)
plot(ets(eeadj2), xlab="")
stl_fit3<- stl(eeadj2, t.window=5,s.window="periodic")
fcast3 <- forecast(stl_fit3, method="ets", h=h)
plot(fcast3)
accuracy(fcast3, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fcast3)
#t.window=5 , RMSE = 7.848912 


####5####
fc1 <- hw(Bankrupt_train, seasonal="mult", h=h)
fc2 <- hw(Bankrupt_train,seasonal="mult", damped=TRUE, h=h)
fc3 <- hw(Bankrupt_train, seasonal = "additive", h=h)
fc4 <- hw(Bankrupt_train, seasonal = "additive", damped = TRUE, h=h)

a_fc1 <-  accuracy(fc1, Bankrupt_test)[,c(2,3,5,6)]
a_fc2 <- accuracy(fc2, Bankrupt_test)[,c(2,3,5,6)]
a_fc3 <- accuracy(fc3, Bankrupt_test)[,c(2,3,5,6)]
a_fc4 <- accuracy(fc4, Bankrupt_test)[,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
acc

checkresiduals(fc1)
res1 <- residuals(fc1)
tsdisplay(res1)

fit <- rbind(fc1$model$aic, fc2$model$aic, fc3$model$aic, fc4$model$aic)
colnames(fit) <- c("AIC")
rownames(fit) <- c("a_fc1", "a_fc2", "a_fc3", "a_fc4")
fit

checkresiduals(fc1)
res1 <- residuals(fc1)
tsdisplay(res1)


fc5 <- ets(Bankrupt_train)
fc5
plot(forecast(fc5 ))
fcast5 <- forecast(fc5,h=h)
plot(fcast5)
accuracy(fcast5, Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fcast5)
#ETS(M,Ad,M) 
#RMSE = 9.478141 


####6####
tsdisplay(Bankrupt_train)
l_Bankrupt_train<-diff(Bankrupt_train,12)
plot(l_Bankrupt_train)
tsdisplay(l_Bankrupt_train)

nsdiffs(l_Bankrupt_train, alpha=0.1)
ndiffs(diff(l_Bankrupt_train,12), alpha=0.1)

l_Bankrupt_test<-diff(Bankrupt_test)
tsdisplay(l_Bankrupt_test)

plot(l_Bankrupt_train)
lines(l_Bankrupt_test, col="red")

ndiffs(l_Bankrupt_train, test = "kpss")
ndiffs(l_Bankrupt_train, test = "adf")
ndiffs(l_Bankrupt_train, test = "pp")

fit_opt <- auto.arima(l_Bankrupt_train)
summary(fit_opt)
#install.packages("lmtest")
library(lmtest)

coeftest(fit_opt)
res <- residuals(fit_opt)
tsdisplay(res)
adf.test(res, alternative="stationary")
fcast <- forecast(fit_opt, h=h)
plot(fcast)

accuracy(fit_opt, l_Bankrupt_test)[,c(2,3,5,6)]
checkresiduals(fit_opt)


qqnorm(fit_opt$residuals)
qqline(fit_opt$residuals)
Box.test(fit_opt$residuals,type="Ljung-Box")

####7####
#snaive : RMSE = 7.700692 
#stl rwf t.window=5 , RMSE = 7.698283 
#stl t.window=5 , RMSE = 6.960799
#fc1 <- hw(Bankrupt_train, seasonal="mult", h=h) RMSE = 7.163298 
#ARIMA(3,0,2)(0,0,1)[12] with zero mean RMSE = 5.899612 



####8####
setwd("D:/IESEG/Ô¤²â") # Specify you own working directory here.
basetable <- read_excel("DataSets2021.xlsx", sheet="Bankrupt")
Bankrupt = tsclean(Bankrupt)
plot(basetable)
basetable <- ts(basetable[,2], frequency = 12, start = 2000)
basetable
basetable <- basetable/monthdays(basetable)

tsdisplay(basetable)
l_basetable<-diff(basetable,12)
plot(l_basetable)
tsdisplay(l_basetable)

nsdiffs(l_basetable, alpha=0.1)
ndiffs(l_basetable, test = "kpss")
ndiffs(l_basetable, test = "adf")
ndiffs(l_basetable, test = "pp")


fc5 <- auto.arima(basetable)
fc5
fcast5 <- forecast(fc5, h=26)
plot(fcast5)
accuracy(fcast5)
checkresiduals(fcast5)

####9####
# Decompose the time series
stl_fit <- stl(basetable[,1], s.window="periodic")
# Create the seasonally adjusted series
eeadj3 <- seasadj(stl_fit)
BoxCox.lambda(eeadj3)
#ets,ETS(A,N,N)
fit_ets<-ets(eeadj3)
fit_ets
ffit_ets <- forecast(fit_ets,model="A,N,N", h=26)
plot(ffit_ets)
accuracy(ffit_ets)[,c(2,3,5,6)]
checkresiduals(ffit_ets)

fc6 <- ets(eeadj3)
fc6
fcast6 <- forecast(fc6, h=26)
plot(fcast6)
accuracy(fcast6)[,c(2,3,5,6)]
checkresiduals(fcast6)

# auto.arima , ARIMA
fit_arima<- auto.arima(eeadj3)
fit_arima
ffit_arima <- forecast(fit_arima, h=26)
plot(ffit_arima)
accuracy(ffit_arima)[,c(2,3,5,6)]
checkresiduals(ffit_arima)

fc7 <- auto.arima(eeadj3)
fc7
fcast7 <- forecast(fc7, h=26)
plot(fcast7)
accuracy(fcast7)[,c(2,3,5,6)]
checkresiduals(fcast7)









####Q2####
####Q2####
library(readxl)
library(fpp2)
setwd("D:/IESEG/Ô¤²â/project") # Specify you own working directory here.
data <- read_excel("Social_consumer_goods.xls", sheet="Social_consumer_goods")
sales <- ts(data[,2], frequency = 12, start = 2000)
sales = tsclean(sales)
plot(sales)

#(P)ACF plot
tsdisplay(sales)

# seasonal plot
seasonplot(sales, year.labels = TRUE,year.labels.left=TRUE,col=rainbow(20))

#seasonal subseries plot
monthplot(sales)

sales_train <- window(sales, end = c(2017,12))
plot(sales_train,ylab = 'sales' , xlab = 'Month')

sales_test <- window(sales, start = c(2018,1))
plot(sales_test,ylab = 'sales' , xlab = 'Month')
h <- length(sales_test)

sales_train <- sales_train/monthdays(sales_train)
sales_test <- sales_test/monthdays(sales_test)



#Box-Cox transformations
n1 <- snaive(sales_train, h=h) 
plot(sales, ylab="sales", xlab="Month")
lines(n1$mean,col=4)
legend("topleft", lty=1, col=c(4), legend=c("Seasonal Naive method"))

accuracy(n1, sales_test)[,c(2,3,5,6)]
#RMSE = 146.80448 
checkresiduals(n1)


BoxCox.lambda(sales_train)
plot(BoxCox(sales_train,lambda=0.3095922))
n2 <- snaive(sales_train, lambda=0.3095922,h=h)
plot(Bankrupt, ylab="sales", xlab="")
lines(n2$mean,col=4)
legend("topleft", lty=1, col=c(4), legend=c("Seasonal Naive method"))

accuracy(n2, sales_test, lambda=0.3095922)[,c(2,3,5,6)]
checkresiduals(n2)


snaive_fit <- snaive(sales_train, lambda=0.3095922,h=h)
plot(snaive_fit)
accuracy(snaive_fit, sales_test)[,c(2,3,5,6)]
checkresiduals(snaive_fit)
#snaive : RMSE = 146.80448  

fc1 <- hw(sales_train, seasonal="mult", h=h)
fc2 <- hw(sales_train,seasonal="mult", damped=TRUE, h=h)
fc3 <- hw(sales_train, seasonal = "additive", h=h)
fc4 <- hw(sales_train, seasonal = "additive", damped = TRUE, h=h)

a_fc1 <-  accuracy(fc1, sales_test)[,c(2,3,5,6)]
a_fc2 <- accuracy(fc2, sales_test)[,c(2,3,5,6)]
a_fc3 <- accuracy(fc3, sales_test)[,c(2,3,5,6)]
a_fc4 <- accuracy(fc4, sales_test)[,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
acc

checkresiduals(fc1)
res3 <- residuals(fc1)
tsdisplay(res1)

fit <- rbind(fc1$model$aic, fc2$model$aic, fc3$model$aic, fc4$model$aic)
colnames(fit) <- c("AIC")
rownames(fit) <- c("a_fc1", "a_fc2", "a_fc3", "a_fc4")
fit

checkresiduals(fc1)
res1 <- residuals(fc1)
tsdisplay(res1)


fc5 <- ets(sales_train)
fc5
plot(forecast(fc5 ))
fcast5 <- forecast(fc5,h=h)
plot(fcast5)
accuracy(fcast5, sales_test)[,c(2,3,5,6)]
checkresiduals(fcast5)


###############################################
#t.window and s.window
stl_fit <- stl(sales_train, t.window=15,s.window="periodic")
plot(stl_fit)
fcast <- forecast(stl_fit, h=h)
plot(fcast)
accuracy(fcast, sales_test)[,c(2,3,5,6)]
checkresiduals(fcast)
#t.window=15 , RMSE = 6.996758 

#t.window and s.window
stl_fit <- stl(sales_train, t.window=5,s.window="periodic")
plot(stl_fit)
fcast <- forecast(stl_fit, h=h)
plot(fcast)
accuracy(fcast, sales_test)[,c(2,3,5,6)]
checkresiduals(fcast)
#t.window=5 , RMSE = 6.960799  



#rwf
stl_fit2 <- stl(sales_train, t.window=5,s.window="periodic")
eeadj1 <- seasadj(stl_fit2)
plot(rwf(eeadj1), xlab="")
stl_fit2<- stl(eeadj1, t.window=5,s.window="periodic")
fcast2 <- forecast(stl_fit2, method="rwdrift", h=h)
plot(fcast2)
accuracy(fcast2, sales_test)[,c(2,3,5,6)]
checkresiduals(fcast2)



#ets
stl_fit3 <- stl(sales_train, t.window=5,s.window="periodic")
eeadj2 <- seasadj(stl_fit3)
plot(ets(eeadj2), xlab="")
stl_fit3<- stl(eeadj2, t.window=5,s.window="periodic")
fcast3 <- forecast(stl_fit3, method="ets", h=h)
plot(fcast3)
accuracy(fcast3, sales_test)[,c(2,3,5,6)]
checkresiduals(fcast3)


################################################################
tsdisplay(sales_train)
l_sales_train<-diff(sales_train,12)
plot(l_sales_train)
tsdisplay(l_sales_train)

ndiffs(l_sales_train, test = "kpss")
ndiffs(l_sales_train, test = "adf")
ndiffs(l_sales_train, test = "pp")
ndiffs(diff(l_sales_train,12), alpha=0.1)

l_l_sales_train<-diff(l_sales_train,12)
ndiffs(l_l_sales_train, test = "kpss")
ndiffs(l_l_sales_train, test = "adf")
ndiffs(l_l_sales_train, test = "pp")

l_sales_test<-diff(sales_test)
l_l_sales_test<-diff(l_sales_test)


plot(l_l_sales_train)
lines(l_l_sales_test, col="red")

ndiffs(l_l_sales_train, test = "kpss")
ndiffs(l_l_sales_train, test = "adf")
ndiffs(l_l_sales_train, test = "pp")

fit_opt <- auto.arima(l_l_sales_train)
summary(fit_opt)
#install.packages("lmtest")
library(lmtest)

coeftest(fit_opt)
res <- residuals(fit_opt)
tsdisplay(res)
adf.test(res, alternative="stationary")
fcast <- forecast(fit_opt, h=h)
plot(fcast)

accuracy(fit_opt)[,c(2,3,5,6)]
checkresiduals(fit_opt)


qqnorm(fit_opt$residuals)
qqline(fit_opt$residuals)
Box.test(fit_opt$residuals,type="Ljung-Box")

##############################################################
setwd("D:/IESEG/Ô¤²â/project") # Specify you own working directory here.
data <- read_excel("Social_consumer_goods.xls", sheet="Social_consumer_goods")
sales <- ts(data[,2], frequency = 12, start = 2000)
sales = tsclean(sales)
plot(sales)
sales <- sales/monthdays(sales)

fc5 <- ets(sales)
fc5
plot(forecast(fc5 ))
fcast5 <- forecast(fc5,h=24)
plot(fcast5)
accuracy(fcast5)[,c(2,3,5,6)]
checkresiduals(fcast5)

