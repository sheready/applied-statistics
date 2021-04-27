install.packages("tseries")
install.packages("dynlm")
library(dynlm)
library(tseries)
library(ggplot2)
library(lmtest)

#load the data
mydata = gdp_and_unemp

#specifying data that will be used is a time series data
GDP.ts = ts(mydata$GDP)
Unemp.ts = ts(mydata$Unemp)

#create model with quaterly lag
dlmodel=dynlm(GDP.ts~L(Unemp.ts,0:4),data=mydata)
summary(dlmodel)
#Conclusion:the last two values are statistically insignificant because their t-values are less than 0.Hence we get rid of them by changing our lag.
#It also shows that if our unemployment rate decreases by 1.1% will have an effect on GDP by 1.1%.
dlmodel=dynlm(GDP.ts~L(Unemp.ts,0:2),data=mydata)
summary(dlmodel)

mydata=GDP_to_Unemp_Time_Series

#you have to specify that the data is Time series use commands below
GDP.ts=ts(mydata$GDP)
Unemp.ts=ts(mydata$Unemp)

#Changing the lag
dlmodel=dynlm(GDP.ts~L(Unemp.ts,0:2),data=mydata)
summary(dlmodel)


plot(mydata$Unemp,type="l")
plot(mydata$GDP,type="l")


#Autocorrelation 
dwtest(dlmodel) # DW=1.77 no autocorrelation in first order.
#Conclusion:P value autocorrelation is less than 0 thus the null hypothesis is true. 

bgtest(dlmodel,2) # There is no autocorrelation in 2nd order.
#Conclusion:There is no significant autocorrelation.P value is less than 0,we accept the null hypothesis.

#Stationarity Test
adf.test(GDP.ts,alternative="stationary",k=2)
#Conclusion:Our p value is 0.01 and our lags are 2 hence our K is 2 and that our data is stationary,the p value is less than the computed value.
adf.test(Unemp.ts,alternative="stationary",k=2)
#Conclusion:P value is higher than computed, we therefore reject the null and that our data is not stationary