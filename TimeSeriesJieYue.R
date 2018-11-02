library(dplyr)
library(tidyr)
library(TTR)
library(forecast)
library(MatrixModels)
repay =  readRDS(file="repay.rds")

repay$paytime=substr(repay$PAY_DATE,regexpr("[0-9]",repay$PAY_DATE),regexpr("\\s",repay$PAY_DATE))
#repay$repaytime=substr(repay$REPAY_DATE,regexpr("[0-9]",repay$REPAY_DATE),regexpr("\\s",repay$REPAY_DATE))
repay=select(repay,-UPDATE_TIME,-CREATE_TIME,-PAY_DATE)
x = na.omit(repay)


repay$paytime=as.Date(repay$paytime,format = "%Y/%m/%d")
repay1 = filter(repay,paytime<"2018/7/26" )
#行为数据
repay$qiancha = (repay$MUST_BASE+repay$MUST_INST)-(repay$REAL_BASE+repay$REAL_INST)#不还款的两种因素
laolai = filter(repay,(MUST_PENALTY!=0|MUST_DEFAULT!=0|qiancha!=0))
laolai=na.omit(laolai)

laolai%>%
  
  group_by(paytime)%>%
   arrange(paytime)%>%
  summarise(money = sum(qian))->repaid
rep = filter(repaid,paytime>"2017/1/1")

plot(rep$paytime, rep$money, main = "TIme series",
     xlab = "time",
     ylab = "Count(missrepay)perday ",type="l")
#############################################################################
rep$yearmonth=strftime(rep$paytime, format = "%y-%m")
rep$monthDate=strftime(rep$paytime, format = "%d")
#############################################################################
rep%>%
arrange(desc(paytime))%>%
  group_by(yearmonth)%>%
mutate(sumbymonth=sum(money))%>%
  arrange(paytime)%>%
  mutate(weight=money/sumbymonth)->ques

#############################################################################

rep0 = filter(rep,paytime>'2017/1/30')
repp = select(rep0,-paytime)

ques1 =spread(repp,monthDate,money)#这个是关于钱的时间序列
ques0=filter(ques,paytime>'2017/1/30')
repp1 = select(ques0,yearmonth,monthDate, weight)
#这个是关于每日还款占当月还款比例的时间序列，目的是把随着时间增长的趋势抹平
ques11 =spread(repp1,monthDate,weight)
names(ques11)
ques11[is.na(ques11)]<-0
ques1[is.na(ques1)]<-0
############time series######################################################
ts(ques$weight)
plot(ts(ques$money))
abline(lm(ts(ques$money)~time(ques$paytime)))
plot(ts(ques$weight))
abline(lm(ts(ques$weight)~time(ques$paytime)))
a = lm(ts(ques$money)~time(ques$paytime))
b=lm(ts(ques$weight)~time(ques$paytime))
summary(a)
summary(b)
#从这里可以看出，用比重在时间序列里做出的数据分析




############################################################################
ques[is.na(ques)]<-0
ques = filter(ques,paytime>'2017/1/30')
ques$yearmonth
#acf(tsSMA)
aa=ts(ques$weight,frequency=11)

auto.arima(aa,trace=T)

data.fit=arima(aa,order=c(3,0,1),seasonal=list(order=c(1,0,0),period=1),method="ML") 

airforecast <- forecast::forecast(data.fit,h=13,level=c(.1))

airforecast

plot(airforecast)

#######################################################################noswat

#acf(tsSMA0)
aab=ts(ques$money,frequency=11)

auto.arima(aab,trace=T)

data.fit0=arima(aab,order=c(4,0,4),seasonal=list(order=c(1,0,1),period=11),method="ML") 
??forecast.arima
airforecast0 <- forecast::forecast(data.fit0,h=11,level=c(0.1))

airforecast0

plot(airforecast0)
##############forecast validation/ and show the plot and details of transform 
weighttrans=gather(ques11,`01`, `05`, `06` ,`08` ,`09`, `10`,`11`, `14`,`16`,`26`,`27`,`28`,key=monthDate,value = 'weight')
moneytrans=gather(ques1,`01`, `05`, `06` ,`08` ,`09`, `10`,`11`, `14`,`16`,`26`,`27`,`28`,key=monthDate,value = 'money')

weighttrans=arrange(weighttrans,yearmonth,monthDate)
moneytrans=arrange(moneytrans,yearmonth,monthDate)

trainw=weighttrans[1:168,]
testw=weighttrans[169:184,]

aab=ts(trainw$weight,frequency=12)

auto.arima(aab,trace=T)

data.fit0=arima(aab,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12),method="ML") 

forecast0 <- forecast::forecast(data.fit0,h=16,level=c(0.1))
forecast0$mean  
testw$weight
b=   c(0.42307069 ,0.02126879, 0.04722411, 0.02137607 ,0.02988654 ,0.03889301, 0.03150717 ,0.00355773, 0.24555460, 0.03332303, 0.05149679,
       0.05287017, 0.42307069, 0.02126879, 0.04722411, 0.02137607)
mae(b,testw$weight)
###############################

trainm=moneytrans[1:168,]
testm=moneytrans[169:184,]

XXB=ts(trainm$money,frequency=12)

auto.arima(XXB,trace=T)

data.fit1=arima(XXB,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12),method="ML") 
??forecast.arima
forecast1 <- forecast::forecast(data.fit1,h=16,level=c(0.1))

summary = summary(forecast1)
a = summary$`Point Forecast`
a
testm$money
a<-c(5203118.1,261983.7,580230.3,262614.9,367157.9,477801.7,387066.4,43706.8,3016644.4,409374.2,632639.4,
     649511.3,5203118.1,261983.7,580230.3,262614.9)
mae(a,testm$money)
forecastfuture <- forecast::forecast(data.fit1,h=24,level=c(0.1))
plot(forecastfuture)
