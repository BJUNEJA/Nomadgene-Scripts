  #clear memory
rm( list=ls() )
library(ff)
library(zoo)
library(xts)
setwd("C:/bhupi/Personal/data/eur_usd_tick_data");
#lets use this as our training data... 
d<- read.table("eur_usd_tick.csv", sep=",", header=FALSE, as.is=TRUE, fill=TRUE,nrows=2000000)
#we can seperately generate a test / validation data to see how good our forecast does.  
View(d)
#create an xts object 
d_xts<-xts(d[,-1],order.by=as.POSIXct(d[,1]))
# create a zoo object Zeileis Ordered Observations. 
d_zoo <-zoo(d[-1], order.by = as.POSIXct(d[,1]))

#d1<-cbind(t=seq(1,dim(d)[1]),e=(d[,2]+d[,3])/2);
d2<-cbind((t=d[,1]) ,  (e=d[,2]));
d3<-cbind(t=seq(1,dim(d)[1]),e=d[,2]);
plot(d3,type="l")

############################################################################
## A basic filtering example. 
#http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
mav <- function(x,n=20){filter(x,rep(1/n,n), sides=1)}
d100<- read.table("eur_usd_tick.csv", sep=",", header=FALSE, as.is=TRUE, fill=TRUE,nrows=1000000)
d3<-cbind(t=seq(1,dim(d100)[1]),e=d100[,2]);
d100_zoo <-zoo(d100[-1], order.by = as.POSIXct(d100[,1]))


mav5 <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}
mav20 <- function(x,n=20){filter(x,rep(1/n,n), sides=1)}
mav60 <- function(x,n=60){filter(x,rep(1/n,n), sides=1)}
dmav<-mav(d100[,2])
plot(d3,type="l",col=grey(.5))
lines(dmav5,col="red")
lines(dmav20,col="green")
lines(dmav60,col="blue")


eur_usd_tickdata_subset<-paste("C:/bhupi/Personal/data/eur_usd_tick_data_100.csv",sep="")
write.csv(d100,eur_usd_tickdata_subset)

############################################################################
###### Decompose time series into its components including Trend/seasonality/ and irregular components.  
d100_stl <-stl(d100, s.window=13)
d100_dec <-decompose(d100_zoo)




eur_usd_tickdata_subset<-paste("C:/bhupi/Personal/data/eur_usd_tick_data_1000000.csv",sep="")
write.csv(d,eur_usd_tickdata_subset)

View(d2)

for(i in 1:100){
  #plot(d1[i:(i+99),1],d1[i:(i+99),2],type="l", col=4);
  plot(d1[i:(i+30),1],d1[i:(i+30),2],type="l", col=4);
  Sys.sleep(0.2)
}

## Using FFDF
system.time(FFDF <- read.csv.ffdf(file="eur_usd_tick.csv",nrows=1000)) 
d<-as.data.frame(cbind(EXCH=FFDF[,2],INDEX=seq(1,dim(FFDF)[1])));
plot(d$INDEX, d$EXCH, type="l", col=4)
View(d) 





library(urca)
library(forecast)

y<-d$EXCH[1:9900];

arma30<-arima(y, order=c(3,0,0));

armaauto<-auto.arima(y, max.p=3, max.q=3, start.p=1, start.q=1, ic="aic");
arma.pred<-predict(arma30, n.ahead=100)

lines(seq(9901,10000), arma.pred$pred, col=2)

library(mgcv)
d5<-d[1:10000,]
m3<-gam(EXCH~s(INDEX), data=d5)
p2<-predict(m3, newdata=d[9900:10000,])
lines(seq(9900,10000),p2,col=3,lwd=2)
p1<-predict(m3)
lines(seq(1,10000),p1,col=2)

y<-d5$EXCH-p1;
plot(seq(1,10000),y, type="l", col=4)


armaauto<-auto.arima(y, max.p=3, max.q=3, start.p=1, start.q=1, ic="aic");
arma.pred<-predict(armaauto, n.ahead=100)

lines(seq(9901,10000),arma.pred$pred, col=2)

plot(seq(1,10000),y, type="l", col=4)


library(spectral.methods)

m1<-calcFrequency(y, plot.periodogram=TRUE)
m2<-filterTseriesSSA(y), borders.wl = list(a = c(8, 12), b = c(80, 120)
, c = c(0, 10, 100, Inf)),
M = c(30, 200, 100),
n.comp = c(10, 20, 20),
harmonics = c(1, 0, 0),
plot.spectra = TRUE, open.plot = FALSE)
predict(m1)