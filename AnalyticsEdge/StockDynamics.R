#Assignment 2 - Stock
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
#1.1 - 480
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
str(IBM)
#1.2 - 1970
min(IBM$Date)
min(GE$Date)
min(ProcterGamble$Date)
min(CocaCola$Date)
min(Boeing$Date)
#1.2 - 2009
max(IBM$Date)
max(GE$Date)
max(ProcterGamble$Date)
max(CocaCola$Date)
max(Boeing$Date)
#1.3 - 144.375
 mean(IBM$StockPrice)
#1.4 - 9.293636
min(GE$StockPrice)
#1.5 - 146.5843
max(CocaCola$StockPrice)
#1.6 - 44.8834
median(Boeing$StockPrice)
#1.7 - 18.19414
sd(ProcterGamble$StockPrice)

#2.1 - 1973, 1980
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")

#2.2 - CocaCola
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")

#2.3 - CocaCola, CocaCola
abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1983-12-31")), lwd=2)

#3.1 - GE
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="orange")

#3.2 - IBM
#3.3 - procter Gamble, Boeing
abline(v=as.Date(c("1997-09-01","1997-11-30","1997-10-01")),lwd=2)

#3.4 - GE, Boeing
abline(v=as.Date(c("2004-01-01","2005-12-31")),lwd=2)

#4.1 - Jan, Feb. Mar. Apr, May
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

#4.2 - April
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

#4.3 - December
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)

