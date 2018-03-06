
#Assignment - Analytical Detective
mvt = read.csv("mvtWeek1.csv")
str(mvt)
max(mvt$ID)
min(mvt$Beat)
TrueArrest = subset(mvt,Arrest==TRUE)
str(TrueArrest)
 AlleyLocation = subset(mvt,LocationDescription=="ALLEY")
str(AlleyLocation)
mvt$Date[1]
#2.2 - May 21
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
#2.3 - February
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
#2.4 - Friday
table(mvt$Weekday)
#2.5 - January
TrueArrest = subset(mvt,Arrest==TRUE)
table(TrueArrest$Month)
#3.1 - Decreases, Decreases, Increases
hist(mvt$Date, breaks=100)
#3.2 - First Half
boxplot(mvt$Date ~ mvt$Arrest)
#3.3 - 0.1041173
Data2001 = subset(mvt,Date < as.Date("2002/01/01 00:00"))
prop.table(table(Data2001$Arrest))
#3.4 - 0.08487395
Data2007 = subset(mvt,Date < as.Date("2008/01/01 00:00")&Date > as.Date("2006/12/31 23:59"))
prop.table(table(Data2007$Arrest))
#3.5 -  0.03902924
Data2012 = subset(mvt,Date < as.Date("2013/01/01 00:00")&Date > as.Date("2011/12/31 23:59"))
prop.table(table(Data2012$Arrest))
#4.1 - Gas Station, Street, Paking Lot, Alley, Driveway (Res)
sort(table(mvt$LocationDescription))
#4.2 - 177510
Top5 = subset(mvt,LocationDescription %in% c("STREET","PARKING LOT/GARAGE(NON.RESID.)","ALLEY","GAS STATION","DRIVEWAY - RESIDENTIAL"))
#4.3 - Parking Lot/Garage (Non-Residential)
Top5$LocationDescription = factor(Top5$LocationDescription)
prop.table(table(Top5$Arrest,Top5$LocationDescription))
#4.4 - Saturday
table(Top5$Weekday,Top5$LocationDescription)
#4.5 - Saturday