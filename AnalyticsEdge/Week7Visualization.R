##QQ
#Normally, a scatterplot only allows us to visualize two dimensions - one on the x-axis, and one on the y-axis. In the previous video we were able to show a third dimension on the scatterplot using what attribute?
#Ans - Color 
## Why is it particularly helpful for WHO to provide data visualizations? Select all that apply.
#Ans
# When communicating information to the general public, a visualization like the Energy Consumption one is much easier to absorb than a table of numbers would be.  
# Visualizations can easily be used by policymakers and others who wish to present data from WHO. 
# In the Scatterplot, what are the geometric objects?
# Points
# In the Histogram, what are the geometric objects?
# Bars 
# In the US Map, what are the geometric objects?
# Polygons
# All three of these plots defined a particular aesthetic property. What is it?
# Colors
####Video 4: Basic Scatterplots using ggplot
WHO = read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)
install.packages("ggplot2")
library(ggplot2)
scatterplot=ggplot(WHO,aes(x=GNI, y=FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color="blue", size=3, shape=17)
scatterplot + geom_point(color="red", size=3, shape=8) + ggtitle("Fertility Rate vs Gross National Income")
fertilityGNIplot = scatterplot + geom_point(color="red", size=3, shape=8) + ggtitle("Fertility Rate vs Gross National Income")
#Create and Open file
pdf("MyPlot.pdf")
#Write to file
print(fertilityGNIplot)
#Close file
dev.off()
colors()
###Quick Questions
#In R, change the shape of your points to the number 15. What shape are the points now?
scatterplot + geom_point(color="red", size=3, shape=15) + ggtitle("Fertility Rate vs Gross National Income")
#Ans - Squares
####Video 5: Advanced Scatterplots using ggplot#######
ggplot(WHO, aes (x=GNI, y=FertilityRate, color=Region))+geom_point()
ggplot(WHO, aes (x=GNI, y=FertilityRate, color=LifeExpectancy))+geom_point()
ggplot(WHO, aes (x=FertilityRate, y=Under15))+geom_point()
ggplot(WHO, aes (x=log(FertilityRate), y=Under15))+geom_point()
model=lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)
#Add Linear Regression model
ggplot(WHO, aes (x=log(FertilityRate), y=Under15))+geom_point()+stat_smooth(method=lm)
#Change confidence boundaries
ggplot(WHO, aes (x=log(FertilityRate), y=Under15))+geom_point()+stat_smooth(method=lm,level=0.99)
#Remove confidence boundaries
ggplot(WHO, aes (x=log(FertilityRate), y=Under15))+geom_point()+stat_smooth(method=lm,se=FALSE)
#Change color of line
ggplot(WHO, aes (x=log(FertilityRate), y=Under15))+geom_point()+stat_smooth(method=lm,se=FALSE,color="orange")
###Quick Question
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(WHO, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point()
#One region in particular has a lot of countries with a very low fertility rate and a very low percentage of the population under 15. Which region is it?
#Ans - Europe
###############
#The Los Angeles Police Department sees the benefits of predictive policing as which of the following? Select all that apply.
# Allowing more intelligent officer deployment  
# Preventing crime  
# Using resources more effectively 
#########
For which of the following situations would a heat map be an appropriate visualization choice? Select all that apply.
# Visualizing the areas on a geographical map with the most crime  
# Comparing crime counts by police district and time throughout a city  
###########Video3: A Line Plot
mvt=read.csv("mvt.csv",stringsAsFactors=FALSE)
str(mvt)
mvt$Date = strptime(mvt$Date,format="%m/%d/%y %H:%M")
mvt$WeekDay = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
WeekdayCounts=as.data.frame(table(mvt$WeekDay))
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1))
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1))+xlab("Day of the week")+ylab("Total Motor Vehicle Thefts")
########
# geom_line(aes(group=1), linetype=2) - What does this do?
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1),linetype=2)+xlab("Day of the week")+ylab("Total Motor Vehicle Thefts")
# Ans -Makes the line dashed
####
#Now, change the alpha parameter to 0.3 by replacing "linetype=2" with "alpha=0.3" in the plot command. What does this do?
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1),alpha=0.3)+xlab("Day of the week")+ylab("Total Motor Vehicle Thefts")
# Ans - Makes the line lighter in color 
#########Video 4: Heatmap
table(mvt$WeekDay, mvt$Hour)
DayHourCounts = as.data.frame(table(mvt$WeekDay, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x=Hour, y=Freq))+geom_line(aes(group=Var1))
ggplot(DayHourCounts, aes(x=Hour, y=Freq))+geom_line(aes(group=Var1, color=Var1),size=2)
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
ggplot(DayHourCounts, aes(x=Hour, y=Var1))+geom_tile(aes(fill=Freq))
#Remove y axis label, add legend name
ggplot(DayHourCounts, aes(x=Hour, y=Var1))+geom_tile(aes(fill=Freq))+scale_fill_gradient(name="Total MV Thefts")+theme(axis.title.y=element_blank())
#Change color gradient
ggplot(DayHourCounts, aes(x=Hour, y=Var1))+geom_tile(aes(fill=Freq))+scale_fill_gradient(name="Total MV Thefts", low="white", high="red")+theme(axis.title.y=element_blank())
########
# Which argument(s) did we change to get Plot (2)? Select all that apply.
# x, y
# Which argument(s) did we change to get Plot (3)? Select all that apply.
# high
############Video 5: A Geographical Hot Spot Map
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
chicago = get_map(location = "chicago", zoom=11)
#This shows the map
ggmap(chicago)
#This plots the Chicago theft (first 100) based on Longitude and Lattitude to the map
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude, y=Latitude))
#Below rounds the longitide and lattitude so that we dont get a blob of all points
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long=as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat=as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))
#Change color scheme
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))+scale_color_gradient(low="yellow", high="red")
#Heat map
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")
#Heat map with gradient
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq, color=Freq))+scale_color_gradient(low="yellow", high="red")
#####################
#Remoe freq>0
LatLonCounts2 = subset(LatLonCounts, Freq>0)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")
#How many observations did we remove?
#Ans - 952
############
#####Video 6: A Heatmap of the United States
murders=read.csv("murders.csv")
str(murders)
statesMap=map_data("state")
str(statesMap)
#plot statesMap
ggplot(statesMap, aes(x=long, y=lat, group=group))+geom_polygon(fill="white", color="black")
#verify state names in data frames
murders$region= tolower(murders$State)
murderMap = merge(statesMap, murders, by="region")
str(murderMap)
#plot based on Murders
ggplot(murderMap,aes(x=long,y=lat, group=group, fill=Murders))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red",guide="legend")
#plot based on population (to compare if Murders are more because of population)
ggplot(murderMap,aes(x=long,y=lat, group=group, fill=Population))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red",guide="legend")
#Murder rate
murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000
ggplot(murderMap,aes(x=long,y=lat, group=group, fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red",guide="legend")
#The above is all maroon with no distinction. So provide range from 0 to 10
ggplot(murderMap,aes(x=long,y=lat, group=group, fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red",guide="legend",limits=c(0,10))
##################
#Which of the following states has the highest gun ownership rate? 
ggplot(murderMap,aes(x=long,y=lat, group=group, fill=GunOwnership))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red",guide="legend")
# Montana
########Recitation 1 - Video 3
intl = read.csv("intl.csv")
str(intl)
#Bar chart with label on top of bars, Stat idetity means use value of y as is (other options count of x)
ggplot(intl, aes(x=Region, y=PercentOfIntl))+geom_bar(stat="identity")+geom_text(aes(label=PercentOfIntl))
#When I try the below, it shows that stat_count cannot be used with a y aesthetic. This shows default it takes count of x as y. Since we have given a y aesthetic, it cannot take count of y.
ggplot(intl, aes(x=Region, y=PercentOfIntl))+geom_bar()
#Make region ordered based on desc percentage
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
#Make Percent as Percent instead of decimal
intl$PercentOfIntl = intl$PercentOfIntl * 100
#Fill dark blue, Orient label with readbility, add good ylabel, remove x label, orient X text
ggplot(intl, aes(x=Region, y=PercentOfIntl))+
geom_bar(stat="identity", fill="dark blue")+
geom_text(aes(label=PercentOfIntl), vjust=-0.4)+ 
ylab("Percent of International Students")+
theme(axis.title.x = element_blank(), axis.text.x=element_text(angle=45, hjust=1))
#####Video 6: World Maps in R
intlall = read.csv("intlall.csv", stringsAsFactors=FALSE)
str(intlall)
head(intlall)
#Change NA to 0
intlall[is.na(intlall)]=0
#Load world map
world_map = map_data("world")
world_map = merge(world_map,intlall,by.x="region",by.y="Citizenship")
#plot
ggplot(world_map,aes(x=long, y=lat, group=group))+geom_polygon(fill="white",color="black")+coord_map("mercator")+ scale_x_continuous(limits = c(-180, 180))
#Reorder the data
world_map = world_map[order(world_map$group,world_map$order),]
#Plot and notice China missing
ggplot(world_map,aes(x=long, y=lat, group=group))+geom_polygon(fill="white",color="black")+coord_map("mercator")+ scale_x_continuous(limits = c(-180, 180))
#Confirm if all country names are intact in intlall
table(intlall$Citizenship)
#Name of China is not matching world map. So change it
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"]="China"
#Merge again
world_map = merge(map_data("world"),intlall,by.x="region",by.y="Citizenship")
#Reorder the data
world_map = world_map[order(world_map$group,world_map$order),]
#Plot again, fill countries based on Total students
ggplot(world_map,aes(x=long, y=lat, group=group))+geom_polygon(aes(fill=Total),color="black")+coord_map("mercator")+ scale_x_continuous(limits = c(-180, 180))
#Orthographic projection, with orientation where in the world we are focusing (Imagine like a globe)
ggplot(world_map,aes(x=long, y=lat, group=group))+geom_polygon(aes(fill=Total),color="black")+coord_map("ortho",orientation=c(20,30,0))+ scale_x_continuous(limits = c(-180, 180))
#####Video 7 Line Chart
household=read.csv("households.csv")
str(household)
#Use melt so that we can transform column name to row value. This is needed to put this as aes in ggplot
library(reshape2)
household[,1:2]
melt(household, id="Year")
#plot
ggplot(melt(household, id="Year"), aes(x=Year,y=value,color=variable))+geom_line(size=2)+geom_point(size=5)+ylab("Percentage of Households")
