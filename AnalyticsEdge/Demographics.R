#1.1 - 131302
CPS=read.csv("CPSData.csv")
str(CPS)
#1.2 - Educational and health services
table(CPS$Industry)
#1.3 New Mexico,California
sort(table(CPS$State))
#1.4 0.88832615
prop.table(table(CPS$Citizenship))
#1.5 -  American Indian, Black, Multiracial, White 
table(CPS$Race[CPS$Hispanic == 1])

#2.1 - MetroAreaCode, Married, Eductaion, EmploymentStatus, Industry
summary(CPS)

#2.2 - Age
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

#2.3 --  2(Alaska, Wyoming), 3 (DC,NewJersey,EhodeIsland)
table(CPS$State, is.na(CPS$MetroAreaCode))

#2.4 - MidWest
table(CPS$Region, is.na(CPS$MetroAreaCode))

#2.5 - Wisconsin, Montana
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

#3.1 271,149
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)

#3.2 MetroArea, 34238
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

#3.3 Boston
sort(table(CPS$MetroArea))

#3.4 Laredo, TX
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

#3.5 - 4
sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))

#3.6 - Iowa City, IA
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

#4.1 - Country, 176
CPS = merge(CPS,CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

#4.2 - Mexico
sort(table(CPS$Country))

#4.3 -  0.308660252
 tapply(CPS$Country!="United States",CPS$MetroArea,mean,na.rm=TRUE)

#4.4 -  New York, Boston, Minneapolis
sort( table(CPS$MetroArea[CPS$Country=="India"]))
sort( table(CPS$MetroArea[CPS$Country=="Brazil"]))
sort( table(CPS$MetroArea[CPS$Country=="Somalia"]))


