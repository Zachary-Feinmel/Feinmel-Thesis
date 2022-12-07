setwd("C:/Users/zjfei/OneDrive/Desktop/Data")
library(dplyr)
library(tidyverse)
library(rio)
library(readxl)
library(XLS)
library(reshape2)
library(stringr)


#### GDP ####
GDP = read_excel("RealGSP.xls")
GDP = GDP[-c(1, 53:60),]
rGDP = melt(GDP)

rGDP = rGDP %>% rename('State' = GeoName,
                       'Quarter' = variable, 
                       'GDP' = value)
rGDP$Q = as.numeric(str_sub(rGDP$Quarter, -1, -1))
rGDP$Y = as.numeric(str_sub(rGDP$Quarter, 1,4))
rGDP = rGDP[,-2]
rGDP = rGDP %>% rename(Quarter = 'Q', 
                       Year = 'Y')
rGDP$logGDP = log(rGDP$GDP)
rGDP = rGDP %>% group_by(State, Year, Quarter, GDP, logGDP)
rGDP2 = subset(rGDP, rGDP$State != 'District of Columbia')

rGDP2$State = state.abb[match(rGDP2$State, state.name)]

GDP = rGDP2
rm(rGDP, rGDP2)


#### UR ####
ur = import('UR.xlsx')
ur$Month = month(ur$Date)
ur$Year = year(ur$Date)


ur$Quarter = ifelse(ur$Month >=1 & ur$Month<=3, 1,
                    ifelse(ur$Month >= 4 & ur$Month<=6, 2,
                           ifelse(ur$Month >= 7 & ur$Month <=9, 3, 4)))


ur$YQ <- as.yearqtr(ur$Year, " ", ur$Quarter)

ur$UR <- as.numeric(ur$UR)
ur$Date <- as.Date(as.yearmon(paste(ur$Year, ur$Month), "%Y %m"))
urtest <- ur %>% mutate(quarter=cut.Date(Date,"quarter", labels=F)) %>% 
  group_by(quarter,State) %>% 
  summarise(Date=min(Date), UR=mean(UR))


ur = urtest ; rm(urtest)

ur$Year = format(as.Date(ur$Date, format="%d/%m/%Y"),"%Y")
ur$Q = format(as.Date(ur$Date, format = "%d%m%Y"), "%m")

ur = ur %>% rename('Quarter' = quarter)

ur$Quarter = ifelse(ur$Q == "01", 1, ur$Quarter)
ur$Quarter = ifelse(ur$Q == "04", 2, ur$Quarter)
ur$Quarter = ifelse(ur$Q == "07", 3, ur$Quarter)
ur$Quarter = ifelse(ur$Q == "10", 4, ur$Quarter)

ur = ur[,-c(3, 6)]
ur$UR = round(ur$UR, 2)


#### Wage ####

  # 2010 q1
wage10q1 <- read_excel("2010_all_county_high_level/allhlcn101.xlsx")
wage10q1[c('State', 'Class')] = str_split_fixed(wage10q1$Area, "--", 2)
sub = wage10q1 %>% subset(wage10q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage10q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  # 2010 q2
wage10q2 <- read_excel("2010_all_county_high_level/allhlcn102.xlsx")
wage10q2[c('State', 'Class')] = str_split_fixed(wage10q2$Area, "--", 2)
sub = wage10q2 %>% subset(wage10q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage10q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  #2010 q3
wage10q3 <- read_excel("2010_all_county_high_level/allhlcn103.xlsx")
wage10q3[c('State', 'Class')] = str_split_fixed(wage10q3$Area, "--", 2)
sub = wage10q3 %>% subset(wage10q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage10q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  #2010q4
wage10q4 <- read_excel("2010_all_county_high_level/allhlcn104.xlsx")
wage10q4[c('State', 'Class')] = str_split_fixed(wage10q4$Area, "--", 2)
sub = wage10q4 %>% subset(wage10q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage10q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage10 = rbind(wage10q1, wage10q3, wage10q3, wage10q4)

wage10 = wage10 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage10q1, wage10q2, wage10q3, wage10q4)


  #2011 q1
wage11q1 <- read_excel("2011_all_county_high_level/allhlcn111.xlsx")
wage11q1[c('State', 'Class')] = str_split_fixed(wage11q1$Area, "--", 2)
sub = wage11q1 %>% subset(wage11q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage11q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  #2011 q3
wage11q2 <- read_excel("2011_all_county_high_level/allhlcn112.xlsx")
wage11q2[c('State', 'Class')] = str_split_fixed(wage11q2$Area, "--", 2)
sub = wage11q2 %>% subset(wage11q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage11q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  # 2011 q3
wage11q3 <- read_excel("2011_all_county_high_level/allhlcn113.xlsx")
wage11q3[c('State', 'Class')] = str_split_fixed(wage11q3$Area, "--", 2)
sub = wage11q3 %>% subset(wage11q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage11q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


  # 2011 q4
wage11q4 <- read_excel("2011_all_county_high_level/allhlcn114.xlsx")
wage11q4[c('State', 'Class')] = str_split_fixed(wage11q4$Area, "--", 2)
sub = wage11q4 %>% subset(wage11q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage11q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage11 = rbind(wage11q1, wage11q3, wage11q3, wage11q4)
wage11 = wage11 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage11q1, wage11q2, wage11q3, wage11q4)




#2012 q1
wage12q1 <- read_excel("2012_all_county_high_level/allhlcn121.xlsx")
wage12q1[c('State', 'Class')] = str_split_fixed(wage12q1$Area, "--", 2)
sub = wage12q1 %>% subset(wage12q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage12q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


#2012 q3
wage12q2 <- read_excel("2012_all_county_high_level/allhlcn122.xlsx")
wage12q2[c('State', 'Class')] = str_split_fixed(wage12q2$Area, "--", 2)
sub = wage12q2 %>% subset(wage12q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage12q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2012 q3
wage12q3 <- read_excel("2012_all_county_high_level/allhlcn123.xlsx")
wage12q3[c('State', 'Class')] = str_split_fixed(wage12q3$Area, "--", 2)
sub = wage12q3 %>% subset(wage12q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage12q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2012 q4
wage12q4 <- read_excel("2012_all_county_high_level/allhlcn124.xlsx")
wage12q4[c('State', 'Class')] = str_split_fixed(wage12q4$Area, "--", 2)
sub = wage12q4 %>% subset(wage12q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage12q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage12 = rbind(wage12q1, wage12q3, wage12q3, wage12q4)
wage12 = wage12 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage12q1, wage12q2, wage12q3, wage12q4)



#2013 q1
wage13q1 <- read_excel("2013_all_county_high_level/allhlcn131.xlsx")
wage13q1[c('State', 'Class')] = str_split_fixed(wage13q1$Area, "--", 2)
sub = wage13q1 %>% subset(wage13q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage13q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


#2013 q3
wage13q2 <- read_excel("2013_all_county_high_level/allhlcn132.xlsx")
wage13q2[c('State', 'Class')] = str_split_fixed(wage13q2$Area, "--", 2)
sub = wage13q2 %>% subset(wage13q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage13q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2013 q3
wage13q3 <- read_excel("2013_all_county_high_level/allhlcn133.xlsx")
wage13q3[c('State', 'Class')] = str_split_fixed(wage13q3$Area, "--", 2)
sub = wage13q3 %>% subset(wage13q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage13q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2013 q4
wage13q4 <- read_excel("2013_all_county_high_level/allhlcn134.xlsx")
wage13q4[c('State', 'Class')] = str_split_fixed(wage13q4$Area, "--", 2)
sub = wage13q4 %>% subset(wage13q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage13q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage13 = rbind(wage13q1, wage13q3, wage13q3, wage13q4)
wage13 = wage13 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage13q1, wage13q2, wage13q3, wage13q4)



#2014 q1
wage14q1 <- read_excel("2014_all_county_high_level/allhlcn141.xlsx")
wage14q1[c('State', 'Class')] = str_split_fixed(wage14q1$Area, "--", 2)
sub = wage14q1 %>% subset(wage14q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage14q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


#2014 q3
wage14q2 <- read_excel("2014_all_county_high_level/allhlcn142.xlsx")
wage14q2[c('State', 'Class')] = str_split_fixed(wage14q2$Area, "--", 2)
sub = wage14q2 %>% subset(wage14q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage14q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2014 q3
wage14q3 <- read_excel("2014_all_county_high_level/allhlcn143.xlsx")
wage14q3[c('State', 'Class')] = str_split_fixed(wage14q3$Area, "--", 2)
sub = wage14q3 %>% subset(wage14q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage14q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2014 q4
wage14q4 <- read_excel("2014_all_county_high_level/allhlcn144.xlsx")
wage14q4[c('State', 'Class')] = str_split_fixed(wage14q4$Area, "--", 2)
sub = wage14q4 %>% subset(wage14q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage14q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage14 = rbind(wage14q1, wage14q3, wage14q3, wage14q4)
wage14 = wage14 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage14q1, wage14q2, wage14q3, wage14q4)




#2015 q1
wage15q1 <- read_excel("2015_all_county_high_level/allhlcn151.xlsx")
wage15q1[c('State', 'Class')] = str_split_fixed(wage15q1$Area, "--", 2)
sub = wage15q1 %>% subset(wage15q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage15q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


#2015 q3
wage15q2 <- read_excel("2015_all_county_high_level/allhlcn152.xlsx")
wage15q2[c('State', 'Class')] = str_split_fixed(wage15q2$Area, "--", 2)
sub = wage15q2 %>% subset(wage15q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage15q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2015 q3
wage15q3 <- read_excel("2015_all_county_high_level/allhlcn153.xlsx")
wage15q3[c('State', 'Class')] = str_split_fixed(wage15q3$Area, "--", 2)
sub = wage15q3 %>% subset(wage15q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage15q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2015 q4
wage15q4 <- read_excel("2015_all_county_high_level/allhlcn154.xlsx")
wage15q4[c('State', 'Class')] = str_split_fixed(wage15q4$Area, "--", 2)
sub = wage15q4 %>% subset(wage15q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage15q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage15 = rbind(wage15q1, wage15q3, wage15q3, wage15q4)
wage15 = wage15 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage15q1, wage15q2, wage15q3, wage15q4)



#2016 q1
wage16q1 <- read_excel("2016_all_county_high_level/allhlcn161.xlsx")
wage16q1[c('State', 'Class')] = str_split_fixed(wage16q1$Area, "--", 2)
sub = wage16q1 %>% subset(wage16q1$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage16q1 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


#2016 q3
wage16q2 <- read_excel("2016_all_county_high_level/allhlcn162.xlsx")
wage16q2[c('State', 'Class')] = str_split_fixed(wage16q2$Area, "--", 2)
sub = wage16q2 %>% subset(wage16q2$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage16q2 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2016 q3
wage16q3 <- read_excel("2016_all_county_high_level/allhlcn163.xlsx")
wage16q3[c('State', 'Class')] = str_split_fixed(wage16q3$Area, "--", 2)
sub = wage16q3 %>% subset(wage16q3$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage16q3 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


# 2016 q4
wage16q4 <- read_excel("2016_all_county_high_level/allhlcn164.xlsx")
wage16q4[c('State', 'Class')] = str_split_fixed(wage16q4$Area, "--", 2)
sub = wage16q4 %>% subset(wage16q4$Class == ' Statewide')
sub2 = sub %>% subset(sub$Ownership == 'Total Covered')

wage16q4 = data.frame(sub2$State, sub2$Year, sub2$Qtr, sub2$`Average Weekly Wage`)


wage16 = rbind(wage16q1, wage16q3, wage16q3, wage16q4)
wage16 = wage16 %>% rename('State' = sub2.State, 
                           'Year' = sub2.Year, 
                           'Quarter' = sub2.Qtr, 
                           'Wage' = sub2..Average.Weekly.Wage.)

rm(sub, sub2, wage16q1, wage16q2, wage16q3, wage16q4)



wage = rbind(wage10, wage11, wage12, wage13, wage14, wage15, wage16)
rm(wage10, wage11, wage12, wage13, wage14, wage15, wage16)

wage = import('finalwage.xlsx')


wage$logWage = log(wage$Wage)

wage$State = state.abb[match(wage$State, state.name)]


abr = c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 
            'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 
            'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 
            'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 
            'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')

s = rep(abr, each = 4, times = 7)

wage2 = wage

wage2$State = s

merge = merge(GDP, wage2, by = c("State", "Year", "Quarter"))
merge$Quarter2 = rep(1:4, times = 350)
merge$Quarter = merge$Quarter2
merge = merge[, -8]

rm(wage, wage2)
#### Police ####
p10 = import('police10.xls')
p11 = import('police11.xls')
p12 = import('police12.xls')
p13 = import('police13.xls')
p14 = import('police14.xls') 
p15 = import('police15.xls')
p16 = import('police16.xlsx')


p10$Year = 2010
p11$Year = 2011
p12$Year = 2012
p13$Year = 2013
p14$Year = 2014
p15$Year = 2015
p16$Year = 2016

p10 = p10[,c(1,2,11)]
p11 = p11[,c(1,2,9)]
p12 = p12[,c(1,2,9)]
p13 = p13[,c(1,2,9)]
p14 = p14[,c(1,2,9)]
p15 = p15[,c(1,2,13)]
p16 = p16[,c(1,2,4)]


p10 = p10 %>% rename('Police' = `Total law enforcement employees`)
p11 = p11 %>% rename('Police' = `Total law
enforcement
employees`)
p12 = p12 %>% rename('Police' = `Total law
enforcement
employees`)
p13 = p13 %>% rename('Police' = `Total law
enforcement
employees`)
p14 = p14 %>% rename('Police' = `Total law
enforcement
employees`)
p15 = p15 %>% rename('Police' = `Total law
enforcement
employees`)
p16 = p16 %>% rename('Police' = totallaw)


p16 = p16 %>% rename('State' = state)




police = rbind(p10, p11, p12, p13, p14, p15, p16)
police$State = str_to_title(police$State)
police = subset(police, State != 'District Of Columbia\n')
police$State = state.abb[match(police$State, state.name)]

p2 = na.omit(police)


merge2 = merge(merge, police, by = c("State", "Year"))

rm(p10, p11, p12, p13, p14, p15, p16)
rm(police)

#### Divorce Rates ####
  # Per 1000 total population residing in the area

divorce = import('MarriageRate.xlsx')

divorce = divorce %>% rename('Divorce' = MarriageRate)

divorce$State = ifelse(divorce$State == 'California 1', 'California', divorce$State)
divorce = subset(divorce, divorce$State != 'District of Columbia')
divorce$State = state.abb[match(divorce$State, state.name)]

merge3 = merge(merge2, divorce, by = c("State", "Year"))



#### OD ####
od <- read.delim("C:/Users/zjfei/OneDrive/Desktop/Data/Multiple Cause of Death, 1999-2020.txt")

od = od %>% rename('Date' = Month.Code)
od = od[,-c(1, 9, 10)]

od$Month = as.numeric(str_sub(od$Date, -2, -1))
od$Year = as.numeric(str_sub(od$Date, 1,4))
od = od[,-c(2,6)]
od = od[,-3]
od = subset(od, od$State != 'District of Columbia')
od = subset(od, od$State != "")

od$Deaths = ifelse(od$Deaths == 'Suppressed', NA, od$Deaths)


od$Quarter = ifelse(od$Month >=1 & od$Month<=3, 1,
                           ifelse(od$Month >= 4 & od$Month<=6, 2,
                                  ifelse(od$Month >= 7 & od$Month <=9, 3, 4)))


od$YQ <- as.yearqtr(od$Year, " ", od$Quarter)

library('zoo')
od$Deaths <- as.numeric(od$Deaths)
od$Date <- as.Date(as.yearmon(paste(od$Year, od$Month), "%Y %m"))
odtest <- od %>% mutate(quarter=cut.Date(Date,"quarter", labels=F)) %>% group_by(quarter,State) %>% summarise(Date=min(Date), Deaths=mean(Deaths))

od = odtest
rm(odtest)

od = od %>% rename('Quarter' = quarter)
od$Deaths = round(od$Deaths, 2)

od$Year = format(as.Date(od$Date, format="%d/%m/%Y"),"%Y")
od$Q = format(as.Date(od$Date, format = "%d%m%Y"), "%m")

od$Quarter = ifelse(od$Q == "01", 1, od$Quarter)
od$Quarter = ifelse(od$Q == "04", 2, od$Quarter)
od$Quarter = ifelse(od$Q == "07", 3, od$Quarter)
od$Quarter = ifelse(od$Q == "10", 4, od$Quarter)

od = od[,-c(3,6)]
od

od$State = state.abb[match(od$State, state.name)]

merge4 = merge(merge3, od, by = c("State", "Quarter", "Year"))
control = merge4


merge4 = import('realfinalcontrol.xlsx')
merge5 = merge(merge4, ur)

export(merge4, 'realfinalcontrol.xlsx')


