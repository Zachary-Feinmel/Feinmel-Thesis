
rm(list = ls())
library(tidyverse)
library(haven)
library(data.table)
library(collapse)
library(dplyr)
library(zoo)
library(rio)


setwd("C:/Users/zjfei/OneDrive/Desktop/Data")

# 2010 NIBRS
d2010 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2010.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, B1008, B1009, B1011, 
                                B2005, B3011,V20061,V20062,V20063,
                                V50071, V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2010, c('B1008','B1009','B1011','B2005','B3011','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

# 'V50071, V50072', 'V50073', 'V50081', 'V50082', 'V50083', 'V50091', 
#'V50092', 'V50093'

d2010 = d2010 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2010$prost1 = d2010$UCRCODE1 == 401 | d2010$UCRCODE1 == 402
d2010$prost2 = d2010$UCRCODE2 == 401 | d2010$UCRCODE2 == 402
d2010$prost3 = d2010$UCRCODE3 == 401 | d2010$UCRCODE3 == 402
d2010$prost = d2010$prost1 + d2010$prost2 + d2010$prost3

prost2010 = subset(d2010, prost > 0)

prost2010$white1 = prost2010$RACE1 == 1
prost2010$white2 = prost2010$RACE2 == 1
prost2010$white3 = prost2010$RACE3 == 1
prost2010$white = prost2010$white1 + prost2010$white2 + prost2010$white3

prost2010$black1 = prost2010$RACE1 == 2
prost2010$black2 = prost2010$RACE2 == 2
prost2010$black3 = prost2010$RACE2 == 2
prost2010$black = prost2010$black1 + prost2010$black2 + prost2010$black3



prost2010$year = substr(prost2010$INCDATE,1,4)
prost2010$month = substr(prost2010$INCDATE,5,6)


prost2010$month = as.numeric(prost2010$month)

prost2010$quarter = ifelse(prost2010$month >=1 & prost2010$month<=3, 1,
                           ifelse(prost2010$month >= 4 & prost2010$month<=6, 2,
                                  ifelse(prost2010$month >= 7 & prost2010$month <=9, 3, 4)))


prost2010$Year = 2010

rm(d2010)

count2010 = prost2010 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                          Total_White = sum(white), 
                                          Total_Black = sum(black))

count2010$Year = 2010







# 2011

d2011 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2011.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, B1008, B1009, B1011, 
                                B2005, B3011,V20061,V20062,V20063,
                                V50071, V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2011, c('B1008','B1009','B1011','B2005','B3011','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

# 'V50071, V50072', 'V50073', 'V50081', 'V50082', 'V50083', 'V50091', 
#'V50092', 'V50093'

d2011 = d2011 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2011$prost1 = d2011$UCRCODE1 == 401 | d2011$UCRCODE1 == 402
d2011$prost2 = d2011$UCRCODE2 == 401 | d2011$UCRCODE2 == 402
d2011$prost3 = d2011$UCRCODE3 == 401 | d2011$UCRCODE3 == 402
d2011$prost = d2011$prost1 + d2011$prost2 + d2011$prost3

prost2011 = subset(d2011, prost > 0)

prost2011$white1 = prost2011$RACE1 == 1
prost2011$white2 = prost2011$RACE2 == 1
prost2011$white3 = prost2011$RACE3 == 1
prost2011$white = prost2011$white1 + prost2011$white2 + prost2011$white3

prost2011$black1 = prost2011$RACE1 == 2
prost2011$black2 = prost2011$RACE2 == 2
prost2011$black3 = prost2011$RACE2 == 2
prost2011$black = prost2011$black1 + prost2011$black2 + prost2011$black3



prost2011$year = substr(prost2011$INCDATE,1,4)
prost2011$month = substr(prost2011$INCDATE,5,6)


prost2011$month = as.numeric(prost2011$month)

prost2011$quarter = ifelse(prost2011$month >=1 & prost2011$month<=3, 1,
                           ifelse(prost2011$month >= 4 & prost2011$month<=6, 2,
                                  ifelse(prost2011$month >= 7 & prost2011$month <=9, 3, 4)))


prost2011$Year = 2011

rm(d2011)

count2011 = prost2011 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                          Total_White = sum(white), 
                                          Total_Black = sum(black))


count2011$Year = 2011




# 2012
d2012 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2012.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, B1008, B1009, B1011, 
                                B2005, B3011,V20061,V20062,V20063,
                                V50071, V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2012, c('B1008','B1009','B1011','B2005','B3011','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

# 'V50071, V50072', 'V50073', 'V50081', 'V50082', 'V50083', 'V50091', 
#'V50092', 'V50093'

d2012 = d2012 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2012$prost1 = d2012$UCRCODE1 == 401 | d2012$UCRCODE1 == 402
d2012$prost2 = d2012$UCRCODE2 == 401 | d2012$UCRCODE2 == 402
d2012$prost3 = d2012$UCRCODE3 == 401 | d2012$UCRCODE3 == 402
d2012$prost = d2012$prost1 + d2012$prost2 + d2012$prost3

prost2012 = subset(d2012, prost > 0)

prost2012$white1 = prost2012$RACE1 == 1
prost2012$white2 = prost2012$RACE2 == 1
prost2012$white3 = prost2012$RACE3 == 1
prost2012$white = prost2012$white1 + prost2012$white2 + prost2012$white3

prost2012$black1 = prost2012$RACE1 == 2
prost2012$black2 = prost2012$RACE2 == 2
prost2012$black3 = prost2012$RACE2 == 2
prost2012$black = prost2012$black1 + prost2012$black2 + prost2012$black3



prost2012$year = substr(prost2012$INCDATE,1,4)
prost2012$month = substr(prost2012$INCDATE,5,6)


prost2012$month = as.numeric(prost2012$month)

prost2012$quarter = ifelse(prost2012$month >=1 & prost2012$month<=3, 1,
                           ifelse(prost2012$month >= 4 & prost2012$month<=6, 2,
                                  ifelse(prost2012$month >= 7 & prost2012$month <=9, 3, 4)))


prost2012$Year = 2012

rm(d2012)

count2012 = prost2012 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                          Total_White = sum(white), 
                                          Total_Black = sum(black))









d2013 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2013.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, BH008, BH009, BH011, 
                                BH019, BH041, V20061, V20062, V20063, V50071, 
                                V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2013, c('BH008','BH009','BH011','BH019','BH041','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

d2013 = d2013 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2013$prost1 = d2013$UCRCODE1 == 401 | d2013$UCRCODE1 == 402
d2013$prost2 = d2013$UCRCODE2 == 401 | d2013$UCRCODE2 == 402
d2013$prost3 = d2013$UCRCODE3 == 401 | d2013$UCRCODE3 == 402
d2013$prost = d2013$prost1 + d2013$prost2 + d2013$prost3

prost2013 = subset(d2013, prost > 0)

prost2013$white1 = prost2013$RACE1 == 1
prost2013$white2 = prost2013$RACE2 == 1
prost2013$white3 = prost2013$RACE3 == 1
prost2013$white = prost2013$white1 + prost2013$white2 + prost2013$white3

prost2013$black1 = prost2013$RACE1 == 2
prost2013$black2 = prost2013$RACE2 == 2
prost2013$black3 = prost2013$RACE2 == 2
prost2013$black = prost2013$black1 + prost2013$black2 + prost2013$black3



prost2013$year = substr(prost2013$INCDATE,1,4)
prost2013$month = substr(prost2013$INCDATE,5,6)


prost2013$month = as.numeric(prost2013$month)

prost2013$quarter = ifelse(prost2013$month >=1 & prost2013$month<=3, 1,
                           ifelse(prost2013$month >= 4 & prost2013$month<=6, 2,
                                  ifelse(prost2013$month >= 7 & prost2013$month <=9, 3, 4)))


prost2013$Year = 2013

rm(d2013)

count2013 = prost2013 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                                         Total_White = sum(white), 
                                                         Total_Black = sum(black))










d2014 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2014.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, BH008, BH009, BH011, 
                                BH019, BH041, V20061, V20062, V20063, V50071, 
                                V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2014, c('BH008','BH009','BH011','BH019','BH041','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

d2014 = d2014 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2014$prost1 = d2014$UCRCODE1 == 401 | d2014$UCRCODE1 == 402
d2014$prost2 = d2014$UCRCODE2 == 401 | d2014$UCRCODE2 == 402
d2014$prost3 = d2014$UCRCODE3 == 401 | d2014$UCRCODE3 == 402
d2014$prost = d2014$prost1 + d2014$prost2 + d2014$prost3

prost2014 = subset(d2014, prost > 0)

prost2014$white1 = prost2014$RACE1 == 1
prost2014$white2 = prost2014$RACE2 == 1
prost2014$white3 = prost2014$RACE3 == 1
prost2014$white = prost2014$white1 + prost2014$white2 + prost2014$white3

prost2014$black1 = prost2014$RACE1 == 2
prost2014$black2 = prost2014$RACE2 == 2
prost2014$black3 = prost2014$RACE2 == 2
prost2014$black = prost2014$black1 + prost2014$black2 + prost2014$black3



prost2014$year = substr(prost2014$INCDATE,1,4)
prost2014$month = substr(prost2014$INCDATE,5,6)


prost2014$month = as.numeric(prost2014$month)

prost2014$quarter = ifelse(prost2014$month >=1 & prost2014$month<=3, 1,
                           ifelse(prost2014$month >= 4 & prost2014$month<=6, 2,
                                  ifelse(prost2014$month >= 7 & prost2014$month <=9, 3, 4)))


prost2014$Year = 2014

rm(d2014)

count2014 = prost2014 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                                         Total_White = sum(white), 
                                                         Total_Black = sum(black))







d2015 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2015.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, BH008, BH009, BH011, 
                                BH019, BH041, V20061, V20062, V20063, V50071, 
                                V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2015, c('BH008','BH009','BH011','BH019','BH041','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

d2015 = d2015 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2015$prost1 = d2015$UCRCODE1 == 401 | d2015$UCRCODE1 == 402
d2015$prost2 = d2015$UCRCODE2 == 401 | d2015$UCRCODE2 == 402
d2015$prost3 = d2015$UCRCODE3 == 401 | d2015$UCRCODE3 == 402
d2015$prost = d2015$prost1 + d2015$prost2 + d2015$prost3

prost2015 = subset(d2015, prost > 0)

prost2015$white1 = prost2015$RACE1 == 1
prost2015$white2 = prost2015$RACE2 == 1
prost2015$white3 = prost2015$RACE3 == 1
prost2015$white = prost2015$white1 + prost2015$white2 + prost2015$white3

prost2015$black1 = prost2015$RACE1 == 2
prost2015$black2 = prost2015$RACE2 == 2
prost2015$black3 = prost2015$RACE2 == 2
prost2015$black = prost2015$black1 + prost2015$black2 + prost2015$black3



prost2015$year = substr(prost2015$INCDATE,1,4)
prost2015$month = substr(prost2015$INCDATE,5,6)


prost2015$month = as.numeric(prost2015$month)

prost2015$quarter = ifelse(prost2015$month >=1 & prost2015$month<=3, 1,
                           ifelse(prost2015$month >= 4 & prost2015$month<=6, 2,
                                  ifelse(prost2015$month >= 7 & prost2015$month <=9, 3, 4)))


prost2015$Year = 2015

rm(d2015)

count2015 = prost2015 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                                         Total_White = sum(white), 
                                                         Total_Black = sum(black))






d2016 = read_dta(file = 'C:/Users/zjfei/OneDrive/Desktop/Data/2016.dta', 
                 col_select = c(STATE, INCNUM, INCDATE, BH008, BH009, BH011, 
                                BH019, BH041, V20061, V20062, V20063, V50071, 
                                V50072, V50073, V50081, V50082, 
                                V50083, V50091, V50092, V50093))

setnames(d2016, c('BH008','BH009','BH011','BH019','BH041','V20061','V20062','V20063'),
         c('STATEABR','POPGROUP','REGION','POP','YEAR','UCRCODE1','UCRCODE2','UCRCODE3'))

d2016 = d2016 %>% rename(AGE1 = 'V50071', AGE2 = 'V50072', AGE3 = 'V50073', 
                         SEX1 = 'V50081', SEX2 = 'V50082', SEX3 = 'V50083', 
                         RACE1 = 'V50091', RACE2 = 'V50092', RACE3 = 'V50093')

d2016$prost1 = d2016$UCRCODE1 == 401 | d2016$UCRCODE1 == 402
d2016$prost2 = d2016$UCRCODE2 == 401 | d2016$UCRCODE2 == 402
d2016$prost3 = d2016$UCRCODE3 == 401 | d2016$UCRCODE3 == 402
d2016$prost = d2016$prost1 + d2016$prost2 + d2016$prost3

prost2016 = subset(d2016, prost > 0)

prost2016$white1 = prost2016$RACE1 == 1
prost2016$white2 = prost2016$RACE2 == 1
prost2016$white3 = prost2016$RACE3 == 1
prost2016$white = prost2016$white1 + prost2016$white2 + prost2016$white3

prost2016$black1 = prost2016$RACE1 == 2
prost2016$black2 = prost2016$RACE2 == 2
prost2016$black3 = prost2016$RACE2 == 2
prost2016$black = prost2016$black1 + prost2016$black2 + prost2016$black3



prost2016$year = substr(prost2016$INCDATE,1,4)
prost2016$month = substr(prost2016$INCDATE,5,6)


prost2016$month = as.numeric(prost2016$month)

prost2016$quarter = ifelse(prost2016$month >=1 & prost2016$month<=3, 1,
                           ifelse(prost2016$month >= 4 & prost2016$month<=6, 2,
                                  ifelse(prost2016$month >= 7 & prost2016$month <=9, 3, 4)))


prost2016$Year = 2016

rm(d2016)

count2016 = prost2016 %>%
  group_by(STATEABR, Year, quarter) %>% summarise(Total_Arrests = sum(prost), 
                                                         Total_White = sum(white), 
                                                         Total_Black = sum(black))

data = rbind(count2010, count2011, count2012, count2013, count2014, count2015, count2016)



export(data, 'RealNIBRSFinal.xlsx')
