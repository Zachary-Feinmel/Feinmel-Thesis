# Analysis
rm(list = ls())
library(dplyr)
library(tidyverse)
library(rio)
library(readxl)
library(XLS)
library(reshape2)
library(haven)
library(collapse)
library(plm)
library(ggplot2)
library(gplots)
library(foreign)
library(metagam)
library(rio)
library(stargazer)
library(lubridate)
library(zoo)
library(stringr)
library(plm)


#### IGNORE #####
#nibrs = import('RealNIBRSFinal.xlsx')
#control = import('realfinalcontrol.xlsx')

#nibrs = nibrs %>% rename('Quarter' = quarter, 
                        # 'State' = STATEABR)

#data = merge(nibrs, control, by = c("State", "Year", "Quarter"))
#data = data %>% subset(State != 'AL')
#data = data %>% subset(State != 'IN')
#data = data %>% subset(State != 'PA')
#data = data %>% subset(State != 'AZ')


#pop = import('pop.xlsx')

#data2 = merge(data, pop, by = c("State", "Year"))
#data = data2

#data$Pop = data$Pop * 1000
#data$PopK = data$Pop / 1000
#data$PolicePopK = data$Police / data$PopK


#data$GDP = data$GDP * 1000000



data = data %>% mutate('Death/Pop' = Deaths / Pop, 
                       'White/Pop' = Total_White / Pop, 
                       'Black/Pop' = Total_Black / Pop, 
                       'Police/Pop' = Police / Pop, 
                       'NIBRS/ Pop' = Total_Arrests / Pop, 
                       'GDPPerCap' = GDP / Pop)

data = data %>% rename('NIBRS' = Total_Arrests)


#### Create YQ) ####
data$YQ = str_c(data$Year,'-', data$Quarter)
data$YQ = as.yearqtr(data$YQ)
data = data %>% rename('Date' = YQ)




##### Convert to Panel Data Frame ####
data.p = pdata.frame(data, index = c("State", "Date"))
data = data.p
rm(data.p)



#### Normalize Racial Data ####
data$Black = data$Total_Black / data$Pop
data = data %>% rename('OD' = Deaths)
data$logBlack = log(data$Black)
data$logBlack = ifelse(data$logBlack == -Inf, NA, data$logBlack)


data = data %>% rename('Marriage' = Divorce) # Read in as wrong data initially


data$Overdose = data$OD / data$Pop # Normalize Overdose data


#### Summary Statistics)
sum = data[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
stargazer(sum, summary = TRUE, 
          type = 'html',
          title = 'Summary Descriptive Statistics',
          out = 'FinalSummary.html')


#### Yearly Summary ####
data10 = subset(data, Year == 2010)
data11 = subset(data, Year == 2011)
data12 = subset(data, Year == 2012)
data13 = subset(data, Year == 2013)
data14 = subset(data, Year == 2014)
data15 = subset(data, Year == 2015)
data16 = subset(data, Year == 2016)

sum10 = data10[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data10)
stargazer(sum10, summary = TRUE, 
          type = 'html', 
          out = 'Summary10.html', 
          title = 'Descriptive Statistics: 2010 Averages among States')

sum11 = data11[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data11)
stargazer(sum11, summary = TRUE, 
          type = 'html', 
          out = 'Summary11.html', 
          title = 'Descriptive Statistics: 2011 Averages among States')

sum12 = data12[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data12)
stargazer(sum12, summary = TRUE, 
          type = 'html', 
          out = 'Summary12.html', 
          title = 'Descriptive Statistics: 2012 Averages among States')

sum13 = data13[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data13)
stargazer(sum13, summary = TRUE, 
          type = 'html', 
          out = 'Summary13.html', 
          title = 'Descriptive Statistics: 2013 Averages among States')

sum14 = data14[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data14)
stargazer(sum14, summary = TRUE, 
          type = 'html', 
          out = 'Summary14.html', 
          title = 'Descriptive Statistics: 2014 Averages among States')

sum15 = data15[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data15)
stargazer(sum15, summary = TRUE, 
          type = 'html', 
          out = 'Summary15.html', 
          title = 'Descriptive Statistics: 2015 Averages among States')

sum16 = data16[,c('NIBRS', 'GDPPerCap', 'UR', 'Wage', 'Police.Pop', 'Marriage', 'Overdose', 'Black')]
summary(data16)
stargazer(sum16, summary = TRUE, 
          type = 'html', 
          out = 'Summary16.html', 
          title = 'Descriptive Statistics: 2016 Averages among States')

mean = rbind(sum10, sum11, sum12, sum13, sum14, sum15, sum16)
stargazer(mean, summary = TRUE, 
          type = 'html', 
          out = 'realsum.html', 
          title = 'Descriptive Statistics throughout the Sample')




Year = c(2010:2016)
nibrs = c(mean(sum10$NIBRS), mean(sum11$NIBRS), mean(sum12$NIBRS), mean(sum13$NIBRS), mean(sum14$NIBRS), 
          mean(sum15$NIBRS), mean(sum16$NIBRS))
viewN = data.frame(Year, nibrs)

gdp = c(mean(sum10$GDPPerCap), mean(sum11$GDPPerCap), mean(sum12$GDPPerCap), mean(sum13$GDPPerCap), mean(sum14$GDPPerCap), 
          mean(sum15$GDPPerCap), mean(sum16$GDPPerCap))
viewG = data.frame(Year, gdp)

ur = c(mean(sum10$UR), mean(sum11$UR), mean(sum12$UR), mean(sum13$UR), mean(sum14$UR), 
          mean(sum15$UR), mean(sum16$UR))
viewU = data.frame(Year, ur)

wage = c(mean(sum10$Wage), mean(sum11$Wage), mean(sum12$Wage), mean(sum13$Wage), mean(sum14$Wage), 
          mean(sum15$Wage), mean(sum16$Wage))
viewW = data.frame(Year, wage)

police.Pop = c(mean(sum10$Police.Pop), mean(sum11$Police.Pop), mean(sum12$Police.Pop), mean(sum13$Police.Pop), mean(sum14$Police.Pop, na.rm = T), 
          mean(sum15$Police.Pop), mean(sum16$Police.Pop))
viewP = data.frame(Year, police.Pop)

Marriage = c(mean(sum10$Marriage), mean(sum11$Marriage), mean(sum12$Marriage), mean(sum13$Marriage), mean(sum14$Marriage), 
          mean(sum15$Marriage), mean(sum16$Marriage))
viewd = data.frame(Year, Marriage)

overdose = c(mean(sum10$Overdose, na.rm = T), mean(sum11$Overdose, na.rm = T), mean(sum12$Overdose, na.rm = T),
             mean(sum13$Overdose, na.rm = T), mean(sum14$Overdose, na.rm = T), 
          mean(sum15$Overdose, na.rm = T), mean(sum16$Overdose, na.rm = T))
viewO = data.frame(Year, overdose)

black = c(mean(sum10$Black), mean(sum11$Black), mean(sum12$Black), mean(sum13$Black), mean(sum14$Black), 
          mean(sum15$Black), mean(sum16$Black))
viewB = data.frame(Year, black)

plot(viewN$Year, viewN$nibrs, type = 'b', main = 'NIBRS Average', 
     xlab = 'Year', ylab = 'Number of NIBRS Incidents', lwd = 3, col = 'dodgerblue4')

plot(viewG$Year, viewG$gdp, type = 'b', main = 'GDP Average', 
             xlab = 'Year', ylab = 'Real GDP Per Capita', lwd = 3, col = 'dodgerblue4')

plot(viewU$Year, viewU$ur, type = 'b', main = 'Unemployment Average', 
             xlab = 'Year', ylab = 'Unemployment Rate', lwd = 3, col = 'dodgerblue4')

plot(viewW$Year, viewW$wage, type = 'b', main = 'Wage Average', 
             xlab = 'Year', ylab = 'Average Weekly Wage', lwd = 3, col = 'dodgerblue4')

plot(viewP$Year, viewP$police.Pop, type = 'b', main = 'Police Average', 
     xlab = 'Year', ylab = 'Police Officers Per Capita', lwd = 3, col = 'dodgerblue4')

plot(viewd$Year, viewd$Marriage, type = 'b', main = 'Marriage Average', 
     xlab = 'Year', ylab = 'Marriage Rate per 1000', lwd = 3, col = 'dodgerblue4')

plot(viewO$Year, viewO$overdose, type = 'b', main = 'Overdose Average', 
     xlab = 'Year', ylab = 'Overdose', lwd = 3, col = 'dodgerblue4')

plot(viewB$Year, viewB$black, type = 'b', main = 'Black Average', 
     xlab = 'Year', ylab = 'Total Incidents inolving African Americans', lwd = 3, col = 'dodgerblue4')

##### Regressions #####


# OLS pooled regression
ols = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Marriage) + log(Overdose)+ logBlack, data)
summary(ols)
stargazer(ols, fe,
          dep.var.labels = c('OLS', 'Fixed Effects'),
          type = 'html', 
          digits = 2, 
          out = 'olsReg.html')


fe = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Marriage) + log(Overdose)+ logBlack, data, effect = "twoway")
summary(fe)

  


model1 = plm(log(NIBRS) ~ log(GDPPerCap) + log(Police.Pop) + log(Marriage) + log(Overdose) + logBlack, data, effect = 'twoway')
summary(model1)

model2 = plm(log(NIBRS) ~ log(UR) + log(Police.Pop) + log(Marriage) + log(Overdose) + logBlack, data, effect = 'twoway')
summary(model2)

mod3 = plm(log(NIBRS) ~ log(Wage) + log(Police.Pop) + log(Marriage) + log(Overdose) + logBlack, data = data, effect = 'twoway')
summary(model3)



!require(lmtest)
coeftest(fe, vcovHC(fe, type = 'HC1'))

# Robust Standard Errors

plotmeans(data$NIBRS ~ data$Date, legend = T, main = 'Heterogeneity across Time', 
          ylab = 'NIBRS Incidents', 
          xlab = 'Quarter')
plotmeans(data$NIBRS ~ data$State, legend = T, main = 'Heterogeneity across States', 
          ylab = 'NIBRS Incidents', 
          xlab = 'State')

clust = coeftest(fe, vcovHC(fe, type = 'HC1'))
tclust = table(clust)
tclust = data.frame(tclust)

stargazer(clust,
          report = c("vcp"),
          type = 'html', 
          out = 'clustp.html')

stargazer(clust,
          type = 'html', 
          out = 'clust.html')

library(ggplot2)
ggplot(data, mapping = aes(x = Date, y = NIBRS, color = State)) + 
  geom_jitter(aes(size = I(3), alpha = I(0.5))) + 
  geom


stargazer(mod1, mod2, mod3, fe,
          type = 'html', 
          digits = 2, 
          out = 'fereg.html')
summary(mod1)

# Two-Way FE
mod1 = plm(log(NIBRS) ~ log(GDPPerCap), data, effect = "twoway")
summary(mod1)

mod2 = plm(log(NIBRS) ~ log(UR), data, effect = "twoway")
summary(mod2)

mod3 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR), data, effect = "twoway")
summary(mod3)

mod4 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage), data, effect = "twoway")
summary(mod4)

mod5 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop), data, effect = "twoway")
summary(mod5)


mod6 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce), data, effect = "twoway")
summary(mod6)


mod7 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce) + log(OD/Pop), data, effect = "twoway")
summary(mod7)

mod8 = plm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce) + log(OD/Pop)+ Black, data, effect = "twoway")
summary(mod8)


  # OLS
reg1 = lm(log(NIBRS) ~ log(GDPPerCap), data)
summary(reg1)

reg2 = lm(log(NIBRS) ~ log(UR), data)
summary(reg2)

reg3 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR), data)
summary(reg3)

reg4 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage), data)
summary(reg4)

reg5 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop), data)
summary(reg5)

reg6 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce), data)
summary(reg6)

reg7 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce) + log(OD/Pop), data)
summary(reg7)

reg8 = lm(log(NIBRS) ~ log(GDPPerCap) + log(UR) + log(Wage) + log(Police.Pop) + log(Divorce) + log(OD/Pop)+ Black, data)
summary(reg8)

ls(data)

data = data[,-23]
data = data %>% mutate('LogGDPPC' = log(GDPPerCap), 
                       'LogNIBRS' = log(NIBRS), 
                       'LogWage' = log(Wage), 
                       'LogPolice' = log(data$Police.Pop), 
                       'logOD' = log(OD), 
                       'logDivorce' = log(Divorce))




stargazer(mod1, reg1, mod2, reg2, mod3, reg3, mod4, reg4, mod5, reg5, mod6, reg6, mod7, reg7, 
          type = "html",
          digits = 2,
          out = "RegTable.html")

stargazer(data[,c(2, 20, 21, 11, 12, 13)], summary = TRUE, 
          type = 'html', 
          out = 'SummaryLevels.html')

stargazer(data[,c(24, 23, 21, 25, 26, 27)], summary = TRUE, 
          type = 'html', 
          out = 'SummaryLog.html')

stargazer(data, summary = TRUE, 
          type = 'html', 
          out = 'SummaryTotal.html')


r.squared(mod8, dfcor = TRUE)
