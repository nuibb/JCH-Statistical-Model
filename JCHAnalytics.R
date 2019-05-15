
library("readxl")

#Importing the dataset
dataset = read_excel('data.xlsx',1)
dataset
View(dataset)
class(dataset)

#Explore the Dataset
summary(dataset)
str(dataset)

#Data Cleaning
clearedData = na.omit(dataset)
clearedData
any(is.na(clearedData))
View(clearedData)

#Finding most visible user in the dataset
library(dplyr)
library(lubridate)
mvu = count(clearedData,clearedData$agent_user_id)
mvu
class(mvu)
max(mvu$n)
user = mvu[which.max(mvu$n),]
user
class(user[[1,1]])

#Filter data only for most visible user
userdata = filter(clearedData, clearedData$agent_user_id == user[[1,1]])
userdata
View(userdata)

#Considering one month data
monthlyData = userdata[with(userdata, userdata$date >="2017-08-17 12:40:23.997955+06" & userdata$date<="2017-08-29 18:14:26.635888+06"), ]
write.csv(monthlyData, file = "MonthlyData.csv", row.names = FALSE)
View(monthlyData)

library(data.table)
# creating an ordered/keyed data.table
dt <- data.table(monthlyData$date)
dt

# calculating the timedifference
# option 1:
dt[, tdiff := as.integer(difftime(monthlyData$date, shift(monthlyData$date, fill=monthlyData$date[1L]), units="secs"))]
dt$tdiff

#Parse date column to show distribution in hours
hdataset = mutate(monthlyData, Hours=as.POSIXlt(monthlyData$date)$hour, Weekday=as.POSIXlt(monthlyData$date)$wday)
hdataset
View(hdataset)
summary(hdataset)

#hours <- hdataset[['Hours']]
#factor.hours <- factor(hours)
#factor.hours
#levels(factor.hours)

summary(hdataset)

# login counts per hour
l = factor(hdataset$Hours, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
l
class(l)
s = summary(l)
s

hours <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
actions <- summary(l)
df = data.frame(hours,actions)
df
class(df)

summary(df)

write.csv(df, file = "HBMDODAOIT.csv", row.names = FALSE)

barplot(
  names = df$hours,
  height = df$actions,
  col = "skyblue",
  main = "Daily active/inactive time frequency",
  xlab = "Hours",
  ylab = "Action count")

activetime = filter(df, df$actions != 0)
activetime
summary(activetime)
max(activetime$actions)
sum(activetime$action)/10

write.csv(activetime, file = "HBMDOODAT.csv", row.names = FALSE)

barplot(
  names = activetime$hours,
  height = activetime$actions,
  col = "skyblue",
  main = "Only daily active time frequency",
  xlab = "Hours",
  ylab = "Action count")

inactivetime = filter(df, df$actions == 0)
inactivetime

write.csv(inactivetime, file = "HBMDOODIAT.csv", row.names = FALSE)

barplot(
  names = inactivetime$hours,
  height = inactivetime$actions,
  col = "skyblue",
  main = "Only daily inactive time frequency",
  xlab = "Hours",
  ylab = "Action count")


# login counts per week
week <- hdataset[['Weekday']]
factor.week <- factor(week)
factor.week
levels(factor.week)

weekDays = factor(hdataset$Weekday, levels = c(0,1,2,3,4,5,6), labels = c(0,1,2,3,4,5,6))
weekDays
class(weekDays)
w = summary(weekDays)
w

wweeks <- c('Su', 'M', 'T', 'W', 'Th', 'F', 'S')
wdf = data.frame(w,wweeks)
wdf

write.csv(wdf, file = "HBWDOAIT.csv", row.names = FALSE)

barplot(
  names = wdf$wweeks,
  height = wdf$w,
  col = "skyblue",
  main = "Weekly active time frequency",
  xlab = "Week days",
  ylab = "Action count")

monthlyTotalHitInFiveSecDiff = 30*24*60*12
monthlyTotalHitInFiveSecDiff

#timeGapDf = mutate(activetime, timegap=((5*max(activetime$actions))/activetime$actions), hourlyHitInMonth=(60/timegap)*60*22, hourlyHitByVCConMonth=12*60*30, totalHourlyCostByHitsInMonth=(200*hourlyHitInMonth)/monthlyTotalHitInFiveSecDiff, hourlySaveInMonth=max(totalHourlyCostByHitsInMonth)-totalHourlyCostByHitsInMonth)
timeGapDf = mutate(df, timegap=((5*max(df$actions))/df$actions), hourlyHitInMonth=(60/timegap)*60*22, hourlyHitByVCConMonth=12*60*30, totalHourlyCostByHitsInMonth=(200*hourlyHitInMonth)/monthlyTotalHitInFiveSecDiff, hourlySaveInMonth=max(totalHourlyCostByHitsInMonth)-totalHourlyCostByHitsInMonth)
timeGapDf
summary(timeGapDf)

hourlyTotalHitsInMonth = sum(timeGapDf$hourlyHitInMonth)
hourlyTotalHitsInMonth

totalMonthlyCostOnHourlyHits = (hourlyTotalHitsInMonth*200)/monthlyTotalHitInFiveSecDiff
totalMonthlyCostOnHourlyHits
sum(timeGapDf$totalHourlyCostByHitsInMonth)

hourlySaveOnlyInWeekDaysInMonth = sum(timeGapDf$hourlySaveInMonth)
hourlySaveOnlyInWeekDaysInMonth
saveOnlyInWeekends = (8*24*60*12*200)/monthlyTotalHitInFiveSecDiff
saveOnlyInWeekends
monthlyTotalSave = hourlySaveOnlyInWeekDaysInMonth + saveOnlyInWeekends
monthlyTotalSave

summary(timeGapDf)

write.csv(timeGapDf, file = "HourlyCostInMonth.csv", row.names = FALSE)

barplot(
  names = timeGapDf$hours,
  height = timeGapDf$totalHourlyCostByHitsInMonth,
  col = "skyblue",
  main = "Hourly cost in a month",
  xlab = "Hours",
  ylab = "Cost in Cents")

barplot(
  names = timeGapDf$hours,
  height = timeGapDf$hourlySaveInMonth,
  col = "skyblue",
  main = "Hourly save in a month",
  xlab = "Hours",
  ylab = "Cost saved in Cents")

plot(density(timeGapDf$hours))
plot(density(timeGapDf$hourlyHitInMonth))

