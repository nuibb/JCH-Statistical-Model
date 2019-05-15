
library("readxl")

#Importing the dataset
dataset = read_excel('data.xlsx',1)
dataset
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
mvt = count(clearedData,clearedData$agent_user_id)
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
class(userdata)

harMet15.09.11 <- subset(userdata,
                         userdata$date >= as.POSIXct('2017-08-17 12:40:23.997955+06' &
                                                       userdata$date <= as.POSIXct('2017-08-17 12:41:06.748569+06')))

# View first and last records in the object 
head(harMet15.09.11[1])



#Parse date column to show distribution in hours
hdataset = mutate(userdata, Hours=as.POSIXlt(userdata$date)$hour)
hdataset
View(hdataset)

hours <- hdataset[['Hours']]
factor.hours <- factor(hours)
factor.hours
levels(factor.hours)

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

summary(df)

barplot(
  names = df$hours,
  height = df$actions,
  col = "skyblue",
  main = "Daily access time",
  xlab = "Hours",
  ylab = "Action count")

activetime = filter(df, df$actions != 0)
activetime

barplot(
  names = activetime$hours,
  height = activetime$actions,
  col = "skyblue",
  main = "Daily access time",
  xlab = "Hours",
  ylab = "Action count")


