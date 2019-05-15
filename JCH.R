
library("readxl")

# Importing the dataset
dataset = read_excel('agent_access_log.xlsx',1)
dataset
class(dataset)
dataset$login_attempt

#Filter only rows to related login and logout
library(dplyr)
userLoginData = filter(dataset, dataset$login_attempt == 'success' | dataset$login_attempt == 'Logout')
userLoginData
View(userLoginData)

# Get login_attempt column's factors
login_attempt <- userLoginData[['login_attempt']]
class(login_attempt)
factor.login_attempt <- factor(login_attempt)
factor.login_attempt
levels(factor.login_attempt)

# Encoding categorical data
userLoginData$login_attempt = factor(userLoginData$login_attempt, levels = c('Logout', 'success'), labels = c(1, 2))
userLoginData
summary(userLoginData)

firstRowDate= userLoginData$date[1] 
firstRowDate
mydate = as.POSIXlt(firstRowDate)
mydate
names(mydate)
mydate$mday

tm1.lt <- as.POSIXlt(firstRowDate)
tm1.lt
unclass(tm1.lt)
tm1.lt$hour
as.POSIXlt(userLoginData$date)$hour

hdataset = mutate(userLoginData, Hours=as.POSIXlt(userLoginData$date)$hour)
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
summary(l)
class(l)
