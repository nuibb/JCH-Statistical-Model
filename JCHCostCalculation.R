
library("readxl")

#Importing the dataset
dataset = read_excel('VCCDataWN.xlsx',1)
dataset
View(dataset)
class(dataset)
str(dataset)

summary(dataset)

#Total data points in VCC
NROW(dataset)

# Total data size in bytes in VCC
totalSize = sum(dataset$Size)
totalSize

# Total data points and data size from grateway in VCC
gatewayData = dataset[with(dataset, dataset$Key1 =="GW"), ]
gatewayData
View(gatewayData)
gdp = NROW(gatewayData)
gdp
gatewayDataSize = gdp*4 + sum(gatewayData$Size)
gatewayDataSize

# Total data points and data size from IDU in VCC
IDUData = dataset[with(dataset, dataset$Key1 =="IDU"), ]
IDUData
View(IDUData)
idp = NROW(IDUData)
idp

IDUDataSize = (4+1)*160*idp + sum(IDUData$Size)*160
IDUDataSize

# Total data points and data size from ODU in VCC
ODUData = dataset[with(dataset, dataset$Key1 =="ODU"), ]
ODUData
View(ODUData)
odp = NROW(ODUData)
odp
ODUDataSize =  (4+64)*odp + sum(ODUData$Size)*64
ODUDataSize

# Total data points including 'Time'
vcctdp = gdp + idp + odp + 1
vcctdp

# Total data size including 'Time' per hit in VCC
vcctds = gatewayDataSize + IDUDataSize + ODUDataSize + 19 + 4
vcctds

#In kilo byte
storageSizeInVCC = vcctds/1024
storageSizeInVCC

# Monthly total hit in VCC
VCCMonthlyHitInFiveSecDiff = 30*24*60*12
VCCMonthlyHitInFiveSecDiff

# Monthly total data size uploaded to the cloud by VCC
VCCMonthlyTransactionDataInFiveSecDiff = vcctds * VCCMonthlyHitInFiveSecDiff
VCCMonthlyTransactionDataInFiveSecDiff
VCCInKB = VCCMonthlyTransactionDataInFiveSecDiff/1024
VCCInKB
VCCInMb = VCCInKB/1024
VCCInMb
VCCInGB = VCCInMb/1024
VCCInGB

### For MPX Data
library("readxl")

#Importing the dataset
MPXData = read_excel('MPXWN.xlsx',1)
MPXData
View(MPXData)
class(MPXData)
str(MPXData)

#Total data points in MPX
NROW(MPXData)

# Total data size in bytes in MPX
totalMPXSize = sum(MPXData$`Size(byte)`)
totalMPXSize

# Total data points and data size from IDU in MPX
MPXIDUData = MPXData[with(MPXData, MPXData$Key =="IDU"), ]
MPXIDUData
View(MPXIDUData)
midp = NROW(MPXIDUData)
midp

mIDUDataSize = (4+1)*160*midp + sum(MPXIDUData$`Size(byte)`)*160
mIDUDataSize

# Total data points and data size from ODU in MPX
mODUData = MPXData[with(MPXData, MPXData$Key =="ODU"), ]
mODUData
View(mODUData)
modp = NROW(mODUData)
modp
mODUDataSize = (4+64)*modp + + sum(mODUData$`Size(byte)`)*64
mODUDataSize

# Total data points including 'Time'
mpxtdp = midp + modp + 1
mpxtdp

# Total data size including 'Time' per hit in MPX
mpxtds = mIDUDataSize + mODUDataSize + 19 + 4
mpxtds
#In kilo byte
mpxtds/1024

# Monthly total hit in MPX
MPXMonthlyHitInFifteenMinDiff = 30*24*4
MPXMonthlyHitInFifteenMinDiff

# Monthly total data size uploaded to the cloud by MPX
MPXMonthlyTransactionDataInFifteenMinDiff = mpxtds * MPXMonthlyHitInFifteenMinDiff
MPXInKB = MPXMonthlyTransactionDataInFifteenMinDiff/1024
MPXInKB
MPXInMb = MPXInKB/1024
MPXInMb

TotalDataTransmisionWithBothVCCandMPX = VCCMonthlyTransactionDataInFiveSecDiff + MPXMonthlyTransactionDataInFifteenMinDiff
TotalDataTransmisionWithBothVCCandMPX
InKB = TotalDataTransmisionWithBothVCCandMPX/1024
InMB = InKB/1024
InMB
InGB = InMB/1024
InGB

# Findings
# Considering VCC in every 5 sec and MPX in every 15 mins, the cost will be 2$ for 56.07818 GB data transaction in a month

# Changing VCC time to see data volume frequency
VCCLatency <- c(5, 10, 15, 30, 60)
MPXLatency <- c(15, 15, 15, 15, 15)
VCCDataSize <- c(vcctds)
MPXDataSize <- c(mpxtds)
MonthlyCost <- 2
dataFrame = data.frame(VCCLatency, MPXLatency, VCCDataSize, MPXDataSize, MonthlyCost)
dataFrame
View(dataFrame)

library(dplyr)
calculator  = mutate(dataFrame, totalMonthlyHit = (30*24*60*(60/dataFrame$VCCLatency)), VCCMonthlyDataSize = (dataFrame$VCCDataSize *totalMonthlyHit)/1024/1024, MPXMonthlyDataSize = (dataFrame$MPXDataSize * (30*24*(60/dataFrame$MPXLatency)))/1024/1024, monthlyTotalVolume = VCCMonthlyDataSize + MPXMonthlyDataSize, GatewayCovered=max(monthlyTotalVolume)/monthlyTotalVolume)
calculator

barplot(
  names = calculator$VCCTime,
  height = calculator$totalMonthlyHit,
  col = "skyblue",
  main = "Time Difference vs hit count in max data volume",
  xlab = "Time difference",
  ylab = "Hit count")


Percentage <- c(5,10,20,30,40,50,60,70,80,90,95)
dataFrame2 = data.frame(VCCDataSize, Percentage, DataVolAfterSave = VCCDataSize - (VCCDataSize*Percentage)/100, MPXDataSize, MonthlyCost)
dataFrame2
calculator2  = mutate(dataFrame2, totalMonthlyHit = (30*24*60*(60/5)), VCCMonthlyDataSize = (dataFrame2$DataVolAfterSave *totalMonthlyHit)/1024/1024, GatewayCovered=max(VCCMonthlyDataSize)/VCCMonthlyDataSize)
calculator2

barplot(
  names = calculator2$Percentage,
  height = calculator2$VCCMonthlyDataSize,
  col = "skyblue",
  main = "Saved data by % in fixed no. VCC hit",
  xlab = "Volume save in percentage",
  ylab = "Data Volume in MB")

VCCLatency3 = c(5, 10, 15, 20, 25, 30, 40, 45, 50, 55, 60, 300)
MPXLatency3 = c(15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15)
Percentage3 <- c(5,10,20,30,40,50,60,70,80,90,95,95)
dataFrame3 = data.frame(VCCLatency3, MPXLatency3, VCCDataSize, Percentage3, MPXDataSize)
dataFrame3
calculator3  =  mutate(dataFrame3, DVAS = VCCDataSize - (VCCDataSize*Percentage3)/100, totalMonthlyHit = (30*24*60*(60/VCCLatency3)), MonthlyDataSize = (DVAS *totalMonthlyHit)/1024/1024, GatewayCovered=max(MonthlyDataSize)/MonthlyDataSize)
calculator3

summary(calculator3)

