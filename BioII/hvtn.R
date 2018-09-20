library(plyr)
library(ggplot2)
library(plotly)
dataDir <- "C:\\Users\\happy_000\\Desktop\\Rprojects\\HVTN\\"
dataFile<-"HVTN.csv"
dat<-read.csv(paste(dataDir,dataFile, sep=""),header=T, stringsAsFactors = FALSE)
dat<- dat[1:36, ]
datUS <- dat[dat$origin == '1', ]
datNUS <- dat[dat$origin == '2', ]
datHVTN <- dat[dat$origin == '3', ]

origin_dat<-count(datUS, 'benefit')
origin_dat2<-count(datNUS, 'benefit')
origin_dat3<-count(datHVTN, 'benefit')

print(origin_dat)
print(origin_dat2)
print(origin_dat3)

