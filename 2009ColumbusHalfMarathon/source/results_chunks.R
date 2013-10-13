## ---- setup
rm(list=ls())

repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
source(file.path(repositoryPath, "Common", "Utilities.R"))

racePath <- file.path(repositoryPath, "2009ColumbusHalfMarathon")
dataPath <- file.path(racePath, "data")
outPath <- file.path(racePath, "images")
dataFile1 <- file.path(dataPath, "Online Race Results _ Race Details 2009.htm")
#dataFile2 <- file.path(dataPath, "SplitTimes.csv")
#dataFile3 <- file.path(dataPath, "SplitTimesOfficial.csv")

stopifnot(all(file.exists(c(racePath, dataPath, outPath, dataFile1))))#, dataFile2))))

op <- options(digits.secs=2)

#dat <- read.csv(file=dataFile2, stringsAsFactors=FALSE)
#datOfficial <- read.csv(file=dataFile3, stringsAsFactors=FALSE)

#dat$totalTime <- strptime(dat$total, "%H:%M:%OS")
#dat$splitTime <- strptime(dat$split, "%H:%M:%OS")
#dat$splitTime[10:12] <- as.POSIXlt(
#  (as.numeric(dat$splitTime[12]) - startToday)/3 + startToday, 
#  origin=posixOrigin)
#dat$calcTotalTime <- as.POSIXlt(
#  cumsum(as.numeric(dat$splitTime) - startToday) + startToday, 
#  origin=posixOrigin)

#datOfficial$splitTime <- strptime(datOfficial$split, "%H:%M:%OS")

X <- readHTMLTable(dataFile1, stringsAsFactors=FALSE, colClasses="character")[[7]]
Y <- data.frame(lastName=X$LN, 
                division=X$DIVISION, 
                time10k=X$'10K',
                time=strptime(X$TIME, format="%H:%M:%S"),
                pace=strptime(X$PACE, format="%M:%S"),
                stringsAsFactors=FALSE)
indNA <- which(is.na(Y$time))
indMe <- which(Y$lastName[-indNA] == "Carnell")

## ---- plotchunk2

timeHistogramBoxplot(Y$time[-indNA], indMe, "2009 Columbus Half Marathon",
                     strptime(c(paste("00:", c(0,15,30,45),sep=""),
                                paste("01:", c(0,15,30,45),sep=""),
                                paste("02:", c(0,15,30,45),sep=""),
                                paste("03:", c(0,15,30,45),sep=""),
                                paste("04:", c(0,15,30,45), sep="")),
                              "%H:%M"))
