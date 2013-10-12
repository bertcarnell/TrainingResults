## ---- setup
rm(list=ls())

repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
source(file.path(repositoryPath, "Common", "Utilities.R"))

racePath <- file.path(repositoryPath, "2008ColumbusHalfMarathon")
dataPath <- file.path(racePath, "data")
outPath <- file.path(racePath, "images")
dataFile1 <- file.path(dataPath, "Online Race Results _ Race Details 2008.htm")
dataFile2 <- file.path(dataPath, "SplitTimes.csv")
dataFile3 <- file.path(dataPath, "SplitTimesOfficial.csv")

stopifnot(all(file.exists(c(racePath, dataPath, outPath, dataFile1, dataFile2))))

op <- options(digits.secs=2)

dat <- read.csv(file=dataFile2, stringsAsFactors=FALSE)
datOfficial <- read.csv(file=dataFile3, stringsAsFactors=FALSE)

dat$totalTime <- strptime(dat$total, "%H:%M:%OS")
dat$splitTime <- strptime(dat$split, "%H:%M:%OS")
dat$splitTime[10:12] <- as.POSIXlt(
  (as.numeric(dat$splitTime[12]) - startToday)/3 + startToday, 
  origin=posixOrigin)
dat$calcTotalTime <- as.POSIXlt(
  cumsum(as.numeric(dat$splitTime) - startToday) + startToday, 
  origin=posixOrigin)

datOfficial$splitTime <- strptime(datOfficial$split, "%H:%M:%OS")

X <- readHTMLTable(dataFile1, stringsAsFactors=FALSE, colClasses="character")[[7]]
Y <- data.frame(lastName=X$LN, 
                division=X$DIVISION, 
                time10k=X$'10K',
                time=strptime(X$TIME, format="%H:%M:%S"),
                pace=strptime(X$PACE, format="%M:%S"),
                stringsAsFactors=FALSE)
indNA <- which(is.na(Y$time))
indMe <- which(Y$lastName[-indNA] == "Carnell")

## ---- plotchunk1
cols <- c(rep("black", 9), rep("grey", 2), rep("black", 3))

par(oma=c(1,0,0,0))
par(mfrow=c(2,1))
par(mar=c(3,7,1,2))
par(mgp=c(6,1,0))
plot(dat$mile, dat$calcTotalTime, xlim=c(0, 13.1), col=cols, pch=19, 
     xlab="", ylab="Time", axes=FALSE)
axis(1)
my.at <- startToday + c(0,0.5,1,1.5,2.0)*60*60
axis(2, at=my.at, 
     labels=format(as.POSIXlt(my.at, origin=posixOrigin), format="%H:%M:%S"), 
     las=2)
box()
points(datOfficial$mile, datOfficial$splitTime, col="red", pch=19)
abline(h=my.at, col="gray", lty=2)
x <- seq(0, 13.1, by=0.1)
lines(x, startToday + x*11*60, col="blue", lty=2)
lines(x, startToday + x*10*60, col="blue", lty=2)
lines(x, startToday + x*9*60, col="blue", lty=2)
legend("topleft", legend=c("Chip Time", "Rob Splits", "Interpolated"),
  col=c("red", "black", "grey"), pch=19, bg="white")
  
plot(dat$mile, dat$splitTime, col=cols, pch=19, xlim=c(0,13.1),
     ylim=as.numeric(strptime(c("00:08:00", "00:11:00"), "%T")),
     xlab="", ylab="Pace (min/mile)", axes=FALSE)
axis(1)
my.at <- strptime(c("08", "09", "10", "11"), "%M")
axis(2, at=as.numeric(my.at), labels=format(my.at, "%M:%S"), las=2)
box()
abline(h=as.numeric(my.at), col="gray", lty=2)
points(datOfficial$mile, 
       (as.numeric(datOfficial$splitTime) - startToday)/datOfficial$mile + startToday, 
       col="red", pch=19)
legend("topleft", legend=c("Avg Chip Pace", "Rob Splits", "Interpolated"),
  col=c("red", "black", "grey"), pch=19, ncol=3, bg="white")

mtext("Distance (mi)", side=1, line=0, outer=TRUE)

## ---- plotchunk2

timeHistogramBoxplot(Y$time[-indNA], indMe, "2008 Columbus Half Marathon",
                     strptime(c(paste("00:", c(0,15,30,45),sep=""),
                                paste("01:", c(0,15,30,45),sep=""),
                                paste("02:", c(0,15,30,45),sep=""),
                                paste("03:", c(0,15,30,45),sep=""),
                                paste("04:", c(0,15,30,45), sep="")),
                              "%H:%M"))
