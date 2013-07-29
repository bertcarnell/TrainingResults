rm(list=ls())

charToHundredth <- function(x)
{
  # xx:xx:xx.xx
  #x <- "01:23:45.67"
  xlen <- length(x)
  ret <- numeric(xlen)
  for (i in 1:xlen)
  {
    if (is.na(x[i]) || !is.character(x[i]))
    {
      ret[i] <- NA
      next
    }
    y <- strsplit(x[i], ":")
    y <- unlist(y)
    stopifnot(length(y) == 3)
    y <- as.numeric(y)
    ret[i] <- y[1]*60*60*100 + y[2]*60*100 + y[3]*100
  }
  return(ret)
}

hundrethToChar <- function(x)
{
  #x <- 502567
  xlen <- length(x)
  stopifnot(all(is.numeric(x)))
  indNA <- which(is.na(x))
  hours <- floor(x/60/60/100)
  x <- x - hours * 60 * 60 * 100
  minutes <- floor(x/60/100)
  x <- x - minutes * 60 * 100
  seconds <- floor(x/100)
  x <- x - seconds * 100
  hundredths <- x
  hours <- as.character(hours)
  ind <- which(nchar(hours) == 1)
  hours[ind] <- paste("0", hours[ind], sep="")
  minutes <- as.character(minutes)
  ind <- which(nchar(minutes) == 1)
  minutes[ind] <- paste("0", minutes[ind], sep="")
  seconds <- as.character(seconds)
  ind <- which(nchar(seconds) == 1)
  seconds[ind] <- paste("0", seconds[ind], sep="")
  hundredths <- as.character(hundredths)
  ind <- which(nchar(hundredths) == 1)
  hundredths[ind] <- paste("0", hundredths[ind], sep="")

  ret <- paste(hours, ":", minutes, ":", seconds, ".", hundredths, sep="")
  ret[indNA] <- NA
  return(ret)
}

hundrethToChar(charToHundredth("01:23:45.67"))
hundrethToChar(charToHundredth("00:01:25.00"))
hundrethToChar(charToHundredth(NA))

dat <- scan(what=list("character", "character", "character"))
0 00:00:00.0 00:00:00.00
1 00:10:00.20 00:10:00.20
2 00:19:40.02 00:09:39.82
3 00:28:44.96 00:09:04.94
4 00:38:11.92 00:09:26.96
5 00:47:19.94 00:09:08.02
6 00:56:28.77 00:09:08.83
7 01:06:00.00 00:09:32.03
8 01:15:29.00 00:09:29.56
9 NA NA
10 NA NA
11 01:43:16.00 00:27:47.00
12 01:52:38.00 00:09:21.04
13 02:01:00.00 00:08:22.15

dat <- as.data.frame(dat)
names(dat) <- c("mile", "total", "split")
dat$mile <- as.numeric(as.character(dat$mile))
dat$total <- as.character(dat$total)
dat$split <- as.character(dat$split)
dat$totalHund <- charToHundredth(dat$total)
dat$splitHund <- charToHundredth(dat$split)
dat$splitHund[10:12] <- dat$splitHund[12]/3
dat$calcTotalHund <- cumsum(dat$splitHund)

cols <- c(rep("black", 9), rep("grey", 2), rep("black", 3))
par(oma=c(1,0,0,0))
par(mfrow=c(2,1))
par(mar=c(3,7,1,2))
par(mgp=c(6,1,0))
plot(dat$mile, dat$calcTotalHund, xlim=c(0, 13.1), col=cols, pch=19, xlab="", ylab="Time", axes=FALSE)
axis(1)
my.at <- c(0,0.5,1,1.5,2.0)*60*60*100
axis(2, at=my.at, labels=hundrethToChar(my.at), las=2)
box()
points(13.1, charToHundredth("02:02:09.00"), col="red", pch=19)
points(6.2137119224, charToHundredth("00:58:34.00"), col="red", pch=19)
legend("topleft", legend=c("Chip Time", "Rob Splits", "Interpolated"),
  col=c("red", "black", "grey"), pch=19)
abline(h=my.at, col="gray", lty=2)
x <- seq(0, 13.1, by=0.1)
lines(x, x*11*60*100)
lines(x, x*10*60*100)
lines(x, x*9*60*100)

  
plot(dat$mile, dat$splitHund, col=cols, pch=19, xlim=c(0,13.1),
     ylim=c(charToHundredth("00:08:00.0"), charToHundredth("00:11:00.0")),
     xlab="", ylab="Pace (min/mile)", axes=FALSE)
axis(1)
my.at <- charToHundredth(c("00:08:00.00", "00:09:00.00", "00:10:00.00", "00:11:00.00"))
axis(2, at=my.at, labels=hundrethToChar(my.at), las=2)
box()
abline(h=my.at, col="gray", lty=2)
points(13.1, charToHundredth("02:02:09.00")/13.1, col="red", pch=19)
points(6.2137119224, charToHundredth("00:58:34.00")/6.2137119224, col="red", pch=19)
legend("topleft", legend=c("Avg Chip Pace", "Rob Splits", "Interpolated"),
  col=c("red", "black", "grey"), pch=19, ncol=3, bg="white")

mtext("Distance (mi)", side=1, line=0, outer=TRUE)








