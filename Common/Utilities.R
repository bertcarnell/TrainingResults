require(XML)

startToday <- as.numeric(strptime("00:00:00","%T"))
posixOrigin <- "1970-01-01 00:00:00"

timeHistogram <- function(times, indMe, title)
{
  hist(times, breaks=50, freq=TRUE, col="blue", main=title,
       xlab="Final Time", ylab="Frequency")
  abline(v=times[indMe], col="red", lwd=2)
  abline(v=median(times), col="green", lwd=1)
  abline(v=mean(times), col="green", lwd=1, lty=2)
  legend("topright", 
         legend=c(paste(round(ecdf(times)(times[indMe])*100, digits=0), "%", 
                        strftime(times[indMe], format="%M:%S")),
                  "Median",
                  "Mean"),
         bg="white", lty=c(1,1,2), lwd=c(2,1,1), col=c("red","green","green"))
}
timeBoxplot <- function(times, indMe, title)
{
  boxplot(times, main=title, ylab="Final Time", axes=FALSE)
  points(rep(1, length(times)), times, pch=1, cex=0.8)
  points(1, mean(times), pch=19, cex=2, col="green")
  points(1, times[indMe], pch=19, cex=2, col="red")
  axis(1, at=1, labels="Overall")
  axis(2, at=as.numeric(strptime(as.character(seq(15,50,by=5)), format="%M")),
       labels=paste(seq(15,50,by=5), ":00", sep=""))
  legend("topright", 
         legend=c(paste(round(ecdf(times)(times[indMe])*100, digits=0), "%", 
                        strftime(times[indMe], format="%M:%S")),
                  "Median",
                  "Mean"),
         bg="white", 
         pch=c(19,NA,19), 
         lty=c(NA,1,NA), 
         lwd=c(NA,3,NA), 
         col=c("red","black","green"))
}

timeHistogramBoxplot <- function(times, indMe, title, xTimes)
{
  layout(matrix(c(1,2), nrow=2), heights=c(0.7, 0.3))
  ## Histogram
  par(mar=c(0,4,2,2))
  h <- hist(times, breaks=50, freq=TRUE, col="blue", main=title,
       xlab="", ylab="Frequency", axes=FALSE)
  axis(2)
  abline(v=times[indMe], col="red", lwd=2)
  abline(v=median(times), col="green", lwd=1)
  abline(v=mean(times), col="green", lwd=1, lty=2)
  legend("topright", 
         legend=c(paste(round(ecdf(times)(times[indMe])*100, digits=0), "%", 
                        strftime(times[indMe], format="%M:%S")),
                  "Median",
                  "Mean"),
         bg="white", 
         lty=c(1,1,2), 
         lwd=c(2,1,1), 
         col=c("red","green","green"))
  ## Boxplot
  par(mar=c(5,4,0,2))
  boxplot(times, main="", xlab="Final Time", axes=FALSE, horizontal=TRUE, 
          ylim=range(h$breaks))
  points(rep(1, length(times)), times, pch=1, cex=0.8)
  points(mean(times), 1, pch=19, cex=2, col="green")
  points(times[indMe], 1, pch=19, cex=2, col="red")
  axis(1, at=as.numeric(xTimes),
       labels=format(xTimes, format="%H:%M"))
}
