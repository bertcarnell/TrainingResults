require(XML)

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
