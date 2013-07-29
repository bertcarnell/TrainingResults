require(XML)

options(stringsAsFactors=FALSE)

defaultPath <- file.path("C:","Users","carnellr","Downloads","Half Marathon")

Y <- readHTMLTable(file.path(defaultPath, "AllResults.htm"),
                   which=c(1,2,4))

Y1 <- cbind(Y[[1]][2:4,], rep("M",3))
names(Y1) <- names(Y[[3]])[c(1,2,3,4,5,7,6,8)]
Y2 <- cbind(Y[[2]][2:4,], rep("F", 3))
names(Y2) <- names(Y[[3]])[c(1,2,3,4,5,7,6,8)]
Z <- rbind(Y1, Y2, Y[[3]])

Z$time <- as.numeric(as.POSIXct(strptime(Z$"Total Time", "%T"))-as.POSIXct(strptime("00:00:00", "%T")))
Z$time2 <- strptime(Z$"Total Time", "%T")

target <- which(Z$"Runner Name" == "Carnell, Robert")
targettime <- Z$time2[target]

graphPlace <- function(x, mn)
{
  windows()
  h <- hist(x, breaks=50, main=mn, xlab="Time")
  abline(v=as.numeric(targettime), col="blue", lwd=2)
  abline(v=as.numeric(mean(x)), col="red", lty=2, lwd=2)
  abline(v=as.numeric(median(x)), col="green", lty=2, lwd=2)
  Fn <- ecdf(x)
  z <- round(Fn(targettime)*100)
  legend("topright", legend=c(paste("Total:", length(x)), 
                              paste("Percentile: ", z, "%", sep="")))
}

graphPlace(Z$time2, "All Runners")
graphPlace(Z$time2[which(Z$Gender == "M")], "Men")
graphPlace(Z$time2[which(Z$Division == "M3539")], "Men 35-39")
graphPlace(Z$time2[which(Z$Age %in% c("35","36") & Z$Gender == "M")], "Men 35,36")

windows()
plot(table(Z$Division), axes=FALSE, xlab="", ylab="", col="blue", type="h")
axis(2)
axis(1, las=2, at=1:length(table(Z$Division)), labels=names(table(Z$Division)))
legend("topright", legend=c(paste("Men", length(which(Z$Gender == "M"))), 
                            paste("Women", length(which(Z$Gender == "F")))))



