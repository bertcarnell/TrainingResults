require(rgdal)
require(maptools)

dsn <- file.path("C:","Users","carnellr","Downloads",
                 "Half Marathon","activity_175623034.kml")
ogrListLayers(dsn)
X <- readOGR(dsn, layer="Capital City Half Marathon")
plot(X)
X@lines[[1]]@Lines[[1]]@coords[1:10,]
Y <- readOGR(dsn, layer="Track Points")
plot(Y)
Y@coords[1:10,]
Z <- readOGR(dsn, layer="Laps")
plot(Z)
Z@coords[1:10,]

xml <- readLines(dsn)
ind1 <- grep("Track Points", xml)
xml <- xml[-(1:ind1)]
ind <- grep("begin", xml)
ind2 <- grep("coordinates", xml)

beginRaw <- gsub("[[:blank:]]*<begin>", "", xml[ind])
beginRaw <- gsub("</begin>","", beginRaw)
beginDateTime <- strptime(beginRaw, "%Y-%m-%dT%H:%M:%S")
endRaw <- gsub("[[:blank:]]*<end>", "", xml[ind+1])
endRaw <- gsub("</end>","",endRaw)
endDateTime <- strptime(endRaw, "%Y-%m-%dT%H:%M:%S")
coords <- gsub("[[:blank:]]*<coordinates>", "", xml[ind2])
coords <- gsub("</coordinates>","",coords)
coords <- sapply(coords, function(x) as.numeric(strsplit(x, ",")[[1]]), USE.NAMES=FALSE)

dists <- numeric(ncol(coords))
dists[1] <- 0
for (i in 2:ncol(coords))
{
  dists[i] <- spDists(t(coords[1:2,i]), t(coords[1:2, i-1]), longlat=TRUE)
}

timeDiffs <- as.numeric(endDateTime - beginDateTime)

kmPerSec <- ifelse(timeDiffs == 0, 0, dists / timeDiffs)

require(RColorBrewer)

# 1 / (km/sec*mi/km*sec/min)
kmPerSecToMinPerMi <- function(x) ifelse(x==0, NA, 1/(x/1.60934*60))
# min/mi*mi/km*sec/min
minPerMiToKmPerSec <- function(x) ifelse(x==0, NA, 1/(x/1.60934*60))
#color ramp from 12 min miles to 6 min miles
maxSpeed <- minPerMiToKmPerSec(6.0)
minSpeed <- minPerMiToKmPerSec(12.0)
speedToColor <- function(kmPerSec)
{
  ind <- which(kmPerSec < minSpeed | kmPerSec > maxSpeed)
  if (length(ind) == 0)
  {
    retColors <- colorRamp(brewer.pal(6, "Blues"))((kmPerSec - minSpeed) / (maxSpeed-minSpeed))
  } else {
    retColors <- matrix(NA, nrow=length(kmPerSec), ncol=3)
    retColors[which(kmPerSec >= minSpeed & kmPerSec <= maxSpeed),] <- colorRamp(brewer.pal(6, "Blues"))((kmPerSec[-ind] - minSpeed) / (maxSpeed-minSpeed))
    ind <- which(kmPerSec < minSpeed)
    if (length(ind) > 0)
      retColors[ind,] <- matrix(c(255, 0, 0), nrow=length(ind), ncol=3, byrow=TRUE) # red
    ind <- which(kmPerSec > maxSpeed)
    if (length(ind) > 0)
      retColors[ind,] <- matrix(c(0, 255, 0), nrow=length(ind), ncol=3, byrow=TRUE) # "green"
  }
  return(apply(retColors, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255)))  
}

colors <- speedToColor(kmPerSec)

cexs <- 2.0 - (2.0 - 0.5)*(ifelse(kmPerSec < minSpeed, minSpeed, kmPerSec) - minSpeed)/(maxSpeed - minSpeed)
  
windows(h=7, w=7)
par(plt=c(0,0.9,0,1))
plot(Y, col=colors, pch=19, cex=cexs)
par(mar=c(0,4,4,0))
par(plt=c(0.9,1,0,1), new=TRUE)
uc <- unique(colors)
len <- length(uc)
colors2 <- colorRamp(brewer.pal(6, "Blues"))((0:len)/len)
colors2 <- apply(colors2, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
plot(rep(0,len), minSpeed + (1:len)/len*(maxSpeed-minSpeed), col=colors2, pch=19, cex=0.6,
     axes=FALSE, xlab="", ylab="Speed (min/mi)")
axis(2, at=minPerMiToKmPerSec(12:6), labels=12:6)
box()






#Code supplied by james cheshire Feb 2012
#load packages and enter development mode
library(devtools)
dev_mode()
library(ggplot2)
library(proto)

#if your map data is a shapefile use maptools
library(maptools)
gpclibPermit()

#create GeomSegment2 function
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
  objname <- "geom_segment2"
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  segmentsGrob(x, y, xend, yend, default.units="native",
                               gp = gpar(col=alpha(colour, alpha), lwd=size * .pt,
                                         lty=linetype, lineend = "round"),
                               arrow = arrow)
                  ))
    }
  }})

geom_segment2 <- function(mapping = NULL, data = NULL, stat =
  "identity", position = "identity", arrow = NULL, ...) {
  GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                   position = position, arrow = arrow, ...)
}

#load data stlat/stlong are the start points elat/elong are the end points of the lines
lon<- read.csv("bikes_london.csv", header=F, sep=";")
names(lon)<-c("stlat", "stlon", "elat", "elong", "count")

#load spatial data. You need to fortify if loaded as a shapefile
water<- fortify(readShapePoly("waterfeatures.shp"))
built<- fortify(readShapePoly("buildings.shp"))

#This step removes the axes labels etc when called in the plot.
xquiet<- scale_x_continuous("", breaks=NA)
yquiet<-scale_y_continuous("", breaks=NA)
quiet<-list(xquiet, yquiet)

#create base plot
plon1<- ggplot(lon, aes(x=stlon, y=stlat))

#ready the plot layers
pbuilt<-c(geom_polygon(data=built, aes(x=long, y=lat, group=group), colour= "#4B4B4B", fill="#4F4F4F", lwd=0.2))
pwater<-c(geom_polygon(data=water, aes(x=long, y=lat, group=group), colour= "#708090", fill="#708090"))

#create plot
plon2<- plon1 +pbuilt+ pwater+ geom_segment2(aes(xend=elong, yend=elat, size= count, colour=count))+scale_size(range=c(0.06, 1.8))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb")+coord_equal(ratio=1/cos(lon$elat[1]*pi/180))+quiet+ opts(panel.background=theme_rect(fill="#404040"))

plon2

