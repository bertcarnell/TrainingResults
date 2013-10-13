## ---- setup
repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
source(file.path(repositoryPath, "Common", "Utilities.R"))

racePath <- file.path(repositoryPath, "2008PumpkinPie5k")
dataPath <- file.path(racePath, "data")
dataFile1 <- file.path(dataPath, "pumpkin08.htm")

stopifnot(all(file.exists(c(racePath, dataPath, dataFile1))))

X <- readHTMLTable(dataFile1, stringsAsFactors=FALSE, colClasses="character")[[4]]

ind <- 5:(nrow(X)-1)
Y <- data.frame(Athlete=X[ind,2], 
                Age=X[ind,4], 
                Final=strptime(X[ind,5], format="%M:%S"),
                stringsAsFactors=FALSE)

indNA <- which(is.na(Y$Final))
indMe <- which(Y$Athlete[-indNA] == "Richard Carnell")

## ---- plotchunk
timeHistogramBoxplot(Y$Final[-indNA], indMe, "2008 Pumpkin Pie 5k", 
                     strptime(as.character(seq(15,50,by=5)), "%M"))
