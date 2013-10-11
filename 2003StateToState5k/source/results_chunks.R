## ---- setup
repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
source(file.path(repositoryPath, "Common", "Utilities.R"))

racePath <- file.path(repositoryPath, "2003StateToState5k")
dataPath <- file.path(racePath, "data")
outPath <- file.path(racePath, "images")
dataFile1 <- file.path(dataPath, "State-To-State 5K 2003 Race Results.htm")
dataFile2 <- file.path(dataPath, "State-To-State 5K 2003 Race Results 2.htm")

stopifnot(all(file.exists(c(racePath, dataPath, outPath, dataFile1, dataFile2))))

X <- readHTMLTable(dataFile1, stringsAsFactors=FALSE, colClasses="character")[[13]]
ind <- 3:(nrow(X)-2)
Y <- data.frame(Athlete=X[ind,5], 
                Age=X[ind,7], 
                Final=strptime(X[ind,9], format="%M:%S"),
                Pace=strptime(X[ind,10], format="%M:%S"),
                stringsAsFactors=FALSE)
X <- readHTMLTable(dataFile2, stringsAsFactors=FALSE, colClasses="character")[[13]]
ind <- 3:(nrow(X)-2)
Y <- rbind(Y, data.frame(Athlete=X[ind,5], 
                Age=X[ind,7], 
                Final=strptime(X[ind,9], format="%M:%S"),
                Pace=strptime(X[ind,10], format="%M:%S"),
                stringsAsFactors=FALSE))

indMe <- which(Y$Athlete == "Robert Carnell")

## ---- plotchunk
timeHistogramBoxplot(Y$Final, indMe, "2003 State to State 5k", 
                     strptime(as.character(seq(15,50,by=5)), "%M"))
