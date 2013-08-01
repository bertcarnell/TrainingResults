
repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
source(file.path(repositoryPath, "Common", "Utilities.R"))

racePath <- file.path(repositoryPath, "2003StateToState5k")
dataPath <- file.path(racePath, "data")
dataFile1 <- file.path(dataPath, "State-To-State 5K 2003 Race Results.htm")
dataFile2 <- file.path(dataPath, "State-To-State 5K 2003 Race Results 2.htm")

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

windows()
timeHistogram(Y$Final, indMe, "2008 State to State 5K")
windows()
timeBoxplot(Y$Final, indMe, "2008 State to State 5k")


