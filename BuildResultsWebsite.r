rm(list=ls())

require(knitr)

repositoryPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                            "TrainingResults")
pagesPath <- file.path("C:","Users","carnellr","Documents","Repositories",
                       "TrainingResultsPages")

races <- c(
  "2003StateToState5k", "2008ColumbusHalfMarathon",
  "2008PumpkinPie5k", "2009ColumbusHalfMarathon",
  "2009NationwideSprintTriathlon", "2011ColumbusTurketTrot",
  "2012CapitalCityHalfMarathon", "2012ColumbusHalfMarathon",
  "2013CapitalCityHalfMarathon", "2013GiantEagleMultisportsTriathlon"
)

filenamesNoExt <- list(
  file.path(repositoryPath, races[1], "source", "results")
)

### knit the files to html
dummy <- sapply(filenamesNoExt, function(x) {
  knitr::knit2html(paste(x, ".Rmd", sep=""))
})

### copy the results to the gh-pages branch
dummy <- mapply(function(x,y) {
  file.copy(paste(x, ".html", sep=""), 
            file.path(pagesPath, paste(races[1], ".html", sep="")), 
            overwrite=TRUE)
  }, filenamesNoExt, races[1])
