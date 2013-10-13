rm(list=ls())

require(knitr)
require(markdown)

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

################################################################################

filenamesNoExt <- file.path(repositoryPath, races[1:4], "source")

stopifnot(all(sapply(filenamesNoExt, function(x){
  file.exists(file.path(x, "results.Rmd"))
})))

### knit the files to html
dummy <- sapply(filenamesNoExt, function(x) {
  setwd(x)
  knitr::knit2html(file.path(x, "results.Rmd"), quiet=FALSE)
})

### copy the results to the gh-pages branch
dummy <- mapply(function(x,y) {
  file.copy(file.path(x, "results.html"), 
            file.path(pagesPath, paste(y, ".html", sep="")), 
            overwrite=TRUE)
  }, filenamesNoExt, races[1:4])
