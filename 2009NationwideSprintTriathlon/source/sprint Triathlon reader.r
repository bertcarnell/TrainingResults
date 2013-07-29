inputFile <- file.path("C:","Users","carnellr","Desktop",
  "sprint Triathlon dataset.txt")

colWidths <- c(6,22,5,4,5,8,8,8,5,8,5,5,8,6,8,7)
colNames <- read.fwf(inputFile, colWidths, skip=1, n=1)
junk <- read.fwf(inputFile, colWidths, skip=2, n=1)

X <- read.fwf(inputFile, colWidths, skip=3, n=304)

preStripWhite <- function(x)
{
  starts <- regexpr("[[:blank:]]+", x)
  if (is.null(starts) || is.na(starts) || starts != 1) print(x)
  lens <- attr(starts, "match.length")
  lenTotal <- nchar(x)
  
  substr(x, lens+1, lenTotal)
}

Y <- apply(X, c(1,2), preStripWhite)

