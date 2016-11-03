# required libraries
library(xlsx)
library(knitr)
library(stringi)
library(lubridate)
library(lhs)
library(tictoc)
library(EMD) 
library(hht)
library(BMS)
library(GA)
library(lattice)
library(ggplot2)

# source the required function
sourceDir <- function(path, trace = T, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm),...)
    if(trace) cat("\n")
  }
}

sourceDir('funcs/')