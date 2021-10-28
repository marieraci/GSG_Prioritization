# set directory
setwd("path")

# run all .R files in directory
sapply( list.files(getwd(), full.names=TRUE), source )
