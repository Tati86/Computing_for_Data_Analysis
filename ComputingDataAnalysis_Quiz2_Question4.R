library(datasets)
data(iris)

virginicaData <- iris[, "Sepal.Length"]
g <- gl(3, 50)
tapply(virginicaData, g, mean)