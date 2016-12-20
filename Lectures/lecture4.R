library(data.table)

iris_dt <- as.data.table(iris)
iris_copy <- copy(iris_dt)

iris_copy[, meanPL := mean(Petal.Length), by=Species]
iris_copy[, meanPL2 := (Sepal.Length - meanPL)^2]
iris_copy[, sumMeanPL2 := sum(meanPL2), by=Species]
iris_copy[, n := .N, by=Species]
iris_copy[, s := sqrt(1/(n-1) * sumMeanPL2)]

iris_copy[, sd := sd(Petal.Length), by=Species]

iris_copy[, identical(s, sd)]
