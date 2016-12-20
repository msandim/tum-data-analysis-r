# Ex1
data(iris)
iris
dim(iris)
str(iris)

# Ex2
par(mfrow=c(2,2))

for(i in 1:4)
{
  hist(iris[,i], main=colnames(iris)[1])
}

boxplot(iris[,1:4])

library(beanplot)
beanplot(iris[,1:4])

# Ex4
boxplot(Sepal.Length~Species, data=iris)
stripchart(Sepal.Length ~ Species, vertical = TRUE, data = iris, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')
#beanplot(Sepal.Length~Species, data=iris)

# Ex5 - There are not relationships
library(corrplot)
corrplot(cor(iris[,1:4]))

# Ex7
corrplot(cor(data[,4:6]))
# We can see that 1-2, and 2-3 are correlated

par(mfrow=c(1,3))
plot(data[,"exp1"], data[,"exp2"]) #*
plot(data[,"exp2"], data[,"exp3"]) #*
plot(data[,"exp1"], data[,"exp3"])

library(lattice)
xyplot(exp1~exp2|genotype1, data=data) 
xyplot(exp1~exp2|genotype2, data=data) # 
xyplot(exp1~exp2|genotype3, data=data)

xyplot(exp2~exp3|genotype1, data=data) 
xyplot(exp2~exp3|genotype2, data=data) # 
xyplot(exp2~exp3|genotype3, data=data)

xyplot(exp1~exp3|genotype1, data=data) 
xyplot(exp1~exp3|genotype2, data=data)
xyplot(exp1~exp3|genotype3, data=data)

# Ex8
corrplot(cor(expr))

library(gplots)
heatmap.2(expr, trace="none", col=redblue(75)) # DOHH2 is an outlier

# Ex6
library(data.table)
library(dplyr)
coffee <- read.csv("coffee_readable.csv", row.names=1)
heatmap.2(as.matrix(coffee), trace="none", col=redblue(75))
