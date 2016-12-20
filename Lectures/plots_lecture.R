library(data.table)
library(ggplot2)
library(dplyr)

data(anscombe)
anscombe

## enm x1, y1 é o normal
## em x2, y2 pearson correlation n consegue detectar bem relações não-lineares, perason correlationd etects the goodness of fit for liner model, solution: fit a quadratic model
## in x3, y3 a correlação com o outlier ficou subestimada por causa do outler
## em x4, y4 a correlação com o outlier fico sobreestimada (não há correlação)

## boxplot
#ggplot(anscombe, aes(x1, y1)) + 
#  geom_point()

Sc.affy2ensg.data <- as.data.table(Sc.affy2ensg, keep.rownames = TRUE)
Sc.tnumber.data <- as.data.table(Sc.tnumber, keep.rownames = TRUE)
Raw.datamat.data <- as.data.table(Raw.datamat, keep.rownames = TRUE)

## merge evertyhing into datamat
Raw.datamat.data <- dplyr::left_join(Raw.datamat.data, Sc.affy2ensg.data, by="rn")
Raw.datamat.data <- dplyr::left_join(Raw.datamat.data, Sc.tnumber.data, by=c("Sc.affy2ensg" = "rn"))
Raw.datamat.data <- as.data.table(Raw.datamat.data)
Raw.datamat.data <- Raw.datamat.data[, y := `L WTSF xxxx 02 D +00-+06 ssss tt rrrr PM.CEL`/`T WTSF xxxx 02 D +00-+06 ssss tt rrrr PM.CEL`]

## ggplot
ggplot(Raw.datamat.data, aes(Sc.tnumber, y = log(y))) + geom_point() + geom_density2d()

require(LSD)
heatscatter(Raw.datamat.data[, Sc.tnumber], log(Raw.datamat.data[, y]), xlim = c(0, 2000))
