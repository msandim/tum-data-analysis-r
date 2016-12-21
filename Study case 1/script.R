library(data.table)
library(ggplot2)

marker <- fread("marker.txt")
growth <- fread("growth.txt")
genotype <- fread("genotype.txt")
gene <- fread("gene.txt")
expression <- fread("expression.txt")
#combine
gt <- melt(genotype,id='strain',variable.name = 'marker', value.name = 'type')[marker,on=list(marker=id)]
gt <- melt(growth,id='strain',variable.name = 'medium', value.name = 'growth.rate')[gt,on='strain', allow.cartesian=TRUE]
mark <- marker[chrom=='chr07' & start==1069229,id]
ggplot(gt[marker==mark & medium == 'YPMalt'],aes(x=type,y=growth.rate)) + geom_boxplot() + ggtitle(mark)