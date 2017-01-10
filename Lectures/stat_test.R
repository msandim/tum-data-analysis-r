#library(ggplot2)
library(data.table)
#library(magrittr)
#library(tidyr)
#library(dplyr)

#expression <- read.delim("../Study case 1/expression.txt")
gene <- read.delim("../Study case 1/gene.txt")
genotype <- read.delim("../Study case 1/genotype.txt")

#genotype <- genotype %>% melt(id.vars = 'strain', variable.name = 'marker', value.name = 'genotype')
growth <- read.delim("../Study case 1/growth.txt")
#growth <- growth %>% melt(id.vars = "strain", variable.name = 'media', value.name = 'growth_rate')
marker <- read.delim("../Study case 1/marker.txt")

mygeno <- genotype[, which(marker$chrom == "chr07" & marker$start == 1069229)]
names(mygeno) <- genotype$strain
dt <- data.table(Genotype = mygeno[growth$strain], YPMalt = growth$YPMalt)

dt[, median(YPMalt, na.rm = TRUE), by = "Genotype"]

dt[, ]

###
mystat <- function(dt)
{
  
}