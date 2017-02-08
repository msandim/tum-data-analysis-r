library(data.table)
library(magrittr)
library(ggplot2)

library(dplyr)
library(tidyr)

setwd("C:/Users/Nessi/Desktop/Studium/WS16/Data_Analysis+Visualization/Exercise/extdata/Case_StudyII")
info <- jsonlite::fromJSON("case_study_info.json")
dt <- readRDS("case_study_dt.rds")
utr3_6mer <- as.data.table(readRDS("case_study_utr3_6mer.rds"))

# heatmap strains
gplots::heatmap.2(as.matrix(dt[,info$strains,with=F]))

###start: code from yesterday
# linear model with only WT as Covariate
models_pvalues <- sapply(utr3_6mer, function(x,dt)
{
  sum <- summary(lm(x ~ dt))$coefficients[8]
},dt=dt$WT)

models_pvalues_adjusted_fdr <- p.adjust(models_pvalues, "fdr")
models_pvalues_adjusted_bon <- p.adjust(models_pvalues, "bonferroni")
hist(models_pvalues, breaks=100)
hist(models_pvalues_adjusted_fdr, breaks=100)
hist(models_pvalues_adjusted_bon, breaks=100)

sum(models_pvalues_adjusted_bon <= 0.05)
#### end: code from yesterday
## linear model with all strains
all_models_pvalues <- sapply(utr3_6mer, function(x,dt)
{
  sum <- summary(lm(x ~ .,data = dt))$coefficients[(ncol(dt)+1)*4]
},dt=dt[,c(info$strains, info$codons), with=F])

all.pvals.fdr <- p.adjust(models_pvalues, "fdr")
sum(all.pvals.fdr <= 0.05)
identical(names(which(all.pvals.fdr <= 0.05)),names(which(models_pvalues_adjusted_fdr <= 0.05)))


### look at the half time
names_significant <- names(which(models_pvalues_adjusted_fdr <= 0.05))

utr3_significant <- utr3_6mer[, names_significant, with=FALSE]

utr3_significant$genename <- dt$genename

utr3_significant$sum <- apply(utr3_significant[, !"genename"], 1, sum)
barplot(table(utr3_significant$sum), main = "Total amount of significant 6-mers in one gene", xlab = "sum of significant 6-mers", ylab = "Frequency")

wt_dt_significant <- merge(utr3_significant, dt[, c("WT", "genename")], by="genename")

## boxplot of halflife in WT by amount of 6-mers in sequence
ggplot(wt_dt_significant,aes(factor(sum),WT))+
  geom_boxplot()+
  ggtitle("Count of significant 6-mers in Sequence")+
  labs(x = "Amount of Significant 6-mers", y = "halflife in WT")

#wt_dt_significant[sum==33,]

# histogram of frequencies of the k-mers
barplot(sort(t(colSums(wt_dt_significant[ ,!c("genename","sum","WT")]))),main = "Sums of each significant 6-mer", las = 2)
library(beanplot)
beanplot(colSums(wt_dt_significant[,!c("genename","sum","WT")]),main="frequency of motifs in all genes",xlab="motifs",ylab="occurrences")

# TODO look at halflife where no significant motif is present
beanplot(wt_dt_significant[sum==0 , WT], main="Half-life of genes with no significant motif present",ylab="half-life",xlab="WT")
genes_nosig_motif <- wt_dt_significant[sum == 0 , genename]
halflifes_nosig_motif <- dt[genename %in% genes_nosig_motif,info$strains,with=F]
boxplot(halflifes_nosig_motif,main="Half-life of genes with no significant motif present",ylab="half-life",xlab="strains",las=2)

halflifes_sig_motifs <- dt[genename %in% utr3_significant$genename, info$strains,with=F]
boxplot(halflifes_sig_motifs,main="Half-life with motifs present",ylab="half-life",xlab="strains",las=2)

# boxplot of halflife for every significant marker -> melt table
melted_motifs <- melt(wt_dt_significant[,!"sum"],id.vars = c("genename","WT"),measure.vars=names(wt_dt_significant[,!c("sum","genename","WT")]),variable.name ="motif" , value.name = "freq")
names(melted_motifs)[2]<- "halflife"
melted_motifs <- melted_motifs[freq>0,]
head(melted_motifs)

## look at genes with only one significant 6-mer that have a high half-life and check if they are also included in others that have high half lifes
hist(wt_dt_significant[sum==1,WT])
single.sig.motifs.high.halflife.genename <- wt_dt_significant[sum==1&WT>4,genename]
single.sig.motifs.low.halflife.genename <- wt_dt_significant[sum==1&WT<2,genename]

## try to match all motifs that occur together
motifs.pergene <- sapply(unique(melted_motifs$genename), function(gene,dt){
  dt[genename == gene,motif]
}, dt = melted_motifs)

#length(motifs.pergene[c(single.sig.motifs.high.halflife.genename)])
sig.motifs.high.halflife <- as.character(unlist(motifs.pergene[c(single.sig.motifs.high.halflife.genename)]))
table(sig.motifs.high.halflife)

sig.motifs.low.halflife <- as.character(unlist(motifs.pergene[c(single.sig.motifs.low.halflife.genename)]))
table(sig.motifs.low.halflife)

in.both <- intersect(sig.motifs.high.halflife, sig.motifs.low.halflife)

low.and.high <- melted_motifs.high.halflife[,motif] %in% in.both

ggplot(melted_motifs.high.halflife,aes(motif,halflife, fill=low.and.high))+
  geom_boxplot()+
  ggtitle("Halflife in WT with 6-mers alone assoziated with high halflife") +
  scale_fill_discrete(name="halflife",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("high", "high and low")) +
  labs(x = "6-mers") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# are those in genes with low half-lifes?


## get genes with different methylation status classification: 
range(melted_motifs$halflife)

melted_motifs.high.halflife <- melted_motifs[motif %in% (unique(sig.motifs.high.halflife)),]
ggplot(melted_motifs.high.halflife,aes(motif,halflife))+
  geom_boxplot()+
  ggtitle("Count of significant 6-mers in Sequence") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


