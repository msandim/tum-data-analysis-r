library(dplyr)
library(tidyr)
library(data.table)
library(jsonlite)
library(corrplot)

dt <- readRDS("case_study_dt.rds")
utr3 <- as.data.table(readRDS("case_study_utr3_6mer.rds"))

json <- fromJSON("case_study_info.json")

merged <- data.table(dt, utr3)

models_pvalues <- sapply(utr3, function(x,dt)
{
  sum <- summary(lm(x ~ dt))$coefficients[8]
},dt=dt$WT)

models_pvalues_adjusted_fdr <- p.adjust(models_pvalues, "fdr")
models_pvalues_adjusted_bon <- p.adjust(models_pvalues, "bonferroni")
hist(models_pvalues, breaks=100)
hist(models_pvalues_adjusted_fdr, breaks=100)
hist(models_pvalues_adjusted_bon, breaks=100)

sum(models_pvalues_adjusted_fdr < 0.05)
sum(models_pvalues_adjusted_bon < 0.05)

### look at the half time
names_significant <- names(which(models_pvalues_adjusted_fdr < 0.05))

utr3_significant <- utr3[, names_significant, with=FALSE]

utr3_significant$genename <- dt$genename

utr3_significant$sum <- apply(utr3_significant[, -ncol(utr3_significant)], 1, sum)
hist(utr3_significant$sum)

utr3_significant <- utr3_significant[utr3_significant$sum != 0, ]

wt_dt_significant <- merge(utr3_significant, dt[, c("WT", "genename")], by="genename")


#sum <- summary(lm(utr3$AAAAAA ~ dt$WT))


#corrplot(dt %>% select(WT, TTT:GGG) %>% cor %>% .[1,,drop = FALSE], method="circle")

