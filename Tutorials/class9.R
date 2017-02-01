library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

# Read
expression <- fread("data/expression.txt")
gene <- fread("data/gene.txt")
genotype <- fread("data/genotype.txt")
genotype <- genotype %>% melt(id.vars = 'strain', variable.name = 'marker', value.name = 'genotype')
growth <- fread("data/growth.txt")
growth <- growth %>% melt(id.vars = "strain", variable.name = 'media', value.name = 'growth_rate')
marker <- fread("data/marker.txt")

### Question 1

getMaltoseDt = function(mrk){
  growth_mrk <- genotype[marker == mrk, .(strain, genotype)] %>%
    merge(growth, ., by = 'strain')
  growth_mrk[media == "YPMalt"]
}

median_diff <- function(dt){
  dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T)] -
    dt[genotype == 'Lab strain', median(growth_rate, na.rm=T)]
}

p_val_medians <- function(dt, N_permu = 1000){
  T_ref <- median_diff(dt)
  T_star <- c()
  i = 1
  while(i <= N_permu){
    T_star <- c(T_star, median_diff(dt[, genotype := sample(genotype)]))
    i <- i + 1
  }
  hist(T_star, xlim = c(-3, 3))
  abline(v = T_ref, lwd = 2)
  p_val <- (sum(T_star > T_ref | T_star < -T_ref) + 1) / (N_permu + 1)
  p_val
}
p_val_medians(getMaltoseDt("mrk_5211"))


### Question 2

# H0: there is no difference between mrk_5211 and mrk_5091, therefore we can't refute the null hyphotesis

##### Run this to create mks_geno
mks_geno <- genotype[marker %in% c('mrk_5211', 'mrk_5091')] %>%
  spread(marker, genotype)

table(mks_geno[,.(mrk_5091, mrk_5211)])

diagonal_diff <- function(dt)
{
  tab <- table(dt[,.(mrk_5091, mrk_5211)])
  return(tab[1,1] + tab[2,2])
}

p_val_diagonal <- function(dt, N_permu = 10000)
{
  T_ref <- diagonal_diff(dt)
  T_star <- c()
  i = 1
  while(i <= N_permu)
  {
    T_star <- c(T_star, diagonal_diff(dt[, mrk_5091 := sample(mrk_5091)]))
    i <- i + 1
  }
  hist(T_star, xlim = c(50, 120))
  abline(v = T_ref, lwd = 2, col='red')
  p_val <- (sum(T_star > T_ref | T_star < -T_ref) + 1) / (N_permu + 1)
  p_val
}

p_val_diagonal(mks_geno)

### Question 3

# We need to reset mks_geno here

mks_geno <- mks_geno %>% left_join(growth %>% filter(media == "YPMalt") %>% select(-media), by = "strain") %>% as.data.table

#mks_geno_lab <- mks_geno %>% filter(mrk_5211 == "Lab strain")
#mks_geno_wild <- mks_geno %>% filter(mrk_5211 == "Wild isolate")

ggplot(mks_geno, aes(mrk_5091, growth_rate)) +
  geom_boxplot() +
  labs(title = "Conditioning") +
  facet_grid(~ mrk_5211)

# ##### We're gonna calculate th pvalue by suming the differences between the medians

## Sabemos que o mrk_5211 cresce fixe, mas será que o 5091 cresce fixe também?

#### H0: 

diff_med <- function(dt)
{
  abs(dt[mrk_5091 == 'Wild isolate' & mrk_5211 == "Lab strain", median(growth_rate, na.rm=T)] -
    dt[mrk_5091 == 'Lab strain' & mrk_5211 == "Lab strain", median(growth_rate, na.rm=T)]) +
  abs(dt[mrk_5091 == 'Wild isolate' & mrk_5211 == "Wild isolate", median(growth_rate, na.rm=T)] -
      dt[mrk_5091 == 'Lab strain' & mrk_5211 == "Wild isolate", median(growth_rate, na.rm=T)])
}

p_val_med <- function(dt, N_permu = 1000)
{
  T_ref <- diff_med(dt)
  T_star <- c()
  i = 1
  while(i <= N_permu)
  {
    T_star <- c(T_star, diff_med(dt[, growth_rate := sample(growth_rate), by = mrk_5211]))
    i <- i + 1
  }
  hist(T_star, xlim = c(0, 3))
  abline(v = T_ref, lwd = 2, col='red')
  p_val <- (sum(T_star > T_ref | T_star < -T_ref) + 1) / (N_permu + 1)
  p_val
}

p_val_med(mks_geno)

#ggplot(mks_geno_lab, aes(mrk_5091, growth_rate)) +
#  geom_boxplot() +
#  labs(title = "Geno lab") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(mks_geno_wild, aes(mrk_5091, growth_rate)) +
#  geom_boxplot() +
#  labs(title = "Geno wild") +
#  theme(plot.title = element_text(hjust = 0.5))
