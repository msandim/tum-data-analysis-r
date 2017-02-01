library(ggplot2)

## 1.2

x_values <- rnorm(500, mean = 0, sd = sqrt(20))

# 1000 simulations
beta_values <- sapply(1:1000, function(x)
{
  data <- data.frame(x = x_values, y = -0.5 + 1.5*x_values) # the y has linear regression
  data$y <- data$y + rnorm(500, mean = 0, sd = sqrt(0.7)) # add an error
  
  model <- lm(y ~ x, data)
  alpha <- summary(model)$coefficients[1]
  beta <- summary(model)$coefficients[2]
  return(beta)
})

results1.2 <- data.frame(beta_values_simulation = beta_values)

ggplot(results1.2, aes(x=beta_values_simulation)) +
  geom_histogram(binwidth = 0.001) + 
  geom_vline(xintercept = 1.5, color="red") +
  geom_vline(aes(xintercept = mean(beta_values_simulation)), color="blue") #+
  #stat_function(fun=dnorm, args=list(mean=1.5, sd=sd(sqrt(0.7 / (length(x_values) * var(x_values))))))
  #geom_density(aes(x=beta_values_theoretical))

ggplot(results1.2, aes(x=beta_values_theoretical)) +
  geom_density() + 
  geom_vline(xintercept = 1.5, color="red") +
  geom_vline(aes(xintercept = mean(beta_values_simulation)), color="blue")

############################################################################################
# 1.3
############################################################################################

library(data.table)
library(dplyr)

expression <- fread("data/eqtl/expression.txt")
gene <- fread("data/eqtl/gene.txt")
genotype <- fread ("data/eqtl/genotype.txt")
genotype <- genotype %>% melt (id.var='strain', variable.name='marker', value.name='genotype')
growth <- fread("data/eqtl/growth.txt")
growth <- growth %>% melt(id.vars="strain", variable.name='media', value.name='growth_rate')
marker <- fread("data/eqtl/marker.txt")

library(ggplot2)
plot_condition_on <- function(test_mk = "mrk_5078", condition_on = "mrk_5211", genotype_dt = genotype, growth_dt = growth)
{
  test_mk_geno <- genotype_dt[marker == test_mk, .(strain, genotype)]
  test_mk_geno[, genotype := paste(test_mk, genotype)]
  condition_on_mk_geno <- genotype_dt[marker == condition_on, genotype]
  test_mk_geno[, condition_on_mk_geno := paste(condition_on, condition_on_mk_geno)]
  dt <-  merge(test_mk_geno, growth_dt[media == 'YPMalt'], by = 'strain')
  
  full <- lm(growth_rate ~ genotype + condition_on_mk_geno, data=dt)
  reduced <- lm(growth_rate ~ condition_on_mk_geno, data=dt) # por no full model o que queremos ver se ajuda a melhorar  modelo, e metemos no reduced o "normal"
  anova(reduced, full)
  #ggplot(dt, aes (genotype, growth_rate)) +
  #  geom_boxplot () +
  #  labs(title = paste(test_mk, "conditioning on", condition_on)) +
  #  facet_wrap(~ condition_on_mk_geno) +
  #  theme(plot.title = element_text(hjust = 0.5))
}

plot_condition_on(test_mk = "mrk_5091", condition_on = "mrk_5211")

## Exercise:

##############################################
###################### 1.4 ###################
##############################################
data(mtcars)
mtcars

ggplot(mtcars) + geom_point(aes(cyl, mpg)) ## sim
ggplot(mtcars) + geom_point(aes(disp, mpg)) # sim
ggplot(mtcars) + geom_point(aes(hp, mpg)) # sim
ggplot(mtcars) + geom_point(aes(drat, mpg)) #hummmm, sim
ggplot(mtcars) + geom_point(aes(wt, mpg)) # sim
ggplot(mtcars) + geom_point(aes(qsec, mpg)) # hummm, sim
ggplot(mtcars) + geom_point(aes(vs, mpg)) #hhummmm, sim
ggplot(mtcars) + geom_point(aes(am, mpg)) # +/-, mas n
ggplot(mtcars) + geom_point(aes(gear, mpg)) # +/- mas n
ggplot(mtcars) + geom_point(aes(carb, mpg)) # not really

#### forward elimination
# 