library(ggplot2)

q1 <- function(sample_size, N)
{
  sapply(seq(N), function(x)
  {
    x <- rnorm(sample_size)
    y <- rnorm(sample_size)
    
    t.test(x, y)$p.value
  })
}

p_values <- q1(50, 10000)

p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

# Histogram of the p_values
ggplot(p_values, aes(p_values)) + geom_histogram()

# p_value rank plotting
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()
# These 2 plots give the same conclusion

# Adjusting the p-values:
p_values$adjusted <- p.adjust(p_values$p_values, method="bonferroni")
ggplot(p_values, aes(adjusted)) + geom_histogram()
ggplot(p_values, aes(x = adjusted, y = rank)) + geom_point()


p_values$adjusted <- p.adjust(p_values$p_values, method="hochberg")
ggplot(p_values, aes(adjusted)) + geom_histogram()
ggplot(p_values, aes(x = adjusted, y = rank)) + geom_point()


p_values$adjusted <- p.adjust(p_values$p_values, method="BH")
ggplot(p_values, aes(adjusted)) + geom_histogram()
ggplot(p_values, aes(x = adjusted, y = rank)) + geom_point()

########## 1.4 ##########

q1.4 <- function(sample_size, N)
{
  sapply(seq(N), function(x)
  {
    x <- rnorm(sample_size, mean = 0)
    y <- rnorm(sample_size, mean = 0.5)
    
    t.test(x, y)$p.value
  })
}

p_values <- q1.4(50, 1000)

p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

ggplot(p_values, aes(p_values)) + geom_histogram()
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()

# the difference between the means is significant

######## 1.5 ###########
p_values <- q1.4(50, 1000)

p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

ggplot(p_values, aes(p_values)) + geom_histogram()
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()

p_values <- q1.4(100, 1000)

p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

ggplot(p_values, aes(p_values)) + geom_histogram()
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()

p_values <- q1.4(150, 1000)

p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

ggplot(p_values, aes(p_values)) + geom_histogram()
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()

## As the sample size increases, it goes towards 0 more. Anything can be significant with a very big sample size

########## 1.6 ##########

p_values <- c(q1(50, 10000), q1.4(50, 1000))
p_values <- as.data.frame(p_values)
p_values$rank <- rank(p_values$p_values)

ggplot(p_values, aes(p_values)) + geom_histogram()
ggplot(p_values, aes(x = p_values, y = rank)) + geom_point()

# dá um mix

######## 1.7 #########
p_values$adjusted <- p.adjust(p_values$p_values, method="BH")
ggplot(p_values, aes(adjusted)) + geom_histogram()
ggplot(p_values, aes(x = adjusted, y = rank)) + geom_point()

p_values_1 <- q1(50, 10000)
p_values_4 <- q1.4(50, 1000)

names(p_values_1) <- rep("H0", length(p_values_1))
names(p_values_4) <- rep("H1", length(p_values_4))

p_values <- data.frame(p_values = c(p_values_1, p_values_4), hip = c(rep("H0", length(p_values_1)), rep("H1", length(p_values_4))))
p_values$cutoff <- p_values$p_values < 0.05

table(p_values$hip, p_values$cutoff)

# A false discovery rate

#library(data.table)
#res <- data.table(p_val = c(p_values_1, p_values_4), TRUE_H := )


                         