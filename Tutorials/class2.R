# 1.1
m <- matrix(1:12, 3, 4)
mdf <- as.data.frame(m)

# 1.2
mdf[2, c(2,4)]

# 1.3
mdf[2, mdf[2,] > 4] <- 0
mdf

# 1.4
rownames(mdf) <- paste0("row", 1:3)
colnames(mdf) <- paste0("col", 1:4)
mdf

# 1.5
mdf[, paste0("col", 3:4)]

# 1.6
# i)
lapply(mdf, function(x) x[x %% 2 == 0])

# ii)
mdf[mdf %% 2 == 0]
