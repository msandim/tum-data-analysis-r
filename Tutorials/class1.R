# Classes: 00.08.055 => aLWAYS BETWEEN 14H-17H
# 02.13.010 => Oct 26 - Jan 11
# 00.11.038 => Jan 18, Jan 25
# 02.13.010 => Feb 1, Feb 8

# 1.1 There are no scalar types. Everything is a vector.

# 1.2 Lists can have elements of different types.
# To transform them into a vector, use "as.blabla" 
# pode ser preciso usar unlist()
# Can't use is.vector to see if something is a vector, because everything is a vector..

a <- list(c(1,2,3), TRUE, 2)

unlist(a)
as.character(a)

# 1.3. It's converted into the most flexible type
c(1, "a")
c(1.0, 1)
c(1.5, 1)

mean(c(TRUE, FALSE, TRUE)) # dá 0.66667 (TRUE => 1, FALSE => 0). Contudo se fizer as.numeric nos booleans, dá erro

# 1.4. 
x <- list(list(1, 2), c(3, 4))
y <- c(list(1, 2), c(3, 4))

str(x)
str(y) # ele faz cast de tudo para o tipo mais flexível (list), e faz cohersive de tudo porque queremos um vector

c(list(1, 2), list(3, 4))
c(c(1, 2), c(3, 4))

# 1.5.
v1 <- 1:10
v2 <- seq(1,11)
v3 <- seq(1,12, by = 1) # pode ter valores não discretos, logo pode ser um double

names(v1) <- letters[v1] # coloca o attributes names
names(v2) <- letters[v2]
names(v3) <- letters[v3]

lista <- list(v1, v2, v3) # attributes desaparecem

df <- as.data.frame(lista) # dá erro, porque a dataframe precisa de listas do mesmo tamanho
lista[[1]] <- c(lista[[1]], NA, NA)
lista[[2]] <- c(lista[[2]], NA)

df <- as.data.frame(lista)
names(df) <- LETTERS[1:3]

rownames(df)

# 1.6.
attr(df, 'class') <- 'list'
str(df)
attributes(df)

attr(df, 'class') <- 'lole'

# attr(, 'class'), vai buscar o output da cena anterior

# 1.7.
# Factors are integers, they just have a mapping to certain names, 
power <- gl(3, 10, labels = c("Rita Repulsa", "Lord Zedd", "Rito Revolto")) # Cuidado com esta ordem
power[2] <- "Shredder" # mete um NA nesta posição
# but power[1] <- "Lord Zedd" works

# 1.8.
f1 <- factor(letters)
levels(f1) <- rev(levels(f1))
f2 <- rev(factor(letters))
f3 <- factor(letters, levels = rev(letters))
f4 <- factor(rev(letters)) # same as f2

# 1.9
df <- data.frame(a = factor(rep(1:3, 2)), b = factor(rep(1:3, 2)), c = factor(rep(1:3, 2)), d = factor(rep(1:3, 2)))

# mudar levels:
levels(df[,1])[1] <- "What"
levels(df[,3])[3] <- "me no"
levels(df[,4])[1] <- "?"
levels(df[,3])[2] <- "hurt"

# 1.10
df_final <- data.frame(V1 = 1:26,
                       V2 = seq(4,104, by = 4),
                       V3 = letters[rep(seq(1, 25, by = 2), each = 2)])

# 1.11
df_tmp <- data.frame(V4 = rev(rep(seq(1, 25, by = 2), each = 2)),
                     V5 = seq(0, 1.6, by = 0.064))

df_11 <- cbind(df_tmp, df_final[, c("V2", "V3")])

# 1.12
typeof(Inf)
# Pah, porque é assim e acabou. Inf + NA n faz sentido