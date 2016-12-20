library(data.table)
library(magrittr)
library(tidyr)
library(XLConnect)
library(dplyr)

## Section 1

poke_wb <- loadWorkbook("pokemon.xlsx")
poke_dt <- readWorksheet(poke_wb, sheet=1) %>% as.data.table
head(poke_dt)
moves_dt <- readWorksheet(poke_wb, sheet=2) %>% as.data.table
evolution_dt <- readWorksheet(poke_wb, sheet=3) %>% as.data.table
typeChart_dt <- readWorksheet(poke_wb, sheet=4) %>% as.data.table

colnames(poke_dt)

setnames(poke_dt, "X.", "Number") # change name of the column of the dataframe
colnames(poke_dt) <- gsub("\\.", "_", colnames(poke_dt))

sapply(poke_dt, class) # ou str(poke_dt), para ver as classes

poke_dt[, Number := gsub(intToUtf8(160),'', Number)] # remove space (" 001")

# 3
poke_dt[, Number := as.integer(Number)]
poke_dt[, Type := as.factor(Type)]

# 4
grep("Mega", poke_dt$Name, value=T) # These are the ones we want to remove

poke_dt <- poke_dt[!grep("Mega", Name)] # tb dá invert = TRUE no arugmento do mega
poke_dt <- poke_dt[Number <= 150]

## Section 2
# 1
unique(poke_dt$Type) # tb dá levels(poke_dt$Type)
poke_dt[, .N, by = Type]

# 2
poke_dt[Type == "ICE"]

# 3
stats <- c("HP", "Attack", "Defense", "Special_Attack", "Special_Defense", "Speed", "Total")

poke_dt[,max(HP)]
sapply(stats, function(s) poke_dt[, max(get(s))]) # o get dá o valor de uma variável com o nome "s"

poke_dt[, sapply(.SD, max), .SDcols = stats] # outra hipotese

#colSums(poke_dt[, .SD, .SDcols = stats])

colMax <- function(DT, colNames)
{
  DT[, sapply(.SD, max), .SDcols = colNames]
}

colMax(poke_dt, stats)

# 4 é repetido
# 5
poke_dt[HP == colMax(poke_dt, "HP")]
poke_dt[which.max(HP)]
sapply(stats, function(s) poke_dt[which.max(get(s))])
poke_dt[sapply(stats, function(s) which.max(get(s)))]

# 6

which_min <- function(v)
{
  which(v == min(v, na.rm = T))
}

which_max <- function(v)
{
  which(v == max(v, na.rm = T))
}

which_min(c(3,1,1))
which_max(c(3,1,1))

# 7
poke_dt[, .SD[1], by = Type] # first
poke_dt[, .SD[which_max(Total)], by = Type] # strongest
poke_dt[, .SD[which_min(Total)], by = Type] # weakest

# 8
(setorder(poke_dt[, .(meanT = mean(Total)), by = Type], meanT))

## Section 3
# 1
colnames(evolution_dt)
setnames(evolution_dt, c("Evolving.from", "Evolving.to"), c("Name", "Evolution"))
sapply(evolution_dt, class)

evo_dt <- evolution_dt[Name %in% poke_dt$Name & Evolution %in% poke_dt$Name]

# 2
poke_dt[! Name %in% union(evo_dt$Name, evo_dt$Evolution), Name] %>% unique

# 3
poke_join <- left_join(poke_dt, evo_dt[, .(Name, Evolution, Level)], by = "Name") %>% as.data.table

# 4
evolution_dt[, max(Level, na.rm = T)]
#poke_join[, .(max = max(Level, na.rm = T), ), by = "Type"]
poke_join[, .SD[which_max(Level)], by = "Type"]
#left_join(poke_dt, evo_dt[, .(Name, Level)], by = "Name") %>% as.data.table
#  %>% 

# 5
setnames(moves_dt, c("Cat.", "Acc.", "Prob....."), c("Category", "Accuracy", "Probability"))
moves_dt[, Accuracy := as.numeric(Accuracy)]

# 6
unique(moves_dt$Category)

# 7
moves_dt[Type %in% poke_dt[Name == "Bulbasaur", Type]] %>% head

# 8
moves_dt[, Real_Power := Power * (Accuracy/100)]
moves_dt[which_max(Real_Power)]
moves_dt[, .SD[which_max(Real_Power)][,.(Real_Power)], by = .(Category, Type)]

# 9
typeChart_dt[Multiplier > 1 & Attack == "FAIRY"]

# 10
( setorder(typeChart_dt[, .(Att_eff := sum(Multiplier)), by = Attack]) )
( setorder(typeChart_dt[, .(Def_eff = sum(Multiplier)), by = Defense]) )

# Homework 4
