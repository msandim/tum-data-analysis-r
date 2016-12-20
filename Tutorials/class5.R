# Question 5
library(data.table)
library(dplyr)
library(tidyr)

file_list <- list.files(path = "baby-names")

dfs <- lapply(file_list, function(file_name) {
  df <- fread(file.path("baby-names/", file_name))
  year <- strsplit(file_name, "\\.")[[1]][1]
  gender <- strsplit(file_name, "\\.")[[1]][2]
  df[, `:=` (year = year, gender = gender)]
  df
})

dataset <- rbindlist(dfs)

# Question 6
original_dt <- fread("gene_expression.tds")

nutrient_names <- c(G = "Glucose", L = "Leucine", P = "Phosphate",
                    S = "Sulfate", N = "Amnonia", U = "Uracil")

original_dt %>%
  data.table::melt(id.vars = c("GID", "YORF", "NAME", "GWEIGHT"),
                 variable.name = "nutrate",
                 value.name = "expression") %>%
  tidyr::separate("nutrate", sep = 1,
                  into = c("nutrient",
                           "rate")) %>%
  tidyr::separate("NAME", sep="\\s?\\|\\|\\s?",
                  into = c("name",
                           "biological_process",
                           "molecular_function",
                           "systematic_name", "other")) %>%
  as.data.table %>%
  .[, lalala]
  