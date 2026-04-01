library(readxl)
library(dplyr)

dados <- read_excel("banco_dados.xlsx", sheet = "dados_010")

cat("Unique values in Parcela:\n")
print(unique(dados$Parcela))

cat("\nUnique values in Cultura:\n")
print(unique(dados$Cultura))

cat("\nStructure of Parcela:\n")
str(dados$Parcela)
