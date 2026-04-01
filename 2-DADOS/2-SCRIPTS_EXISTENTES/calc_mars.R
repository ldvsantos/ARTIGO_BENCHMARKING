library(readxl)
library(dplyr)

# Read the validation data
dados <- read_excel("Dados_Validacao_Completa.xlsx")

# Calculate Ranks
dados$Rank_Original <- rank(dados$APSWCI_Original)
dados$Rank_NoYield <- rank(dados$APSWCI_NoYield)
dados$Rank_NoCOS <- rank(dados$APSWCI_NoCOS)

# Calculate MARS
mars_yield <- mean(abs(dados$Rank_Original - dados$Rank_NoYield))
mars_cos <- mean(abs(dados$Rank_Original - dados$Rank_NoCOS))

max_yield <- max(abs(dados$Rank_Original - dados$Rank_NoYield))
max_cos <- max(abs(dados$Rank_Original - dados$Rank_NoCOS))

cat("MARS Yield:", mars_yield, "\n")
cat("MARS COS:", mars_cos, "\n")
cat("MAX Yield:", max_yield, "\n")
cat("MAX COS:", max_cos, "\n")
