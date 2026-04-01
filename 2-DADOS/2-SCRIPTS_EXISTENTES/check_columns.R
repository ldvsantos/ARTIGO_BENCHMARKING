library(readxl)
if(file.exists("banco_dados.xlsx")) {
  path <- "banco_dados.xlsx"
} else {
  path <- "2 - DADOS/banco_dados.xlsx"
}
df <- read_excel(path, sheet = "dados_010", n_max = 5)
print(colnames(df))
