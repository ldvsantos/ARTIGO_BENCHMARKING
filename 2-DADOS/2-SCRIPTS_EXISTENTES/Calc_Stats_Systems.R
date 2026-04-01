
library(readxl)
library(dplyr)
library(FuzzyR)

# Carregar dados
dados_brutos <- read_excel("banco_dados.xlsx")

# Normalização (mesma do script principal)
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * 10
}

dados_norm <- dados_brutos %>%
  mutate(
    Produtividade_norm = normalize(Produtividade),
    Materia_Organica_norm = normalize(`Matéria Orgânica`),
    Estabilidade_Agregados_norm = normalize(`Estabilidade de Agregados`),
    Permeabilidade_norm = normalize(Permeabilidade)
  )

# Recriar APSWCI (Simplificado apenas para ter a variável, assumindo que já foi calculado ou recalculando rápido)
# Como o cálculo do Fuzzy é complexo e depende de muitas funções, vou tentar ler do arquivo gerado se possível, 
# mas como não salvei o dataframe com APSWCI no passo anterior, vou ter que refazer o fuzzy ou ler de um arquivo intermediário.
# O script 'Gerar_Figuras_Revisao_Completas.R' roda tudo. Vou criar um script leve que faz o source dele ou copia a parte relevante.
# Melhor: Vou usar o script principal para calcular e imprimir apenas o que eu quero.

# Vou criar um script pequeno que carrega os dados e calcula as stats por SISTEMA.
# Vou copiar a lógica do fuzzy do script principal para garantir consistência.

# ... (Lógica Fuzzy omitida para brevidade, vou usar o script principal modificado)
