################################################################################
# SCRIPT DE IMPLEMENTAÇÃO DO ÍNDICE FUZZY ISPC
# Índice de Produtividade Agrícola e Conservação do Solo (ISPC)
#
# DESCRIÇÃO:
# Este script implementa um sistema de inferência fuzzy para o cálculo do ISPC 
# (Índice de Produtividade Agrícola e Conservação do Solo), o qual tem como objetivo 
# integrar atributos conservacionistas do solo e indicadores de produtividade agrícola, 
# fornecendo uma métrica sintética e interpretável para avaliação comparativa de sistemas de manejo.
#
# O modelo fuzzy considera 15 variáveis de entrada, selecionadas com base em sua relevância 
# ecológica, agronômica e funcional. As variáveis foram extraídas de parcelas experimentais 
# submetidas a diferentes sistemas de uso do solo e cultivos, permitindo comparações multiatributo.
#
# VARIÁVEIS DE ENTRADA (ordem conforme planilha de dados):
#  1.  DMG – Dano Mecânico Geral (quanto menor, melhor)
#  2.  DMP – Dano Mecânico Potencial
#  3.  RMP – Risco de Manejo Produtivo (quanto menor, melhor)
#  4.  Densidade – Densidade do solo (quanto menor, melhor)
#  5.  Estoque de C – Estoque de carbono no solo
#  6.  Na – Sódio trocável (quanto menor, melhor)
#  7.  ICV(%) – Índice de Cobertura Vegetal
#  8.  Altura de Plantas
#  9.  Diâmetro da espiga
# 10.  Comprimento da espiga
# 11.  Número de plantas por hectare
# 12.  Número total de espigas por hectare
# 13.  Número de espigas comerciais por hectare
# 14.  Peso de espigas comerciais por hectare
# 15.  Produtividade (kg/ha)
#
# VARIÁVEL DE SAÍDA:
#  - ISPC (Índice Fuzzy de Produtividade Agrícola e Conservação do Solo), em escala de 0 a 10.
#
# ESTRUTURA DO SCRIPT:
#  1. Instalação e carregamento de pacotes
#  2. Importação e filtragem dos dados
#  3. Normalização das variáveis (linear e invertida)
#  4. Construção do sistema fuzzy (definição de variáveis, funções de pertinência e regras)
#  5. Inferência fuzzy aplicada linha a linha aos dados normalizados
#  6. Cálculo do ISPC por grupo (Parcela x Cultura)
#  7. Geração de gráficos de dispersão e colunas (com erro-padrão)
#  8. Exportação dos resultados
#
# AUTOR: Diego (Professor Universitário / Pesquisador)
# ÚLTIMA ATUALIZAÇÃO: Julho de 2025
################################################################################
# 
# 
# ## INSTALAÇÃO E CARREGAMENTO DE PACOTES ----
# #install.packages(c("FuzzyR", "readxl", "ggplot2", "dplyr", "ggpattern", "plotly"))
# 
# 
library(FuzzyR)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpattern)
library(plotly)
# 
## LIMPEZA INICIAL -----
rm(list = ls())
graphics.off()

# ## FUNÇÕES DE NORMALIZAÇÃO ----
# Normaliza os valores para uma escala de 0 a 10 (quanto maior o valor original, maior o normalizado)
normalizar_conservador <- function(x) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(5, length(x))) # Retorna 5 se todos forem NA
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  # Se não houver variação, retorna 5 (valor neutro)
  if (abs(max_val - min_val) < .Machine$double.eps^0.5) return(rep(5, length(x)))
  x_norm <- 10 * (x - min_val) / (max_val - min_val)
  x_norm <- pmin(pmax(x_norm, 0), 10) # Garante que os valores fiquem entre 0 e 10
  x_norm[is.na(x_norm)] <- 5 # Substitui NAs restantes por 5
  return(x_norm)
}

# Normaliza os valores de forma invertida (quanto maior o valor original, menor o normalizado)
normalizar_invertido <- function(x) {
  x <- -1 * x
  return(normalizar_conservador(x))
}

# ## IMPORTAÇÃO E FILTRAGEM DOS DADOS ----
# # Certifique-se de que o arquivo "banco_dados.xlsx" está no seu diretório de trabalho
dados <- read_xlsx("banco_dados.xlsx", sheet = "dados_010")
cat("Colunas disponíveis:\n")
print(names(dados))
# 
# # Colunas necessárias para a análise
colunas_necessarias <- c(
  "DMG", "DMP", "RMP", "Densidade", "Estoque de C", "Na", "ICV(%)",
  "Altura de Plantas", "Diâmetro espiga", "Comprimento espiga",
  "Número de plantas_ha", "N total de espigas_ha", "N de espigas comerciais_ha",
  "Peso de espigas comerciais_ha", "Produtividade",
  "Cultura", "Parcela")
# 
# Remove linhas com valores NA nas colunas essenciais
dados <- dados[complete.cases(dados[, colunas_necessarias]), ]
# 
## SISTEMA FUZZY COM VARIÁVEIS SELECIONADAS ----
criar_sistema_fuzzy_reduzido <- function() {
  fis <- newfis("ISPC", defuzzMethod = "centroid")
  
  # Adiciona as 15 variáveis de entrada
  entradas <- c(
    "dmg",            # 1
    "dmp",            # 2
    "rmp",            # 3
    "densidade",      # 4
    "estoque_c",      # 5
    "na",             # 6
    "icv",            # 7
    "altura",         # 8
    "diam_espiga",    # 9
    "comp_espiga",    # 10
    "n_plantas",      # 11
    "n_espigas",      # 12
    "n_espigas_com",  # 13
    "peso_espigas",   # 14
    "produtividade"   # 15
  )
  
  for (var in entradas) {
    fis <- addvar(fis, "input", var, c(0, 10))
  }
  
  # Adiciona a variável de saída
  fis <- addvar(fis, "output", "ISPC", c(0, 10))
  
  # Funções de pertinência trianular (baixa, média, alta)
  params_baixa <- c(0, 0, 5)
  params_media <- c(0, 5, 10)
  params_alta  <- c(5, 10, 10)
  
  for (i in seq_along(entradas)) {
    fis <- addmf(fis, "input", i, "baixa", "trimf", params_baixa)
    fis <- addmf(fis, "input", i, "media", "trimf", params_media)
    fis <- addmf(fis, "input", i, "alta",  "trimf", params_alta)
  }
  
  # Pertinência para a saída
  fis <- addmf(fis, "output", 1, "baixa", "trimf", params_baixa)
  fis <- addmf(fis, "output", 1, "media", "trimf", params_media)
  fis <- addmf(fis, "output", 1, "alta",  "trimf", params_alta)
  
  
  
  # # Matriz de regras com foco em produção
   regras <- matrix(c(
  # DMG   DMP RMP   Dens EstC Na ICV Alt Diam Comp   N_Plantas N_Espigas N_Esp_Comerc  Peso_Espigas Prod  Saída Peso Conector
     1,    2,  3,    1,   1,  1,   1,   1,   1,   1,     1,       1,         1,            1,         1,    1,  0.8,    1,
     2,    2,  3,    2,   2,  2,   2,   2,   2,   2,     2,       2,         2,            2,         2,    1,  1.0,    1,
     3,    2,  1,    3,   3,  3,   3,   3,   3,   3,     3,       3,         3,            3,         3,    3,  1.2,    1,
     3,    2,  1,    2,   2,  2,   2,   3,   3,   3,     3,       3,         3,            3,         3,    3,  1.1,    1,
     3,    2,  2,    3,   1,  3,   1,   3,   3,   3,     3,       3,         3,            3,         3,    2,  0.8,    1,
     2,    2,  2,    3,   1,  3,   1,   2,   2,   2,     2,       2,         2,            2,         2,    2,  0.9,    1,
     2,    2,  2,    2,   3,  2,   2,   3,   3,   3,     3,       3,         3,            3,         3,    3,  1.2,    1,
     1,    2,  2,    1,   3,  3,   3,   3,   3,   3,     3,       3,         3,            3,         3,    3,  1.2,    1,
     3,    2,  2,    3,   1,  1,   1,   1,   3,   3,     3,       3,         3,            3,         3,    2,  0.8,    1,
     2,    2,  2,    2,   3,  3,   3,   3,   3,   3,     3,       3,         3,            3,         3,    3,  1.2,    1,
     2,    2,  2,    2,   1,  1,   1,   1,   2,   2,     2,       2,         2,            2,         2,    1,  0.8,    1,
     2,    2,  2,    1,   3,  2,   2,   3,   2,   2,     2,       2,         2,            2,         2,    3,  1.2,    1,
     1,    2,  2,    3,   2,  3,   2,   2,   2,   2,     2,       2,         2,            2,         2,    3,  1.2,    1,
     2,    2,  3,    2,   2,  3,   2,   2,   2,   2,     2,       2,         2,            2,         2,    1,  1.2,    1,
     2,    2,  2,    2,   2,  1,   2,   2,   2,   2,     2,       2,         2,            2,         2,    2,  1.2,    1,
     1, 	 2,	 3,	   3,	  1,	1,   1,	  3,	 3,	  3,	   3,	      3,	       3,	           3,	        3,	  1,	0.7,    1,
     2,	   2,	 1,	   1,	  3,	2,	 3,	  1,	 1,	  1,	   1,	      1,	       1,	           1,	        1,	  2,	0.9,    1,
     3,	   2,	 3,	   3,	  1,	3,	 1,	  1,   1,	  1,	   1,	      1,	       1,	           1,	        1,	  1,	0.8,    1,
     1,	   2,	 2,	   2,	  3,	3,	 3,	  2,   2,	  2,	   2,	      2,	       2,	           2,	        2,	  3,	1.0,    1,
     2,	   2,	 3,	   3,	  2,	2,	 2,	  2,	 2,	  2,	   2,	      2,	       2,	           2,	        2,    1,	0.8,    1,
     3,	   2,	 1,	   1,	  2,	1,   2,	  3,   3,	  3,	   3,      	3,	       3,	           3,	        3,	  2,	0.9,    1
     
   ), ncol = 18, byrow = TRUE)
   
  

  # O ajuste agora usa a coluna 15 (Produtividade).
for (i in 1:nrow(regras)) {
    prod <- regras[i, 15]         # Produtividade
    estc <- regras[i, 5]          # Estoque de C
    dens <- regras[i, 4]          # Densidade do solo
    icvp <- regras[i, 7]          # ICV%
    
    peso <- 1.0  # valor base neutro
    
    if (!is.na(prod) && prod == 3) peso <- peso + 0.5
    if (!is.na(estc) && estc == 3) peso <- peso + 1.0
    if (!is.na(icvp) && icvp == 3) peso <- peso + 0.5
    if (!is.na(prod) && prod == 1) peso <- peso - 0.5
    if (!is.na(dens) && dens == 1) peso <- peso - 1.0 #valor inverso
    if (!is.na(estc) && estc == 1) peso <- peso - 0.5
    if (!is.na(icvp) && icvp == 1) peso <- peso - 0.5
    if (!is.na(dens) && dens == 3) peso <- peso + 0.5 #valor inverso
    
    
    # Limita o peso entre 0.6 e 1.5 para evitar distorções extremas
    regras[i, 17] <- min(max(peso, 0.6), 1.5)
  }

  
  fis <- addrule(fis, regras)
  return(fis)
}


## CRIAÇÃO DO SISTEMA FUZZY ----
fis <- criar_sistema_fuzzy_reduzido()

## NORMALIZAÇÃO GLOBAL ANTES DA DIVISÃO
cat("\n--- NORMALIZANDO DADOS DE FORMA GLOBAL ---\n")
dados_norm <- dados %>%
  mutate(
    dmg_norm = normalizar_conservador(`DMG`),
    dmp_norm = normalizar_conservador(`DMP`),
    rmp_norm = normalizar_invertido(`RMP`),
    densidade_norm = normalizar_invertido(`Densidade`),
    estoque_c_norm = normalizar_conservador(`Estoque de C`),
    na_norm = normalizar_invertido(`Na`),
    icv_norm = normalizar_conservador(`ICV(%)`),
    altura_norm = normalizar_conservador(`Altura de Plantas`),
    diam_espiga_norm = normalizar_conservador(`Diâmetro espiga`),
    comp_espiga_norm = normalizar_conservador(`Comprimento espiga`),
    peso_espigas_norm = normalizar_conservador(`Peso de espigas comerciais_ha`),
    produtividade_norm = normalizar_conservador(`Produtividade`),
    n_plantas_norm = normalizar_conservador(`Número de plantas_ha`),
    n_espigas_norm = normalizar_conservador(`N total de espigas_ha`),
    n_espigas_com_norm = normalizar_conservador(`N de espigas comerciais_ha`)
  )

## DIVISÃO DOS DADOS JÁ NORMALIZADOS ----
dados_por_topo <- split(dados_norm, interaction(dados_norm$Parcela, dados_norm$Cultura, drop = TRUE))


## APLICAÇÃO DO SISTEMA A TODOS OS DADOS POR GRUPO ----
cat("--- PROCESSANDO DADOS COMPLETOS POR GRUPO ---\n")
resultados_topo <- lapply(dados_por_topo, function(sub) {
  tryCatch({
  
  #SELEÇÃO DAS VARIÁVEIS DE ENTRADA ----
  
    input_cols <- c(
      "dmg_norm", "dmp_norm", "rmp_norm", "densidade_norm",
      "estoque_c_norm", "na_norm", "icv_norm", "altura_norm",
      "diam_espiga_norm", "comp_espiga_norm", "n_plantas_norm",
      "n_espigas_norm", "n_espigas_com_norm", "peso_espigas_norm",
      "produtividade_norm"
    )
    
    # Garante a ordem correta das colunas
    input_data <- as.matrix(sub[, input_cols])
    
    # Aplica a inferência fuzzy linha por linha
    sub$ISPC <- apply(input_data, 1, function(l) evalfis(matrix(l, nrow = 1), fis))
    return(sub)
    
  }, error = function(e) {
    # Mensagem de erro mais informativa
    message("Erro no grupo: ", unique(sub$Parcela), "-", unique(sub$Cultura), ": ", e$message)
    sub$ISPC <- NA
    return(sub)
  })
})

## CONSOLIDAÇÃO DOS RESULTADOS ----
dados_final <- bind_rows(resultados_topo) %>% filter(!is.na(ISPC))




## VISUALIZAÇÃO E EXPORTAÇÃO ----
if (nrow(dados_final) > 0) {
  # Cria uma coluna de interação para o gráfico
  # Ordenação personalizada dos níveis
  ordem_desejada <- c("CC", "CM", "PD")
  # Garante que a variável 'Parcela' esteja com os níveis corretos
  dados_final$Parcela <- factor(trimws(dados_final$Parcela), levels = ordem_desejada)
  
  # Recria a variável Parcela_Cultura com a nova ordenação
  dados_final <- dados_final %>%
    mutate(Parcela_Cultura = interaction(Parcela, trimws(Cultura), drop = TRUE)) %>%
    mutate(Parcela_Cultura = factor(Parcela_Cultura, levels = unique(Parcela_Cultura[order(Parcela)])))
  
  cat("\n--- RESUMO DOS RESULTADOS FINAIS (ISPC) ---\n")
  print(summary(dados_final$ISPC))
  
  # Gera o gráfico de boxplot
  p <- ggplot(dados_final, aes(x = Parcela_Cultura, y = ISPC, fill = Cultura)) +
    geom_boxplot(outlier.shape = NA, na.rm = TRUE) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, na.rm = TRUE) +
    labs(title = "ISPC por Parcela / Cultura", x = "", y = "Índice ISPC") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(hjust = 0.5))
  
  print(p)
  
  # Salva os resultados em um arquivo CSV
  write.csv(dados_final, "ISPC_resultados_reduzido.csv", row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nArquivo salvo com sucesso: ISPC_resultados_reduzido.csv\n")
  
} else {
  cat("Erro: Nenhum dado válido foi gerado após o processamento.\n")
}





#########################################
## GRÁFICO COLUNAS
#########################################


# Cria resumo estatístico do ISPC por Parcela e Cultura
resumo_ISPC <- dados_final %>%
  group_by(Parcela, Cultura) %>%
  summarise(
    media = mean(ISPC, na.rm = TRUE),
    se = sd(ISPC, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )



# Organiza ordem das parcelas (no eixo X agora)
resumo_ISPC$Parcela <- factor(resumo_ISPC$Parcela, levels = c("CC", "CM", "PD"))

# Culturas e padrões
culturas_unicas <- unique(resumo_ISPC$Cultura)
padroes_disponiveis <- c("none", "stripe", "crosshatch", "circle", "wave", "grid", "dot")

if (length(culturas_unicas) > length(padroes_disponiveis)) {
  stop("Adicione mais padrões ao vetor 'padroes_disponiveis'.")
}

padroes_cultura <- setNames(padroes_disponiveis[1:length(culturas_unicas)], culturas_unicas)

# Gráfico de colunas verticais com padrões
ggplot(resumo_ISPC, aes(x = Parcela, y = media,
                         fill = Cultura,
                         pattern = Cultura)) +
  geom_col_pattern(
    color = "black",
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.5,
    pattern_angle = 45,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.6,
    position = position_dodge(width = 0.8),
    width = 0.6
  ) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_brewer(palette = "BuPu", name = "Cultura") +
  scale_pattern_manual(values = padroes_cultura, name = "Cultura") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    x = "Parcela",
    y = "Índice ISPC (média ± EP)",
    title = "Média do ISPC por Parcela e Cultura"
  ) +
  theme_light(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray40")
  )




# #####################
# # GRAFICO DE PERTINENCIA (CORRIGIR)
# #####################
# 
# # Corrigir o número de entradas (15 variáveis)
# variaveis_constantes <- rep(5, 15)
# 
# # Define os índices das variáveis a variar
# idx_produtividade <- 15  # Produtividade
# idx_densidade <- 4       # Densidade

# # Valores normalizados para o grid
# x_vals <- seq(0, 10, by = 0.5)  # Produtividade
# y_vals <- seq(0, 10, by = 0.5)  # Densidade
# 
# # Inicializa a matriz de saída
# z_matrix <- matrix(NA, nrow = length(x_vals), ncol = length(y_vals))
# 
# # Preenche a matriz com os resultados fuzzy
# for (i in seq_along(x_vals)) {
#   for (j in seq_along(y_vals)) {
#     entrada <- variaveis_constantes
#     entrada[idx_produtividade] <- x_vals[i]
#     entrada[idx_densidade] <- y_vals[j]
#     z_matrix[i, j] <- evalfis(matrix(entrada, nrow = 1), fis)
#   }
# }
# 
# # Converte para dataframe para plotagem
# plot_data <- expand.grid(Produtividade = x_vals, Densidade = y_vals)
# plot_data$ISPC <- as.vector(z_matrix)
# 
# # Gráfico interativo com plotly
# library(plotly)
# 
# fig <- plot_ly(
#   x = ~plot_data$Produtividade,
#   y = ~plot_data$Densidade,
#   z = ~plot_data$ISPC,
#   type = "surface",
#   colorscale = "Viridis"
# ) %>%
#   layout(
#     title = "Superfície Fuzzy: Produtividade × Densidade → ISPC",
#     scene = list(
#       xaxis = list(title = "Produtividade (normalizado)"),
#       yaxis = list(title = "Densidade (normalizado)"),
#       zaxis = list(title = "Índice ISPC")
#     )
#   )
# 
# fig


