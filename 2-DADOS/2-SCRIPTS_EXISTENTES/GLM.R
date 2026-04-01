# ------------------------------------------------------------------
# Script GLM + Médias + Post-hoc para variáveis do índice Fuzzy
# (Versão com gráficos em tons pastel e hachuras aprimoradas)
# ------------------------------------------------------------------

# Pacotes necessários
# Certifique-se de que todos os pacotes estão instalados executando a linha abaixo, se necessário:
# install.packages(c("readxl", "dplyr", "ggplot2", "car", "emmeans", "multcomp", "multcompView", "openxlsx", "ggpattern", "RColorBrewer"))

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(openxlsx)
library(ggpattern)
library(RColorBrewer)


## 1) LIMPEZA DO AMBIENTE ----
rm(list = ls())
graphics.off()


# ------------------------------------------------------------------
# Importar dados
# ------------------------------------------------------------------
# Substitua "DADOS_GLM.xlsx" pelo caminho correto do seu arquivo
dados <- read_excel("DADOS_GLM.xlsx")

# Boa prática: garantir que as variáveis categóricas sejam fatores
dados <- dados %>%
  mutate(Manejo = as.factor(Manejo),
         Cultura = as.factor(Cultura))

# ------------------------------------------------------------------
# Variáveis resposta
# ------------------------------------------------------------------
variaveis <- c("Produtividade_norm", "ARQ", "COS", "STAND")

# Listas para armazenar os resultados
resultados <- list()
tabelas_finais <- list()

# --- Definições de Estilo para os Gráficos (fora do loop) ---
# Isso garante consistência de cores e padrões em todos os gráficos.

# Contar o número de culturas para tornar o script robusto
n_culturas <- length(unique(dados$Cultura))
culturas_unicas <- unique(dados$Cultura)

# 1. Paleta de cores em tons pastel
# brewer.pal() precisa de pelo menos 3 cores. Este código agora lida com casos com menos de 3 culturas.
if (n_culturas < 3) {
  cores_base <- c("#FBB4AE", "#B3CDE3") # Cores de "Pastel1" para 1 ou 2 categorias
  cores_pastel <- cores_base[1:n_culturas]
} else {
  cores_pastel <- brewer.pal(n = n_culturas, name = "Pastel1")
}
names(cores_pastel) <- culturas_unicas

# 2. Padrões de hachura (um para cada cultura)
# CORREÇÃO: Cria dinamicamente a lista de padrões para corresponder ao número de culturas.
lista_de_padroes <- c("stripe", "crosshatch", "circle", "wave", "grid", "hexagon", "right45", "left45") 
if (n_culturas > length(lista_de_padroes)) {
  warning(paste("Aviso: O número de culturas (", n_culturas, ") excede o número de padrões de hachura únicos (", length(lista_de_padroes), "). Os padrões serão repetidos.", sep=""))
  padroes_hachura <- rep(lista_de_padroes, length.out = n_culturas)
} else {
  padroes_hachura <- lista_de_padroes[1:n_culturas]
}
names(padroes_hachura) <- culturas_unicas


# ------------------------------------------------------------------
# Loop para análise e geração de gráficos
# ------------------------------------------------------------------
for (var in variaveis) {
  
  cat("\n\n#############################\n")
  cat("Analisando:", var, "\n")
  cat("#############################\n")
  
  # Modelo GLM
  formula_modelo <- as.formula(paste(var, "~ Manejo * Cultura"))
  modelo <- lm(formula_modelo, data = dados)
  print(Anova(modelo, type = "II"))
  
  # Médias e post-hoc
  emm <- emmeans(modelo, ~ Manejo * Cultura)
  contrastes <- contrast(emm, method = "pairwise", adjust = "tukey")
  print(contrastes)
  
  # Letras de agrupamento ('a' = maior média)
  cld_res <- multcomp::cld(emm, Letters = letters, adjust = "tukey", rev = TRUE)
  print(cld_res)
  
  resultados[[var]] <- list(modelo = modelo, emm = emm, cld = cld_res)
  
  # Tabela organizada para o gráfico
  tab <- as.data.frame(cld_res) %>%
    rename(
      Media = emmean,
      Erro = SE,
      Grupo = .group
    ) %>%
    mutate(
      Variavel = var,
      # Remove espaços em branco do grupo de letras para melhor alinhamento
      Grupo = trimws(Grupo)
    )
  
  tabelas_finais[[var]] <- tab
  
  # --- Geração do Gráfico ---
  # Adiciona uma verificação para garantir que há dados para plotar
  if (nrow(tab) > 0) {
    g <- ggplot(tab, aes(x = Manejo, y = Media, fill = Cultura, pattern = Cultura)) +
      geom_bar_pattern(
        stat = "identity",
        position = position_dodge(width = 0.9),
        colour = "black",           # Cor da borda da barra (preto)
        pattern_fill = "gray20",    # Cor da hachura (cinza escuro para suavizar)
        pattern_density = 0.1,      # Densidade da hachura (0 a 1)
        pattern_spacing = 0.02,     # Espaçamento entre as linhas da hachura
        pattern_key_scale_factor = 0.6 # Tamanho do padrão na legenda
      ) +
      geom_errorbar(
        aes(ymin = Media - Erro, ymax = Media + Erro),
        width = 0.25,
        position = position_dodge(width = 0.9),
        linewidth = 0.5
      ) +
      geom_text(
        # Posiciona o texto acima da barra de erro para evitar sobreposição
        aes(y = Media + Erro, label = Grupo), 
        position = position_dodge(width = 0.9),
        vjust = -0.5, # Ajuste vertical para afastar o texto
        size = 4,
        color = "black"
      ) +
      labs(
        title = paste("Médias ajustadas para", var),
        #subtitle = "Letras diferentes indicam diferença estatística (Tukey p<0.05)",
        y = var,
        x = "Manejo do Solo"
      ) +
      scale_fill_manual(values = cores_pastel) +
      scale_pattern_manual(values = padroes_hachura) +
      theme_classic(base_size = 14) + # Um tema limpo, ideal para publicações
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text = element_text(color = "black"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
    
    # Imprime o gráfico no painel de plotagem do RStudio
    print(g)
  } else {
    cat(paste("\nAVISO: Não foi possível gerar o gráfico para '", var, "' pois não há dados para plotar.\n", sep=""))
  }
  
  # Opcional: Salvar cada gráfico em um arquivo PNG de alta qualidade
  # ggsave(paste0("Grafico_", var, ".png"), plot = g, width = 8, height = 6, dpi = 300)
}

# ------------------------------------------------------------------
# Exportar resultados combinados para Excel
# ------------------------------------------------------------------
tabela_final <- bind_rows(tabelas_finais)
write.xlsx(tabela_final, "Resultados_GLM_PostHoc.xlsx", row.names = FALSE)

cat("\nArquivo 'Resultados_GLM_PostHoc.xlsx' salvo com sucesso!\n")

