# ------------------------------------------------------------------
# SCRIPT PARA GERAR FIGURAS E TABELAS DA REVISÃO (AGRONOMY JOURNAL)
# Data: 17/12/2025
# Autor: Copilot (Assistente)
# ------------------------------------------------------------------

# 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES ----
pacotes <- c("readxl", "dplyr", "ggplot2", "corrplot", "tidyr", "openxlsx", "ggpubr", "FuzzyR")

for(p in pacotes) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# 2. CARREGAMENTO DOS DADOS ----
# Tenta carregar o arquivo de dados brutos ou o arquivo já processado
if(file.exists("banco_dados.xlsx")) {
  dados <- read_excel("banco_dados.xlsx", sheet = "dados_010")
} else if(file.exists("2 - DADOS/banco_dados.xlsx")) {
  dados <- read_excel("2 - DADOS/banco_dados.xlsx", sheet = "dados_010")
} else {
  # Se não achar o excel, tenta o CSV processado se existir
  if(file.exists("IPACSA_resultados_completos.csv")) {
    dados_proc <- read.csv("IPACSA_resultados_completos.csv")
    cat("Usando dados processados (IPACSA_resultados_completos.csv)...\n")
  } else {
    stop("ERRO: Arquivo 'banco_dados.xlsx' não encontrado. Verifique o diretório de trabalho.")
  }
}

# Se carregou o Excel, precisa reprocessar o Fuzzy (usando o script de validação existente)
# Para simplificar, vamos assumir que o usuário vai rodar o 'Validacao_APSWCI.R' que é mais completo.
# Este script aqui foca apenas em GERAR OS GRÁFICOS FINAIS a partir dos dados processados.

if(!exists("dados_proc")) {
  # Se não carregou o CSV, vamos rodar o script de validação para gerar os dados
  if(file.exists("Validacao_APSWCI.R")) {
    source("Validacao_APSWCI.R")
    # O script Validacao_APSWCI.R gera o objeto 'dados_norm'
    dados_proc <- dados_norm
  } else {
    stop("Script 'Validacao_APSWCI.R' não encontrado para processar os dados.")
  }
}

# Diretório de saída das figuras (mesmo usado no manuscrito)
fig_dir <- file.path("..", "FIGURAS_USUAIS")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# 3. GERAR FIGURA: SCATTER PLOT (ADDED VALUE) ----
# Gráfico de dispersão APSWCI vs Produtividade, colorido por Sistema de Preparo
cat("Gerando Scatter Plot (APSWCI vs Yield)...\n")

p_scatter <- ggplot(dados_proc, aes(x = Produtividade_norm, y = APSWCI_Original, color = Parcela)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group = 1), color = "black", alpha = 0.5) +
  # Adicionar linhas de quadrantes (média 5,5)
  geom_vline(xintercept = 5, linetype = "dotted", color = "gray50") +
  geom_hline(yintercept = 5, linetype = "dotted", color = "gray50") +
  labs(
    title = "Added Value Evaluation: APSWCI vs. Grain Yield",
    subtitle = "Comparison showing how APSWCI differentiates treatments beyond yield alone",
    x = "Normalized Grain Yield (0-10)",
    y = "APSWCI (0-10)",
    color = "Tillage System"
  ) +
  theme_pubr(base_size = 16) + # Aumenta fontes para publicação
  scale_color_brewer(palette = "Dark2") +
  annotate("text", x = 9, y = 2, label = "High Yield\nLow Sustainability", color = "red", size = 5) +
  annotate("text", x = 2, y = 8, label = "Low Yield\nHigh Conservation", color = "blue", size = 5) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Scatter_AddedValue.png"), p_scatter, width = 8, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figura_Revisao_Scatter_AddedValue.pdf"), p_scatter, width = 8, height = 6)


# 4. GERAR TABELA: CORRELAÇÃO DE SPEARMAN ----
cat("Gerando Tabela de Correlação...\n")

# Selecionar variáveis para correlação
vars_corr <- dados_proc %>%
  select(
    APSWCI = APSWCI_Original,
    Yield = Produtividade_norm,
    SC = COS, # Soil Conservation
    MrP = ARQ, # Morphology
    STAND = STAND
  )

# Calcular correlação de Spearman
cor_spearman <- cor(vars_corr, method = "spearman", use = "pairwise.complete.obs")

# Salvar em CSV
write.csv(round(cor_spearman, 3), "Tabela_Revisao_Correlacao_Spearman.csv")

# Gerar gráfico da matriz de correlação (opcional, para visualização)
png("Figura_Revisao_Matriz_Correlacao.png", width = 2000, height = 2000, res = 300)
corrplot.mixed(cor_spearman, lower = "number", upper = "circle", tl.col = "black")
dev.off()


# 5. GERAR FIGURA: ABLATION STUDY (RANKING) ----
cat("Gerando Gráfico de Ablation Study...\n")

# Calcular rankings
dados_proc$Rank_Full <- rank(dados_proc$APSWCI_Original)
dados_proc$Rank_NoYield <- rank(dados_proc$APSWCI_NoYield)
dados_proc$Rank_NoSC <- rank(dados_proc$APSWCI_NoCOS)

# Preparar dados para plotagem (formato longo)
dados_ablation <- dados_proc %>%
  select(Parcela, Rank_Full, Rank_NoYield, Rank_NoSC) %>%
  pivot_longer(cols = c(Rank_NoYield, Rank_NoSC), names_to = "Model", values_to = "Rank_Ablated") %>%
  mutate(Model = recode(Model, 
                        "Rank_NoYield" = "Without Yield",
                        "Rank_NoSC" = "Without Soil Conservation"))

p_ablation <- ggplot(dados_ablation, aes(x = Rank_Full, y = Rank_Ablated, color = Parcela)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Model) +
  labs(
    title = "Sensitivity Analysis (Ablation Study)",
    subtitle = "Impact of removing variables on the final ranking of plots",
    x = "Rank (Full APSWCI)",
    y = "Rank (Ablated Index)",
    color = "Tillage System"
  ) +
  theme_bw(base_size = 16) +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Ablation.png"), p_ablation, width = 10, height = 5, dpi = 600)
ggsave(file.path(fig_dir, "Figura_Revisao_Ablation.pdf"), p_ablation, width = 10, height = 5)


# 6. DIAGNÓSTICO SUNN HEMP (CAIXA PRETA) ----
cat("Gerando Diagnóstico Sunn Hemp (CT vs NT)...\n")

# Filtrar apenas Sunn Hemp
sunn_hemp_data <- dados_proc %>%
  filter(Cultura == "Sunn hemp" | Cultura == "Crotalaria") %>% # Ajuste conforme nome exato
  group_by(Parcela) %>%
  summarise(
    Mean_APSWCI = mean(APSWCI_Original, na.rm = TRUE),
    Mean_Yield = mean(Produtividade_norm, na.rm = TRUE),
    Mean_SC = mean(COS, na.rm = TRUE),
    Mean_MrP = mean(ARQ, na.rm = TRUE),
    Mean_STAND = mean(STAND, na.rm = TRUE)
  )

print(sunn_hemp_data)
write.csv(sunn_hemp_data, "Tabela_Diagnostico_SunnHemp.csv")

cat("\n--------------------------------------------------------\n")
cat("SUCESSO! Todos os arquivos foram gerados na pasta atual.\n")
cat("Verifique os arquivos PNG, PDF e CSV gerados.\n")
cat("--------------------------------------------------------\n")
