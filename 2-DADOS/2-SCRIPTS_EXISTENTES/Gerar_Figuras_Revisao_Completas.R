# ------------------------------------------------------------------
# SCRIPT PARA GERAR FIGURAS E TABELAS DA REVISÃO (AGRONOMY JOURNAL)
# VERSÃO ATUALIZADA: INCLUI PROFUNDIDADES 0-10 e 10-20 cm
# Data: 17/12/2025
# ------------------------------------------------------------------

# 1. SETUP E PACOTES ----
req_pkgs <- c("readxl", "dplyr", "FuzzyR", "ggplot2", "corrplot", "tidyr", "openxlsx", "ggpubr", "ggpattern", "RColorBrewer")
for(p in req_pkgs) {
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

rm(list = ls())
graphics.off()

# 2. CARREGAR DADOS (AMBAS AS PROFUNDIDADES) ----
if(file.exists("banco_dados.xlsx")) {
  path_db <- "banco_dados.xlsx"
} else if(file.exists("2 - DADOS/banco_dados.xlsx")) {
  path_db <- "2 - DADOS/banco_dados.xlsx"
} else {
  stop("Arquivo banco_dados.xlsx não encontrado.")
}

# Carregar 0-10 cm
dados_010 <- read_excel(path_db, sheet = "dados_010")
dados_010$Profundidade <- "0-10 cm"

# Carregar 10-20 cm
dados_1020 <- read_excel(path_db, sheet = "dados_1020")
dados_1020$Profundidade <- "10-20 cm"

# Combinar os dados
colunas_comuns <- intersect(names(dados_010), names(dados_1020))
dados <- rbind(
  dados_010[, colunas_comuns],
  dados_1020[, colunas_comuns]
)

# Colunas necessárias para o índice
colunas_necessarias <- c(
  "DMG","DMP","RMP","Densidade","Estoque de C","Na","ICV(%)",
  "Altura de Plantas","Diâmetro espiga","Comprimento espiga",
  "Número de plantas_ha","N total de espigas_ha","N de espigas comerciais_ha",
  "Peso de espigas comerciais_ha","Produtividade","Cultura","Parcela", "Profundidade"
)

# Filtrar colunas e remover NAs
dados <- dados[, colunas_necessarias]
dados <- dados[complete.cases(dados), ]

# 3. FUNÇÕES AUXILIARES E NORMALIZAÇÃO ----

direcao <- c(
  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,
  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,
  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,
  `Peso de espigas comerciais_ha`=1, Produtividade=1
)

normalizar_robusto <- function(x, maior_melhor = TRUE) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(5, length(x)))
  q <- quantile(x, probs = c(.05, .95), na.rm = TRUE, type = 8)
  if (diff(q) == 0) return(rep(5, length(x)))
  x2 <- (x - q[1]) / (q[2] - q[1])
  x2 <- pmin(pmax(x2, 0), 1) * 10
  if (!maior_melhor) x2 <- 10 - x2
  x2[is.na(x2)] <- 5
  x2
}

# Normalizar o conjunto combinado (escala unificada para ambas profundidades)
dados_norm <- dados %>%
  mutate(across(names(direcao), 
                ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),
                .names = "{.col}_norm"))

add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {
  x <- x[is.finite(x)]
  qs <- quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)
  L <- -2; U <- 12 
  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))
  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))
  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))
  fis
}

# 4. DEFINIÇÃO DOS SISTEMAS FUZZY ----

criar_fis_COS <- function(dn) {
  fis <- newfis("COS", defuzzMethod = "centroid")
  in_vars <- c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "COS", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  
  regras_orig <- rbind(c(2,2,1,1,1,3,3, 3, 1,1), c(2,2,2,2,2,2,2, 2, 1,1), c(1,1,3,3,3,1,1, 1, 1,1))
  
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

criar_fis_ARQ <- function(dn) {
  fis <- newfis("ARQ", defuzzMethod = "centroid")
  in_vars <- c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",
               "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "ARQ", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  
  regras_orig <- rbind(c(3,3,3,3,3,3, 3, 1,1), c(2,2,2,2,2,2, 2, 1,1), c(1,1,1,1,1,1, 1, 1,1))
  
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

criar_fis_STAND <- function(dn) {
  fis <- newfis("STAND", defuzzMethod = "centroid")
  in_var <- "Número de plantas_ha_norm"
  fis <- addvar(fis, "input", in_var, c(0,10))
  fis <- add_tri_from_quartis(fis, 1, dn[[in_var]])
  fis <- addvar(fis, "output", "STAND", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(3, 3, 1,1), c(2, 2, 1,1), c(1, 1, 1,1))
  addrule(fis, regras)
}

criar_fis_FINAL <- function() {
  fis <- newfis("IPACSA_final", defuzzMethod = "centroid")
  in_vars <- c("COS","ARQ","STAND","Produtividade_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- addmf(fis, "input", i, "baixa", "trimf", c(0,2.5,5))
    fis <- addmf(fis, "input", i, "media", "trimf", c(2.5,5,7.5))
    fis <- addmf(fis, "input", i, "alta",  "trimf", c(5,7.5,10))
  }
  fis <- addvar(fis, "output", "IPACSA", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  
  regras_orig <- rbind(
    c(2,3,2,3, 3, 1.2, 1),
    c(2,2,2,2, 2, 1.0, 1),
    c(1,1,1,1, 1, 0.8, 1),
    c(3,2,2,2, 3, 1.1, 1),
    c(2,3,2,3, 3, 1.1, 1),
    c(2,2,3,2, 3, 1.1, 1),
    c(2,3,2,2, 3, 1.15,1)
  )
  
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

# 5. CÁLCULO DOS ÍNDICES ----
cat("Calculando subíndices Fuzzy...\n")
fis_COS <- criar_fis_COS(dados_norm)
fis_ARQ <- criar_fis_ARQ(dados_norm)
fis_STAND <- criar_fis_STAND(dados_norm)

Xcos <- as.matrix(dados_norm[, c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")])
Xarq <- as.matrix(dados_norm[, c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",
                                 "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")])
Xstd <- as.matrix(dados_norm[, "Número de plantas_ha_norm", drop=FALSE])

dados_norm$COS <- apply(Xcos, 1, function(l) evalfis(matrix(l,1), fis_COS))
dados_norm$ARQ <- apply(Xarq, 1, function(l) evalfis(matrix(l,1), fis_ARQ))
dados_norm$STAND <- apply(Xstd, 1, function(l) evalfis(matrix(l,1), fis_STAND))

cat("Calculando APSWCI Final...\n")
fis_FINAL <- criar_fis_FINAL()
Xfinal <- as.matrix(dados_norm[, c("COS","ARQ","STAND","Produtividade_norm")])
dados_norm$APSWCI_Original <- apply(Xfinal, 1, function(l) evalfis(matrix(l,1), fis_FINAL))

# 6. CÁLCULO PARA ABLATION STUDY (RANKING) ----
# Necessário para o gráfico de Ablation
criar_fis_NO_YIELD <- function() {
  fis <- newfis("IPACSA_NoYield", defuzzMethod = "centroid")
  in_vars <- c("COS","ARQ","STAND")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- addmf(fis, "input", i, "baixa", "trimf", c(0,2.5,5))
    fis <- addmf(fis, "input", i, "media", "trimf", c(2.5,5,7.5))
    fis <- addmf(fis, "input", i, "alta",  "trimf", c(5,7.5,10))
  }
  fis <- addvar(fis, "output", "IPACSA", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  
  regras_orig <- rbind(
    c(2,3,2, 3, 1.2, 1),
    c(2,2,2, 2, 1.0, 1),
    c(1,1,1, 1, 0.8, 1),
    c(3,2,2, 3, 1.1, 1),
    c(2,3,2, 3, 1.1, 1),
    c(2,2,3, 3, 1.1, 1),
    c(2,3,2, 3, 1.15,1)
  )
  
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

fis_NO_YIELD <- criar_fis_NO_YIELD()
Xfinal_noyield <- as.matrix(dados_norm[, c("COS","ARQ","STAND")])
dados_norm$APSWCI_NoYield <- apply(Xfinal_noyield, 1, function(l) evalfis(matrix(l,1), fis_NO_YIELD))

# 6.2 APSWCI Sem Conservação (Removendo COS)
criar_fis_NO_COS <- function() {
  fis <- newfis("IPACSA_NoCOS", defuzzMethod = "centroid")
  in_vars <- c("ARQ","STAND","Produtividade_norm") # Sem COS
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- addmf(fis, "input", i, "baixa", "trimf", c(0,2.5,5))
    fis <- addmf(fis, "input", i, "media", "trimf", c(2.5,5,7.5))
    fis <- addmf(fis, "input", i, "alta",  "trimf", c(5,7.5,10))
  }
  fis <- addvar(fis, "output", "IPACSA", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  
  # Regras Adaptadas (Removendo a 1ª coluna do antecedente - COS)
  regras_orig <- rbind(
    c(3,2,3, 3, 1.2, 1),
    c(2,2,2, 2, 1.0, 1),
    c(1,1,1, 1, 0.8, 1),
    c(2,2,2, 3, 1.1, 1),
    c(3,2,3, 3, 1.1, 1),
    c(2,3,2, 3, 1.1, 1),
    c(3,2,2, 3, 1.15,1)
  )
  
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  
  addrule(fis, rbind(regras_orig, regras_fill))
}

fis_NO_COS <- criar_fis_NO_COS()
Xfinal_nocos <- as.matrix(dados_norm[, c("ARQ","STAND","Produtividade_norm")])
dados_norm$APSWCI_NoCOS <- apply(Xfinal_nocos, 1, function(l) evalfis(matrix(l,1), fis_NO_COS))

dados_norm$Rank_Full <- rank(dados_norm$APSWCI_Original)
dados_norm$Rank_NoYield <- rank(dados_norm$APSWCI_NoYield)
dados_norm$Rank_NoSC <- rank(dados_norm$APSWCI_NoCOS)

# 7. GERAR FIGURAS ATUALIZADAS ----

# 7.1 Scatter Plot (Added Value) - COM FACET POR PROFUNDIDADE
cat("Gerando Scatter Plot (Added Value) com Profundidades...\n")

# Diretório de saída das figuras (mesmo usado no manuscrito)
fig_dir <- file.path("..", "FIGURAS_USUAIS")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

p_scatter <- ggplot(dados_norm, aes(x = Produtividade_norm, y = APSWCI_Original, color = Parcela)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(fill = Parcela), alpha = 0.5) + # Regressão por Sistema
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5.5) + # Stats por Sistema (enlarged R2 proof)
  geom_vline(xintercept = 5, linetype = "dotted", color = "gray50") +
  geom_hline(yintercept = 5, linetype = "dotted", color = "gray50") +
  facet_wrap(~Profundidade) + # AQUI ESTÁ A MUDANÇA PRINCIPAL
  labs(
    x = "Normalized Grain Yield (0-10)",
    y = "APSWCI (0-10)",
    color = "Tillage System"
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_bw(base_size = 20) + # Enlarged for editor revision (R2 proof)
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey85", color = "black"),
    strip.text = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Scatter_AddedValue.png"), p_scatter, width = 12, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figura_Revisao_Scatter_AddedValue.pdf"), p_scatter, width = 12, height = 6)

# 7.2 Ablation Study (Ranking) - Atualizado com dados completos
cat("Gerando Gráfico de Ablation Study (Dados Completos)...\n")

dados_ablation <- dados_norm %>%
  select(Parcela, Profundidade, Rank_Full, Rank_NoYield, Rank_NoSC) %>%
  pivot_longer(cols = c(Rank_NoYield, Rank_NoSC), names_to = "Model", values_to = "Rank_Ablated") %>%
  mutate(Model = recode(Model, 
                        "Rank_NoYield" = "Without Yield",
                        "Rank_NoSC" = "Without Soil Conservation"))

p_ablation <- ggplot(dados_ablation, aes(x = Rank_Full, y = Rank_Ablated, color = Parcela)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5.5) + # Stats por Sistema (enlarged R2 proof)
  facet_grid(Profundidade ~ Model) + # Grid 2x2: Linhas=Profundidade, Colunas=Modelo
  labs(
    x = "Rank (Full APSWCI)",
    y = "Rank (Ablated Index)",
    color = "Tillage System"
  ) +
  theme_bw(base_size = 20) + # Enlarged for editor revision (R2 proof)
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey85", color = "black"),
    strip.text = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Ablation.png"), p_ablation, width = 12, height = 7, dpi = 600)
ggsave(file.path(fig_dir, "Figura_Revisao_Ablation.pdf"), p_ablation, width = 12, height = 7)

# 7.4 Boxplot APSWCI (Figura 4) - Estilo Facetado com Rachuras (Patterns)
cat("Gerando Boxplot APSWCI com Facet e Rachuras (Figura 4)...\n")

# Reordenar fatores para o gráfico
dados_norm$Parcela <- factor(dados_norm$Parcela, levels = c("CT", "MT", "NT"))

# Definição de Cores e Padrões (Estilo Antigo)
unique_crops <- unique(dados_norm$Cultura)
n_crops <- length(unique_crops)

# 1. Pastel color palette
if (n_crops < 3) {
  base_colors <- c("#FBB4AE", "#B3CDE3")
  pastel_colors <- base_colors[1:n_crops]
} else {
  pastel_colors <- brewer.pal(n = n_crops, name = "Pastel1")
}
names(pastel_colors) <- unique_crops

# 2. Hatching patterns
pattern_list <- c("stripe", "crosshatch", "circle", "wave", "grid", "hexagon", "right45", "left45")
if (n_crops > length(pattern_list)) {
  hatch_patterns <- rep(pattern_list, length.out = n_crops)
} else {
  hatch_patterns <- pattern_list[1:n_crops]
}
names(hatch_patterns) <- unique_crops

p_boxplot <- ggplot(dados_norm, aes(x = Parcela, y = APSWCI_Original, fill = Cultura, pattern = Cultura)) +
  geom_boxplot_pattern(
    outlier.shape = 21, 
    alpha = 0.8,
    pattern_fill = "gray20",    # Cor da rachura
    pattern_density = 0.1,      # Densidade
    pattern_spacing = 0.02,     # Espaçamento
    pattern_key_scale_factor = 0.6
  ) +
  facet_wrap(~Profundidade) + # Caixas solicitadas
  labs(
    title = "APSWCI Distribution by Tillage System and Cover Crop",
    subtitle = "Comparison by Soil Depth",
    x = "Tillage System",
    y = "APSWCI (0-10)",
    fill = "Cover Crop",
    pattern = "Cover Crop"
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_bw(base_size = 20) + # Enlarged for editor revision (R2 proof)
  scale_fill_manual(values = pastel_colors) +
  scale_pattern_manual(values = hatch_patterns) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Boxplot_APSWCI.png"), p_boxplot, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figura_Revisao_Boxplot_APSWCI.pdf"), p_boxplot, width = 10, height = 6)

# 7.5 Boxplot SC (Soil Conservation) - Caso queira substituir a 3c
cat("Gerando Boxplot SC com Facet...\n")

p_boxplot_sc <- ggplot(dados_norm, aes(x = Parcela, y = COS, fill = Cultura)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  facet_wrap(~Profundidade) +
  labs(
    title = "Soil Conservation (SC) Sub-index",
    subtitle = "Comparison by Soil Depth",
    x = "Tillage System",
    y = "SC Index (0-10)",
    fill = "Cover Crop"
  ) +
  theme_bw(base_size = 20) + # Enlarged for editor revision (R2 proof)
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave(file.path(fig_dir, "Figura_Revisao_Boxplot_SC.png"), p_boxplot_sc, width = 10, height = 6, dpi = 600)

# 7.3 Matriz de Correlação (Geral)
cat("Gerando Tabela de Correlação (Geral)...\n")
vars_corr <- dados_norm %>%
  select(
    APSWCI = APSWCI_Original,
    Yield = Produtividade_norm,
    SC = COS,
    MrP = ARQ,
    STAND = STAND
  )
cor_spearman <- cor(vars_corr, method = "spearman", use = "pairwise.complete.obs")
write.csv(round(cor_spearman, 3), "Tabela_Revisao_Correlacao_Spearman.csv")

cat("\n--------------------------------------------------------\n")
cat("SUCESSO! Figuras atualizadas com ambas as profundidades.\n")
cat("--------------------------------------------------------\n")

# 8. ESTATÍSTICAS PARA O TEXTO (BETA, R2, P-VALUE) - POR SISTEMA ----
cat("\n=== ESTATÍSTICAS PARA O TEXTO (POR SISTEMA) ===\n")

# Função para calcular e imprimir stats
calc_stats_system <- function(data, system_name) {
  model <- lm(Produtividade_norm ~ APSWCI_Original, data = data)
  s <- summary(model)
  
  beta <- coef(model)[2] # Slope
  r2 <- s$r.squared
  p_val <- coef(s)[2, 4]
  
  mean_prod <- mean(data$Produtividade_norm, na.rm = TRUE)
  perc_inc <- (beta / mean_prod) * 100
  
  cat(sprintf("\nSistema: %s\n", system_name))
  cat(sprintf("  Beta: %.4f\n", beta))
  cat(sprintf("  R2: %.4f\n", r2))
  cat(sprintf("  p-valor: %.4e\n", p_val))
  cat(sprintf("  Incremento (Beta/Mean): %.2f%%\n", perc_inc))
}

# Iterar por Sistema
systems <- unique(dados_norm$Parcela)

for (sys in systems) {
  sub_data <- dados_norm %>% filter(Parcela == sys)
  calc_stats_system(sub_data, sys)
}



