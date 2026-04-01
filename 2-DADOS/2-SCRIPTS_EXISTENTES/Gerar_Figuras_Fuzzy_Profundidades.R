
################################################################################
# SCRIPT - IPACSA POR PROFUNDIDADE (0-10 e 10-20 cm)
# Autor: Diego Vidal (Adaptado por Copilot)
# Data: 01/10/2025
# Descrição: Calcula o índice Fuzzy (IPACSA) para duas profundidades e gera
#            gráficos (Boxplot e Heatmap) com estilo padronizado.
################################################################################

## 0) PACOTES ----
req_pkgs <- c("readxl", "dplyr", "ggplot2", "ggpattern", "RColorBrewer", "FuzzyR", "tidyr", "forcats", "multcomp", "multcompView", "emmeans")
if (length(ins <- setdiff(req_pkgs, rownames(installed.packages())))) install.packages(ins, dependencies = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

## 1) LIMPEZA E FUNÇÕES AUXILIARES ----
rm(list = ls())
graphics.off()

# Função de Normalização Robusta
normalizar_robusto <- function(x, maior_melhor = TRUE) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(5, length(x)))
  q <- stats::quantile(x, probs = c(.05, .95), na.rm = TRUE, type = 8)
  if (diff(q) == 0) return(rep(5, length(x)))
  x2 <- (x - q[1]) / (q[2] - q[1])
  x2 <- pmin(pmax(x2, 0), 1) * 10
  if (!maior_melhor) x2 <- 10 - x2
  x2[is.na(x2)] <- 5
  x2
}

# Função para criar MFs triangulares baseadas em quartis
add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {
  x <- x[is.finite(x)]
  qs <- stats::quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)
  L <- -2; U <- 12
  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))
  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))
  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))
  fis
}

## 2) IMPORTAÇÃO E PREPARAÇÃO DOS DADOS ----
# Ler as duas abas
dados_010 <- readxl::read_xlsx("banco_dados.xlsx", sheet = "dados_010") %>% mutate(Depth = "0-10 cm")
dados_1020 <- readxl::read_xlsx("banco_dados.xlsx", sheet = "dados_1020") %>% mutate(Depth = "10-20 cm")

# Combinar
dados <- bind_rows(dados_010, dados_1020)

# Direção das variáveis
direcao <- c(
  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,
  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,
  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,
  `Peso de espigas comerciais_ha`=1, Produtividade=1
)

# Normalização (Global para manter consistência entre profundidades)
dados_norm <- dados %>%
  dplyr::mutate(
    dplyr::across(
      names(direcao),
      ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),
      .names = "{.col}_norm"
    )
  )

## 3) DEFINIÇÃO DOS SISTEMAS FUZZY ----

# COS
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
  
  regras_orig <- rbind(
    c(2,2,1,1,1,3,3, 3, 1,1),
    c(2,2,2,2,2,2,2, 2, 1,1),
    c(1,1,3,3,3,1,1, 1, 1,1)
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

# ARQ
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
  
  regras_orig <- rbind(
    c(3,3,3,3,3,3, 3, 1,1),
    c(2,2,2,2,2,2, 2, 1,1),
    c(1,1,1,1,1,1, 1, 1,1)
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

# STAND
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

# FINAL
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
    c(2,3,2,3, 3, 1.2, 1), c(2,2,2,2, 2, 1.0, 1), c(1,1,1,1, 1, 0.8, 1),
    c(3,2,2,2, 3, 1.1, 1), c(2,3,2,3, 3, 1.1, 1), c(2,2,3,2, 3, 1.1, 1), c(2,3,2,2, 3, 1.15,1)
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

## 4) CÁLCULO ----
fis_COS    <- criar_fis_COS(dados_norm)
fis_ARQ    <- criar_fis_ARQ(dados_norm)
fis_STAND  <- criar_fis_STAND(dados_norm)
fis_FINAL  <- criar_fis_FINAL()

calc_subindices <- function(df) {
  Xcos <- as.matrix(df[, c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")])
  Xarq <- as.matrix(df[, c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",
                           "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")])
  Xstd <- as.matrix(df[, "Número de plantas_ha_norm", drop=FALSE])
  
  df$COS   <- apply(Xcos, 1, function(l) evalfis(matrix(l,1), fis_COS))
  df$ARQ   <- apply(Xarq, 1, function(l) evalfis(matrix(l,1), fis_ARQ))
  df$STAND <- apply(Xstd, 1, function(l) evalfis(matrix(l,1), fis_STAND))
  df
}

dados_norm <- calc_subindices(dados_norm)
Xfinal <- as.matrix(dados_norm[, c("COS","ARQ","STAND","Produtividade_norm")])
dados_norm$IPACSA <- apply(Xfinal, 1, function(l) evalfis(matrix(l,1), fis_FINAL))

## 5) PREPARAÇÃO PARA GRÁFICOS E ESTATÍSTICA ----
ordem_parcela <- c("CT","MT","NT")
dados_final <- dados_norm %>%
  dplyr::mutate(
    Parcela = factor(trimws(Parcela), levels = ordem_parcela),
    Cultura = as.factor(trimws(Cultura)),
    Depth = factor(Depth, levels = c("0-10 cm", "10-20 cm"))
  ) %>%
  dplyr::filter(is.finite(IPACSA))

# Estilos
n_culturas <- length(unique(dados_final$Cultura))
culturas_unicas <- unique(dados_final$Cultura)
cores_pastel <- RColorBrewer::brewer.pal(n = max(3, n_culturas), name = "Pastel1")[1:n_culturas]
names(cores_pastel) <- culturas_unicas
# Se houver mais culturas do que padrões, repete/cicla para evitar NA (que quebra o ggpattern)
padroes_disponiveis <- c("stripe", "crosshatch", "circle", "weave", "polka_dot", "grid")
padroes_hachura <- rep_len(padroes_disponiveis, n_culturas)
names(padroes_hachura) <- culturas_unicas

# Função para letras de Tukey via emmeans
get_letters <- function(df) {
  # Ajustar modelo
  model <- aov(IPACSA ~ Parcela * Cultura, data = df)
  # Calcular médias marginais e letras
  emm <- emmeans(model, ~ Parcela * Cultura)
  res <- cld(emm, Letters = letters, adjust = "tukey", reversed = TRUE)
  res$.group <- trimws(res$.group)
  return(res)
}

# Calcular letras por profundidade
tukey_labels <- dados_final %>%
  group_by(Depth) %>%
  group_modify(~ get_letters(.x)) %>%
  ungroup() %>%
  dplyr::select(Depth, Parcela, Cultura, .group)

# Calcular posição Y para as letras (max + buffer)
max_vals <- dados_final %>%
  group_by(Depth, Parcela, Cultura) %>%
  summarise(max_val = max(IPACSA, na.rm=TRUE), .groups="drop")

tukey_plot_data <- left_join(tukey_labels, max_vals, by = c("Depth", "Parcela", "Cultura")) %>%
  mutate(y_pos = max_val + 0.5) # Offset para cima da barra de erro/boxplot

## 6) BOXPLOT (Figura 5) ----
# Facetado por Profundidade com Letras de Tukey
p_box <- ggplot(dados_final, aes(x = Parcela, y = IPACSA, fill = Cultura, pattern = Cultura)) +
  geom_boxplot_pattern(
    outlier.shape = NA, 
    colour = "black",
    pattern_fill = "gray20",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    position = position_dodge(width = 0.8)
  ) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.4, size = 1) +
  # Adicionar letras de significância
  geom_text(
    data = tukey_plot_data,
    aes(x = Parcela, y = y_pos, label = .group, group = Cultura),
    position = position_dodge(width = 0.8),
    vjust = 0,
    size = 6.5,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(~Depth, ncol = 2) +
  scale_y_continuous(limits = c(0, 11.5), breaks = 0:10) +
  scale_fill_manual(values = cores_pastel) +
  scale_pattern_manual(values = padroes_hachura) +
  labs(x = "Soil Management System", y = "APSWCI Index", fill = "Cover Crop", pattern = "Cover Crop") +
  theme_classic(base_size = 22) + # Enlarged for editor revision (R2 proof)
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave("../FIGURAS_USUAIS/Figure_5_Boxplot_Depths.png", p_box, width = 12, height = 6, dpi = 600)

## 7) HEATMAP (Figura 6) ----
# Agrupar dados para heatmap
heatmap_data <- dados_final %>%
  group_by(Parcela, Cultura, Depth) %>%
  summarise(IPACSA_medio = mean(IPACSA, na.rm = TRUE), .groups = 'drop')

p_heat <- ggplot(heatmap_data, aes(x = Parcela, y = Cultura, fill = IPACSA_medio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", IPACSA_medio)), color = "black", size = 6.5) +
  facet_wrap(~Depth, ncol = 2) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(0, 10)) +
  labs(x = "Soil Management System", y = "Cover Crop", fill = "APSWCI") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(color = "black", size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggsave("../FIGURAS_USUAIS/Figure_6_Heatmap_Depths.png", p_heat, width = 12, height = 5, dpi = 600)

cat("Figuras geradas com sucesso em ../FIGURAS_USUAIS/\n")
