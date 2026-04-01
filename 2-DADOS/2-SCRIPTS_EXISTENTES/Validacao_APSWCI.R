# ------------------------------------------------------------------
# SCRIPT DE VALIDAÇÃO E ANÁLISE DE SENSIBILIDADE (APSWCI)
# Atendendo à revisão do Agronomy Journal (17/12/2025)
# ------------------------------------------------------------------

# 1. SETUP E PACOTES ----
req_pkgs <- c("readxl", "dplyr", "FuzzyR", "ggplot2", "corrplot", "tidyr", "openxlsx", "ggpubr")
for(p in req_pkgs) {
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

rm(list = ls())
graphics.off()

# 2. CARREGAR DADOS ----
# Ajuste o caminho se necessário
if(file.exists("banco_dados.xlsx")) {
  dados <- read_excel("banco_dados.xlsx", sheet = "dados_010")
} else if(file.exists("2 - DADOS/banco_dados.xlsx")) {
  dados <- read_excel("2 - DADOS/banco_dados.xlsx", sheet = "dados_010")
} else {
  stop("Arquivo banco_dados.xlsx não encontrado.")
}

# Colunas necessárias
colunas_necessarias <- c(
  "DMG","DMP","RMP","Densidade","Estoque de C","Na","ICV(%)",
  "Altura de Plantas","Diâmetro espiga","Comprimento espiga",
  "Número de plantas_ha","N total de espigas_ha","N de espigas comerciais_ha",
  "Peso de espigas comerciais_ha","Produtividade","Cultura","Parcela"
)
dados <- dados[complete.cases(dados[, colunas_necessarias]), ]

# 3. FUNÇÕES AUXILIARES (DO SCRIPT ORIGINAL) ----

# Direção das variáveis (1: maior melhor, -1: menor melhor)
direcao <- c(
  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,
  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,
  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,
  `Peso de espigas comerciais_ha`=1, Produtividade=1
)

# Normalização Robusta
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

# Aplicar Normalização
dados_norm <- dados %>%
  mutate(across(names(direcao), 
                ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),
                .names = "{.col}_norm"))

# Função para criar MFs triangulares baseadas em quartis
add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {
  x <- x[is.finite(x)]
  qs <- quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)
  # Ajuste: Estender os limites para garantir cobertura em 0 e 10
  L <- -2; U <- 12 
  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))
  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))
  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))
  fis
}

# 4. RECONSTRUÇÃO DOS SISTEMAS FUZZY (ORIGINAL) ----

# 4.1 Subíndices
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
  
  # Regras Originais
  regras_orig <- rbind(c(2,2,1,1,1,3,3, 3, 1,1), c(2,2,2,2,2,2,2, 2, 1,1), c(1,1,3,3,3,1,1, 1, 1,1))
  
  # Regras de Preenchimento (Main Effects) para evitar NAs
  # Se Var_i é Baixa -> COS Baixa (peso 0.1)
  # Se Var_i é Alta  -> COS Alta  (peso 0.1)
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    # Baixa -> Baixa
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    # Media -> Media
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    # Alta -> Alta
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
  
  # Regras de Preenchimento
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

# 4.2 FIS Final Original
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
  
  # Regras Originais
  regras_orig <- rbind(
    c(2,3,2,3, 3, 1.2, 1),
    c(2,2,2,2, 2, 1.0, 1),
    c(1,1,1,1, 1, 0.8, 1),
    c(3,2,2,2, 3, 1.1, 1),
    c(2,3,2,3, 3, 1.1, 1),
    c(2,2,3,2, 3, 1.1, 1),
    c(2,3,2,2, 3, 1.15,1)
  )
  
  # Regras de Preenchimento
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

# 5. CÁLCULO DOS SUBÍNDICES ----
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

# Debug NAs nos subíndices
cat("NAs em COS:", sum(is.na(dados_norm$COS)), "\n")
cat("NAs em ARQ:", sum(is.na(dados_norm$ARQ)), "\n")
cat("NAs em STAND:", sum(is.na(dados_norm$STAND)), "\n")


# 6. CÁLCULO DO APSWCI ORIGINAL ----
fis_FINAL <- criar_fis_FINAL()
Xfinal <- as.matrix(dados_norm[, c("COS","ARQ","STAND","Produtividade_norm")])
dados_norm$APSWCI_Original <- apply(Xfinal, 1, function(l) evalfis(matrix(l,1), fis_FINAL))

# 7. ABLATION STUDY (SENSIBILIDADE) ----

# 7.1 APSWCI Sem Yield (Removendo Produtividade)
# Estratégia: Criar novo FIS com apenas 3 inputs (COS, ARQ, STAND)
# Regras: Adaptar as regras originais removendo a 4ª coluna (Produtividade)
criar_fis_NO_YIELD <- function() {
  fis <- newfis("IPACSA_NoYield", defuzzMethod = "centroid")
  in_vars <- c("COS","ARQ","STAND") # Sem Produtividade
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
  
  # Regras Adaptadas (Removendo a 4ª coluna do antecedente)
  # Original: c(COS, ARQ, STAND, PROD, OUTPUT, WEIGHT, AND)
  # Nova:     c(COS, ARQ, STAND,       OUTPUT, WEIGHT, AND)
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

# 7.2 APSWCI Sem Conservação (Removendo COS)
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


# 8. ANÁLISE ESTATÍSTICA E GRÁFICOS ----

# Diagnóstico de NAs
cat("Total de linhas:", nrow(dados_norm), "\n")
cat("NAs em APSWCI_Original:", sum(is.na(dados_norm$APSWCI_Original)), "\n")
cat("NAs em Produtividade_norm:", sum(is.na(dados_norm$Produtividade_norm)), "\n")

# 8.1 Correlação de Spearman
vars_corr <- dados_norm %>% 
  select(APSWCI_Original, Produtividade_norm, COS, ARQ, STAND, APSWCI_NoYield, APSWCI_NoCOS)

cor_matrix <- cor(vars_corr, method = "spearman", use = "pairwise.complete.obs")
print("Matriz de Correlação (Spearman):")
print(round(cor_matrix, 2))

# Salvar matriz
write.csv(cor_matrix, "Matriz_Correlacao_Validacao.csv")

# 8.2 Gráfico de Dispersão: APSWCI vs Yield (Added Value)
p_added_value <- ggplot(dados_norm, aes(x = Produtividade_norm, y = APSWCI_Original, color = Parcela)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "Added Value Analysis: APSWCI vs Productivity",
       subtitle = "Points below the line indicate yield penalty due to poor conservation/quality",
       x = "Normalized Productivity (0-10)",
       y = "APSWCI (0-10)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

ggsave("Validacao_AddedValue_Scatter.png", p_added_value, width = 8, height = 6, dpi = 300)

# 8.3 Gráfico de Comparação de Rankings (Ablation)
# Vamos comparar o ranking dos tratamentos entre Original e NoYield
dados_norm$Rank_Original <- rank(dados_norm$APSWCI_Original)
dados_norm$Rank_NoYield <- rank(dados_norm$APSWCI_NoYield)

p_ablation <- ggplot(dados_norm, aes(x = Rank_Original, y = Rank_NoYield, color = Parcela)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Ablation Study: Rank Stability",
       subtitle = "Deviation from diagonal shows influence of Productivity on final ranking",
       x = "Rank (Original APSWCI)",
       y = "Rank (APSWCI without Yield)") +
  theme_minimal()

ggsave("Validacao_Ablation_Rank.png", p_ablation, width = 8, height = 6, dpi = 300)

# 9. EXPORTAR DADOS VALIDADOS ----
write.xlsx(dados_norm, "Dados_Validacao_Completa.xlsx")

cat("\nProcesso de validação concluído!\n")
cat("Arquivos gerados:\n")
cat("- Matriz_Correlacao_Validacao.csv\n")
cat("- Validacao_AddedValue_Scatter.png\n")
cat("- Validacao_Ablation_Rank.png\n")
cat("- Dados_Validacao_Completa.xlsx\n")
