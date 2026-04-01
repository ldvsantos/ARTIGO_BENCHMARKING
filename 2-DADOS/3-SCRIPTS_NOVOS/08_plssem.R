#!/usr/bin/env Rscript
# 08_plssem.R — PLS-SEM com Mode A/B misto via seminr (two-stage)
# Saída: IQS_PLS_resultados.csv (scores), diagnósticos no console
#
# Indicadores excluídos por multicolinearidade (VIF > 5 ou aliasing):
#   DMP  — r=0.92 com RMP, r=0.70 com DMG (mesmo domínio: diâmetro de agregados)
#   RMP  — r=0.92 com DMG/DMP (VIF > 6 em Mode B)
#   Ds   — r=-0.99 com ECOS (VIF > 56, anticolinear)
#   NESPC— r=1.00 com PROD (variáveis algebricamente idênticas)
#
# Especificação final (11 indicadores, two-stage):
#   Estágio 1 — 3 construtos de 1a ordem:
#     FISICO (Mode B): DMG, ECOS, NAG, ICV
#     AGRO   (Mode B): ALT, DESP, CESP, STND
#     REND   (Mode A): NESP, PESP, PROD
#   Modelo estrutural: FISICO → REND, AGRO → REND
#
#   Estágio 2 — IQS_PLS = PC1 dos 3 construct scores (escala 0-10)

library(seminr)

# -------------------------------------------------------------------
# 1. Leitura e renomeação
# -------------------------------------------------------------------
d <- read.csv("1-DADOS_BRUTOS/IPACSA_resultados_completos.csv")
cat("Dimensões:", nrow(d), "x", ncol(d), "\n")

rename_map <- c(
  "DMG_norm"                          = "DMG",
  "Estoque.de.C_norm"                 = "ECOS",
  "Na_norm"                           = "NAG",
  "ICV..._norm"                       = "ICV",
  "Altura.de.Plantas_norm"            = "ALT",
  "Diâmetro.espiga_norm"              = "DESP",
  "Comprimento.espiga_norm"           = "CESP",
  "Número.de.plantas_ha_norm"         = "STND",
  "N.total.de.espigas_ha_norm"        = "NESP",
  "Peso.de.espigas.comerciais_ha_norm"= "PESP",
  "Produtividade_norm"                = "PROD"
)
for (old_name in names(rename_map)) {
  idx <- which(names(d) == old_name)
  if (length(idx) == 1) names(d)[idx] <- rename_map[old_name]
}

vars_pls <- c("DMG","ECOS","NAG","ICV",
              "ALT","DESP","CESP","STND",
              "NESP","PESP","PROD")

cat("Variáveis disponíveis:", all(vars_pls %in% names(d)), "\n")
cat("NAs:", sum(is.na(d[, vars_pls])), "\n")

# -------------------------------------------------------------------
# 2. ESTÁGIO 1: Modelo de medida + estrutural (3 construtos)
# -------------------------------------------------------------------
mm <- constructs(
  composite("FISICO", c("DMG","ECOS","NAG","ICV"),
            weights = mode_B),
  composite("AGRO",   c("ALT","DESP","CESP","STND"),
            weights = mode_B),
  composite("REND",   c("NESP","PESP","PROD"),
            weights = mode_A)
)

sm <- relationships(
  paths(from = "FISICO", to = "REND"),
  paths(from = "AGRO",   to = "REND")
)

cat("\n=== ESTÁGIO 1: ESTIMANDO PLS-SEM ===\n")
pls_model <- estimate_pls(
  data              = d,
  measurement_model = mm,
  structural_model  = sm,
  inner_weights     = path_weighting,
  missing           = mean_replacement,
  missing_value     = NA
)

s <- summary(pls_model)

cat("\n=== PESOS EXTERNOS (Outer Weights) ===\n")
print(s$weights)

cat("\n=== CARGAS EXTERNAS (Outer Loadings) ===\n")
print(s$loadings)

cat("\n=== VIF dos indicadores ===\n")
print(s$validity$vif_items)

cat("\n=== Coeficientes de caminho ===\n")
print(s$paths)

cat("\n=== R² do REND ===\n")
cat("R²:", s$paths[1, 1], "\n")
cat("AdjR²:", s$paths[2, 1], "\n")

# -------------------------------------------------------------------
# 3. Bootstrap (5000 reamostras)
# -------------------------------------------------------------------
cat("\n=== BOOTSTRAP (5000 reamostras) ===\n")
set.seed(42)
boot <- bootstrap_model(pls_model, nboot = 5000, seed = 42)
sb <- summary(boot)

cat("\n--- Bootstrapped Paths ---\n")
print(sb$bootstrapped_paths)

cat("\n--- Bootstrapped Weights ---\n")
print(sb$bootstrapped_weights)

# -------------------------------------------------------------------
# 4. ESTÁGIO 2: IQS_PLS via PCA dos 3 construct scores
# -------------------------------------------------------------------
cat("\n=== ESTÁGIO 2: IQS via PCA dos construct scores ===\n")
scores_mat <- pls_model$construct_scores[, c("FISICO","AGRO","REND")]
cat("Correlação entre construtos:\n")
print(round(cor(scores_mat), 4))

pca_iqs <- prcomp(scores_mat, center = TRUE, scale. = TRUE)
cat("\nVariância explicada por PC:\n")
print(summary(pca_iqs))

# IQS = PC1 (primeiro componente principal)
iqs_raw <- pca_iqs$x[, 1]

# Inverter se necessário (PC1 deve correlacionar positivamente com os scores)
if (cor(iqs_raw, scores_mat[, "REND"]) < 0) iqs_raw <- -iqs_raw

# Escalar para 0-10
iqs_scaled <- (iqs_raw - min(iqs_raw)) / (max(iqs_raw) - min(iqs_raw)) * 10

cat("\n=== IQS_PLS Sumário (escala 0-10) ===\n")
cat("Min:", round(min(iqs_scaled), 4), "\n")
cat("Max:", round(max(iqs_scaled), 4), "\n")
cat("Mean:", round(mean(iqs_scaled), 4), "\n")
cat("SD:", round(sd(iqs_scaled), 4), "\n")

# Pesos do IQS (loadings do PC1)
cat("\n=== Pesos do IQS (PC1 loadings) ===\n")
cat("FISICO:", round(pca_iqs$rotation[1, 1], 4), "\n")
cat("AGRO:  ", round(pca_iqs$rotation[2, 1], 4), "\n")
cat("REND:  ", round(pca_iqs$rotation[3, 1], 4), "\n")

# Médias por célula manejo×cobertura
out <- data.frame(
  Parcela = d$Parcela,
  Cultura = d$Cultura,
  IQS_PLS = round(iqs_scaled, 4),
  FISICO  = round(as.numeric(scores_mat[, "FISICO"]), 4),
  AGRO    = round(as.numeric(scores_mat[, "AGRO"]), 4),
  REND    = round(as.numeric(scores_mat[, "REND"]), 4)
)

cat("\n=== Médias IQS_PLS por célula ===\n")
agg <- aggregate(IQS_PLS ~ Parcela + Cultura, data = out, FUN = mean)
agg <- agg[order(-agg$IQS_PLS), ]
print(agg)

# Correlação com IPACSA original (fuzzy)
iqs_fuzzy <- d$IPACSA
cat("\n=== Correlação IQS_PLS vs IPACSA (fuzzy) ===\n")
cat("Pearson:", round(cor(iqs_scaled, iqs_fuzzy), 4), "\n")
cat("Spearman:", round(cor(iqs_scaled, iqs_fuzzy, method = "spearman"), 4), "\n")

# -------------------------------------------------------------------
# 5. Salvar resultados
# -------------------------------------------------------------------
write.csv(out, "1-DADOS_BRUTOS/IQS_PLS_resultados.csv", row.names = FALSE)
cat("\nArquivo salvo: 1-DADOS_BRUTOS/IQS_PLS_resultados.csv\n")

# Salvar path coefficients para Apêndice
path_df <- data.frame(
  From = c("FISICO","AGRO"),
  To   = c("REND","REND"),
  Coef = round(as.numeric(s$paths[3:4, 1]), 4)
)
write.csv(path_df, "1-DADOS_BRUTOS/PLS_path_coefficients.csv", row.names = FALSE)
cat("Arquivo salvo: 1-DADOS_BRUTOS/PLS_path_coefficients.csv\n")

cat("\n=== CONCLUÍDO ===\n")

# -------------------------------------------------------------------
# 3. Modelo estrutural
# -------------------------------------------------------------------
sm <- relationships(
  paths(from = c("FISICO","AGRO","REND"), to = "IQS")
)

# -------------------------------------------------------------------
# 4. Estimação PLS
# -------------------------------------------------------------------
cat("\n=== ESTIMANDO MODELO PLS-SEM ===\n")
pls_model <- estimate_pls(
  data        = d,
  measurement_model = mm,
  structural_model  = sm,
  inner_weights     = path_weighting,
  missing           = mean_replacement,
  missing_value     = NA
)

s <- summary(pls_model)

# -------------------------------------------------------------------
# 5. Diagnósticos Mode B
# -------------------------------------------------------------------
cat("\n=== PESOS EXTERNOS (Outer Weights) ===\n")
print(s$weights)

cat("\n=== CARGAS EXTERNAS (Outer Loadings) ===\n")
print(s$loadings)

cat("\n=== VIF dos indicadores (deve ser < 5) ===\n")
print(s$validity$vif_items)

cat("\n=== R² do construto IQS ===\n")
print(s$paths)

cat("\n=== Redundância e Comunalidade ===\n")
if (!is.null(s$quality)) print(s$quality)

# -------------------------------------------------------------------
# 6. Bootstrap (5000 reamostras)
# -------------------------------------------------------------------
cat("\n=== BOOTSTRAP (5000 reamostras) ===\n")
set.seed(42)
boot <- bootstrap_model(pls_model, nboot = 5000, seed = 42)
sb <- summary(boot)

cat("\n--- Bootstrapped Paths ---\n")
print(sb$bootstrapped_paths)

cat("\n--- Bootstrapped Weights ---\n")
print(sb$bootstrapped_weights)

cat("\n--- Bootstrapped Loadings ---\n")
print(sb$bootstrapped_loadings)

# -------------------------------------------------------------------
# 7. Extrair e escalar scores IQS_PLS (0-10)
# -------------------------------------------------------------------
iqs_raw <- as.numeric(pls_model$construct_scores[, "IQS"])
iqs_scaled <- (iqs_raw - min(iqs_raw)) / (max(iqs_raw) - min(iqs_raw)) * 10

cat("\n=== IQS_PLS Sumário (escala 0-10) ===\n")
cat("Min:", round(min(iqs_scaled), 4), "\n")
cat("Max:", round(max(iqs_scaled), 4), "\n")
cat("Mean:", round(mean(iqs_scaled), 4), "\n")
cat("SD:", round(sd(iqs_scaled), 4), "\n")

# Sumário por célula manejo×cobertura
out <- data.frame(
  Parcela = d$Parcela,
  Cultura = d$Cultura,
  IQS_PLS = round(iqs_scaled, 4)
)

cat("\n=== Médias por célula ===\n")
agg <- aggregate(IQS_PLS ~ Parcela + Cultura, data = out, FUN = mean)
agg <- agg[order(agg$Parcela, agg$Cultura), ]
print(agg)

# GoF
cat("\n=== Goodness-of-Fit (GoF) ===\n")
# GoF = sqrt(mean(communality) * mean(R²))
# communality = mean(loadings²) por construto
loadings_matrix <- s$loadings
# Calcular GoF manualmente se necessário
if (!is.null(s$paths)) {
  r2_val <- s$paths[nrow(s$paths), "R^2"]
  cat("R² IQS:", r2_val, "\n")
}

# -------------------------------------------------------------------
# 8. Salvar resultados
# -------------------------------------------------------------------
write.csv(out, "1-DADOS_BRUTOS/IQS_PLS_resultados.csv", row.names = FALSE)
cat("\nArquivo salvo: 1-DADOS_BRUTOS/IQS_PLS_resultados.csv\n")

# Salvar coeficientes de caminho para Apêndice
path_coefs <- data.frame(
  Caminho = rownames(s$paths),
  Coef    = s$paths[, 1]
)
write.csv(path_coefs, "1-DADOS_BRUTOS/PLS_path_coefficients.csv", row.names = FALSE)
cat("Arquivo salvo: 1-DADOS_BRUTOS/PLS_path_coefficients.csv\n")

cat("\n=== CONCLUÍDO ===\n")
