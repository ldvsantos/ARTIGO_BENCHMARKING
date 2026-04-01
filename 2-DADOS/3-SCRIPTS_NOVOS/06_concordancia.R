## ============================================================
## 06_concordancia.R
## Concordância inter-paradigma: Blom, Kendall W, ICC, Gwet AC2,
## Bland-Altman, Spearman, meta-IQS Borda, testes H1 e H2
## Artigo: Benchmarking multi-paradigma
## ============================================================

library(irr)
library(psych)
library(irrCAC)
library(ggplot2)

## ----------------------------------------------------------
## 1. Leitura dos dados e montagem da matriz de IQS
## ----------------------------------------------------------
dados <- read.csv(
  file.path("..", "1-DADOS_BRUTOS", "IPACSA_resultados_completos.csv")
)

## IQS_Fuzzy já existe (IPACSA)
## IQS_RF: verificar se já foi gerado
rf_file <- file.path("..", "1-DADOS_BRUTOS", "IQS_RF_resultados.csv")
if (file.exists(rf_file)) {
  rf_res <- read.csv(rf_file)
  dados$IQS_RF <- rf_res$IQS_RF
} else {
  stop("Execute 05_random_forest.R antes deste script.")
}

## IQS_ANOVA: média ponderada das variáveis normalizadas (pesos iguais como proxy)
norm_cols <- c(
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

## PCA para pesos (Paradigma 1 - ANOVA/MDS)
pca_res <- prcomp(dados[, norm_cols], center = TRUE, scale. = TRUE)
## Reter componentes com eigenvalue > 1
eigvals <- pca_res$sdev^2
n_comp <- sum(eigvals > 1)
cat("PCA: componentes retidos =", n_comp, "\n")
cat("Variância explicada:", round(cumsum(eigvals / sum(eigvals))[1:n_comp] * 100, 1), "%\n")

## Pesos proporcionais à variância explicada
weights_pca <- eigvals[1:n_comp] / sum(eigvals[1:n_comp])
scores_pca <- pca_res$x[, 1:n_comp]
dados$IQS_ANOVA <- as.numeric(scores_pca %*% weights_pca)
## Rescalar para 0-10
rng <- range(dados$IQS_ANOVA)
dados$IQS_ANOVA <- (dados$IQS_ANOVA - rng[1]) / (rng[2] - rng[1]) * 10

## IQS_PLS: scores reais do PLS-SEM (08_plssem.R)
pls_file <- file.path("..", "1-DADOS_BRUTOS", "IQS_PLS_resultados.csv")
if (file.exists(pls_file)) {
  pls_res <- read.csv(pls_file)
  dados$IQS_PLS <- pls_res$IQS_PLS
} else {
  stop("Execute 08_plssem.R antes deste script.")
}

## IQS_MARS: modelo MARS como proxy (usa earth)
library(earth)
set.seed(123)
mars_fit <- earth(IPACSA ~ ., data = dados[, c(norm_cols, "IPACSA")], degree = 2)
dados$IQS_MARS <- predict(mars_fit)[, 1]

cat("\n--- Sumário dos 5 IQS ---\n")
iqs_mat <- datos_iqs <- data.frame(
  IQS_ANOVA = dados$IQS_ANOVA,
  IQS_Fuzzy = dados$IPACSA,
  IQS_PLS   = dados$IQS_PLS,
  IQS_MARS  = dados$IQS_MARS,
  IQS_RF    = dados$IQS_RF
)
print(summary(iqs_mat))

## ----------------------------------------------------------
## 2. Transformação de Blom (rank-based inverse normal)
## ----------------------------------------------------------
blom_transform <- function(x) {
  n <- length(x)
  r <- rank(x, ties.method = "average")
  qnorm((r - 0.5) / n)
}

iqs_blom <- as.data.frame(lapply(iqs_mat, blom_transform))
names(iqs_blom) <- paste0(names(iqs_mat), "_blom")

cat("\n--- IQS após transformação de Blom ---\n")
print(summary(iqs_blom))

## ----------------------------------------------------------
## 3. Kendall W (H1: W > 0)
## ----------------------------------------------------------
## irr::kendall espera: linhas = sujeitos, colunas = juízes
rank_mat <- apply(iqs_mat, 2, rank)
kw_result <- irr::kendall(rank_mat, correct = TRUE)
cat("\n--- Kendall W ---\n")
cat("W =", round(kw_result$value, 4), "\n")
cat("Chi² =", round(kw_result$statistic, 2), "\n")
cat("p =", format.pval(kw_result$p.value, digits = 4), "\n")

## Teste de permutação para H1 (W > 0)
set.seed(42)
n_perm <- 10000
W_obs <- kw_result$value
W_perm <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  perm_mat <- apply(rank_mat, 2, sample)  # permuta postos dentro de cada coluna
  W_perm[i] <- irr::kendall(perm_mat, correct = FALSE)$value
}

p_perm <- mean(W_perm >= W_obs)
cat("H1 - Teste de permutação (10000 reps): p =", format.pval(p_perm, digits = 4), "\n")
cat("H1 resultado:", ifelse(p_perm < 0.05, "REJEITA H0 (concordância significativa)",
                             "NÃO rejeita H0"), "\n")

## ----------------------------------------------------------
## 4. ICC(2,1)
## ----------------------------------------------------------
icc_result <- psych::ICC(iqs_blom)
cat("\n--- ICC ---\n")
print(icc_result$results[2, ])  # ICC(2,1) = second row

## ----------------------------------------------------------
## 5. Gwet AC2
## ----------------------------------------------------------
## irrCAC trabalha com categorias; Para dados contínuos, discretizamos em quintis
quintile_mat <- apply(iqs_mat, 2, function(x) {
  cut(x, breaks = quantile(x, probs = 0:5/5), include.lowest = TRUE, labels = 1:5)
})
quintile_df <- as.data.frame(quintile_mat)

gwet_result <- irrCAC::gwet.ac1.raw(quintile_df)
cat("\n--- Gwet AC2 ---\n")
print(gwet_result$est)

## ----------------------------------------------------------
## 6. Spearman correlation matrix
## ----------------------------------------------------------
spearman_mat <- cor(iqs_mat, method = "spearman")
cat("\n--- Matriz de correlação de Spearman ---\n")
print(round(spearman_mat, 3))

## ----------------------------------------------------------
## 7. Teste H2: paradigmas não lineares convergem mais entre si
## ----------------------------------------------------------
## Paradigmas: ANOVA(linear), Fuzzy(NL), PLS(NL), MARS(NL), RF(NL)
## Fisher-Z transform
fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r))

## Pares não-linear x não-linear
nl_names <- c("IQS_Fuzzy", "IQS_PLS", "IQS_MARS", "IQS_RF")
lin_name <- "IQS_ANOVA"

rho_nl_nl <- c()
for (i in 1:(length(nl_names) - 1)) {
  for (j in (i + 1):length(nl_names)) {
    rho_nl_nl <- c(rho_nl_nl, spearman_mat[nl_names[i], nl_names[j]])
  }
}

## Pares linear x não-linear
rho_lin_nl <- spearman_mat[lin_name, nl_names]

cat("\n--- H2: Spearman rho ---\n")
cat("NL-NL pairs:", round(rho_nl_nl, 3), " | mean =", round(mean(rho_nl_nl), 3), "\n")
cat("Lin-NL pairs:", round(rho_lin_nl, 3), " | mean =", round(mean(rho_lin_nl), 3), "\n")

## Fisher-Z
z_nl_nl <- fisher_z(rho_nl_nl)
z_lin_nl <- fisher_z(rho_lin_nl)

## Wilcoxon pareado não é possível (tamanhos diferentes: 6 vs 4)
## Usamos Mann-Whitney (Wilcoxon rank-sum)
wilcox_h2 <- wilcox.test(z_nl_nl, z_lin_nl, alternative = "greater")
cat("H2 - Wilcoxon rank-sum test (NL-NL > Lin-NL): W =",
    wilcox_h2$statistic, ", p =", format.pval(wilcox_h2$p.value, digits = 4), "\n")
cat("H2 resultado:", ifelse(wilcox_h2$p.value < 0.05,
    "REJEITA H0 (NL convergem mais)", "NÃO rejeita H0"), "\n")

## ----------------------------------------------------------
## 8. Bland-Altman (para cada par)
## ----------------------------------------------------------
paradigm_names <- names(iqs_mat)
ba_results <- list()

dir.create("../3-FIGURAS", showWarnings = FALSE)

pairs_done <- 0
for (i in 1:(length(paradigm_names) - 1)) {
  for (j in (i + 1):length(paradigm_names)) {
    a <- iqs_blom[, i]
    b <- iqs_blom[, j]
    diff_ab <- a - b
    mean_ab <- (a + b) / 2
    mean_diff <- mean(diff_ab)
    sd_diff <- sd(diff_ab)
    
    ba_results[[paste(paradigm_names[i], paradigm_names[j], sep = "_vs_")]] <- data.frame(
      mean_diff = mean_diff,
      sd_diff = sd_diff,
      lower_loa = mean_diff - 1.96 * sd_diff,
      upper_loa = mean_diff + 1.96 * sd_diff
    )
    pairs_done <- pairs_done + 1
  }
}

ba_summary <- do.call(rbind, ba_results)
cat("\n--- Bland-Altman Summary (Blom scale) ---\n")
print(round(ba_summary, 4))

## ----------------------------------------------------------
## 9. Meta-IQS (Borda count)
## ----------------------------------------------------------
rank_mat_orig <- apply(iqs_mat, 2, rank)
dados$MetaIQS_Borda <- rowMeans(rank_mat_orig)

cat("\n--- Meta-IQS (Borda) ---\n")
cat("Min:", round(min(dados$MetaIQS_Borda), 2),
    " | Median:", round(median(dados$MetaIQS_Borda), 2),
    " | Max:", round(max(dados$MetaIQS_Borda), 2), "\n")

## Correlação do meta-IQS com cada paradigma
meta_cor <- cor(dados$MetaIQS_Borda, iqs_mat, method = "spearman")
cat("Spearman rho (Borda vs paradigmas):", round(meta_cor, 3), "\n")

## ----------------------------------------------------------
## 10. Exportar resultados
## ----------------------------------------------------------
## Matriz completa de IQS
export_df <- data.frame(
  Parcela = dados$Parcela,
  Cultura = dados$Cultura,
  iqs_mat,
  iqs_blom,
  MetaIQS_Borda = dados$MetaIQS_Borda
)
write.csv(export_df, file = "../1-DADOS_BRUTOS/IQS_todos_paradigmas.csv", row.names = FALSE)

## Spearman matrix
write.csv(round(spearman_mat, 4), file = "../1-DADOS_BRUTOS/Spearman_matrix.csv")

## Bland-Altman summary
write.csv(round(ba_summary, 4), file = "../1-DADOS_BRUTOS/BlandAltman_summary.csv")

cat("\n=== 06_concordancia.R concluído ===\n")
