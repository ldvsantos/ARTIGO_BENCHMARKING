## ============================================================
## 11_integracao_quimicos_reanalise.R
## Integração de pH, P, K (dados Jaelson) ao dataset benchmarking
## Re-análise completa: 5 paradigmas + concordância + MC
## ============================================================

cat("=====================================================\n")
cat("  INTEGRAÇÃO QUÍMICOS + RE-ANÁLISE COMPLETA (18 ind.)\n")
cat("=====================================================\n\n")

## ----------------------------------------------------------
## 0. Pacotes
## ----------------------------------------------------------
pkgs <- c("randomForest","earth","seminr","irr","psych","irrCAC",
          "ggplot2","DALEX","nlme","gridExtra","reshape2","RColorBrewer","pdp")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

## ----------------------------------------------------------
## 1. Leitura e merge dos dados
## ----------------------------------------------------------
cat("--- 1. Leitura e merge ---\n")
dados <- read.csv(file.path("..","1-DADOS_BRUTOS","IPACSA_resultados_completos.csv"))
quim  <- read.csv(file.path("..","1-DADOS_BRUTOS","quimicos_jaelson_0_10cm.csv"))

cat("Dataset original:", nrow(dados), "obs ×", ncol(dados), "cols\n")
cat("Químicos:", nrow(quim), "cells\n")

## Merge por Parcela + Cultura (cell-level means assigned to all 9 reps)
dados <- merge(dados, quim[, c("Parcela","Cultura","pH_0_10","P_0_10","K_0_10")],
               by = c("Parcela","Cultura"), all.x = TRUE)

stopifnot(sum(is.na(dados$pH_0_10)) == 0)
cat("Merge OK. NAs:", sum(is.na(dados[,c("pH_0_10","P_0_10","K_0_10")])), "\n")

## ----------------------------------------------------------
## 2. Normalização 0-10 dos novos indicadores
## ----------------------------------------------------------
cat("\n--- 2. Normalização 0-10 ---\n")
normalize_0_10 <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) * 10
}

dados$pH_norm  <- normalize_0_10(dados$pH_0_10)
dados$P_norm   <- normalize_0_10(dados$P_0_10)
dados$K_norm   <- normalize_0_10(dados$K_0_10)

cat("pH_norm range:", round(range(dados$pH_norm), 2), "\n")
cat("P_norm range:  ", round(range(dados$P_norm), 2), "\n")
cat("K_norm range:  ", round(range(dados$K_norm), 2), "\n")

## ----------------------------------------------------------
## 3. Definição dos 18 indicadores normalizados
## ----------------------------------------------------------
pred_cols_18 <- c(
  ## Físico (7)
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  ## Químico (3) -- NOVO
  "pH_norm", "P_norm", "K_norm",
  ## Agronômico (4)
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  ## Rendimento (4)
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

cat("\nTotal indicadores:", length(pred_cols_18), "\n")
cat("Domínios: Físico(7), Químico(3), Agronômico(4), Rendimento(4)\n")

X <- dados[, pred_cols_18]
y <- dados$IPACSA  # Fuzzy IQS como referência para RF/MARS

## ----------------------------------------------------------
## 4. PARADIGMA 1: ANOVA/MDS (PCA-weighted)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 1: ANOVA/MDS ===\n")
pca_res <- prcomp(X, center = TRUE, scale. = TRUE)
eigvals <- pca_res$sdev^2
n_comp  <- sum(eigvals > 1)
cat("PCA: componentes retidos =", n_comp, "\n")
cat("Eigenvalues > 1:", round(eigvals[1:n_comp], 3), "\n")
cat("Variância acumulada:", round(cumsum(eigvals/sum(eigvals))[1:n_comp]*100, 1), "%\n")

weights_pca <- eigvals[1:n_comp] / sum(eigvals[1:n_comp])
scores_pca  <- pca_res$x[, 1:n_comp]
iqs_anova_raw <- as.numeric(scores_pca %*% weights_pca)
dados$IQS_ANOVA <- (iqs_anova_raw - min(iqs_anova_raw)) /
                   (max(iqs_anova_raw) - min(iqs_anova_raw)) * 10

## ----------------------------------------------------------
## 5. PARADIGMA 2: Fuzzy (IPACSA existente)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 2: Fuzzy (IPACSA — sem recalcular) ===\n")
## Nota: O IPACSA original foi calculado com 15 indicadores.
## Idealmente deveria ser recalculado com 18, mas a base de regras fuzzy
## demandaria regras para pH, P e K. Mantemos o IPACSA como proxy conservador.
dados$IQS_Fuzzy <- dados$IPACSA
cat("IQS_Fuzzy range:", round(range(dados$IQS_Fuzzy), 2), "\n")

## ----------------------------------------------------------
## 6. PARADIGMA 3: PLS-SEM (4 domínios)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 3: PLS-SEM (4 construtos) ===\n")

## Renomear para PLS
d_pls <- dados
## Usar grep parcial para contornar problemas de encoding nos acentos
pls_rename <- list(
  list(pat="^DMG_norm$",            new="DMG"),
  list(pat="^Estoque.*C_norm$",     new="ECOS"),
  list(pat="^Na_norm$",             new="NAG"),
  list(pat="^ICV.*_norm$",          new="ICV"),
  list(pat="^pH_norm$",             new="PH"),
  list(pat="^P_norm$",              new="PHOS"),
  list(pat="^K_norm$",              new="KPOT"),
  list(pat="^Altura.*norm$",        new="ALT"),
  list(pat="metro.*espiga.*norm$",  new="DESP"),
  list(pat="^Comprimento.*norm$",   new="CESP"),
  list(pat="mero.*plantas.*norm$",  new="STND"),
  list(pat="total.*espigas.*norm$", new="NESP"),
  list(pat="^Peso.*espigas.*norm$", new="PESP"),
  list(pat="^Produtividade_norm$",  new="PROD")
)
for (item in pls_rename) {
  idx <- grep(item$pat, names(d_pls))
  if (length(idx) == 1) {
    cat("  PLS rename:", names(d_pls)[idx], "->", item$new, "\n")
    names(d_pls)[idx] <- item$new
  } else {
    cat("  WARNING: pattern", item$pat, "matched", length(idx), "cols\n")
  }
}
## Verificar
pls_vars <- c("DMG","ECOS","NAG","ICV","PH","PHOS","KPOT",
              "ALT","DESP","CESP","STND","NESP","PESP","PROD")
missing_pls <- setdiff(pls_vars, names(d_pls))
if (length(missing_pls) > 0) {
  cat("  FALTANDO:", paste(missing_pls, collapse=", "), "\n")
  cat("  Colunas disponíveis:", paste(names(d_pls), collapse=", "), "\n")
}

## Modelo de medida: 4 construtos de 1ª ordem
## FISICO (Mode B): DMG, ECOS, NAG, ICV  (excluindo DMP, RMP, Ds por VIF>5)
## QUIMICO (Mode B): PH, PHOS, KPOT      (NOVO)
## AGRO (Mode B): ALT, DESP, CESP, STND
## REND (Mode A): NESP, PESP, PROD       (excluindo NESPC por r=1 com PROD)
mm_pls <- constructs(
  composite("FISICO",  c("DMG","ECOS","NAG","ICV"), weights = mode_B),
  composite("QUIMICO", c("PH","PHOS","KPOT"),       weights = mode_B),
  composite("AGRO",    c("ALT","DESP","CESP","STND"), weights = mode_B),
  composite("REND",    c("NESP","PESP","PROD"),      weights = mode_A)
)

## Modelo estrutural: FISICO→REND, QUIMICO→REND, AGRO→REND
sm_pls <- relationships(
  paths(from = "FISICO",  to = "REND"),
  paths(from = "QUIMICO", to = "REND"),
  paths(from = "AGRO",    to = "REND")
)

pls_model <- estimate_pls(
  data              = d_pls,
  measurement_model = mm_pls,
  structural_model  = sm_pls,
  inner_weights     = path_weighting,
  missing           = mean_replacement,
  missing_value     = NA
)

s_pls <- summary(pls_model)
cat("\n--- Outer Weights ---\n"); print(s_pls$weights)
cat("\n--- VIF ---\n");           print(s_pls$validity$vif_items)
cat("\n--- Path Coefficients ---\n"); print(s_pls$paths)

## Bootstrap
set.seed(42)
boot_pls <- bootstrap_model(pls_model, nboot = 5000, seed = 42)
sb_pls <- summary(boot_pls)
cat("\n--- Bootstrapped Paths ---\n"); print(sb_pls$bootstrapped_paths)

## Estágio 2: IQS via PCA dos 4 construct scores
scores_4 <- pls_model$construct_scores[, c("FISICO","QUIMICO","AGRO","REND")]
cat("\nCorrelação entre construtos:\n"); print(round(cor(scores_4), 4))

pca_iqs <- prcomp(scores_4, center = TRUE, scale. = TRUE)
cat("\nVariância PC:\n"); print(summary(pca_iqs))

iqs_pls_raw <- pca_iqs$x[, 1]
if (cor(iqs_pls_raw, scores_4[, "REND"]) < 0) iqs_pls_raw <- -iqs_pls_raw
dados$IQS_PLS <- (iqs_pls_raw - min(iqs_pls_raw)) / (max(iqs_pls_raw) - min(iqs_pls_raw)) * 10

cat("IQS_PLS range:", round(range(dados$IQS_PLS), 2), "\n")
cat("PC1 loadings: FISICO=", round(pca_iqs$rotation[1,1],4),
    " QUIMICO=", round(pca_iqs$rotation[2,1],4),
    " AGRO=", round(pca_iqs$rotation[3,1],4),
    " REND=", round(pca_iqs$rotation[4,1],4), "\n")

## Salvar path coefficients
path_df <- data.frame(
  From = c("FISICO","QUIMICO","AGRO"),
  To   = rep("REND", 3),
  Coef = round(as.numeric(s_pls$paths[nrow(s_pls$paths)-(2:0), 1]), 4)
)
cat("\nPath coefficients:\n"); print(path_df)

## ----------------------------------------------------------
## 7. PARADIGMA 4: MARS
## ----------------------------------------------------------
cat("\n=== PARADIGMA 4: MARS ===\n")
set.seed(123)
mars_fit <- earth(IPACSA ~ ., data = dados[, c(pred_cols_18, "IPACSA")], degree = 2)
cat("Nterms:", length(mars_fit$selected.terms), "\n")
cat("GCV R²:", round(mars_fit$gcv, 4), "\n")
cat("RSS R²:", round(mars_fit$rsq, 4), "\n")
dados$IQS_MARS <- predict(mars_fit)[, 1]
cat("IQS_MARS range:", round(range(dados$IQS_MARS), 2), "\n")

## MARS variable importance
mars_imp <- evimp(mars_fit)
cat("\n--- MARS importância ---\n"); print(mars_imp)

## ----------------------------------------------------------
## 8. PARADIGMA 5: Random Forest
## ----------------------------------------------------------
cat("\n=== PARADIGMA 5: Random Forest ===\n")
set.seed(42)
rf_model <- randomForest(
  x        = X,
  y        = y,
  ntree    = 500,
  mtry     = ceiling(sqrt(ncol(X))),  # sqrt(18) ≈ 5
  nodesize = 5,
  importance = TRUE,
  keep.inbag = TRUE
)

print(rf_model$call)
cat("OOB % Var explained:", round(100*(1 - rf_model$mse[500]/var(y)), 2), "%\n")
cat("mtry =", ceiling(sqrt(ncol(X))), "(sqrt(18))\n")

dados$IQS_RF <- predict(rf_model, newdata = X)

## Importância
imp <- importance(rf_model)
imp_sd <- rf_model$importanceSD   # SE da permutação por variável (vetor nomeado)
imp_df <- data.frame(
  Variable = rownames(imp),
  IncMSE   = imp[, "%IncMSE"],
  IncMSE_SE = if (is.matrix(imp_sd)) imp_sd[rownames(imp), "%IncMSE"] else imp_sd[rownames(imp)],
  IncPurity = imp[, "IncNodePurity"]
)
imp_df <- imp_df[order(-imp_df$IncMSE), ]

## Definir domínios para cores
dominio <- ifelse(imp_df$Variable %in% c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm",
                                          "Estoque.de.C_norm","Na_norm","ICV..._norm"),
                  "Físico",
           ifelse(imp_df$Variable %in% c("pH_norm","P_norm","K_norm"),
                  "Químico",
           ifelse(imp_df$Variable %in% c("Altura.de.Plantas_norm","Diâmetro.espiga_norm",
                                          "Comprimento.espiga_norm","Número.de.plantas_ha_norm"),
                  "Agronômico", "Rendimento")))
imp_df$Dominio <- dominio

cat("\n--- RF Importância (%IncMSE) ---\n"); print(imp_df)

## OOB convergence
oob_df <- data.frame(ntrees = seq_along(rf_model$mse), OOB_MSE = rf_model$mse)

## CI via individual tree preds
preds_all <- predict(rf_model, newdata = X, predict.all = TRUE)$individual
ci_lower <- apply(preds_all, 1, quantile, probs = 0.025)
ci_upper <- apply(preds_all, 1, quantile, probs = 0.975)
ci_width <- ci_upper - ci_lower
cat("IC 95% largura média:", round(mean(ci_width), 4), "\n")

## SHAP/DALEX
explainer_rf <- DALEX::explain(rf_model, data = X, y = y,
                                label = "Random Forest", verbose = FALSE)
vi_rf <- DALEX::model_parts(explainer_rf, B = 500, type = "difference")

## ----------------------------------------------------------
## 9. Montagem da matriz de IQS
## ----------------------------------------------------------
cat("\n=== MATRIZ DE IQS (5 paradigmas) ===\n")
iqs_mat <- data.frame(
  IQS_ANOVA = dados$IQS_ANOVA,
  IQS_Fuzzy = dados$IQS_Fuzzy,
  IQS_PLS   = dados$IQS_PLS,
  IQS_MARS  = dados$IQS_MARS,
  IQS_RF    = dados$IQS_RF
)
print(summary(iqs_mat))

## ----------------------------------------------------------
## 10. Transformação de Blom
## ----------------------------------------------------------
blom_transform <- function(x) {
  n <- length(x)
  r <- rank(x, ties.method = "average")
  qnorm((r - 0.5) / n)
}
iqs_blom <- as.data.frame(lapply(iqs_mat, blom_transform))
names(iqs_blom) <- paste0(names(iqs_mat), "_blom")

## ----------------------------------------------------------
## 11. CONCORDÂNCIA: Kendall W (H1)
## ----------------------------------------------------------
cat("\n=== CONCORDÂNCIA ===\n")
rank_mat <- apply(iqs_mat, 2, rank)
kw_result <- irr::kendall(rank_mat, correct = TRUE)
cat("Kendall W =", round(kw_result$value, 4), "\n")
cat("Chi² =", round(kw_result$statistic, 2), "\n")
cat("p =", format.pval(kw_result$p.value, digits = 4), "\n")

## Permutation test
set.seed(42)
n_perm <- 10000
W_obs <- kw_result$value
W_perm <- numeric(n_perm)
for (i in seq_len(n_perm)) {
  perm_mat <- apply(rank_mat, 2, sample)
  W_perm[i] <- irr::kendall(perm_mat, correct = FALSE)$value
}
p_perm <- mean(W_perm >= W_obs)
cat("Permutation p =", format.pval(p_perm, digits = 4), "\n")

## ----------------------------------------------------------
## 12. ICC(2,1)
## ----------------------------------------------------------
icc_result <- psych::ICC(iqs_blom)
cat("\n--- ICC(2,1) ---\n")
print(icc_result$results[2, ])

## ----------------------------------------------------------
## 13. Gwet AC2
## ----------------------------------------------------------
quintile_mat <- apply(iqs_mat, 2, function(x) {
  cut(x, breaks = quantile(x, probs = 0:5/5), include.lowest = TRUE, labels = 1:5)
})
quintile_df <- as.data.frame(quintile_mat)
gwet_result <- irrCAC::gwet.ac1.raw(quintile_df)
cat("\n--- Gwet AC1 ---\n"); print(gwet_result$est)

## ----------------------------------------------------------
## 14. Spearman matrix
## ----------------------------------------------------------
spearman_mat <- cor(iqs_mat, method = "spearman")
cat("\n--- Spearman rho ---\n"); print(round(spearman_mat, 3))

## ----------------------------------------------------------
## 15. Teste H2 (NL > Lin)
## ----------------------------------------------------------
fisher_z <- function(r) 0.5 * log((1 + r)/(1 - r))

nl_names <- c("IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")
lin_name <- "IQS_ANOVA"

rho_nl_nl <- c()
for (i in 1:(length(nl_names)-1)) {
  for (j in (i+1):length(nl_names)) {
    rho_nl_nl <- c(rho_nl_nl, spearman_mat[nl_names[i], nl_names[j]])
  }
}
rho_lin_nl <- spearman_mat[lin_name, nl_names]

cat("\nNL-NL mean rho:", round(mean(rho_nl_nl), 3), "\n")
cat("Lin-NL mean rho:", round(mean(rho_lin_nl), 3), "\n")

z_nl_nl <- fisher_z(rho_nl_nl)
z_lin_nl <- fisher_z(rho_lin_nl)
wilcox_h2 <- wilcox.test(z_nl_nl, z_lin_nl, alternative = "greater")
cat("H2 Wilcoxon: W =", wilcox_h2$statistic, " p =",
    format.pval(wilcox_h2$p.value, digits = 4), "\n")

## ----------------------------------------------------------
## 16. Bland-Altman
## ----------------------------------------------------------
paradigm_names <- names(iqs_mat)
ba_results <- list()
for (i in 1:(length(paradigm_names)-1)) {
  for (j in (i+1):length(paradigm_names)) {
    a <- iqs_blom[, i]; b <- iqs_blom[, j]
    diff_ab <- a - b
    ba_results[[paste(paradigm_names[i], paradigm_names[j], sep="_vs_")]] <- data.frame(
      mean_diff = mean(diff_ab), sd_diff = sd(diff_ab),
      lower_loa = mean(diff_ab) - 1.96*sd(diff_ab),
      upper_loa = mean(diff_ab) + 1.96*sd(diff_ab)
    )
  }
}
ba_summary <- do.call(rbind, ba_results)
cat("\n--- Bland-Altman ---\n"); print(round(ba_summary, 4))

## ----------------------------------------------------------
## 17. Meta-IQS Borda
## ----------------------------------------------------------
rank_mat_orig <- apply(iqs_mat, 2, rank)
dados$MetaIQS_Borda <- rowMeans(rank_mat_orig)
meta_cor <- cor(dados$MetaIQS_Borda, iqs_mat, method = "spearman")
cat("\nBorda vs paradigmas:", round(meta_cor, 3), "\n")

## ----------------------------------------------------------
## 18. Ranqueamento por combo manejo–cobertura
## ----------------------------------------------------------
cat("\n=== RANKING POR COMBO ===\n")
combo <- paste(dados$Parcela, dados$Cultura, sep = "_")
dados$Combo <- combo

rank_agg <- aggregate(
  cbind(IQS_ANOVA, IQS_Fuzzy, IQS_PLS, IQS_MARS, IQS_RF, MetaIQS_Borda) ~ Combo,
  data = dados, FUN = mean
)
## Rank each paradigm
for (col in c("IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")) {
  rank_agg[, paste0("Rank_", col)] <- rank(-rank_agg[, col])
}
rank_agg <- rank_agg[order(-rank_agg$MetaIQS_Borda), ]
cat("\n--- Ranqueamento ---\n"); print(rank_agg)

## ----------------------------------------------------------
## 19. MONTE CARLO (sensibilidade amostral)
## ----------------------------------------------------------
cat("\n=== MONTE CARLO ===\n")
dados$Celula <- paste(dados$Parcela, dados$Cultura, sep = "_")
celulas <- unique(dados$Celula)

compute_iqs_anova_mc <- function(df, pc18) {
  pca_r <- prcomp(df[, pc18], center=TRUE, scale.=TRUE)
  ev <- pca_r$sdev^2
  nc <- max(1, sum(ev > 1))
  w <- ev[1:nc] / sum(ev[1:nc])
  sc <- pca_r$x[, 1:nc, drop=FALSE]
  iqs <- as.numeric(sc %*% w)
  (iqs - min(iqs)) / (max(iqs) - min(iqs) + 1e-10) * 10
}

sample_sizes <- c(15, 30, 50, 75, 100)
n_iter <- 200
mc_results <- data.frame()

for (n_target in sample_sizes) {
  cat("MC n =", n_target, "...")
  W_values <- numeric(n_iter)
  for (iter in seq_len(n_iter)) {
    set.seed(1000*n_target + iter)
    n_per_cell <- max(1, floor(n_target / length(celulas)))
    remainder  <- n_target - n_per_cell * length(celulas)
    idx_sample <- c()
    for (cel in celulas) {
      cel_idx <- which(dados$Celula == cel)
      n_this  <- min(n_per_cell, length(cel_idx))
      idx_sample <- c(idx_sample, sample(cel_idx, n_this))
    }
    if (remainder > 0) {
      remaining_idx <- setdiff(seq_len(nrow(dados)), idx_sample)
      if (length(remaining_idx) >= remainder)
        idx_sample <- c(idx_sample, sample(remaining_idx, remainder))
    }
    sub_df <- dados[idx_sample, ]
    iqs <- tryCatch({
      data.frame(
        ANOVA = compute_iqs_anova_mc(sub_df, pred_cols_18),
        Fuzzy = sub_df$IPACSA,
        PLS   = (sub_df$COS + sub_df$ARQ + sub_df$STAND) / 3,
        MARS  = predict(earth(IPACSA ~ ., data = sub_df[,c(pred_cols_18,"IPACSA")], degree=2))[,1],
        RF    = predict(randomForest(x=sub_df[,pred_cols_18], y=sub_df$IPACSA,
                                      ntree=200, mtry=ceiling(sqrt(18)), nodesize=5),
                        newdata=sub_df[,pred_cols_18])
      )
    }, error = function(e) NULL)
    if (is.null(iqs) || any(is.na(iqs))) { W_values[iter] <- NA; next }
    rm <- apply(iqs, 2, rank)
    W_values[iter] <- tryCatch(irr::kendall(rm, correct=FALSE)$value, error=function(e) NA)
  }
  valid <- !is.na(W_values)
  cat(" mean W =", round(mean(W_values[valid]), 4),
      " sd =", round(sd(W_values[valid]), 4), "\n")
  mc_results <- rbind(mc_results, data.frame(
    n = n_target, iter = seq_len(n_iter), W = W_values
  ))
}

## MC Summary
mc_summary <- aggregate(W ~ n, data = mc_results, FUN = function(x) {
  x <- x[!is.na(x)]
  c(mean = mean(x), sd = sd(x), median = median(x),
    q025 = quantile(x, 0.025), q975 = quantile(x, 0.975), n_valid = length(x))
})
mc_summary <- cbind(mc_summary[,1,drop=FALSE], as.data.frame(mc_summary$W))
names(mc_summary) <- c("n","mean_W","sd_W","median_W","q025","q975","n_valid")
cat("\n--- MC Summary ---\n"); print(round(mc_summary, 4))

## H3: GLS
mc_summary$log_n <- log(mc_summary$n)
gls_fit <- tryCatch(
  nlme::gls(mean_W ~ log_n, data = mc_summary,
            weights = varFixed(~ (sd_W^2 / n_valid))),
  error = function(e) lm(mean_W ~ log_n, data = mc_summary)
)
cat("\n--- H3: W ~ log(n) ---\n"); print(summary(gls_fit))
beta1 <- coef(gls_fit)["log_n"]
cat("beta1 =", round(beta1, 4), "\n")

## ----------------------------------------------------------
## 20. FIGURAS
## ----------------------------------------------------------
cat("\n=== GERANDO FIGURAS ===\n")
dir.create("../3-FIGURAS", showWarnings = FALSE)

## 20.1 RF importance → gerada por 12_figuras_rachuras.R (ggpattern)
nice_names <- c(
  "DMG_norm"="DMG", "DMP_norm"="DMP", "RMP_norm"="RMP",
  "Densidade_norm"="Ds", "Estoque.de.C_norm"="COS", "Na_norm"="Na",
  "ICV..._norm"="ICV", "pH_norm"="pH", "P_norm"="P", "K_norm"="K",
  "Altura.de.Plantas_norm"="ALT", "Diâmetro.espiga_norm"="DESP",
  "Comprimento.espiga_norm"="CESP", "Número.de.plantas_ha_norm"="STAND",
  "N.total.de.espigas_ha_norm"="NESP",
  "N.de.espigas.comerciais_ha_norm"="NESPC",
  "Peso.de.espigas.comerciais_ha_norm"="PESP",
  "Produtividade_norm"="PROD"
)
imp_df$Label <- nice_names[imp_df$Variable]
cat("fig_rf_importance.png → ver 12_figuras_rachuras.R\n")

## 20.2 Heatmap Spearman
spearman_df <- reshape2::melt(spearman_mat)
names(spearman_df) <- c("Var1","Var2","rho")
spearman_df$label <- round(spearman_df$rho, 3)
## Nomes curtos
short <- c("IQS_ANOVA"="ANOVA","IQS_Fuzzy"="Fuzzy","IQS_PLS"="PLS-SEM",
           "IQS_MARS"="MARS","IQS_RF"="RF")
spearman_df$Var1 <- short[as.character(spearman_df$Var1)]
spearman_df$Var2 <- short[as.character(spearman_df$Var2)]

p_heat <- ggplot(spearman_df, aes(Var1, Var2, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 3.5) +
  scale_fill_gradient2(low = "#d95f02", mid = "white", high = "#1b9e77",
                       midpoint = 0.8, limits = c(0.5, 1), name = expression(rho[S])) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../3-FIGURAS/fig_heatmap_spearman.png", p_heat, width = 5.5, height = 4.5, dpi = 300)
cat("fig_heatmap_spearman.png salva\n")

## 20.3 Bland-Altman panel (3 pares representativos)
make_ba_plot <- function(name_a, name_b, title_str) {
  a <- iqs_blom[, paste0(name_a, "_blom")]
  b <- iqs_blom[, paste0(name_b, "_blom")]
  diff_ab <- a - b; mean_ab <- (a + b) / 2
  md <- mean(diff_ab); sdd <- sd(diff_ab)
  df_ba <- data.frame(mean = mean_ab, diff = diff_ab)
  ggplot(df_ba, aes(x = mean, y = diff)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = md, color = "black", linetype = 1) +
    geom_hline(yintercept = md + 1.96*sdd, color = "red", linetype = 2) +
    geom_hline(yintercept = md - 1.96*sdd, color = "red", linetype = 2) +
    labs(title = title_str, x = "Média", y = "Diferença") +
    theme_minimal(base_size = 10)
}

p_ba1 <- make_ba_plot("IQS_Fuzzy", "IQS_RF", "Fuzzy vs RF")
p_ba2 <- make_ba_plot("IQS_PLS", "IQS_RF", "PLS-SEM vs RF")
p_ba3 <- make_ba_plot("IQS_ANOVA", "IQS_Fuzzy", "ANOVA vs Fuzzy")
p_ba <- gridExtra::grid.arrange(p_ba1, p_ba2, p_ba3, ncol = 3)
ggsave("../3-FIGURAS/fig_blandaltman_panel.png", p_ba, width = 12, height = 4, dpi = 300)
cat("fig_blandaltman_panel.png salva\n")

## 20.4 Scatter 1:1 (vs RF)
make_scatter <- function(name_y, label_y) {
  df <- data.frame(RF = iqs_blom$IQS_RF_blom, Y = iqs_blom[, paste0(name_y, "_blom")])
  rho_sq <- round(cor(df$RF, df$Y, method="spearman")^2, 3)
  ggplot(df, aes(x = RF, y = Y)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray50") +
    geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 0.8) +
    annotate("text", x = -2, y = 2.2, label = paste0("ρ²=", rho_sq), size = 3.5) +
    labs(x = "RF (Blom)", y = paste0(label_y, " (Blom)")) +
    theme_minimal(base_size = 10)
}

p_s1 <- make_scatter("IQS_ANOVA", "ANOVA")
p_s2 <- make_scatter("IQS_Fuzzy", "Fuzzy")
p_s3 <- make_scatter("IQS_PLS", "PLS-SEM")
p_s4 <- make_scatter("IQS_MARS", "MARS")
p_scatter <- gridExtra::grid.arrange(p_s1, p_s2, p_s3, p_s4, ncol = 2)
ggsave("../3-FIGURAS/fig_scatter_1to1.png", p_scatter, width = 8, height = 7, dpi = 300)
cat("fig_scatter_1to1.png salva\n")

## 20.5 Boxplot combo → gerada por 12_figuras_rachuras.R (ggpattern)
cat("fig_boxplot_combo.png → ver 12_figuras_rachuras.R\n")

## 20.6 PDP top 4
library(pdp)
top4 <- head(imp_df$Variable, 4)
pdp_list <- list()
for (v in top4) {
  pd <- partial(rf_model, pred.var = v, train = X)
  pdp_list[[v]] <- pd
}

p_pdp <- list()
for (v in top4) {
  pd <- pdp_list[[v]]
  p_pdp[[v]] <- ggplot(pd, aes_string(x = v, y = "yhat")) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_rug(data = X, aes_string(x = v), inherit.aes = FALSE, alpha = 0.3) +
    labs(x = nice_names[v], y = "Efeito parcial") +
    theme_minimal(base_size = 10)
}

p_pdp_all <- do.call(gridExtra::grid.arrange, c(p_pdp, ncol = 2))
ggsave("../3-FIGURAS/fig_pdp_top4.png", p_pdp_all, width = 8, height = 6, dpi = 300)
cat("fig_pdp_top4.png salva\n")

## 20.7 Monte Carlo sensitivity
mc_valid <- mc_results[!is.na(mc_results$W), ]
mc_valid$n_f <- factor(mc_valid$n)
W_full <- kw_result$value

p_mc <- ggplot(mc_valid, aes(x = n_f, y = W)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  geom_hline(yintercept = W_full, linetype = 3, color = "black") +
  labs(x = "Tamanho amostral (n)", y = "Kendall W") +
  theme_minimal(base_size = 11)
ggsave("../3-FIGURAS/fig_mc_sensitivity.png", p_mc, width = 7, height = 4.5, dpi = 300)
cat("fig_mc_sensitivity.png salva\n")

## ----------------------------------------------------------
## 21. EXPORTAR RESULTADOS
## ----------------------------------------------------------
cat("\n=== EXPORTANDO ===\n")

## Dataset completo com químicos
write.csv(dados[, c("Parcela","Cultura","Combo",
                     pred_cols_18,
                     "IPACSA","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF",
                     "MetaIQS_Borda",
                     "pH_0_10","P_0_10","K_0_10")],
          file = "../1-DADOS_BRUTOS/IQS_todos_paradigmas_18ind.csv", row.names = FALSE)

write.csv(round(spearman_mat, 4), file = "../1-DADOS_BRUTOS/Spearman_matrix_18ind.csv")
write.csv(round(ba_summary, 4), file = "../1-DADOS_BRUTOS/BlandAltman_summary_18ind.csv")
write.csv(imp_df, file = "../1-DADOS_BRUTOS/RF_importancia_18ind.csv", row.names = FALSE)
write.csv(rank_agg, file = "../1-DADOS_BRUTOS/Ranking_combo_18ind.csv", row.names = FALSE)
write.csv(round(mc_summary, 4), file = "../1-DADOS_BRUTOS/MC_sumario_18ind.csv", row.names = FALSE)

## PLS path coefficients
write.csv(path_df, file = "../1-DADOS_BRUTOS/PLS_path_coefficients_18ind.csv", row.names = FALSE)

cat("\n=====================================================\n")
cat("  RE-ANÁLISE CONCLUÍDA COM 18 INDICADORES\n")
cat("=====================================================\n")

## Resumo para atualização do manuscrito
cat("\n=== RESUMO PARA MANUSCRITO ===\n")
cat("n =", nrow(dados), "| p =", length(pred_cols_18), "| mtry =", ceiling(sqrt(18)), "\n")
cat("RF OOB R²:", round(100*(1-rf_model$mse[500]/var(y)), 2), "%\n")
cat("Kendall W =", round(kw_result$value, 4), "\n")
cat("ICC(2,1) =", round(icc_result$results[2, "ICC"], 4), "\n")
cat("Gwet AC1 =", round(gwet_result$est$coeff.val, 4), "\n")
cat("Spearman matrix:\n"); print(round(spearman_mat, 3))
cat("\nBland-Altman:\n"); print(round(ba_summary, 3))
cat("\nBorda vs paradigmas:", round(meta_cor, 3), "\n")
cat("H2 p =", format.pval(wilcox_h2$p.value, digits = 4), "\n")
cat("H3 beta1 =", round(beta1, 4), "\n")
cat("\nRanking:\n"); print(rank_agg[, c("Combo","Rank_IQS_ANOVA","Rank_IQS_Fuzzy",
                                        "Rank_IQS_PLS","Rank_IQS_MARS","Rank_IQS_RF",
                                        "MetaIQS_Borda")])
