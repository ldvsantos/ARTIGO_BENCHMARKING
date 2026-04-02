## ============================================================
## 12_reanalise_34ind.R
## Re-análise completa: 5 paradigmas + concordância + MC
## 34 indicadores (18 originais + 16 novos da Sara 2019)
## ============================================================

cat("=====================================================\n")
cat("  RE-ANÁLISE COMPLETA (34 indicadores)\n")
cat("  18 originais + 16 Sara 2019 (17º ano)\n")
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
sara  <- read.csv(file.path("..","1-DADOS_BRUTOS","Sara2019_medias_tratamento.csv"))

cat("Dataset original:", nrow(dados), "obs ×", ncol(dados), "cols\n")
cat("Químicos Jaelson:", nrow(quim), "cells\n")
cat("Sara tratamentos:", nrow(sara), "cells ×", ncol(sara), "cols\n")

## Merge Jaelson (pH, P, K — 2022)
dados <- merge(dados, quim[, c("Parcela","Cultura","pH_0_10","P_0_10","K_0_10")],
               by = c("Parcela","Cultura"), all.x = TRUE)

## Merge Sara (20 variáveis — 2017, médias por tratamento)
sara_vars_raw <- c("Ca_cmolc_dm3","Mg_cmolc_dm3","Al_cmolc_dm3","HAl_cmolc_dm3",
                    "SB_cmolc_dm3","T_cmolc_dm3","V_pct","m_pct","MOS_g_kg",
                    "EstN_kg_ha","MI_pct","MA_pct","VIB_mm_h","RP_KPa","AD_PT",
                    "qMIC","qCO2","CCO2_g_kg","CMIC_ug_g","NMIC_mg_kg")
dados <- merge(dados, sara[, c("Parcela","Cultura", sara_vars_raw)],
               by = c("Parcela","Cultura"), all.x = TRUE)

cat("Pós-merge:", nrow(dados), "obs ×", ncol(dados), "cols\n")
stopifnot(nrow(dados) == 108)
stopifnot(sum(is.na(dados$Ca_cmolc_dm3)) == 0)
cat("Merge OK. NAs em Sara vars:", sum(is.na(dados[, sara_vars_raw])), "\n")

## ----------------------------------------------------------
## 2. Normalização 0-10
## ----------------------------------------------------------
cat("\n--- 2. Normalização 0-10 ---\n")
normalize_0_10 <- function(x) {
  rng <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
  if (rng == 0) return(rep(5, length(x)))
  (x - min(x, na.rm=TRUE)) / rng * 10
}

## Jaelson (2022)
dados$pH_norm  <- normalize_0_10(dados$pH_0_10)
dados$P_norm   <- normalize_0_10(dados$P_0_10)
dados$K_norm   <- normalize_0_10(dados$K_0_10)

## Sara (2017) — 16 variáveis análise (excluindo SB, H+Al, V%, m% por colinearidade)
sara_analysis_raw <- c("Ca_cmolc_dm3","Mg_cmolc_dm3","Al_cmolc_dm3","T_cmolc_dm3",
                        "MOS_g_kg","EstN_kg_ha","MI_pct","MA_pct","VIB_mm_h",
                        "RP_KPa","AD_PT","qMIC","qCO2","CCO2_g_kg","CMIC_ug_g",
                        "NMIC_mg_kg")
sara_norm_names <- c("Ca_norm","Mg_norm","Al_norm","CTC_norm","MOS_norm","EstN_norm",
                      "MI_norm","MA_norm","VIB_norm","RP_norm","AD_PT_norm",
                      "qMIC_norm","qCO2_norm","CCO2_norm","CMIC_norm","NMIC_norm")

for (i in seq_along(sara_analysis_raw)) {
  dados[[sara_norm_names[i]]] <- normalize_0_10(dados[[sara_analysis_raw[i]]])
  cat(sprintf("  %-12s: range [%5.2f, %5.2f]\n",
              sara_norm_names[i],
              min(dados[[sara_norm_names[i]]]),
              max(dados[[sara_norm_names[i]]])))
}

## ----------------------------------------------------------
## 3. Definição dos 34 indicadores normalizados
## ----------------------------------------------------------
pred_cols_orig <- c(
  ## Físico original (7)
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  ## Químico Jaelson 2022 (3)
  "pH_norm", "P_norm", "K_norm",
  ## Agronômico (4)
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  ## Rendimento (4)
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

pred_cols_sara <- sara_norm_names  # 16 novos

pred_cols_34 <- c(pred_cols_orig, pred_cols_sara)

cat("\n--- Indicadores ---\n")
cat("Originais:", length(pred_cols_orig), "\n")
cat("Sara novos:", length(pred_cols_sara), "\n")
cat("Total:", length(pred_cols_34), "\n")
cat("n/p =", round(108/length(pred_cols_34), 2), "\n")

X <- dados[, pred_cols_34]
y <- dados$IPACSA  # Fuzzy IQS como referência para RF/MARS

## Domínios para cores/classificação
dominio_map <- c(
  ## Físico original
  "DMG_norm"="Físico", "DMP_norm"="Físico", "RMP_norm"="Físico",
  "Densidade_norm"="Físico", "Estoque.de.C_norm"="Físico",
  "Na_norm"="Físico", "ICV..._norm"="Físico",
  ## Físico Sara
  "MI_norm"="Físico", "MA_norm"="Físico", "VIB_norm"="Físico",
  "RP_norm"="Físico", "AD_PT_norm"="Físico",
  ## Químico
  "pH_norm"="Químico", "P_norm"="Químico", "K_norm"="Químico",
  "Ca_norm"="Químico", "Mg_norm"="Químico", "Al_norm"="Químico",
  "CTC_norm"="Químico", "MOS_norm"="Químico", "EstN_norm"="Químico",
  ## Microbiológico
  "qMIC_norm"="Microbiológico", "qCO2_norm"="Microbiológico",
  "CCO2_norm"="Microbiológico", "CMIC_norm"="Microbiológico",
  "NMIC_norm"="Microbiológico",
  ## Agronômico
  "Altura.de.Plantas_norm"="Agronômico", "Diâmetro.espiga_norm"="Agronômico",
  "Comprimento.espiga_norm"="Agronômico", "Número.de.plantas_ha_norm"="Agronômico",
  ## Rendimento
  "N.total.de.espigas_ha_norm"="Rendimento",
  "N.de.espigas.comerciais_ha_norm"="Rendimento",
  "Peso.de.espigas.comerciais_ha_norm"="Rendimento",
  "Produtividade_norm"="Rendimento"
)

## Nomes amigáveis para figuras
nice_names_34 <- c(
  "DMG_norm"="DMG", "DMP_norm"="DMP", "RMP_norm"="RMP",
  "Densidade_norm"="Ds", "Estoque.de.C_norm"="COS", "Na_norm"="Na",
  "ICV..._norm"="ICV",
  "pH_norm"="pH", "P_norm"="P", "K_norm"="K",
  "Ca_norm"="Ca", "Mg_norm"="Mg", "Al_norm"="Al",
  "CTC_norm"="CTC", "MOS_norm"="MOS", "EstN_norm"="EstN",
  "MI_norm"="MI", "MA_norm"="MA", "VIB_norm"="VIB",
  "RP_norm"="RP", "AD_PT_norm"="AD/PT",
  "qMIC_norm"="qMIC", "qCO2_norm"="qCO2", "CCO2_norm"="CCO2",
  "CMIC_norm"="CMIC", "NMIC_norm"="NMIC",
  "Altura.de.Plantas_norm"="ALT", "Diâmetro.espiga_norm"="DESP",
  "Comprimento.espiga_norm"="CESP", "Número.de.plantas_ha_norm"="STAND",
  "N.total.de.espigas_ha_norm"="NESP",
  "N.de.espigas.comerciais_ha_norm"="NESPC",
  "Peso.de.espigas.comerciais_ha_norm"="PESP",
  "Produtividade_norm"="PROD"
)

## ----------------------------------------------------------
## 4. PARADIGMA 1: ANOVA/MDS (PCA-weighted)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 1: ANOVA/MDS ===\n")
## PCA com 34 indicadores (handles collinearity via eigenvalue decomposition)
pca_res <- prcomp(X, center = TRUE, scale. = TRUE)
eigvals <- pca_res$sdev^2
n_comp  <- sum(eigvals > 1)
cat("PCA: componentes retidos =", n_comp, "de", length(eigvals), "\n")
cat("Eigenvalues > 1:", round(eigvals[1:min(n_comp, 10)], 3), "\n")
cat("Variância acumulada:", round(cumsum(eigvals/sum(eigvals))[1:n_comp]*100, 1), "%\n")

weights_pca <- eigvals[1:n_comp] / sum(eigvals[1:n_comp])
scores_pca  <- pca_res$x[, 1:n_comp]
iqs_anova_raw <- as.numeric(scores_pca %*% weights_pca)
dados$IQS_ANOVA <- (iqs_anova_raw - min(iqs_anova_raw)) /
                   (max(iqs_anova_raw) - min(iqs_anova_raw)) * 10

cat("IQS_ANOVA range:", round(range(dados$IQS_ANOVA), 2), "\n")

## PCA loadings para referência
cat("\nPC1-3 loadings (top 10):\n")
ld <- pca_res$rotation[, 1:3]
rownames(ld) <- nice_names_34[rownames(ld)]
top10 <- head(order(-abs(ld[,1])), 10)
print(round(ld[top10, ], 3))

## ----------------------------------------------------------
## 5. PARADIGMA 2: Fuzzy (IPACSA existente — 15 indicadores)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 2: Fuzzy (IPACSA — 15 indicadores originais) ===\n")
cat("NOTA: IPACSA não recalculado porque a base de regras fuzzy é específica\n")
cat("para 15 indicadores. Recálculo exigiria 16 novas regras de pertinência.\n")
dados$IQS_Fuzzy <- dados$IPACSA
cat("IQS_Fuzzy range:", round(range(dados$IQS_Fuzzy), 2), "\n")

## ----------------------------------------------------------
## 6. PARADIGMA 3: PLS-SEM (5 construtos)
## ----------------------------------------------------------
cat("\n=== PARADIGMA 3: PLS-SEM (5 construtos, 34 indicadores) ===\n")

## Renomear para PLS (alias curtos para evitar problemas de encoding)
d_pls <- dados
pls_rename <- list(
  ## Físico original
  list(pat="^DMG_norm$",            new="DMG"),
  list(pat="^Estoque.*C_norm$",     new="ECOS"),
  list(pat="^Na_norm$",             new="NAG"),
  list(pat="^ICV.*_norm$",          new="ICV"),
  ## Físico Sara
  list(pat="^MI_norm$",             new="MI"),
  list(pat="^MA_norm$",             new="MA"),
  list(pat="^VIB_norm$",            new="VIB"),
  list(pat="^RP_norm$",             new="RP"),
  list(pat="^AD_PT_norm$",          new="ADPT"),
  ## Químico
  list(pat="^pH_norm$",             new="PH"),
  list(pat="^P_norm$",              new="PHOS"),
  list(pat="^K_norm$",              new="KPOT"),
  list(pat="^Ca_norm$",             new="CA"),
  list(pat="^Mg_norm$",             new="MG"),
  list(pat="^CTC_norm$",            new="CTC"),
  list(pat="^MOS_norm$",            new="MOS"),
  list(pat="^EstN_norm$",           new="ESTN"),
  ## Microbiológico
  list(pat="^qMIC_norm$",           new="QMIC"),
  list(pat="^qCO2_norm$",           new="QCO2"),
  list(pat="^CCO2_norm$",           new="CCO2"),
  list(pat="^CMIC_norm$",           new="CMIC"),
  list(pat="^NMIC_norm$",           new="NMIC"),
  ## Agronômico
  list(pat="^Altura.*norm$",        new="ALT"),
  list(pat="metro.*espiga.*norm$",  new="DESP"),
  list(pat="^Comprimento.*norm$",   new="CESP"),
  list(pat="mero.*plantas.*norm$",  new="STND"),
  ## Rendimento
  list(pat="total.*espigas.*norm$", new="NESP"),
  list(pat="^Peso.*espigas.*norm$", new="PESP"),
  list(pat="^Produtividade_norm$",  new="PROD")
)

for (item in pls_rename) {
  idx <- grep(item$pat, names(d_pls))
  if (length(idx) == 1) {
    names(d_pls)[idx] <- item$new
  } else if (length(idx) == 0) {
    cat("  WARNING: pattern", item$pat, "not found\n")
  } else {
    cat("  WARNING: pattern", item$pat, "matched", length(idx), "cols\n")
  }
}

## VIF pré-screening (excluir VIF > 5 iterativamente)
compute_vif <- function(data, vars) {
  vif_vals <- numeric(length(vars))
  names(vif_vals) <- vars
  for (v in vars) {
    others <- setdiff(vars, v)
    if (length(others) < 1) { vif_vals[v] <- 1; next }
    r2 <- tryCatch({
      fit <- lm(reformulate(others, v), data = data)
      summary(fit)$r.squared
    }, error = function(e) 0.999)
    vif_vals[v] <- 1 / (1 - r2)
  }
  vif_vals
}

## Definir candidatos por construto antes de VIF screening
fisico_cands  <- c("DMG","ECOS","NAG","ICV","MI","MA","VIB","RP","ADPT")
quimico_cands <- c("PH","PHOS","KPOT","CA","MG","CTC","MOS","ESTN")
micro_cands   <- c("QMIC","QCO2","CCO2","CMIC","NMIC")
agro_cands    <- c("ALT","DESP","CESP","STND")
rend_cands    <- c("NESP","PESP","PROD")

## VIF screening por construto
screen_vif <- function(data, vars, threshold = 5, label = "") {
  if (length(vars) <= 1) return(vars)
  current <- vars
  repeat {
    vif <- compute_vif(data, current)
    max_vif <- max(vif)
    if (max_vif <= threshold) break
    dropped <- names(which.max(vif))
    cat(sprintf("  [%s] Drop %s (VIF=%.1f)\n", label, dropped, max_vif))
    current <- setdiff(current, dropped)
    if (length(current) <= 1) break
  }
  cat(sprintf("  [%s] Retained: %s\n", label, paste(current, collapse=", ")))
  current
}

cat("\n--- VIF screening (threshold=5) ---\n")
fis_ok  <- screen_vif(d_pls, fisico_cands,  5, "FISICO")
qui_ok  <- screen_vif(d_pls, quimico_cands, 5, "QUIMICO")
mic_ok  <- screen_vif(d_pls, micro_cands,   5, "MICRO")
agr_ok  <- screen_vif(d_pls, agro_cands,    5, "AGRO")
ren_ok  <- screen_vif(d_pls, rend_cands,    5, "REND")

cat("\nIndicadores PLS-SEM após VIF:\n")
cat("  FISICO:", paste(fis_ok, collapse=", "), "\n")
cat("  QUIMICO:", paste(qui_ok, collapse=", "), "\n")
cat("  MICRO:", paste(mic_ok, collapse=", "), "\n")
cat("  AGRO:", paste(agr_ok, collapse=", "), "\n")
cat("  REND:", paste(ren_ok, collapse=", "), "\n")
cat("  Total PLS:", length(fis_ok)+length(qui_ok)+length(mic_ok)+length(agr_ok)+length(ren_ok), "\n")

## Montar modelo PLS
## FISICO, QUIMICO, MICRO = Mode B (formativo); AGRO = Mode B; REND = Mode A (reflexivo)
mm_pls <- constructs(
  composite("FISICO",  fis_ok, weights = mode_B),
  composite("QUIMICO", qui_ok, weights = mode_B),
  composite("MICRO",   mic_ok, weights = mode_B),
  composite("AGRO",    agr_ok, weights = mode_B),
  composite("REND",    ren_ok, weights = mode_A)
)

## Estrutural: todos os domínios do solo → REND
sm_pls <- relationships(
  paths(from = "FISICO",  to = "REND"),
  paths(from = "QUIMICO", to = "REND"),
  paths(from = "MICRO",   to = "REND"),
  paths(from = "AGRO",    to = "REND")
)

pls_model <- tryCatch(
  estimate_pls(
    data              = d_pls,
    measurement_model = mm_pls,
    structural_model  = sm_pls,
    inner_weights     = path_weighting,
    missing           = mean_replacement,
    missing_value     = NA
  ),
  error = function(e) { cat("PLS-SEM FAILED:", e$message, "\n"); NULL }
)

if (!is.null(pls_model)) {
  s_pls <- summary(pls_model)
  cat("\n--- Outer Weights ---\n"); print(s_pls$weights)
  cat("\n--- VIF Items ---\n");     print(s_pls$validity$vif_items)
  cat("\n--- Path Coefficients ---\n"); print(s_pls$paths)
  
  ## Bootstrap
  set.seed(42)
  boot_pls <- bootstrap_model(pls_model, nboot = 500, seed = 42)
  sb_pls <- summary(boot_pls)
  cat("\n--- Bootstrapped Paths ---\n"); print(sb_pls$bootstrapped_paths)
  
  ## Estágio 2: IQS via PCA dos 5 construct scores
  constructs_pls <- c("FISICO","QUIMICO","MICRO","AGRO","REND")
  scores_5 <- pls_model$construct_scores[, constructs_pls]
  cat("\nCorrelação entre construtos:\n"); print(round(cor(scores_5), 3))
  
  pca_iqs <- prcomp(scores_5, center = TRUE, scale. = TRUE)
  cat("\nVariância PC:\n"); print(summary(pca_iqs))
  
  iqs_pls_raw <- pca_iqs$x[, 1]
  if (cor(iqs_pls_raw, scores_5[, "REND"]) < 0) iqs_pls_raw <- -iqs_pls_raw
  dados$IQS_PLS <- (iqs_pls_raw - min(iqs_pls_raw)) / (max(iqs_pls_raw) - min(iqs_pls_raw)) * 10
  
  cat("IQS_PLS range:", round(range(dados$IQS_PLS), 2), "\n")
  cat("PC1 loadings:", round(pca_iqs$rotation[,1], 3), "\n")
  
  ## Salvar paths (filtrar R²/AdjR² — manter só linhas que são construtos)
  path_rows <- c("FISICO","QUIMICO","MICRO","AGRO")
  path_vals <- as.numeric(s_pls$paths[path_rows, "REND"])
  path_df <- data.frame(
    From = path_rows,
    To   = rep("REND", length(path_rows)),
    Coef = path_vals
  )
  cat("\nPath coefficients:\n"); print(path_df)
  
} else {
  cat("PLS-SEM falhou. Usando IQS_PLS = IQS_ANOVA como fallback.\n")
  dados$IQS_PLS <- dados$IQS_ANOVA
  path_df <- data.frame(From = "N/A", To = "N/A", Coef = NA)
}

## ----------------------------------------------------------
## 7. PARADIGMA 4: MARS
## ----------------------------------------------------------
cat("\n=== PARADIGMA 4: MARS (34 indicadores) ===\n")
set.seed(123)
mars_fit <- earth(IPACSA ~ ., data = dados[, c(pred_cols_34, "IPACSA")], degree = 2)
cat("Nterms:", length(mars_fit$selected.terms), "\n")
cat("GCV R²:", round(mars_fit$gcv, 4), "\n")
cat("RSS R²:", round(mars_fit$rsq, 4), "\n")
dados$IQS_MARS <- predict(mars_fit)[, 1]
cat("IQS_MARS range:", round(range(dados$IQS_MARS), 2), "\n")

mars_imp <- evimp(mars_fit)
cat("\n--- MARS importância ---\n"); print(mars_imp)

## ----------------------------------------------------------
## 8. PARADIGMA 5: Random Forest
## ----------------------------------------------------------
cat("\n=== PARADIGMA 5: Random Forest (34 indicadores) ===\n")
set.seed(42)
mtry_val <- ceiling(sqrt(ncol(X)))  # sqrt(34) ≈ 6
cat("mtry =", mtry_val, "(sqrt(", ncol(X), "))\n")

rf_model <- randomForest(
  x        = X,
  y        = y,
  ntree    = 500,
  mtry     = mtry_val,
  nodesize = 5,
  importance = TRUE,
  keep.inbag = TRUE
)

oob_r2 <- round(100*(1 - rf_model$mse[500]/var(y)), 2)
cat("OOB % Var explained:", oob_r2, "%\n")

dados$IQS_RF <- predict(rf_model, newdata = X)

## Importância
imp <- importance(rf_model)
imp_sd <- rf_model$importanceSD
imp_df <- data.frame(
  Variable = rownames(imp),
  IncMSE   = imp[, "%IncMSE"],
  IncPurity = imp[, "IncNodePurity"]
)
imp_df <- imp_df[order(-imp_df$IncMSE), ]
imp_df$Dominio <- dominio_map[imp_df$Variable]
imp_df$Label   <- nice_names_34[imp_df$Variable]

cat("\n--- RF Importância (%IncMSE) ---\n"); print(imp_df)

## OOB convergence data
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
## 18. Ranqueamento por combo
## ----------------------------------------------------------
cat("\n=== RANKING POR COMBO ===\n")
combo <- paste(dados$Parcela, dados$Cultura, sep = "_")
dados$Combo <- combo

rank_agg <- aggregate(
  cbind(IQS_ANOVA, IQS_Fuzzy, IQS_PLS, IQS_MARS, IQS_RF, MetaIQS_Borda) ~ Combo,
  data = dados, FUN = mean
)
for (col in c("IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")) {
  rank_agg[, paste0("Rank_", col)] <- rank(-rank_agg[, col])
}
rank_agg <- rank_agg[order(-rank_agg$MetaIQS_Borda), ]
cat("\n--- Ranqueamento ---\n"); print(rank_agg)

## ----------------------------------------------------------
## 19. MONTE CARLO
## ----------------------------------------------------------
cat("\n=== MONTE CARLO ===\n")
dados$Celula <- paste(dados$Parcela, dados$Cultura, sep = "_")
celulas <- unique(dados$Celula)

compute_iqs_anova_mc <- function(df, pc) {
  pca_r <- prcomp(df[, pc], center=TRUE, scale.=TRUE)
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
        ANOVA = compute_iqs_anova_mc(sub_df, pred_cols_34),
        Fuzzy = sub_df$IPACSA,
        PLS   = rowMeans(sub_df[, pred_cols_34]),  # proxy: média simples dos 34 indicadores
        MARS  = predict(earth(IPACSA ~ ., data = sub_df[,c(pred_cols_34,"IPACSA")], degree=2))[,1],
        RF    = predict(randomForest(x=sub_df[,pred_cols_34], y=sub_df$IPACSA,
                                      ntree=100, mtry=ceiling(sqrt(length(pred_cols_34))),
                                      nodesize=5),
                        newdata=sub_df[,pred_cols_34])
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

## 20.1 Heatmap Spearman
spearman_df <- reshape2::melt(spearman_mat)
names(spearman_df) <- c("Var1","Var2","rho")
spearman_df$label <- round(spearman_df$rho, 3)
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
ggsave("../3-FIGURAS/fig_heatmap_spearman_34.png", p_heat, width = 5.5, height = 4.5, dpi = 300)

## 20.2 Bland-Altman panel
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
ggsave("../3-FIGURAS/fig_blandaltman_panel_34.png", p_ba, width = 12, height = 4, dpi = 300)

## 20.3 Scatter 1:1
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
ggsave("../3-FIGURAS/fig_scatter_1to1_34.png", p_scatter, width = 8, height = 7, dpi = 300)

## 20.4 RF importance barplot
imp_df$Label_f <- factor(imp_df$Label, levels = rev(imp_df$Label))
dom_colors <- c("Físico"="#4DAF4A","Químico"="#E41A1C","Microbiológico"="#FF7F00",
                "Agronômico"="#377EB8","Rendimento"="#984EA3")

p_imp <- ggplot(imp_df, aes(x = Label_f, y = IncMSE, fill = Dominio)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = dom_colors) +
  labs(x = NULL, y = "%IncMSE", fill = "Domínio") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("../3-FIGURAS/fig_rf_importance_34.png", p_imp, width = 7, height = 9, dpi = 300)

## 20.5 PDP top 4
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
    labs(x = nice_names_34[v], y = "Efeito parcial") +
    theme_minimal(base_size = 10)
}
p_pdp_all <- do.call(gridExtra::grid.arrange, c(p_pdp, ncol = 2))
ggsave("../3-FIGURAS/fig_pdp_top4_34.png", p_pdp_all, width = 8, height = 6, dpi = 300)

## 20.6 MC sensitivity
mc_valid <- mc_results[!is.na(mc_results$W), ]
mc_valid$n_f <- factor(mc_valid$n)
W_full <- kw_result$value

p_mc <- ggplot(mc_valid, aes(x = n_f, y = W)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  geom_hline(yintercept = W_full, linetype = 3, color = "black") +
  labs(x = "Tamanho amostral (n)", y = "Kendall W") +
  theme_minimal(base_size = 11)
ggsave("../3-FIGURAS/fig_mc_sensitivity_34.png", p_mc, width = 7, height = 4.5, dpi = 300)

## 20.7 Boxplot IQS por combo e paradigma
iqs_long <- reshape2::melt(
  dados[, c("Combo","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")],
  id.vars = "Combo", variable.name = "Paradigma", value.name = "IQS"
)
iqs_long$Paradigma <- gsub("IQS_", "", iqs_long$Paradigma)

## Ordem pelo Borda
combo_order <- rank_agg$Combo
iqs_long$Combo <- factor(iqs_long$Combo, levels = rev(combo_order))

p_box <- ggplot(iqs_long, aes(x = Combo, y = IQS, fill = Paradigma)) +
  geom_boxplot(outlier.size = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "IQS (escala original)", fill = "Paradigma") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("../3-FIGURAS/fig_boxplot_combo_34.png", p_box, width = 10, height = 7, dpi = 300)

## ----------------------------------------------------------
## 21. EXPORTAR RESULTADOS
## ----------------------------------------------------------
cat("\n=== EXPORTANDO ===\n")

## Dataset mestre com todos os indicadores e IQS
export_cols <- c("Parcela","Cultura","Combo",
                 pred_cols_34,
                 "IPACSA","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF",
                 "MetaIQS_Borda",
                 "pH_0_10","P_0_10","K_0_10",
                 sara_analysis_raw)
write.csv(dados[, export_cols],
          file = "../1-DADOS_BRUTOS/IQS_todos_paradigmas_34ind.csv", row.names = FALSE)

write.csv(round(spearman_mat, 4), file = "../1-DADOS_BRUTOS/Spearman_matrix_34ind.csv")
write.csv(round(ba_summary, 4), file = "../1-DADOS_BRUTOS/BlandAltman_summary_34ind.csv")
write.csv(imp_df, file = "../1-DADOS_BRUTOS/RF_importancia_34ind.csv", row.names = FALSE)
write.csv(rank_agg, file = "../1-DADOS_BRUTOS/Ranking_combo_34ind.csv", row.names = FALSE)
write.csv(round(mc_summary, 4), file = "../1-DADOS_BRUTOS/MC_sumario_34ind.csv", row.names = FALSE)
write.csv(path_df, file = "../1-DADOS_BRUTOS/PLS_path_coefficients_34ind.csv", row.names = FALSE)

cat("\n=====================================================\n")
cat("  RE-ANÁLISE CONCLUÍDA COM 34 INDICADORES\n")
cat("=====================================================\n")

## ----------------------------------------------------------
## RESUMO PARA MANUSCRITO
## ----------------------------------------------------------
cat("\n=== RESUMO PARA MANUSCRITO ===\n")
cat("n =", nrow(dados), "| p =", length(pred_cols_34), "\n")
cat("Domínios: Físico(12), Químico(9), Micro(5), Agronômico(4), Rendimento(4)\n")
cat("mtry =", mtry_val, "\n")
cat("RF OOB R²:", oob_r2, "%\n")
cat("Kendall W =", round(kw_result$value, 4), "\n")
cat("ICC(2,1) =", round(icc_result$results[2, "ICC"], 4), "\n")
cat("Gwet AC1 =", round(gwet_result$est$coeff.val, 4), "\n")
cat("\nSpearman matrix:\n"); print(round(spearman_mat, 3))
cat("\nBland-Altman:\n"); print(round(ba_summary, 3))
cat("\nBorda vs paradigmas:", round(meta_cor, 3), "\n")
cat("H2 p =", format.pval(wilcox_h2$p.value, digits=4), "\n")
cat("H3 beta1 =", round(beta1, 4), "\n")
cat("\nRanking:\n")
print(rank_agg[, c("Combo",
                    "Rank_IQS_ANOVA","Rank_IQS_Fuzzy","Rank_IQS_PLS",
                    "Rank_IQS_MARS","Rank_IQS_RF","MetaIQS_Borda")])
cat("\nPLS paths:\n"); print(path_df)
cat("\nTop 10 RF importância:\n"); print(head(imp_df[, c("Label","IncMSE","Dominio")], 10))
