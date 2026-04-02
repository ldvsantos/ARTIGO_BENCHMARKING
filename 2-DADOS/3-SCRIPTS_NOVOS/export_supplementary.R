## ============================================================
## export_supplementary.R
## Extrai diagnósticos PLS-SEM e PCA do 12_reanalise_34ind.R
## e salva em CSV para as tabelas suplementares S1-S4
## ============================================================

cat("=====================================================\n")
cat("  EXPORTAÇÃO DADOS SUPLEMENTARES\n")
cat("=====================================================\n\n")

pkgs <- c("randomForest","earth","seminr","irr","psych","irrCAC")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

outdir <- file.path("..","1-DADOS_BRUTOS")

## ----------------------------------------------------------
## 1. Leitura e merge (idêntico ao 12_reanalise_34ind.R)
## ----------------------------------------------------------
dados <- read.csv(file.path(outdir,"IPACSA_resultados_completos.csv"))
quim  <- read.csv(file.path(outdir,"quimicos_jaelson_0_10cm.csv"))
sara  <- read.csv(file.path(outdir,"Sara2019_medias_tratamento.csv"))

dados <- merge(dados, quim[, c("Parcela","Cultura","pH_0_10","P_0_10","K_0_10")],
               by = c("Parcela","Cultura"), all.x = TRUE)

sara_vars_raw <- c("Ca_cmolc_dm3","Mg_cmolc_dm3","Al_cmolc_dm3","HAl_cmolc_dm3",
                    "SB_cmolc_dm3","T_cmolc_dm3","V_pct","m_pct","MOS_g_kg",
                    "EstN_kg_ha","MI_pct","MA_pct","VIB_mm_h","RP_KPa","AD_PT",
                    "qMIC","qCO2","CCO2_g_kg","CMIC_ug_g","NMIC_mg_kg")
dados <- merge(dados, sara[, c("Parcela","Cultura", sara_vars_raw)],
               by = c("Parcela","Cultura"), all.x = TRUE)
stopifnot(nrow(dados) == 108)

## ----------------------------------------------------------
## 2. Normalização 0-10
## ----------------------------------------------------------
normalize_0_10 <- function(x) {
  rng <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
  if (rng == 0) return(rep(5, length(x)))
  (x - min(x, na.rm=TRUE)) / rng * 10
}

inverse_vars <- c("Densidade","Na","RP_KPa","Al_cmolc_dm3",
                   "HAl_cmolc_dm3","m_pct","qCO2")

rename_map <- c(
  "pH_0_10"="pH_norm", "P_0_10"="P_norm", "K_0_10"="K_norm",
  "Ca_cmolc_dm3"="Ca_norm", "Mg_cmolc_dm3"="Mg_norm",
  "Al_cmolc_dm3"="Al_norm", "CTC"="CTC_norm",
  "MOS_g_kg"="MOS_norm", "EstN_kg_ha"="EstN_norm",
  "MI_pct"="MI_norm", "MA_pct"="MA_norm",
  "VIB_mm_h"="VIB_norm", "RP_KPa"="RP_norm", "AD_PT"="AD_PT_norm",
  "qMIC"="qMIC_norm", "qCO2"="qCO2_norm",
  "CCO2_g_kg"="CCO2_norm", "CMIC_ug_g"="CMIC_norm", "NMIC_mg_kg"="NMIC_norm"
)

for (src in names(rename_map)) {
  if (src %in% names(dados)) {
    dados[[rename_map[src]]] <- dados[[src]]
  }
}

## CTC calculada
if (!"CTC_norm" %in% names(dados)) {
  if ("SB_cmolc_dm3" %in% names(dados) & "HAl_cmolc_dm3" %in% names(dados)) {
    dados$CTC_norm <- dados$SB_cmolc_dm3 + dados$HAl_cmolc_dm3
  }
}

## Normalizar
norm_cols <- c(
  "DMG_norm","DMP_norm","RMP_norm","Densidade_norm",
  "Estoque.de.C_norm","Na_norm","ICV..._norm",
  "pH_norm","P_norm","K_norm","Ca_norm","Mg_norm","Al_norm",
  "CTC_norm","MOS_norm","EstN_norm",
  "MI_norm","MA_norm","VIB_norm","RP_norm","AD_PT_norm",
  "qMIC_norm","qCO2_norm","CCO2_norm","CMIC_norm","NMIC_norm",
  "Altura.de.Plantas_norm","Diâmetro.espiga_norm",
  "Comprimento.espiga_norm","Número.de.plantas_ha_norm",
  "N.total.de.espigas_ha_norm","N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm","Produtividade_norm"
)

## Only re-normalize those that aren't already 0-10
for (col in norm_cols) {
  if (col %in% names(dados)) {
    rng <- range(dados[[col]], na.rm = TRUE)
    if (rng[1] < 0 | rng[2] > 10.01) {
      inv <- any(sapply(inverse_vars, function(v) grepl(v, col)))
      if (inv) dados[[col]] <- 10 - normalize_0_10(dados[[col]])
      else     dados[[col]] <- normalize_0_10(dados[[col]])
    }
  }
}

X <- dados[, norm_cols[norm_cols %in% names(dados)]]
X <- X[, colSums(is.na(X)) == 0]

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

## ===========================================================
## S1: PCA eigenvalues, loadings, weights (ANOVA/MDS paradigm)
## ===========================================================
cat("\n=== S1: PCA/ANOVA eigenvalues ===\n")
pca_res <- prcomp(X, center = TRUE, scale. = TRUE)
eigvals <- pca_res$sdev^2
n_comp  <- sum(eigvals > 1)

pca_summary <- data.frame(
  Component   = paste0("PC", 1:length(eigvals)),
  Eigenvalue  = round(eigvals, 4),
  VarExplained_pct = round(eigvals / sum(eigvals) * 100, 2),
  CumVar_pct  = round(cumsum(eigvals / sum(eigvals)) * 100, 2),
  Retained    = ifelse(eigvals > 1, "Yes", "No")
)
write.csv(pca_summary, file.path(outdir, "S1_PCA_eigenvalues.csv"), row.names = FALSE)
cat("  S1a saved:", n_comp, "components retained.\n")

## Loadings for retained components
loadings_mat <- pca_res$rotation[, 1:n_comp, drop = FALSE]
rownames(loadings_mat) <- nice_names_34[rownames(loadings_mat)]
loadings_df <- data.frame(Indicator = rownames(loadings_mat), round(loadings_mat, 4))
write.csv(loadings_df, file.path(outdir, "S1_PCA_loadings.csv"), row.names = FALSE)
cat("  S1b saved: loadings for", n_comp, "PCs x", nrow(loadings_df), "indicators.\n")

## ===========================================================
## S3: PLS-SEM diagnostics
## ===========================================================
cat("\n=== S3: PLS-SEM diagnostics ===\n")

## Rename for seminr
d_pls <- dados
pls_rename <- list(
  list(pat="^DMG_norm$",            new="DMG"),
  list(pat="^Estoque.*C_norm$",     new="ECOS"),
  list(pat="^Na_norm$",             new="NAG"),
  list(pat="^ICV.*_norm$",          new="ICV"),
  list(pat="^MI_norm$",             new="MI"),
  list(pat="^MA_norm$",             new="MA"),
  list(pat="^VIB_norm$",            new="VIB"),
  list(pat="^RP_norm$",             new="RP"),
  list(pat="^AD_PT_norm$",          new="ADPT"),
  list(pat="^pH_norm$",             new="PH"),
  list(pat="^P_norm$",              new="PHOS"),
  list(pat="^K_norm$",              new="KPOT"),
  list(pat="^Ca_norm$",             new="CA"),
  list(pat="^Mg_norm$",             new="MG"),
  list(pat="^CTC_norm$",            new="CTC"),
  list(pat="^MOS_norm$",            new="MOS"),
  list(pat="^EstN_norm$",           new="ESTN"),
  list(pat="^qMIC_norm$",           new="QMIC"),
  list(pat="^qCO2_norm$",           new="QCO2"),
  list(pat="^CCO2_norm$",           new="CCO2"),
  list(pat="^CMIC_norm$",           new="CMIC"),
  list(pat="^NMIC_norm$",           new="NMIC"),
  list(pat="^Altura.*norm$",        new="ALT"),
  list(pat="metro.*espiga.*norm$",  new="DESP"),
  list(pat="^Comprimento.*norm$",   new="CESP"),
  list(pat="mero.*plantas.*norm$",  new="STND"),
  list(pat="total.*espigas.*norm$", new="NESP"),
  list(pat="^Peso.*espigas.*norm$", new="PESP"),
  list(pat="^Produtividade_norm$",  new="PROD")
)
## Also handle N.de.espigas.comerciais -> NESPC
d_pls$NESPC <- d_pls[["N.de.espigas.comerciais_ha_norm"]]

for (item in pls_rename) {
  idx <- grep(item$pat, names(d_pls))
  if (length(idx) == 1) names(d_pls)[idx] <- item$new
}

## VIF pré-screening
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
  current
}

fisico_cands  <- c("DMG","ECOS","NAG","ICV","MI","MA","VIB","RP","ADPT")
quimico_cands <- c("PH","PHOS","KPOT","CA","MG","CTC","MOS","ESTN")
micro_cands   <- c("QMIC","QCO2","CCO2","CMIC","NMIC")
agro_cands    <- c("ALT","DESP","CESP","STND")
rend_cands    <- c("NESP","PESP","PROD")

fis_ok  <- screen_vif(d_pls, fisico_cands,  5, "FISICO")
qui_ok  <- screen_vif(d_pls, quimico_cands, 5, "QUIMICO")
mic_ok  <- screen_vif(d_pls, micro_cands,   5, "MICRO")
agr_ok  <- screen_vif(d_pls, agro_cands,    5, "AGRO")
ren_ok  <- screen_vif(d_pls, rend_cands,    5, "REND")

## Save VIF screening results
all_retained <- c(fis_ok, qui_ok, mic_ok, agr_ok, ren_ok)
all_cands    <- c(fisico_cands, quimico_cands, micro_cands, agro_cands, rend_cands)
construct_map <- c(
  rep("PHYSICAL",  length(fisico_cands)),
  rep("CHEMICAL",  length(quimico_cands)),
  rep("MICRO",     length(micro_cands)),
  rep("AGRO",      length(agro_cands)),
  rep("YIELD",     length(rend_cands))
)
vif_screen_df <- data.frame(
  Indicator = all_cands,
  Construct = construct_map,
  Retained  = all_cands %in% all_retained,
  stringsAsFactors = FALSE
)
## compute final VIF for retained
for (i in seq_len(nrow(vif_screen_df))) {
  ind <- vif_screen_df$Indicator[i]
  cst <- vif_screen_df$Construct[i]
  if (vif_screen_df$Retained[i]) {
    grp <- switch(cst,
      PHYSICAL = fis_ok, CHEMICAL = qui_ok, MICRO = mic_ok,
      AGRO = agr_ok, YIELD = ren_ok)
    vif_vals <- compute_vif(d_pls, grp)
    vif_screen_df$VIF_final[i] <- round(vif_vals[ind], 2)
  } else {
    vif_screen_df$VIF_final[i] <- NA
  }
}
write.csv(vif_screen_df, file.path(outdir, "S3_PLS_VIF_screening.csv"), row.names = FALSE)
cat("  VIF screening saved.\n")

## Estimate PLS model
mm_pls <- constructs(
  composite("FISICO",  fis_ok, weights = mode_B),
  composite("QUIMICO", qui_ok, weights = mode_B),
  composite("MICRO",   mic_ok, weights = mode_B),
  composite("AGRO",    agr_ok, weights = mode_B),
  composite("REND",    ren_ok, weights = mode_A)
)
sm_pls <- relationships(
  paths(from = "FISICO",  to = "REND"),
  paths(from = "QUIMICO", to = "REND"),
  paths(from = "MICRO",   to = "REND"),
  paths(from = "AGRO",    to = "REND")
)
pls_model <- estimate_pls(
  data = d_pls, measurement_model = mm_pls, structural_model = sm_pls,
  inner_weights = path_weighting, missing = mean_replacement, missing_value = NA
)
s_pls <- summary(pls_model)

## Outer weights
w_df <- as.data.frame(s_pls$weights)
w_df$Indicator <- rownames(w_df)
write.csv(w_df, file.path(outdir, "S3_PLS_outer_weights.csv"), row.names = FALSE)
cat("  Outer weights saved.\n")

## VIF items (may be a list with unequal lengths per construct)
vif_raw <- s_pls$validity$vif_items
if (is.list(vif_raw) && !is.data.frame(vif_raw)) {
  vif_rows <- list()
  for (cname in names(vif_raw)) {
    vals <- vif_raw[[cname]]
    vif_rows <- c(vif_rows, list(data.frame(
      Indicator = names(vals), Construct = cname, VIF = as.numeric(vals),
      stringsAsFactors = FALSE)))
  }
  vif_df <- do.call(rbind, vif_rows)
} else {
  vif_df <- as.data.frame(vif_raw)
  vif_df$Indicator <- rownames(vif_df)
}
write.csv(vif_df, file.path(outdir, "S3_PLS_vif_items.csv"), row.names = FALSE)
cat("  VIF items saved.\n")

## Path coefficients + R²
paths_raw <- s_pls$paths
paths_df <- data.frame(
  Construct = rownames(paths_raw),
  stringsAsFactors = FALSE
)
for (cn in colnames(paths_raw)) {
  paths_df[[cn]] <- as.numeric(paths_raw[, cn])
}
write.csv(paths_df, file.path(outdir, "S3_PLS_paths_full.csv"), row.names = FALSE)
cat("  Paths (with R2) saved.\n")

## Bootstrap
set.seed(42)
boot_pls <- bootstrap_model(pls_model, nboot = 500, seed = 42)
sb_pls <- summary(boot_pls)

bp_raw <- sb_pls$bootstrapped_paths
bp_df <- data.frame(Path = rownames(bp_raw), stringsAsFactors = FALSE)
for (cn in colnames(bp_raw)) bp_df[[cn]] <- as.numeric(bp_raw[, cn])
write.csv(bp_df, file.path(outdir, "S3_PLS_bootstrap_paths.csv"), row.names = FALSE)
cat("  Bootstrap paths saved.\n")

bw_raw <- sb_pls$bootstrapped_weights
bw_df <- data.frame(Indicator = rownames(bw_raw), stringsAsFactors = FALSE)
for (cn in colnames(bw_raw)) bw_df[[cn]] <- as.numeric(bw_raw[, cn])
write.csv(bw_df, file.path(outdir, "S3_PLS_bootstrap_weights.csv"), row.names = FALSE)
cat("  Bootstrap weights saved.\n")

cat("\n=== DONE ===\n")
cat("Files saved to:", outdir, "\n")
