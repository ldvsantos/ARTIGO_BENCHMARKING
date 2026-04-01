## ============================================================
## 09_figuras_pub.R
## Regenera TODAS as figuras com qualidade de publicação
## Padrão: Springer Nature, 600 DPI, sem títulos internos
## ============================================================

## ----------------------------------------------------------
## 0. Dependências e tema
## ----------------------------------------------------------
library(randomForest)
library(DALEX)
library(reshape2)
library(ggplot2)
library(scales)

source("00_theme_pub.R")

## Restaurar margin do ggplot2 (mascarado por randomForest)
margin <- ggplot2::margin

## Diretório de saída
dir.create("../3-FIGURAS", showWarnings = FALSE)
outdir <- "../3-FIGURAS"

## ----------------------------------------------------------
## Nomes legíveis para variáveis do RF
## ----------------------------------------------------------
var_labels <- c(
  "Número.de.plantas_ha_norm"           = "Stand (plants ha⁻¹)",
  "N.total.de.espigas_ha_norm"          = "Total ears (ears ha⁻¹)",
  "Produtividade_norm"                  = "Yield (kg ha⁻¹)",
  "Diâmetro.espiga_norm"                = "Ear diameter (mm)",
  "N.de.espigas.comerciais_ha_norm"     = "Commercial ears (ears ha⁻¹)",
  "Peso.de.espigas.comerciais_ha_norm"  = "Commercial ear weight (kg ha⁻¹)",
  "Comprimento.espiga_norm"             = "Ear length (cm)",
  "ICV..._norm"                         = "Vegetation cover index (%)",
  "Estoque.de.C_norm"                   = "SOC stock (Mg ha⁻¹)",
  "Densidade_norm"                      = "Bulk density (Mg m⁻³)",
  "Altura.de.Plantas_norm"              = "Plant height (cm)",
  "RMP_norm"                            = "Residual moisture persistence",
  "DMP_norm"                            = "Mean weight diameter (mm)",
  "DMG_norm"                            = "Geometric mean diameter (mm)",
  "Na_norm"                             = "Water-stable aggregates (%)"
)

## Domínio de cada variável (para colorir barras)
var_domain <- c(
  "Número.de.plantas_ha_norm"           = "Yield",
  "N.total.de.espigas_ha_norm"          = "Yield",
  "Produtividade_norm"                  = "Yield",
  "Diâmetro.espiga_norm"                = "Agronomic",
  "N.de.espigas.comerciais_ha_norm"     = "Yield",
  "Peso.de.espigas.comerciais_ha_norm"  = "Yield",
  "Comprimento.espiga_norm"             = "Agronomic",
  "ICV..._norm"                         = "Soil physical",
  "Estoque.de.C_norm"                   = "Soil physical",
  "Densidade_norm"                      = "Soil physical",
  "Altura.de.Plantas_norm"              = "Agronomic",
  "RMP_norm"                            = "Soil physical",
  "DMP_norm"                            = "Soil physical",
  "DMG_norm"                            = "Soil physical",
  "Na_norm"                             = "Soil physical"
)

domain_colors <- c(
  "Soil physical" = "#0072B2",
  "Agronomic"     = "#009E73",
  "Yield"         = "#D55E00"
)

domain_fills <- c(
  "Soil physical" = "#B3D9F2",
  "Agronomic"     = "#A5E0CC",
  "Yield"         = "#F2C6A5"
)

## ===========================================================
## FIGURA 1: Convergência OOB-MSE
## ===========================================================
cat("\n--- Fig 1: OOB convergence ---\n")

dados <- read.csv(file.path("..", "1-DADOS_BRUTOS", "IPACSA_resultados_completos.csv"))

pred_cols <- c(
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

X <- dados[, pred_cols]
y <- dados$IPACSA

set.seed(42)
rf_model <- randomForest(
  x = X, y = y, ntree = 500,
  mtry = ceiling(sqrt(ncol(X))),
  nodesize = 5, importance = TRUE, keep.inbag = TRUE
)

oob_df <- data.frame(
  ntrees  = seq_along(rf_model$mse),
  OOB_MSE = rf_model$mse
)

## Anotação: ponto de estabilização (~200 árvores)
stab_tree <- 200
stab_mse  <- oob_df$OOB_MSE[stab_tree]

p1 <- ggplot(oob_df, aes(x = ntrees, y = OOB_MSE)) +
  geom_line(colour = "#0072B2", linewidth = 0.6) +
  geom_vline(xintercept = stab_tree, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  annotate("text", x = stab_tree + 15, y = max(oob_df$OOB_MSE) * 0.92,
           label = paste0("≈ ", stab_tree, " trees"),
           hjust = 0, size = 2.8, colour = "grey40") +
  scale_x_continuous(breaks = seq(0, 500, 100), expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "Number of trees", y = "OOB-MSE") +
  theme_pub()

save_pub(p1, file.path(outdir, "fig_oob_convergence.png"))

## ===========================================================
## FIGURA 2: Importância das variáveis (%IncMSE)
## ===========================================================
cat("--- Fig 2: Variable importance ---\n")

imp_df <- read.csv(file.path("..", "1-DADOS_BRUTOS", "RF_importancia.csv"))
imp_df$Label  <- var_labels[imp_df$Variable]
imp_df$Domain <- var_domain[imp_df$Variable]
imp_df$Label  <- factor(imp_df$Label, levels = imp_df$Label[order(imp_df$IncMSE)])

p2 <- ggplot(imp_df, aes(x = Label, y = IncMSE, fill = Domain)) +
  geom_col(width = 0.7, colour = "grey30", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = domain_fills, name = "Indicator domain") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "%IncMSE") +
  theme_pub() +
  theme(legend.position = c(0.75, 0.2),
        legend.background = element_rect(fill = alpha("white", 0.9),
                                         colour = "grey80", linewidth = 0.3),
        legend.key.size = unit(0.35, "cm"))

save_pub(p2, file.path(outdir, "fig_rf_importance.png"),
         height = fig_height_tall)

## ===========================================================
## FIGURA 3: SHAP global (DALEX permutation)
## ===========================================================
cat("--- Fig 3: SHAP global ---\n")

explainer_rf <- DALEX::explain(
  model = rf_model, data = X, y = y,
  label = "Random Forest", verbose = FALSE
)

vi_rf <- DALEX::model_parts(explainer_rf, B = 500, type = "difference")
vi_data <- vi_rf[vi_rf$variable != "_full_model_" &
                   vi_rf$variable != "_baseline_", ]

## Agregar mediana por variável
vi_agg <- aggregate(dropout_loss ~ variable, data = vi_data, FUN = median)
vi_agg$Label  <- var_labels[vi_agg$variable]
vi_agg$Domain <- var_domain[vi_agg$variable]
vi_agg$Label  <- factor(vi_agg$Label,
                         levels = vi_agg$Label[order(vi_agg$dropout_loss)])

## Boxplot das 500 permutações
vi_data$Label  <- var_labels[vi_data$variable]
vi_data$Domain <- var_domain[vi_data$variable]
vi_data$Label  <- factor(vi_data$Label, levels = levels(vi_agg$Label))

p3 <- ggplot(vi_data, aes(x = Label, y = dropout_loss, fill = Domain)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3,
               linewidth = 0.3, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = domain_fills, name = "Indicator domain") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Permutation importance (Δ loss)") +
  theme_pub() +
  theme(legend.position = c(0.75, 0.2),
        legend.background = element_rect(fill = alpha("white", 0.9),
                                         colour = "grey80", linewidth = 0.3),
        legend.key.size = unit(0.35, "cm"))

save_pub(p3, file.path(outdir, "fig_rf_shap_global.png"),
         height = fig_height_tall)

## ===========================================================
## FIGURA 4: Monte Carlo — boxplot W vs n
## ===========================================================
cat("--- Fig 4: MC sensitivity boxplot ---\n")

mc_raw <- read.csv(file.path("..", "1-DADOS_BRUTOS", "MC_resultados_brutos.csv"))
mc_raw <- mc_raw[!is.na(mc_raw$W), ]

## Largura proporcional ao log(n)
mc_raw$n_lab <- paste0("n = ", mc_raw$n)
mc_raw$n_lab <- factor(mc_raw$n_lab,
                        levels = paste0("n = ", sort(unique(mc_raw$n))))

## Mediana por grupo para anotação
mc_med <- aggregate(W ~ n, data = mc_raw, FUN = median)

p4 <- ggplot(mc_raw, aes(x = n_lab, y = W)) +
  geom_boxplot(fill = "#B3D9F2", colour = "grey30", outlier.size = 0.5,
               outlier.alpha = 0.3, linewidth = 0.35, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2.2,
               colour = "#D55E00") +
  geom_hline(yintercept = 0.808, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 5.3, y = 0.815,
           label = "W (full sample) = 0.808",
           hjust = 1, size = 2.5, colour = "grey40", fontface = "italic") +
  scale_y_continuous(limits = c(0.15, 1.0),
                     breaks = seq(0.2, 1.0, 0.1)) +
  labs(x = "Sample size", y = expression(italic(W)~"(Kendall)")) +
  theme_pub()

save_pub(p4, file.path(outdir, "fig_mc_sensitivity.png"))

## ===========================================================
## FIGURA 5: Monte Carlo — curva média ± SD com GLS
## ===========================================================
cat("--- Fig 5: MC convergence curve ---\n")

mc_sum <- read.csv(file.path("..", "1-DADOS_BRUTOS", "MC_sumario.csv"))

## Faixa ± 1 SD
mc_sum$lower_sd <- mc_sum$mean_W - mc_sum$sd_W
mc_sum$upper_sd <- mc_sum$mean_W + mc_sum$sd_W

## GLS line (refit para obter predições contínuas)
mc_sum$log_n <- log(mc_sum$n)
lm_fit <- lm(mean_W ~ log_n, data = mc_sum)
n_seq  <- seq(15, 108, length.out = 100)
pred_gls <- data.frame(
  n     = n_seq,
  log_n = log(n_seq)
)
pred_gls$W_pred <- predict(lm_fit, newdata = pred_gls)

p5 <- ggplot() +
  ## Faixa ± 1SD
  geom_ribbon(data = mc_sum,
              aes(x = n, ymin = lower_sd, ymax = upper_sd),
              fill = "#B3D9F2", alpha = 0.5) +
  ## Faixa IC 95% (percentis)
  geom_ribbon(data = mc_sum,
              aes(x = n, ymin = q025, ymax = q975),
              fill = "#0072B2", alpha = 0.15) +
  ## Reta GLS
  geom_line(data = pred_gls, aes(x = n, y = W_pred),
            linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  ## Pontos e linha dos dados
  geom_line(data = mc_sum, aes(x = n, y = mean_W),
            colour = "#0072B2", linewidth = 0.7) +
  geom_point(data = mc_sum, aes(x = n, y = mean_W),
             colour = "#0072B2", size = 2.5, shape = 16) +
  ## Referência
  geom_hline(yintercept = 0.808, linetype = "dotted",
             colour = "#D55E00", linewidth = 0.4) +
  annotate("text", x = 20, y = 0.818,
           label = "W (n = 108) = 0.808", hjust = 0,
           size = 2.5, colour = "#D55E00", fontface = "italic") +
  ## Anotação GLS
  annotate("text", x = 80, y = pred_gls$W_pred[80] - 0.03,
           label = paste0("β₁ = 0.043, p = 0.098"),
           hjust = 0.5, size = 2.3, colour = "grey40", fontface = "italic") +
  scale_x_continuous(breaks = c(15, 30, 50, 75, 100),
                     expand = c(0.02, 0)) +
  scale_y_continuous(limits = c(0.4, 0.95),
                     breaks = seq(0.4, 0.9, 0.1)) +
  labs(x = "Sample size (n)",
       y = expression(bar(italic(W))~"± 1 SD")) +
  theme_pub()

save_pub(p5, file.path(outdir, "fig_mc_curve.png"))

## ===========================================================
## CONCLUSÃO
## ===========================================================
cat("\n=== Todas as 5 figuras salvas em", outdir, "===\n")
cat("  fig_oob_convergence.png\n")
cat("  fig_rf_importance.png\n")
cat("  fig_rf_shap_global.png\n")
cat("  fig_mc_sensitivity.png\n")
cat("  fig_mc_curve.png\n")
