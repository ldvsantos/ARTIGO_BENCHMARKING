## ============================================================
## 05_random_forest.R
## Random Forest para IQS com SHAP, OOB convergence e CIs
## Artigo: Benchmarking multi-paradigma
## ============================================================

library(randomForest)
library(DALEX)
library(ggplot2)

## ----------------------------------------------------------
## 1. Leitura dos dados
## ----------------------------------------------------------
dados <- read.csv(
  file.path("..", "1-DADOS_BRUTOS", "IPACSA_resultados_completos.csv")
)

## Variáveis preditoras normalizadas (0-10)
pred_cols <- c(
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

## Variável resposta: IPACSA (IQS Fuzzy original como referência)
X <- dados[, pred_cols]
y <- dados$IPACSA

cat("n =", nrow(X), " | p =", ncol(X), " | mtry = ceiling(sqrt(p)) =",
    ceiling(sqrt(ncol(X))), "\n")

## ----------------------------------------------------------
## 2. Treinamento do RF
## ----------------------------------------------------------
set.seed(42)
rf_model <- randomForest(
  x      = X,
  y      = y,
  ntree  = 500,
  mtry   = ceiling(sqrt(ncol(X))),   # = 4
  nodesize = 5,
  importance = TRUE,
  keep.inbag = TRUE
)

cat("\n--- Sumário RF ---\n")
print(rf_model)
cat("\nOOB % Var explained:", round(100 * (1 - rf_model$mse[500] / var(y)), 2), "%\n")

## ----------------------------------------------------------
## 3. IQS_RF predito
## ----------------------------------------------------------
dados$IQS_RF <- predict(rf_model, newdata = X)

## ----------------------------------------------------------
## 4. Curva OOB-MSE vs número de árvores
## ----------------------------------------------------------
oob_df <- data.frame(
  ntrees = seq_along(rf_model$mse),
  OOB_MSE = rf_model$mse
)

p_oob <- ggplot(oob_df, aes(x = ntrees, y = OOB_MSE)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(x = "Número de árvores", y = "OOB-MSE",
       title = "Convergência do erro OOB") +
  theme_minimal()

ggsave("../fig_oob_convergence.png", p_oob, width = 7, height = 4, dpi = 300)
cat("Figura OOB salva: fig_oob_convergence.png\n")

## ----------------------------------------------------------
## 5. Importância das variáveis (%IncMSE)
## ----------------------------------------------------------
imp <- importance(rf_model)
imp_df <- data.frame(
  Variable = rownames(imp),
  IncMSE   = imp[, "%IncMSE"],
  IncPurity = imp[, "IncNodePurity"]
)
imp_df <- imp_df[order(-imp_df$IncMSE), ]

cat("\n--- Importância (%IncMSE) ---\n")
print(imp_df)

p_imp <- ggplot(imp_df, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = NULL, y = "%IncMSE", title = "Importância das variáveis (RF)") +
  theme_minimal()

ggsave("../fig_rf_importance.png", p_imp, width = 7, height = 5, dpi = 300)

## ----------------------------------------------------------
## 6. SHAP global via DALEX
## ----------------------------------------------------------
explainer_rf <- DALEX::explain(
  model   = rf_model,
  data    = X,
  y       = y,
  label   = "Random Forest",
  verbose = FALSE
)

## Variable importance via permutation (DALEX)
vi_rf <- DALEX::model_parts(explainer_rf, B = 500, type = "difference")

p_shap <- plot(vi_rf) +
  labs(title = "Importância global (permutação DALEX, 500 reps)") +
  theme_minimal()

ggsave("../fig_rf_shap_global.png", p_shap, width = 7, height = 5, dpi = 300)
cat("Figura SHAP salva: fig_rf_shap_global.png\n")

## ----------------------------------------------------------
## 7. Intervalos de confiança via bootstrap interno do RF
## ----------------------------------------------------------
## Usa predições de cada árvore individual para calcular IC 95%
preds_all <- predict(rf_model, newdata = X, predict.all = TRUE)$individual
ci_lower <- apply(preds_all, 1, quantile, probs = 0.025)
ci_upper <- apply(preds_all, 1, quantile, probs = 0.975)
ci_width <- ci_upper - ci_lower

dados$IQS_RF_CI_lower <- ci_lower
dados$IQS_RF_CI_upper <- ci_upper

cat("\n--- IC 95% das predições RF ---\n")
cat("Largura média IC:", round(mean(ci_width), 4), "\n")
cat("Largura mediana IC:", round(median(ci_width), 4), "\n")

## ----------------------------------------------------------
## 8. Exportar resultados
## ----------------------------------------------------------
write.csv(
  dados[, c("Parcela", "Cultura", "IPACSA", "IQS_RF",
            "IQS_RF_CI_lower", "IQS_RF_CI_upper")],
  file = "../1-DADOS_BRUTOS/IQS_RF_resultados.csv",
  row.names = FALSE
)

write.csv(imp_df, file = "../1-DADOS_BRUTOS/RF_importancia.csv", row.names = FALSE)

cat("\n=== 05_random_forest.R concluído ===\n")
