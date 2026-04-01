## ============================================================
## 10_figuras_complementares.R
## Figuras complementares: heatmap, Bland-Altman, boxplot manejo×cobertura,
## scatter 1:1, violin, PDP, ranking table
## ============================================================

## Carregar theme_pub
source("00_theme_pub.R")

## Pacotes
library(reshape2)
library(randomForest)
library(DALEX)
library(iml)

## Resolver conflito margin
margin <- ggplot2::margin

## ---- Caminhos ----
dir_dados  <- "../1-DADOS_BRUTOS"
dir_figs   <- "../3-FIGURAS"

## ---- Leitura de dados ----
spear   <- read.csv(file.path(dir_dados, "Spearman_matrix.csv"), row.names = 1)
iqs     <- read.csv(file.path(dir_dados, "IQS_todos_paradigmas.csv"))
ipacsa  <- read.csv(file.path(dir_dados, "IPACSA_resultados_completos.csv"))

## Limpar nomes de paradigma
nomes_par <- c("ANOVA", "Fuzzy", "PLS", "MARS", "RF")
colnames(spear) <- nomes_par
rownames(spear) <- nomes_par

cat("Dados carregados.\n")

## ============================================================
## FIG 6 — Heatmap de correlação Spearman anotado
## ============================================================
cat("Gerando Fig 6 — Heatmap Spearman...\n")

mat_long <- melt(as.matrix(spear))
colnames(mat_long) <- c("Paradigma_A", "Paradigma_B", "rho")

p6 <- ggplot(mat_long, aes(Paradigma_A, Paradigma_B, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", rho)),
            size = 3.5, colour = ifelse(mat_long$rho > 0.85, "white", "black")) +
  scale_fill_gradient2(low = "#D55E00", mid = "#F5D7A0", high = "#0072B2",
                       midpoint = 0.8, limits = c(0.55, 1),
                       name = expression(rho[S])) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  theme_pub() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.position = "right",
        panel.grid = element_blank(),
        panel.border = element_blank())

save_pub(p6, file.path(dir_figs, "fig_heatmap_spearman.png"),
         width = fig_width_single + 1.5, height = fig_width_single + 1.0)

## ============================================================
## FIG 7 — Bland-Altman panel (3 pares representativos)
## ============================================================
cat("Gerando Fig 7 — Bland-Altman panel...\n")

## Pares representativos: bloco NL (Fuzzy-RF), intermediário (PLS-RF), divergente (ANOVA-Fuzzy)
blom_cols <- c("IQS_ANOVA_blom", "IQS_Fuzzy_blom", "IQS_PLS_blom",
               "IQS_MARS_blom", "IQS_RF_blom")

make_ba <- function(col_a, col_b, label) {
  diff_ab <- iqs[[col_a]] - iqs[[col_b]]
  mean_ab <- (iqs[[col_a]] + iqs[[col_b]]) / 2
  md  <- mean(diff_ab)
  sdd <- sd(diff_ab)
  data.frame(mean_val = mean_ab, diff_val = diff_ab,
             md = md, upper = md + 1.96 * sdd, lower = md - 1.96 * sdd,
             par = label)
}

ba_data <- rbind(
  make_ba("IQS_Fuzzy_blom", "IQS_RF_blom", "Fuzzy vs RF"),
  make_ba("IQS_PLS_blom",   "IQS_RF_blom", "PLS vs RF"),
  make_ba("IQS_ANOVA_blom", "IQS_Fuzzy_blom", "ANOVA vs Fuzzy")
)
ba_data$par <- factor(ba_data$par, levels = c("Fuzzy vs RF", "PLS vs RF", "ANOVA vs Fuzzy"))

p7 <- ggplot(ba_data, aes(mean_val, diff_val)) +
  geom_point(alpha = 0.5, size = 1.5, colour = "#0072B2") +
  geom_hline(aes(yintercept = md), linetype = "solid", colour = "black", linewidth = 0.4) +
  geom_hline(aes(yintercept = upper), linetype = "dashed", colour = "#D55E00", linewidth = 0.4) +
  geom_hline(aes(yintercept = lower), linetype = "dashed", colour = "#D55E00", linewidth = 0.4) +
  facet_wrap(~par, nrow = 1, scales = "free") +
  labs(x = "Média dos dois paradigmas (escala Blom)",
       y = "Diferença entre paradigmas") +
  theme_pub() +
  theme(strip.text = element_text(size = rel(0.9), face = "bold"))

save_pub(p7, file.path(dir_figs, "fig_blandaltman_panel.png"),
         width = fig_width_double, height = fig_height_standard)

## ============================================================
## FIG 8 — Boxplot IQS por manejo×cobertura (5 paradigmas)
## ============================================================
cat("Gerando Fig 8 — Boxplot manejo×cobertura...\n")

## Criar coluna combinada
iqs$Combo <- paste0(iqs$Parcela, "-", iqs$Cultura)

## Converter para formato longo (5 paradigmas brutos)
iqs_long <- melt(iqs[, c("Combo", "Parcela", "Cultura",
                          "IQS_ANOVA", "IQS_Fuzzy", "IQS_PLS", "IQS_MARS", "IQS_RF")],
                 id.vars = c("Combo", "Parcela", "Cultura"),
                 variable.name = "Paradigma", value.name = "IQS")
iqs_long$Paradigma <- gsub("IQS_", "", iqs_long$Paradigma)

## Ordenar por mediana do MetaIQS
meta_ord <- aggregate(IQS ~ Combo, data = iqs_long[iqs_long$Paradigma == "RF", ], FUN = median)
meta_ord <- meta_ord[order(meta_ord$IQS), ]
iqs_long$Combo <- factor(iqs_long$Combo, levels = meta_ord$Combo)

## Paleta por paradigma
pal_par <- c("ANOVA" = "#E69F00", "Fuzzy" = "#0072B2", "PLS" = "#CC79A7",
             "MARS" = "#009E73", "RF" = "#D55E00")

p8 <- ggplot(iqs_long, aes(x = Combo, y = IQS, fill = Paradigma)) +
  geom_boxplot(outlier.size = 0.8, linewidth = 0.3, position = position_dodge(0.75)) +
  scale_fill_manual(values = pal_par) +
  labs(x = NULL, y = "IQS (escala original)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)),
        legend.position = "bottom")

save_pub(p8, file.path(dir_figs, "fig_boxplot_combo.png"),
         width = fig_width_double, height = fig_height_tall)

## ============================================================
## FIG 9 — Scatter 1:1 (4 pares: cada paradigma vs RF)
## ============================================================
cat("Gerando Fig 9 — Scatter 1:1...\n")

scatter_data <- data.frame(
  RF    = rep(iqs$IQS_RF_blom, 4),
  Other = c(iqs$IQS_ANOVA_blom, iqs$IQS_Fuzzy_blom, iqs$IQS_PLS_blom, iqs$IQS_MARS_blom),
  Paradigma = rep(c("ANOVA", "Fuzzy", "PLS-SEM", "MARS"), each = nrow(iqs))
)

## Calcular R2 por paradigma
r2_labels <- sapply(split(scatter_data, scatter_data$Paradigma), function(d) {
  r2 <- cor(d$RF, d$Other, method = "spearman")^2
  sprintf("rho^2 == %.3f", r2)
})

r2_df <- data.frame(
  Paradigma = names(r2_labels),
  label = r2_labels,
  x = -2.2, y = 2.5
)

p9 <- ggplot(scatter_data, aes(RF, Other)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.4, size = 1.2, colour = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, colour = "#D55E00", linewidth = 0.6) +
  facet_wrap(~Paradigma, nrow = 1) +
  geom_text(data = r2_df, aes(x = x, y = y, label = label),
            parse = TRUE, size = 3, hjust = 0) +
  labs(x = "IQS Random Forest (Blom)", y = "IQS outro paradigma (Blom)") +
  coord_fixed() +
  theme_pub() +
  theme(strip.text = element_text(face = "bold"))

save_pub(p9, file.path(dir_figs, "fig_scatter_1to1.png"),
         width = fig_width_double, height = fig_height_standard + 0.5)

## ============================================================
## FIG 10 — Violin plot dos 5 paradigmas
## ============================================================
cat("Gerando Fig 10 — Violin plot...\n")

violin_data <- melt(iqs[, c("IQS_ANOVA_blom", "IQS_Fuzzy_blom", "IQS_PLS_blom",
                             "IQS_MARS_blom", "IQS_RF_blom")],
                    variable.name = "Paradigma", value.name = "IQS_blom")
violin_data$Paradigma <- gsub("IQS_|_blom", "", violin_data$Paradigma)
violin_data$Paradigma <- factor(violin_data$Paradigma,
                                levels = c("ANOVA", "Fuzzy", "PLS", "MARS", "RF"))

p10 <- ggplot(violin_data, aes(Paradigma, IQS_blom, fill = Paradigma)) +
  geom_violin(alpha = 0.6, linewidth = 0.4) +
  geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.5) +
  scale_fill_manual(values = pal_par) +
  labs(x = NULL, y = "IQS (escala Blom)") +
  theme_pub() +
  theme(legend.position = "none")

save_pub(p10, file.path(dir_figs, "fig_violin_paradigmas.png"),
         width = fig_width_double, height = fig_height_standard)

## ============================================================
## FIG 11 — Partial Dependence Plots (top 4 variáveis do RF)
## ============================================================
cat("Gerando Fig 11 — Partial Dependence Plots...\n")

## Preparar dados para RF (usar colunas normalizadas)
norm_cols <- grep("_norm$", colnames(ipacsa), value = TRUE)
X <- ipacsa[, norm_cols]
y <- ipacsa$IPACSA  # target é o IPACSA fuzzy

## Limpar nomes das colunas
clean_names <- gsub("_norm$", "", colnames(X))
clean_names <- gsub("\\.", " ", clean_names)
colnames(X) <- make.names(clean_names)

set.seed(42)
rf_mod <- randomForest(x = X, y = y, ntree = 500, mtry = 4, nodesize = 5,
                       importance = TRUE)
cat("  RF treinado: R2_OOB =", round(1 - rf_mod$mse[500] / var(y), 4), "\n")

## Top 4 variáveis por %IncMSE
imp <- importance(rf_mod)
top4 <- rownames(imp)[order(imp[, "%IncMSE"], decreasing = TRUE)][1:4]
cat("  Top 4 variáveis:", paste(top4, collapse = ", "), "\n")

## Gerar PDPs usando iml
predictor <- Predictor$new(rf_mod, data = X, y = y)

pdp_list <- lapply(top4, function(v) {
  eff <- FeatureEffect$new(predictor, feature = v, method = "pdp", grid.size = 30)
  d <- eff$results
  colnames(d) <- c("x", "yhat", "type")
  d$variable <- v
  d
})
pdp_all <- do.call(rbind, pdp_list)

## Nomes legíveis
pdp_all$variable <- gsub("\\.", " ", pdp_all$variable)

p11 <- ggplot(pdp_all, aes(x, yhat)) +
  geom_line(colour = "#0072B2", linewidth = 0.8) +
  geom_rug(sides = "b", alpha = 0.2, linewidth = 0.3) +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  labs(x = "Valor do indicador (normalizado)", y = "Efeito parcial sobre IQS") +
  theme_pub() +
  theme(strip.text = element_text(face = "bold", size = rel(0.8)))

save_pub(p11, file.path(dir_figs, "fig_pdp_top4.png"),
         width = fig_width_double, height = fig_height_standard)

## ============================================================
## TABELA — Ranking por combinação manejo×cobertura
## ============================================================
cat("Gerando tabela de ranking por combinação...\n")

rank_table <- aggregate(
  cbind(IQS_ANOVA, IQS_Fuzzy, IQS_PLS, IQS_MARS, IQS_RF, MetaIQS_Borda) ~ Parcela + Cultura,
  data = iqs, FUN = mean
)

## Ordena por MetaIQS decrescente
rank_table <- rank_table[order(-rank_table$MetaIQS_Borda), ]

## Adicionar ranks por paradigma
for (col in c("IQS_ANOVA", "IQS_Fuzzy", "IQS_PLS", "IQS_MARS", "IQS_RF")) {
  rank_table[[paste0("Rank_", gsub("IQS_", "", col))]] <-
    rank(-rank_table[[col]], ties.method = "min")
}

## Salvar
write.csv(rank_table,
          file.path(dir_dados, "Ranking_combo_paradigma.csv"),
          row.names = FALSE)

## Imprimir para inspeção
cat("\n=== RANKING POR COMBINAÇÃO MANEJO×COBERTURA ===\n")
print(rank_table[, c("Parcela", "Cultura", "Rank_ANOVA", "Rank_Fuzzy",
                      "Rank_PLS", "Rank_MARS", "Rank_RF", "MetaIQS_Borda")],
      row.names = FALSE)

cat("\nScript 10 concluído com sucesso.\n")
