## ============================================================
## 15_figuras_profissionais_EN.R
## English-language versions of all figures — journal-ready
## Tableau 10 palette + ggpattern hatches + theme_bw
## ============================================================

cat("=====================================================\n")
cat("  PROFESSIONAL FIGURES — EN (34 indicators)\n")
cat("=====================================================\n\n")

## ----------------------------------------------------------
## 0. Packages
## ----------------------------------------------------------
pkgs <- c("ggplot2","ggpattern","gridExtra","reshape2",
          "randomForest","pdp","scales","grid")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

## ----------------------------------------------------------
## 1. Publication theme + palettes
## ----------------------------------------------------------
theme_pub <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "grey90"),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key.size  = unit(0.45, "cm"),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    plot.title       = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.margin      = ggplot2::margin(5, 10, 5, 5)
  )

## Tableau 10 palette — colorblind-safe
pal_domain <- c(
  "Physical"        = "#4E79A7",
  "Chemical"        = "#F28E2B",
  "Microbiological" = "#E15759",
  "Agronomic"       = "#76B7B2",
  "Yield"           = "#59A14F"
)

pat_domain <- c(
  "Physical"        = "stripe",
  "Chemical"        = "crosshatch",
  "Microbiological" = "circle",
  "Agronomic"       = "none",
  "Yield"           = "stripe"
)

pat_angle_domain <- c(
  "Physical"        = 45,
  "Chemical"        = 45,
  "Microbiological" = 0,
  "Agronomic"       = 0,
  "Yield"           = 135
)

pal_paradigm <- c(
  "ANOVA"   = "#4E79A7",
  "Fuzzy"   = "#F28E2B",
  "PLS-SEM" = "#E15759",
  "MARS"    = "#76B7B2",
  "RF"      = "#59A14F"
)

pat_paradigm <- c(
  "ANOVA"   = "stripe",
  "Fuzzy"   = "crosshatch",
  "PLS-SEM" = "circle",
  "MARS"    = "none",
  "RF"      = "stripe"
)

pat_angle_paradigm <- c(
  "ANOVA"   = 45,
  "Fuzzy"   = 45,
  "PLS-SEM" = 0,
  "MARS"    = 0,
  "RF"      = 135
)

## ----------------------------------------------------------
## 2. Data loading
## ----------------------------------------------------------
cat("--- Reading CSVs ---\n")
base_dir <- file.path("..","1-DADOS_BRUTOS")

master  <- read.csv(file.path(base_dir, "IQS_todos_paradigmas_34ind.csv"))
imp_df  <- read.csv(file.path(base_dir, "RF_importancia_34ind.csv"))
sp_mat  <- read.csv(file.path(base_dir, "Spearman_matrix_34ind.csv"),
                     row.names = 1, check.names = FALSE)
ba_df   <- read.csv(file.path(base_dir, "BlandAltman_summary_34ind.csv"))
mc_df   <- read.csv(file.path(base_dir, "MC_sumario_34ind.csv"))
rank_df <- read.csv(file.path(base_dir, "Ranking_combo_34ind.csv"))

cat("Master:", nrow(master), "obs\n")
cat("Importance:", nrow(imp_df), "var\n")

## Nice names and EN domain mapping
nice_names_34 <- c(
  "DMG_norm"="GMD","DMP_norm"="WMD","RMP_norm"="RMH",
  "Densidade_norm"="BD","Estoque.de.C_norm"="SOC","Na_norm"="WSA",
  "ICV..._norm"="VCI",
  "pH_norm"="pH","P_norm"="P","K_norm"="K",
  "Ca_norm"="Ca","Mg_norm"="Mg","Al_norm"="Al",
  "CTC_norm"="CEC","MOS_norm"="SOM","EstN_norm"="TN",
  "MI_norm"="MI","MA_norm"="MA","VIB_norm"="BIR",
  "RP_norm"="PR","AD_PT_norm"="AW/TP",
  "qMIC_norm"="qMIC","qCO2_norm"="qCO2","CCO2_norm"="BR",
  "CMIC_norm"="MBC","NMIC_norm"="MBN",
  "Altura.de.Plantas_norm"="PH",
  "Di\u00e2metro.espiga_norm"="ED",
  "Comprimento.espiga_norm"="EL",
  "N\u00famero.de.plantas_ha_norm"="STAND",
  "N.total.de.espigas_ha_norm"="TE",
  "N.de.espigas.comerciais_ha_norm"="ME",
  "Peso.de.espigas.comerciais_ha_norm"="MEW",
  "Produtividade_norm"="GY"
)

dominio_map <- c(
  "DMG_norm"="Physical","DMP_norm"="Physical","RMP_norm"="Physical",
  "Densidade_norm"="Physical","Estoque.de.C_norm"="Physical","Na_norm"="Physical",
  "ICV..._norm"="Physical","MI_norm"="Physical","MA_norm"="Physical",
  "VIB_norm"="Physical","RP_norm"="Physical","AD_PT_norm"="Physical",
  "pH_norm"="Chemical","P_norm"="Chemical","K_norm"="Chemical",
  "Ca_norm"="Chemical","Mg_norm"="Chemical","Al_norm"="Chemical",
  "CTC_norm"="Chemical","MOS_norm"="Chemical","EstN_norm"="Chemical",
  "qMIC_norm"="Microbiological","qCO2_norm"="Microbiological",
  "CCO2_norm"="Microbiological","CMIC_norm"="Microbiological",
  "NMIC_norm"="Microbiological",
  "Altura.de.Plantas_norm"="Agronomic",
  "Di\u00e2metro.espiga_norm"="Agronomic",
  "Comprimento.espiga_norm"="Agronomic",
  "N\u00famero.de.plantas_ha_norm"="Agronomic",
  "N.total.de.espigas_ha_norm"="Yield",
  "N.de.espigas.comerciais_ha_norm"="Yield",
  "Peso.de.espigas.comerciais_ha_norm"="Yield",
  "Produtividade_norm"="Yield"
)

## Identify pred columns in master
pred_cols_34 <- names(master)[4:37]
cat("Pred cols identified:", length(pred_cols_34), "\n")

## Response variable
y <- master$IPACSA

## Predictor matrix
X <- master[, pred_cols_34]

## Fix imp_df labels
imp_df$Label <- nice_names_34[as.character(imp_df$Variable)]
imp_df$Dominio <- dominio_map[as.character(imp_df$Variable)]

## Blom transformation for BA and scatter
blom <- function(x) qnorm((rank(x) - 0.375) / (length(x) + 0.25))
iqs_cols <- c("IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")
iqs_blom <- data.frame(lapply(master[, iqs_cols], blom))
names(iqs_blom) <- paste0(iqs_cols, "_blom")

## EN combo labels
combo_labels <- c(
  "MT_Pigeon pea"="MT-Pigeon pea","MT_Cowpea"="MT-Cowpea",
  "MT_Millet"="MT-Millet","NT_Pigeon pea"="NT-Pigeon pea",
  "MT_Sunn hemp"="MT-Sunn hemp","NT_Millet"="NT-Millet",
  "CT_Cowpea"="CT-Cowpea","NT_Sunn hemp"="NT-Sunn hemp",
  "CT_Sunn hemp"="CT-Sunn hemp","NT_Cowpea"="NT-Cowpea",
  "CT_Millet"="CT-Millet","CT_Pigeon pea"="CT-Pigeon pea"
)

dir.create("../3-FIGURAS", showWarnings = FALSE)

## ===========================================================
## FIG 2: RF Importance (horizontal bars with domain hatching)
## ===========================================================
cat("\n--- Fig 2: RF Importance ---\n")

imp_df <- imp_df[order(-imp_df$IncMSE), ]
imp_df$Label_f <- factor(imp_df$Label,
                         levels = rev(imp_df$Label))
imp_df$Dominio_f <- factor(imp_df$Dominio,
                           levels = c("Yield","Agronomic",
                                      "Chemical","Microbiological","Physical"))

p_imp <- ggplot(imp_df,
                aes(x = Label_f, y = IncMSE,
                    fill = Dominio, pattern = Dominio,
                    pattern_angle = Dominio)) +
  geom_col_pattern(
    width = 0.72,
    color = "grey30", linewidth = 0.3,
    pattern_density  = 0.15,
    pattern_spacing  = 0.025,
    pattern_colour   = "grey20",
    pattern_fill     = "grey20",
    pattern_key_scale_factor = 0.6
  ) +
  coord_flip() +
  scale_fill_manual(values = pal_domain, name = "Domain") +
  scale_pattern_manual(values = pat_domain, name = "Domain") +
  scale_pattern_angle_manual(values = pat_angle_domain, name = "Domain") +
  labs(x = NULL, y = "%IncMSE (permutation importance)") +
  theme_pub +
  theme(legend.position = "bottom") +
  guides(fill    = guide_legend(nrow = 1, override.aes = list(
                     pattern = pat_domain, pattern_angle = pat_angle_domain)),
         pattern = "none", pattern_angle = "none")

ggsave("../3-FIGURAS/fig_rf_importance_34_en.png", p_imp,
       width = 7, height = 8.5, dpi = 300)
cat("  -> fig_rf_importance_34_en.png OK\n")


## ===========================================================
## FIG 3: PDP Top 4 (refit RF first)
## ===========================================================
cat("\n--- Fig 3: PDP (refitting RF) ---\n")
set.seed(42)
mtry_val <- ceiling(sqrt(ncol(X)))
rf_model <- randomForest(x = X, y = y, ntree = 500,
                         mtry = mtry_val, nodesize = 5,
                         importance = TRUE)
cat("  RF refit OK (OOB var explained:",
    round(100*(1 - rf_model$mse[500]/var(y)), 1), "%)\n")

top4 <- head(imp_df$Variable, 4)
p_pdp <- list()
for (i in seq_along(top4)) {
  v  <- top4[i]
  pd <- pdp::partial(rf_model, pred.var = v, train = X)
  lbl <- nice_names_34[v]
  p_pdp[[i]] <- ggplot(pd, aes_string(x = v, y = "yhat")) +
    geom_line(color = "#4E79A7", linewidth = 0.9) +
    geom_rug(data = X, aes_string(x = v), inherit.aes = FALSE,
             alpha = 0.25, sides = "b", length = unit(0.04, "npc")) +
    labs(x = lbl, y = "Partial effect (SQI)") +
    theme_pub
}
p_pdp_all <- do.call(gridExtra::grid.arrange, c(p_pdp, ncol = 2))
ggsave("../3-FIGURAS/fig_pdp_top4_34_en.png", p_pdp_all,
       width = 8, height = 6, dpi = 300)
cat("  -> fig_pdp_top4_34_en.png OK\n")


## ===========================================================
## FIG 4: Spearman Heatmap (professional)
## ===========================================================
cat("\n--- Fig 4: Spearman Heatmap ---\n")

sp_mat2 <- as.matrix(sp_mat)
sp_melt <- reshape2::melt(sp_mat2)
names(sp_melt) <- c("Var1","Var2","rho")
short <- c("IQS_ANOVA"="ANOVA","IQS_Fuzzy"="Fuzzy","IQS_PLS"="PLS-SEM",
           "IQS_MARS"="MARS","IQS_RF"="RF")
sp_melt$Var1 <- short[as.character(sp_melt$Var1)]
sp_melt$Var2 <- short[as.character(sp_melt$Var2)]
sp_melt$label <- sprintf("%.3f", sp_melt$rho)

p_heat <- ggplot(sp_melt, aes(Var1, Var2, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 3.8, fontface = "bold") +
  scale_fill_gradient2(
    low = "#E15759", mid = "#FFFFD4", high = "#59A14F",
    midpoint = 0.65, limits = c(0.2, 1),
    name = expression(rho[S]),
    breaks = seq(0.2, 1, 0.2)
  ) +
  labs(x = NULL, y = NULL) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid   = element_blank(),
        panel.border = element_blank())

ggsave("../3-FIGURAS/fig_heatmap_spearman_34_en.png", p_heat,
       width = 5.5, height = 4.5, dpi = 300)
cat("  -> fig_heatmap_spearman_34_en.png OK\n")


## ===========================================================
## FIG 5: Bland-Altman Panel (3 pairs, DeltaM notation)
## ===========================================================
cat("\n--- Fig 5: Bland-Altman Panel ---\n")

make_ba_pro <- function(col_a, col_b, title_str) {
  a <- iqs_blom[, paste0(col_a, "_blom")]
  b <- iqs_blom[, paste0(col_b, "_blom")]
  diff_ab <- a - b
  mean_ab <- (a + b) / 2
  md  <- mean(diff_ab)
  sdd <- sd(diff_ab)
  df_ba <- data.frame(mean_val = mean_ab, diff_val = diff_ab)

  ggplot(df_ba, aes(x = mean_val, y = diff_val)) +
    geom_point(alpha = 0.50, size = 1.8, color = "#4E79A7", shape = 16) +
    geom_hline(yintercept = md, color = "black", linetype = 1, linewidth = 0.5) +
    geom_hline(yintercept = md + 1.96*sdd, color = "#E15759",
               linetype = 2, linewidth = 0.6) +
    geom_hline(yintercept = md - 1.96*sdd, color = "#E15759",
               linetype = 2, linewidth = 0.6) +
    annotate("text", x = max(mean_ab)*0.95, y = md + 1.96*sdd,
             label = paste0("+1.96s = ", round(md + 1.96*sdd, 2)),
             hjust = 1, vjust = -0.5, size = 2.8, color = "#E15759") +
    annotate("text", x = max(mean_ab)*0.95, y = md - 1.96*sdd,
             label = paste0("\u20131.96s = ", round(md - 1.96*sdd, 2)),
             hjust = 1, vjust = 1.5, size = 2.8, color = "#E15759") +
    labs(title = title_str,
         x = expression("Mean" ~ (bar(M))),
         y = expression(Delta*M)) +
    theme_pub +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
}

p_ba1 <- make_ba_pro("IQS_Fuzzy", "IQS_RF",    "Fuzzy vs RF")
p_ba2 <- make_ba_pro("IQS_PLS",   "IQS_RF",    "PLS-SEM vs RF")
p_ba3 <- make_ba_pro("IQS_ANOVA", "IQS_Fuzzy",  "ANOVA vs Fuzzy")
p_ba  <- gridExtra::grid.arrange(p_ba1, p_ba2, p_ba3, ncol = 3)
ggsave("../3-FIGURAS/fig_blandaltman_panel_34_en.png", p_ba,
       width = 12, height = 4.2, dpi = 300)
cat("  -> fig_blandaltman_panel_34_en.png OK\n")


## ===========================================================
## FIG 6: Scatter 1:1 (RF as reference)
## ===========================================================
cat("\n--- Fig 6: Scatter 1:1 ---\n")

make_scatter_pro <- function(col_y, label_y, pt_color) {
  df <- data.frame(RF = iqs_blom$IQS_RF_blom,
                   Y  = iqs_blom[, paste0(col_y, "_blom")])
  rho_sq <- round(cor(df$RF, df$Y, method = "spearman")^2, 3)

  ggplot(df, aes(x = RF, y = Y)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2,
                color = "grey60", linewidth = 0.5) +
    geom_point(alpha = 0.45, size = 1.8, color = pt_color, shape = 16) +
    geom_smooth(method = "loess", se = TRUE, color = "black",
                fill = "grey80", linewidth = 0.7, alpha = 0.3) +
    annotate("text", x = -2.0, y = 2.3,
             label = paste0("rho^2 == ", rho_sq),
             parse = TRUE, size = 3.8, hjust = 0) +
    labs(x = "RF (Blom)", y = paste0(label_y, " (Blom)")) +
    theme_pub
}

p_s1 <- make_scatter_pro("IQS_ANOVA", "ANOVA",   "#4E79A7")
p_s2 <- make_scatter_pro("IQS_Fuzzy", "Fuzzy",   "#F28E2B")
p_s3 <- make_scatter_pro("IQS_PLS",   "PLS-SEM", "#E15759")
p_s4 <- make_scatter_pro("IQS_MARS",  "MARS",    "#76B7B2")
p_scatter <- gridExtra::grid.arrange(p_s1, p_s2, p_s3, p_s4, ncol = 2)
ggsave("../3-FIGURAS/fig_scatter_1to1_34_en.png", p_scatter,
       width = 8, height = 7, dpi = 300)
cat("  -> fig_scatter_1to1_34_en.png OK\n")


## ===========================================================
## FIG 7: Boxplot SQI by management-cover crop x paradigm
## ===========================================================
cat("\n--- Fig 7: Boxplot Combo ---\n")

iqs_long <- reshape2::melt(
  master[, c("Combo","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")],
  id.vars = "Combo", variable.name = "Paradigm", value.name = "SQI"
)
iqs_long$Paradigm <- gsub("IQS_", "", iqs_long$Paradigm)
iqs_long$Paradigm <- ifelse(iqs_long$Paradigm == "PLS", "PLS-SEM",
                            iqs_long$Paradigm)
iqs_long$Paradigm <- factor(iqs_long$Paradigm,
                            levels = c("ANOVA","Fuzzy","PLS-SEM","MARS","RF"))

## EN labels ordered by Borda
iqs_long$Combo_EN <- combo_labels[as.character(iqs_long$Combo)]
combo_order_en <- combo_labels[as.character(rank_df$Combo)]
iqs_long$Combo_EN <- factor(iqs_long$Combo_EN, levels = rev(combo_order_en))

p_box <- ggplot(iqs_long,
                aes(x = Combo_EN, y = SQI,
                    fill = Paradigm, pattern = Paradigm,
                    pattern_angle = Paradigm)) +
  geom_boxplot_pattern(
    position = position_dodge(width = 0.78),
    width = 0.70, outlier.size = 0.8,
    color = "grey30", linewidth = 0.3,
    pattern_density  = 0.12,
    pattern_spacing  = 0.02,
    pattern_colour   = "grey20",
    pattern_fill     = "grey20",
    pattern_key_scale_factor = 0.6
  ) +
  coord_flip() +
  scale_fill_manual(values = pal_paradigm, name = "Paradigm") +
  scale_pattern_manual(values = pat_paradigm, name = "Paradigm") +
  scale_pattern_angle_manual(values = pat_angle_paradigm, name = "Paradigm") +
  labs(x = NULL, y = "SQI (original scale, 0\u201310)") +
  theme_pub +
  theme(legend.position = "bottom") +
  guides(fill    = guide_legend(nrow = 1, override.aes = list(
                     pattern = pat_paradigm,
                     pattern_angle = pat_angle_paradigm)),
         pattern = "none", pattern_angle = "none")

ggsave("../3-FIGURAS/fig_boxplot_combo_34_en.png", p_box,
       width = 10, height = 7, dpi = 300)
cat("  -> fig_boxplot_combo_34_en.png OK\n")


## ===========================================================
## FIG 8: MC Sensitivity (from summary)
## ===========================================================
cat("\n--- Fig 8: MC Sensitivity ---\n")

mc_df$n_f <- factor(mc_df$n)
W_full <- 0.726  # Kendall W from full sample

p_mc <- ggplot(mc_df, aes(x = n_f)) +
  ## Envelope q025-q975
  geom_rect(aes(xmin = as.numeric(n_f) - 0.35,
                xmax = as.numeric(n_f) + 0.35,
                ymin = q025, ymax = q975),
            fill = "#4E79A7", alpha = 0.18) +
  ## Approx IQR bar (mean +/- 1 sd)
  geom_errorbar(aes(ymin = mean_W - sd_W, ymax = mean_W + sd_W),
                width = 0.25, linewidth = 0.6, color = "#4E79A7") +
  ## Median
  geom_point(aes(y = median_W), shape = 21, size = 3,
             fill = "white", color = "#4E79A7", stroke = 0.8) +
  ## Mean
  geom_point(aes(y = mean_W), shape = 18, size = 3.5,
             color = "#E15759") +
  ## Full-sample W reference
  geom_hline(yintercept = W_full, linetype = 3, color = "black",
             linewidth = 0.5) +
  annotate("text", x = 5.4, y = W_full + 0.02,
           label = paste0("W[n==108] == ", W_full),
           parse = TRUE, size = 3.2, hjust = 1) +
  scale_y_continuous(limits = c(0.2, 1.0), breaks = seq(0.2, 1, 0.1)) +
  labs(x = "Sample size (n)",
       y = expression("Kendall" ~ italic(W))) +
  theme_pub

ggsave("../3-FIGURAS/fig_mc_sensitivity_34_en.png", p_mc,
       width = 7, height = 4.5, dpi = 300)
cat("  -> fig_mc_sensitivity_34_en.png OK\n")


cat("\n=====================================================\n")
cat("  ALL EN FIGURES GENERATED SUCCESSFULLY\n")
cat("=====================================================\n")
