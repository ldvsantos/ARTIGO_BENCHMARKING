## ============================================================
## 14_figuras_profissionais.R
## RegeneraÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o de todas as figuras ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Â estilo journal-ready
## Paleta Tableau 10 + hachuras ggpattern + theme_bw
## ============================================================

cat("=====================================================\n")
cat("  FIGURAS PROFISSIONAIS (34 indicadores)\n")
cat("=====================================================\n\n")

## ----------------------------------------------------------
## 0. Pacotes
## ----------------------------------------------------------
pkgs <- c("ggplot2","ggpattern","gridExtra","reshape2",
          "randomForest","pdp","scales","grid")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

## ----------------------------------------------------------
## 1. Tema profissional + paletas
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

## Paleta Tableau 10 ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Â colorblind-safe
pal_domain <- c(
  "FÃƒÆ’Ã‚Â­sico"         = "#4E79A7",
  "QuÃƒÆ’Ã‚Â­mico"        = "#F28E2B",
  "MicrobiolÃƒÆ’Ã‚Â³gico" = "#E15759",
  "AgronÃƒÆ’Ã‚Â´mico"     = "#76B7B2",
  "Rendimento"     = "#59A14F"
)

pat_domain <- c(
  "FÃƒÆ’Ã‚Â­sico"         = "stripe",
  "QuÃƒÆ’Ã‚Â­mico"        = "crosshatch",
  "MicrobiolÃƒÆ’Ã‚Â³gico" = "circle",
  "AgronÃƒÆ’Ã‚Â´mico"     = "none",
  "Rendimento"     = "stripe"
)

pat_angle_domain <- c(
  "FÃƒÆ’Ã‚Â­sico"         = 45,
  "QuÃƒÆ’Ã‚Â­mico"        = 45,
  "MicrobiolÃƒÆ’Ã‚Â³gico" = 0,
  "AgronÃƒÆ’Ã‚Â´mico"     = 0,
  "Rendimento"     = 135
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
## 2. Leitura de dados
## ----------------------------------------------------------
cat("--- Lendo CSVs ---\n")
base_dir <- file.path("..","1-DADOS_BRUTOS")

master  <- read.csv(file.path(base_dir, "IQS_todos_paradigmas_34ind.csv"))
imp_df  <- read.csv(file.path(base_dir, "RF_importancia_34ind.csv"))
sp_mat  <- read.csv(file.path(base_dir, "Spearman_matrix_34ind.csv"),
                     row.names = 1, check.names = FALSE)
ba_df   <- read.csv(file.path(base_dir, "BlandAltman_summary_34ind.csv"))
mc_df   <- read.csv(file.path(base_dir, "MC_sumario_34ind.csv"))
rank_df <- read.csv(file.path(base_dir, "Ranking_combo_34ind.csv"))

cat("Master:", nrow(master), "obs\n")
cat("ImportÃƒÆ’Ã‚Â¢ncia:", nrow(imp_df), "var\n")

## Nomes amigÃƒÆ’Ã‚Â¡veis e domÃƒÆ’Ã‚Â­nios
nice_names_34 <- c(
  "DMG_norm"="DMG","DMP_norm"="DMP","RMP_norm"="RMP",
  "Densidade_norm"="Ds","Estoque.de.C_norm"="COS","Na_norm"="Na",
  "ICV..._norm"="ICV",
  "pH_norm"="pH","P_norm"="P","K_norm"="K",
  "Ca_norm"="Ca","Mg_norm"="Mg","Al_norm"="Al",
  "CTC_norm"="CTC","MOS_norm"="MOS","EstN_norm"="EstN",
  "MI_norm"="MI","MA_norm"="MA","VIB_norm"="VIB",
  "RP_norm"="RP","AD_PT_norm"="AD/PT",
  "qMIC_norm"="qMIC","qCO2_norm"="qCO2","CCO2_norm"="CCO2",
  "CMIC_norm"="CMIC","NMIC_norm"="NMIC",
  "Altura.de.Plantas_norm"="ALT",
  "Di\u00e2metro.espiga_norm"="DESP",  # handle encoding
  "Comprimento.espiga_norm"="CESP",
  "N\u00famero.de.plantas_ha_norm"="STAND",
  "N.total.de.espigas_ha_norm"="NESP",
  "N.de.espigas.comerciais_ha_norm"="NESPC",
  "Peso.de.espigas.comerciais_ha_norm"="PESP",
  "Produtividade_norm"="PROD"
)

dominio_map <- c(
  "DMG_norm"="FÃƒÆ’Ã‚Â­sico","DMP_norm"="FÃƒÆ’Ã‚Â­sico","RMP_norm"="FÃƒÆ’Ã‚Â­sico",
  "Densidade_norm"="FÃƒÆ’Ã‚Â­sico","Estoque.de.C_norm"="FÃƒÆ’Ã‚Â­sico","Na_norm"="FÃƒÆ’Ã‚Â­sico",
  "ICV..._norm"="FÃƒÆ’Ã‚Â­sico","MI_norm"="FÃƒÆ’Ã‚Â­sico","MA_norm"="FÃƒÆ’Ã‚Â­sico",
  "VIB_norm"="FÃƒÆ’Ã‚Â­sico","RP_norm"="FÃƒÆ’Ã‚Â­sico","AD_PT_norm"="FÃƒÆ’Ã‚Â­sico",
  "pH_norm"="QuÃƒÆ’Ã‚Â­mico","P_norm"="QuÃƒÆ’Ã‚Â­mico","K_norm"="QuÃƒÆ’Ã‚Â­mico",
  "Ca_norm"="QuÃƒÆ’Ã‚Â­mico","Mg_norm"="QuÃƒÆ’Ã‚Â­mico","Al_norm"="QuÃƒÆ’Ã‚Â­mico",
  "CTC_norm"="QuÃƒÆ’Ã‚Â­mico","MOS_norm"="QuÃƒÆ’Ã‚Â­mico","EstN_norm"="QuÃƒÆ’Ã‚Â­mico",
  "qMIC_norm"="MicrobiolÃƒÆ’Ã‚Â³gico","qCO2_norm"="MicrobiolÃƒÆ’Ã‚Â³gico",
  "CCO2_norm"="MicrobiolÃƒÆ’Ã‚Â³gico","CMIC_norm"="MicrobiolÃƒÆ’Ã‚Â³gico",
  "NMIC_norm"="MicrobiolÃƒÆ’Ã‚Â³gico",
  "Altura.de.Plantas_norm"="AgronÃƒÆ’Ã‚Â´mico",
  "Di\u00e2metro.espiga_norm"="AgronÃƒÆ’Ã‚Â´mico",
  "Comprimento.espiga_norm"="AgronÃƒÆ’Ã‚Â´mico",
  "N\u00famero.de.plantas_ha_norm"="AgronÃƒÆ’Ã‚Â´mico",
  "N.total.de.espigas_ha_norm"="Rendimento",
  "N.de.espigas.comerciais_ha_norm"="Rendimento",
  "Peso.de.espigas.comerciais_ha_norm"="Rendimento",
  "Produtividade_norm"="Rendimento"
)

## Fix column name encoding in master if needed
fix_col <- function(nms) {
  nms <- gsub("\u00e2", "\u00e2", nms)  # keep as is
  nms
}

## Identify pred columns in master
pred_cols_34 <- names(master)[4:37]
cat("Pred cols identified:", length(pred_cols_34), "\n")

## Response variable
y <- master$IPACSA

## Predictor matrix
X <- master[, pred_cols_34]

## Fix imp_df labels if needed (ensure encoding match)
imp_df$Label <- nice_names_34[as.character(imp_df$Variable)]
imp_df$Dominio <- dominio_map[as.character(imp_df$Variable)]

## Blom transformation for BA and scatter
blom <- function(x) qnorm((rank(x) - 0.375) / (length(x) + 0.25))
iqs_cols <- c("IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")
iqs_blom <- data.frame(lapply(master[, iqs_cols], blom))
names(iqs_blom) <- paste0(iqs_cols, "_blom")

## Combo labels
combo_labels <- c(
  "MT_Pigeon pea"="CM-Guandu","MT_Cowpea"="CM-Caupi",
  "MT_Millet"="CM-Milheto","NT_Pigeon pea"="PD-Guandu",
  "MT_Sunn hemp"="CM-CrotalÃƒÆ’Ã‚Â¡ria","NT_Millet"="PD-Milheto",
  "CT_Cowpea"="PC-Caupi","NT_Sunn hemp"="PD-CrotalÃƒÆ’Ã‚Â¡ria",
  "CT_Sunn hemp"="PC-CrotalÃƒÆ’Ã‚Â¡ria","NT_Cowpea"="PD-Caupi",
  "CT_Millet"="PC-Milheto","CT_Pigeon pea"="PC-Guandu"
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
                           levels = c("Rendimento","AgronÃƒÆ’Ã‚Â´mico",
                                      "QuÃƒÆ’Ã‚Â­mico","MicrobiolÃƒÆ’Ã‚Â³gico","FÃƒÆ’Ã‚Â­sico"))

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
  scale_fill_manual(values = pal_domain, name = "DomÃƒÆ’Ã‚Â­nio") +
  scale_pattern_manual(values = pat_domain, name = "DomÃƒÆ’Ã‚Â­nio") +
  scale_pattern_angle_manual(values = pat_angle_domain, name = "DomÃƒÆ’Ã‚Â­nio") +
  labs(x = NULL, y = "%IncMSE (importÃƒÆ’Ã‚Â¢ncia por permutaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o)") +
  theme_pub +
  theme(legend.position = "bottom") +
  guides(fill    = guide_legend(nrow = 1, override.aes = list(
                     pattern = pat_domain, pattern_angle = pat_angle_domain)),
         pattern = "none", pattern_angle = "none")

ggsave("../3-FIGURAS/fig_rf_importance_34.png", p_imp,
       width = 7, height = 8.5, dpi = 300)
cat("  -> fig_rf_importance_34.png OK\n")


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
    labs(x = lbl, y = "Efeito parcial (IQS)") +
    theme_pub
}
p_pdp_all <- do.call(gridExtra::grid.arrange, c(p_pdp, ncol = 2))
ggsave("../3-FIGURAS/fig_pdp_top4_34.png", p_pdp_all,
       width = 8, height = 6, dpi = 300)
cat("  -> fig_pdp_top4_34.png OK\n")


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

## Escala que cobre o mÃƒÆ’Ã‚Â­nimo real (ÃƒÂ¢Ã¢â‚¬Â°Ã‹â€  0.27)
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

ggsave("../3-FIGURAS/fig_heatmap_spearman_34.png", p_heat,
       width = 5.5, height = 4.5, dpi = 300)
cat("  -> fig_heatmap_spearman_34.png OK\n")


## ===========================================================
## FIG 5: Bland-Altman Panel (3 pares, notaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o ÃƒÅ½Ã¢â‚¬ÂM)
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
         x = expression("MÃƒÆ’Ã‚Â©dia" ~ (bar(M))),
         y = expression(Delta*M)) +
    theme_pub +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
}

p_ba1 <- make_ba_pro("IQS_Fuzzy", "IQS_RF",    "Fuzzy vs RF")
p_ba2 <- make_ba_pro("IQS_PLS",   "IQS_RF",    "PLS-SEM vs RF")
p_ba3 <- make_ba_pro("IQS_ANOVA", "IQS_Fuzzy",  "ANOVA vs Fuzzy")
p_ba  <- gridExtra::grid.arrange(p_ba1, p_ba2, p_ba3, ncol = 3)
ggsave("../3-FIGURAS/fig_blandaltman_panel_34.png", p_ba,
       width = 12, height = 4.2, dpi = 300)
cat("  -> fig_blandaltman_panel_34.png OK\n")


## ===========================================================
## FIG 6: Scatter 1:1 (RF como referÃƒÆ’Ã‚Âªncia)
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
ggsave("../3-FIGURAS/fig_scatter_1to1_34.png", p_scatter,
       width = 8, height = 7, dpi = 300)
cat("  -> fig_scatter_1to1_34.png OK\n")


## ===========================================================
## FIG 7: Boxplot IQS por combinaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o ÃƒÆ’Ã¢â‚¬â€ paradigma (hachuras)
## ===========================================================
cat("\n--- Fig 7: Boxplot Combo ---\n")

iqs_long <- reshape2::melt(
  master[, c("Combo","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")],
  id.vars = "Combo", variable.name = "Paradigma", value.name = "IQS"
)
iqs_long$Paradigma <- gsub("IQS_", "", iqs_long$Paradigma)
iqs_long$Paradigma <- ifelse(iqs_long$Paradigma == "PLS", "PLS-SEM",
                             iqs_long$Paradigma)
iqs_long$Paradigma <- factor(iqs_long$Paradigma,
                             levels = c("ANOVA","Fuzzy","PLS-SEM","MARS","RF"))

## Usar labels em PT e ordenar por Borda
iqs_long$Combo_PT <- combo_labels[as.character(iqs_long$Combo)]
combo_order_pt <- combo_labels[as.character(rank_df$Combo)]
iqs_long$Combo_PT <- factor(iqs_long$Combo_PT, levels = rev(combo_order_pt))

p_box <- ggplot(iqs_long,
                aes(x = Combo_PT, y = IQS,
                    fill = Paradigma, pattern = Paradigma,
                    pattern_angle = Paradigma)) +
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
  scale_fill_manual(values = pal_paradigm, name = "Paradigma") +
  scale_pattern_manual(values = pat_paradigm, name = "Paradigma") +
  scale_pattern_angle_manual(values = pat_angle_paradigm, name = "Paradigma") +
  labs(x = NULL, y = "IQS (escala original, 0\u201310)") +
  theme_pub +
  theme(legend.position = "bottom") +
  guides(fill    = guide_legend(nrow = 1, override.aes = list(
                     pattern = pat_paradigm,
                     pattern_angle = pat_angle_paradigm)),
         pattern = "none", pattern_angle = "none")

ggsave("../3-FIGURAS/fig_boxplot_combo_34.png", p_box,
       width = 10, height = 7, dpi = 300)
cat("  -> fig_boxplot_combo_34.png OK\n")


## ===========================================================
## FIG 8: MC Sensitivity (intervalo a partir do sumÃƒÆ’Ã‚Â¡rio)
## ===========================================================
cat("\n--- Fig 8: MC Sensitivity ---\n")

mc_df$n_f <- factor(mc_df$n)
W_full <- 0.726  # Kendall W da amostra completa

p_mc <- ggplot(mc_df, aes(x = n_f)) +
  ## Envelope q025ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Å“q975
  geom_rect(aes(xmin = as.numeric(n_f) - 0.35,
                xmax = as.numeric(n_f) + 0.35,
                ymin = q025, ymax = q975),
            fill = "#4E79A7", alpha = 0.18) +
  ## Barra IQR aproximada (mean Ãƒâ€šÃ‚Â± 1 sd)
  geom_errorbar(aes(ymin = mean_W - sd_W, ymax = mean_W + sd_W),
                width = 0.25, linewidth = 0.6, color = "#4E79A7") +
  ## Mediana
  geom_point(aes(y = median_W), shape = 21, size = 3,
             fill = "white", color = "#4E79A7", stroke = 0.8) +
  ## MÃƒÆ’Ã‚Â©dia
  geom_point(aes(y = mean_W), shape = 18, size = 3.5,
             color = "#E15759") +
  ## ReferÃƒÆ’Ã‚Âªncia W completo
  geom_hline(yintercept = W_full, linetype = 3, color = "black",
             linewidth = 0.5) +
  annotate("text", x = 5.4, y = W_full + 0.02,
           label = paste0("W[n==108] == ", W_full),
           parse = TRUE, size = 3.2, hjust = 1) +
  scale_y_continuous(limits = c(0.2, 1.0), breaks = seq(0.2, 1, 0.1)) +
  labs(x = "Tamanho amostral (n)",
       y = expression("Kendall" ~ italic(W))) +
  theme_pub

ggsave("../3-FIGURAS/fig_mc_sensitivity_34.png", p_mc,
       width = 7, height = 4.5, dpi = 300)
cat("  -> fig_mc_sensitivity_34.png OK\n")


cat("\n=====================================================\n")
cat("  TODAS AS FIGURAS GERADAS COM SUCESSO\n")
cat("=====================================================\n")
