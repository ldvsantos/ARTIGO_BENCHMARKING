## ============================================================
## 12_figuras_rachuras.R
## Gera Fig.2 (RF importance) e Fig.5 (boxplot combo) com
## rachuras sutis (ggpattern) e paleta pastel para publicação.
## ============================================================

if (!requireNamespace("ggpattern", quietly = TRUE))
  install.packages("ggpattern", repos = "https://cran.r-project.org")

library(ggplot2)
library(ggpattern)
library(reshape2)

## Detectar diretório do script (funciona via Rscript e RStudio)
tryCatch(
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("--file=", "", args[grep("--file=", args)])
    if (length(f)) setwd(dirname(normalizePath(f)))
  }
)

## ---------- dados ----------
imp_df  <- read.csv("../1-DADOS_BRUTOS/RF_importancia_18ind.csv",
                     stringsAsFactors = FALSE)
iqs_all <- read.csv("../1-DADOS_BRUTOS/IQS_todos_paradigmas_18ind.csv",
                     stringsAsFactors = FALSE)
rank_df <- read.csv("../1-DADOS_BRUTOS/Ranking_combo_18ind.csv",
                     stringsAsFactors = FALSE)

dir.create("../3-FIGURAS", showWarnings = FALSE)

## ==========================================================
## PALETA PASTEL – 4 domínios (Fig.2) e 5 paradigmas (Fig.5)
## Tons pastéis suaves, distintos em cores e rachuras,
## mantendo legibilidade em P&B.
## ==========================================================

## --- Domínios (Fig.2 – barras) ---
domain_fill <- c(
  "Físico"     = "#A6CEE3",   # azul claro pastel
 "Químico"    = "#FDBF6F",   # laranja pastel
  "Agronômico" = "#B2DF8A",   # verde pastel
  "Rendimento" = "#CAB2D6"    # roxo pastel
)
domain_pattern <- c(
  "Físico"     = "stripe",
  "Químico"    = "crosshatch",
  "Agronômico" = "none",
  "Rendimento" = "stripe"
)
## Ângulos distintos para diferenciar listras
domain_angle <- c(
  "Físico"     = 45,
  "Químico"    = 45,
  "Agronômico" = 0,
  "Rendimento" = -45
)

## --- Paradigmas (Fig.5 – boxplot) ---
## Mesma paleta pastel (Paired) e rachuras da Fig.2
para_fill <- c(
  "ANOVA" = "#A6CEE3",   # azul pastel  (= Físico)
  "Fuzzy" = "#FDBF6F",   # laranja pastel (= Químico)
  "PLS"   = "#B2DF8A",   # verde pastel  (= Agronômico)
  "MARS"  = "#CAB2D6",   # roxo pastel   (= Rendimento)
  "RF"    = "#FB9A99"    # rosa pastel   (Paired extra)
)
para_pattern <- c(
  "ANOVA" = "stripe",
  "Fuzzy" = "crosshatch",
  "PLS"   = "none",
  "MARS"  = "stripe",
  "RF"    = "crosshatch"
)
para_angle <- c(
  "ANOVA" = 45,
  "Fuzzy" = 45,
  "PLS"   = 0,
  "MARS"  = -45,
  "RF"    = -45
)

## Tema base publicação
theme_pub <- theme_bw(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.title       = element_text(face = "bold", size = 9),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.55, "cm"),
    axis.title         = element_text(size = 9),
    axis.text          = element_text(size = 8, color = "black"),
    plot.margin        = margin(5, 10, 5, 5)
  )

## ==========================================================
## FIG 2 – RF Importance (barras horizontais + rachuras + SE)
## ==========================================================
imp_df$Dominio <- factor(imp_df$Dominio,
                         levels = c("Rendimento","Agronômico","Químico","Físico"))
imp_df$Label   <- factor(imp_df$Label, levels = imp_df$Label[order(imp_df$IncMSE)])

## Barras de erro: ±1 SE (padrão para %IncMSE do randomForest)
imp_df$ymin <- imp_df$IncMSE - imp_df$IncMSE_SE
imp_df$ymax <- imp_df$IncMSE + imp_df$IncMSE_SE

p_imp <- ggplot(imp_df, aes(x = Label, y = IncMSE,
                            fill = Dominio, pattern = Dominio)) +
  geom_col_pattern(
    width             = 0.70,
    colour            = "grey30",
    linewidth         = 0.25,
    pattern_fill      = "grey40",
    pattern_colour    = "grey40",
    pattern_density   = 0.15,
    pattern_spacing   = 0.04,
    pattern_angle     = 45
  ) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                width = 0.25, linewidth = 0.3, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = domain_fill, name = "Domínio",
                    breaks = c("Rendimento","Agronômico","Químico","Físico")) +
  scale_pattern_manual(values = domain_pattern, name = "Domínio",
                       breaks = c("Rendimento","Agronômico","Químico","Físico")) +
  labs(x = NULL, y = "%IncMSE (permutação)") +
  guides(
    fill    = guide_legend(override.aes = list(
                pattern         = domain_pattern[c("Rendimento","Agronômico","Químico","Físico")],
                pattern_fill    = "grey40",
                pattern_colour  = "grey40",
                pattern_density = 0.15,
                pattern_spacing = 0.04)),
    pattern = "none"
  ) +
  theme_pub +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggsave("../3-FIGURAS/fig_rf_importance.png", p_imp,
       width = 7, height = 5.5, dpi = 300)
cat("OK fig_rf_importance.png\n")

## ==========================================================
## FIG 5 – Boxplot por combo x paradigma
## ==========================================================

## Preparar dados
df_box <- melt(
  iqs_all[, c("Combo","IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")],
  id.vars = "Combo", variable.name = "Paradigma", value.name = "IQS"
)
df_box$Paradigma <- gsub("IQS_", "", df_box$Paradigma)
df_box$Paradigma <- factor(df_box$Paradigma,
                           levels = c("ANOVA","Fuzzy","PLS","MARS","RF"))

## Ordenar combos por Borda (crescente = piores embaixo)
borda_order <- rank_df$Combo[order(rank_df$MetaIQS_Borda)]
df_box$Combo <- factor(df_box$Combo, levels = borda_order)

## Rótulos PT-BR
combo_labels <- c(
  "MT_Pigeon pea" = "CM\u2013Guandu",
  "MT_Sunn hemp"  = "CM\u2013Crotal\u00e1ria",
  "MT_Millet"     = "CM\u2013Milheto",
  "CT_Cowpea"     = "PC\u2013Caupi",
  "MT_Cowpea"     = "CM\u2013Caupi",
  "NT_Pigeon pea" = "PD\u2013Guandu",
  "NT_Millet"     = "PD\u2013Milheto",
  "NT_Sunn hemp"  = "PD\u2013Crotal\u00e1ria",
  "CT_Sunn hemp"  = "PC\u2013Crotal\u00e1ria",
  "NT_Cowpea"     = "PD\u2013Caupi",
  "CT_Millet"     = "PC\u2013Milheto",
  "CT_Pigeon pea" = "PC\u2013Guandu"
)

## Construir o plot paradigma a paradigma para controlar pattern_angle
p_box <- ggplot(df_box, aes(x = Combo, y = IQS))

for (par in levels(df_box$Paradigma)) {
  sub <- df_box[df_box$Paradigma == par, , drop = FALSE]
  if (nrow(sub) == 0) next
  p_box <- p_box +
    geom_boxplot_pattern(
      data              = sub,
      aes(fill = Paradigma, pattern = Paradigma, group = interaction(Combo, Paradigma)),
      position          = position_dodge(0.80),
      width             = 0.70,
      outlier.size      = 0.5,
      outlier.alpha     = 0.5,
      colour            = "grey30",
      linewidth         = 0.20,
      pattern_fill      = "grey40",
      pattern_colour    = "grey40",
      pattern_density   = 0.15,
      pattern_spacing   = 0.04,
      pattern_angle     = para_angle[par]
    )
}

p_box <- p_box +
  coord_flip() +
  scale_x_discrete(labels = combo_labels) +
  scale_fill_manual(values = para_fill, name = "Paradigma",
                    breaks = c("ANOVA","Fuzzy","PLS","MARS","RF")) +
  scale_pattern_manual(values = para_pattern, name = "Paradigma",
                       breaks = c("ANOVA","Fuzzy","PLS","MARS","RF")) +
  labs(x = NULL, y = "IQS (escala original, 0\u201310)") +
  guides(
    fill    = guide_legend(override.aes = list(
                pattern         = para_pattern[c("ANOVA","Fuzzy","PLS","MARS","RF")],
                pattern_fill    = "grey40",
                pattern_colour  = "grey40",
                pattern_density = 0.15,
                pattern_spacing = 0.04,
                pattern_angle   = para_angle[c("ANOVA","Fuzzy","PLS","MARS","RF")])),
    pattern = "none"
  ) +
  theme_pub +
  theme(legend.position = "bottom",
        legend.box      = "horizontal")

ggsave("../3-FIGURAS/fig_boxplot_combo.png", p_box,
       width = 9, height = 6.5, dpi = 300)
cat("OK fig_boxplot_combo.png\n")

cat("\nDone - 2 figuras pastel com rachuras salvas em 3-FIGURAS/\n")
