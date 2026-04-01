## ============================================================
## 00_theme_pub.R
## Tema profissional para publicação (Springer Nature, Q1)
## Fonte: calibrado para largura de coluna 84mm / página 174mm
## ============================================================

library(ggplot2)
library(scales)

## Paleta discreta (colorblind-safe, Nature-style)
pal_nature <- c(
  "#0072B2",  # blue

  "#D55E00",  # vermillion
  "#009E73",  # bluish green
  "#CC79A7",  # reddish purple
  "#E69F00",  # orange
  "#56B4E9",  # sky blue
  "#F0E442",  # yellow
  "#000000"   # black
)

## Cores de preenchimento (versão pastel da paleta)
pal_fill <- c(
  "#B3D9F2",  # light blue
  "#F2C6A5",  # light vermillion
  "#A5E0CC",  # light green
  "#E8BDD9",  # light purple
  "#F5D7A0",  # light orange
  "#C0E4F7",  # light sky
  "#FAF3B3",  # light yellow
  "#CCCCCC"   # light grey
)

## Tema principal para publicação
theme_pub <- function(base_size = 10, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      ## Panel
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      panel.background = element_rect(fill = "white"),

      ## Axes
      axis.title   = element_text(size = rel(1.0), face = "bold"),
      axis.text    = element_text(size = rel(0.85), colour = "black"),
      axis.ticks   = element_line(colour = "black", linewidth = 0.4),
      axis.line    = element_blank(),

      ## Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      legend.title      = element_text(size = rel(0.85), face = "bold"),
      legend.text       = element_text(size = rel(0.8)),
      legend.position   = "bottom",
      legend.direction   = "horizontal",

      ## Strip (facets)
      strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.4),
      strip.text       = element_text(size = rel(0.85), face = "bold"),

      ## Plot
      plot.title    = element_blank(),  # títulos vão na caption do LaTeX
      plot.subtitle = element_blank(),
      plot.margin   = margin(8, 8, 8, 8, "pt")
    )
}

## Dimensões padrão para Springer Nature
## Coluna simples: 84mm ≈ 3.31in
## Coluna dupla: 174mm ≈ 6.85in
fig_width_single <- 3.31
fig_width_double <- 6.85
fig_height_standard <- 3.5
fig_height_tall <- 5.0
fig_dpi <- 600

## Função utilitária para salvar
save_pub <- function(plot, filename, width = fig_width_double,
                     height = fig_height_standard, dpi = fig_dpi) {
  ggsave(filename, plot, width = width, height = height, dpi = dpi,
         units = "in", bg = "white")
  cat("Salvo:", filename, "(", width, "x", height, "in @", dpi, "dpi)\n")
}
