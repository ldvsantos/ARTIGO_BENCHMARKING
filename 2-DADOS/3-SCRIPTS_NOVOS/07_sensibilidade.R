## ============================================================
## 07_sensibilidade.R
## Monte Carlo estratificado: W de Kendall vs tamanho amostral
## Teste H3: relação log-linear entre W e n
## Artigo: Benchmarking multi-paradigma
## ============================================================

library(irr)
library(earth)
library(randomForest)
library(nlme)
library(ggplot2)

## ----------------------------------------------------------
## 1. Leitura dos dados
## ----------------------------------------------------------
dados <- read.csv(
  file.path("..", "1-DADOS_BRUTOS", "IPACSA_resultados_completos.csv")
)

norm_cols <- c(
  "DMG_norm", "DMP_norm", "RMP_norm", "Densidade_norm",
  "Estoque.de.C_norm", "Na_norm", "ICV..._norm",
  "Altura.de.Plantas_norm", "Diâmetro.espiga_norm",
  "Comprimento.espiga_norm", "Número.de.plantas_ha_norm",
  "N.total.de.espigas_ha_norm", "N.de.espigas.comerciais_ha_norm",
  "Peso.de.espigas.comerciais_ha_norm", "Produtividade_norm"
)

## Célula fatorial
dados$Celula <- paste(dados$Parcela, dados$Cultura, sep = "_")
celulas <- unique(dados$Celula)
cat("Células fatoriais:", length(celulas), "\n")
cat("n por célula:", table(dados$Celula)[1], "\n")

## ----------------------------------------------------------
## Funções auxiliares: computar IQS para cada paradigma
## ----------------------------------------------------------

compute_iqs_anova <- function(df) {
  pca_res <- prcomp(df[, norm_cols], center = TRUE, scale. = TRUE)
  eigvals <- pca_res$sdev^2
  n_comp <- max(1, sum(eigvals > 1))
  weights <- eigvals[1:n_comp] / sum(eigvals[1:n_comp])
  scores <- pca_res$x[, 1:n_comp, drop = FALSE]
  iqs <- as.numeric(scores %*% weights)
  (iqs - min(iqs)) / (max(iqs) - min(iqs) + 1e-10) * 10
}

compute_iqs_fuzzy <- function(df) {
  df$IPACSA
}

compute_iqs_pls <- function(df) {
  (df$COS + df$ARQ + df$STAND) / 3
}

compute_iqs_mars <- function(df) {
  fit <- tryCatch(
    earth(IPACSA ~ ., data = df[, c(norm_cols, "IPACSA")], degree = 2),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(NA, nrow(df)))
  predict(fit)[, 1]
}

compute_iqs_rf <- function(df) {
  fit <- tryCatch(
    randomForest(
      x = df[, norm_cols], y = df$IPACSA,
      ntree = 200, mtry = ceiling(sqrt(length(norm_cols))),
      nodesize = 5
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(NA, nrow(df)))
  predict(fit, newdata = df[, norm_cols])
}

## ----------------------------------------------------------
## 2. Monte Carlo estratificado
## ----------------------------------------------------------
sample_sizes <- c(15, 30, 50, 75, 100)
n_iter <- 200  # Reduzido de 1000 para viabilidade computacional
results <- data.frame()

cat("\n--- Monte Carlo estratificado ---\n")
cat("Iterações por nível:", n_iter, "\n")
cat("Tamanhos amostrais:", paste(sample_sizes, collapse = ", "), "\n\n")

for (n_target in sample_sizes) {
  cat("Processando n =", n_target, "...")
  seeds <- numeric(n_iter)
  W_values <- numeric(n_iter)
  
  for (iter in seq_len(n_iter)) {
    seed_i <- 1000 * n_target + iter
    set.seed(seed_i)
    seeds[iter] <- seed_i
    
    ## Amostragem estratificada por célula
    n_per_cell <- max(1, floor(n_target / length(celulas)))
    remainder <- n_target - n_per_cell * length(celulas)
    
    idx_sample <- c()
    for (cel in celulas) {
      cel_idx <- which(dados$Celula == cel)
      n_this <- min(n_per_cell, length(cel_idx))
      idx_sample <- c(idx_sample, sample(cel_idx, n_this))
    }
    
    ## Distribuir remainder
    if (remainder > 0) {
      remaining_idx <- setdiff(seq_len(nrow(dados)), idx_sample)
      if (length(remaining_idx) >= remainder) {
        idx_sample <- c(idx_sample, sample(remaining_idx, remainder))
      }
    }
    
    sub_df <- dados[idx_sample, ]
    
    ## Computar 5 IQS
    iqs <- tryCatch({
      data.frame(
        ANOVA = compute_iqs_anova(sub_df),
        Fuzzy = compute_iqs_fuzzy(sub_df),
        PLS   = compute_iqs_pls(sub_df),
        MARS  = compute_iqs_mars(sub_df),
        RF    = compute_iqs_rf(sub_df)
      )
    }, error = function(e) NULL)
    
    if (is.null(iqs) || any(is.na(iqs))) {
      W_values[iter] <- NA
      next
    }
    
    ## Kendall W
    rank_mat <- apply(iqs, 2, rank)
    kw <- tryCatch(
      irr::kendall(rank_mat, correct = FALSE)$value,
      error = function(e) NA
    )
    W_values[iter] <- kw
  }
  
  valid <- !is.na(W_values)
  cat(" concluído. Valid:", sum(valid), "/", n_iter,
      " | mean W =", round(mean(W_values[valid]), 4),
      " | sd =", round(sd(W_values[valid]), 4), "\n")
  
  results <- rbind(results, data.frame(
    n = n_target,
    iter = seq_len(n_iter),
    seed = seeds,
    W = W_values
  ))
}

## ----------------------------------------------------------
## 3. Sumário por tamanho amostral
## ----------------------------------------------------------
summary_mc <- aggregate(W ~ n, data = results, FUN = function(x) {
  x <- x[!is.na(x)]
  c(mean = mean(x), sd = sd(x), median = median(x),
    q025 = quantile(x, 0.025), q975 = quantile(x, 0.975), n_valid = length(x))
})
summary_mc <- cbind(summary_mc[, 1, drop = FALSE],
                    as.data.frame(summary_mc$W))
names(summary_mc) <- c("n", "mean_W", "sd_W", "median_W", "q025", "q975", "n_valid")

cat("\n--- Sumário Monte Carlo ---\n")
print(round(summary_mc, 4))

## ----------------------------------------------------------
## 4. Teste H3: W ~ log(n) via GLS
## ----------------------------------------------------------
summary_mc$log_n <- log(summary_mc$n)

gls_fit <- tryCatch({
  nlme::gls(mean_W ~ log_n, data = summary_mc,
            weights = varFixed(~ (sd_W^2 / n_valid)))
}, error = function(e) {
  cat("GLS falhou, usando lm:\n")
  lm(mean_W ~ log_n, data = summary_mc)
})

cat("\n--- H3: Regressão W ~ log(n) ---\n")
print(summary(gls_fit))

beta1 <- coef(gls_fit)["log_n"]
cat("\nbeta1 (log_n) =", round(beta1, 4), "\n")
cat("H3 resultado:", ifelse(beta1 > 0, "beta1 > 0 (W cresce com log(n))",
                             "beta1 <= 0"), "\n")

## ----------------------------------------------------------
## 5. Figura W vs n
## ----------------------------------------------------------
results_valid <- results[!is.na(results$W), ]

p_mc <- ggplot(results_valid, aes(x = factor(n), y = W)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(x = "Tamanho amostral (n)", y = "W de Kendall",
       title = "Sensibilidade da concordância ao tamanho amostral") +
  theme_minimal()

dir.create("../3-FIGURAS", showWarnings = FALSE)
ggsave("../3-FIGURAS/fig_mc_sensitivity.png", p_mc, width = 7, height = 5, dpi = 300)
cat("\nFigura salva: fig_mc_sensitivity.png\n")

## Curva com IC
p_curve <- ggplot(summary_mc, aes(x = n, y = mean_W)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "lightblue", alpha = 0.5) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = summary_mc$mean_W[summary_mc$n == max(summary_mc$n)],
             linetype = "dashed", color = "red") +
  labs(x = "Tamanho amostral (n)", y = expression(bar(W)),
       title = "Concordância inter-paradigma vs tamanho amostral") +
  theme_minimal()

ggsave("../3-FIGURAS/fig_mc_curve.png", p_curve, width = 7, height = 5, dpi = 300)

## ----------------------------------------------------------
## 6. Exportar
## ----------------------------------------------------------
write.csv(results, file = "../1-DADOS_BRUTOS/MC_resultados_brutos.csv", row.names = FALSE)
write.csv(summary_mc, file = "../1-DADOS_BRUTOS/MC_sumario.csv", row.names = FALSE)

cat("\n=== 07_sensibilidade.R concluído ===\n")
