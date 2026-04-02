library(irr)
library(irrCAC)

base <- "c:/Users/vidal/OneDrive/Documentos/13 - CLONEGIT/artigo-posdoc/7-ARTIGO_BENCHMARKING/2-DADOS/1-DADOS_BRUTOS"
iqs <- read.csv(file.path(base, "IQS_todos_paradigmas_34ind.csv"))

# Concordance
iqs_cols <- c("IQS_ANOVA","IQS_Fuzzy","IQS_PLS","IQS_MARS","IQS_RF")
ranks <- apply(iqs[, iqs_cols], 2, rank)

# Kendall W
kw <- kendall(ranks, correct=TRUE)
cat("=== KENDALL W ===\n")
print(kw)

# ICC(2,1)
icc_res <- icc(iqs[, iqs_cols], model="twoway", type="agreement", unit="single")
cat("\n=== ICC(2,1) ===\n")
print(icc_res)

# Gwet AC1 usando quintis
quintis <- apply(iqs[, iqs_cols], 2, function(x) cut(x, breaks=quantile(x, probs=0:5/5), include.lowest=TRUE, labels=1:5))
df_q <- as.data.frame(quintis)
cat("\n=== GWET AC1 ===\n")
tryCatch({
  gwet_res <- gwet.ac1.raw(as.matrix(df_q))
  print(gwet_res)
}, error=function(e) {
  tryCatch({
    gwet_res <- gwet.ac1.table(as.matrix(df_q))
    print(gwet_res)
  }, error=function(e2) {
    cat("Available irrCAC functions:\n")
    print(ls("package:irrCAC"))
    cat("Error:", e2$message, "\n")
  })
})

# H2 test
library(psych)
sp <- cor(iqs[, iqs_cols], method="spearman")
nl_pairs <- c(sp["IQS_Fuzzy","IQS_MARS"], sp["IQS_Fuzzy","IQS_RF"], sp["IQS_MARS","IQS_RF"])
ln_pairs <- c(sp["IQS_ANOVA","IQS_Fuzzy"], sp["IQS_ANOVA","IQS_MARS"], sp["IQS_ANOVA","IQS_RF"],
              sp["IQS_PLS","IQS_Fuzzy"], sp["IQS_PLS","IQS_MARS"], sp["IQS_PLS","IQS_RF"])
cat("\n=== H2 TEST ===\n")
cat("NL-NL pairs rho:", nl_pairs, "\n")
cat("NL-NL mean:", mean(nl_pairs), "\n")
cat("Lin-NL pairs rho:", ln_pairs, "\n")
cat("Lin-NL mean:", mean(ln_pairs), "\n")
z_nl <- atanh(nl_pairs)
z_ln <- atanh(ln_pairs)
wt <- wilcox.test(z_nl, z_ln, alternative="greater")
cat("Wilcoxon W=", wt$statistic, " p=", wt$p.value, "\n")

# H3 from MC data
mc <- read.csv(file.path(base, "MC_sumario_34ind.csv"))
cat("\n=== H3 (MC regression) ===\n")
fit <- lm(mean_W ~ log_n, data=mc)
cat("beta1=", coef(fit)[2], " p=", summary(fit)$coefficients[2,4], "\n")
cat("R2=", summary(fit)$r.squared, "\n")
print(mc)

# PCA info
cat("\n=== PCA summary ===\n")
norm_cols <- grep("_norm$", names(iqs), value=TRUE)
pca <- prcomp(iqs[, norm_cols], center=TRUE, scale.=TRUE)
ev <- pca$sdev^2
cumvar <- cumsum(ev)/sum(ev)
npc <- which(cumvar >= 0.80)[1]
cat("PCs retained (80%):", npc, "\n")
cat("Cumulative variance at", npc, "PCs:", round(cumvar[npc]*100, 1), "%\n")
cat("Top loadings PC1:", sort(abs(pca$rotation[,1]), decreasing=TRUE)[1:5], "\n")
cat("Top vars PC1:", names(sort(abs(pca$rotation[,1]), decreasing=TRUE))[1:5], "\n")

# IQS descriptive stats
cat("\n=== IQS DESCRIPTIVES ===\n")
for(col in iqs_cols) {
  cat(col, ": mean=", round(mean(iqs[[col]]),3), " sd=", round(sd(iqs[[col]]),3), "\n")
}
