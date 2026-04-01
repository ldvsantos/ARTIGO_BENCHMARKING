# Script to calculate Table 1 values for Sunn hemp (Crotalaria juncea)
# Including CT, MT, NT

req_pkgs <- c("readxl", "dplyr", "FuzzyR", "tidyr")
for(p in req_pkgs) {
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# 1. Load Data
if(file.exists("banco_dados.xlsx")) {
  path_db <- "banco_dados.xlsx"
} else if(file.exists("2 - DADOS/banco_dados.xlsx")) {
  path_db <- "2 - DADOS/banco_dados.xlsx"
} else {
  stop("Arquivo banco_dados.xlsx não encontrado.")
}

dados_010 <- read_excel(path_db, sheet = "dados_010")
dados_010$Profundidade <- "0-10 cm"
dados_1020 <- read_excel(path_db, sheet = "dados_1020")
dados_1020$Profundidade <- "10-20 cm"
colunas_comuns <- intersect(names(dados_010), names(dados_1020))
dados <- rbind(dados_010[, colunas_comuns], dados_1020[, colunas_comuns])

colunas_necessarias <- c(
  "DMG","DMP","RMP","Densidade","Estoque de C","Na","ICV(%)",
  "Altura de Plantas","Diâmetro espiga","Comprimento espiga",
  "Número de plantas_ha","N total de espigas_ha","N de espigas comerciais_ha",
  "Peso de espigas comerciais_ha","Produtividade","Cultura","Parcela", "Profundidade"
)
dados <- dados[, colunas_necessarias]
dados <- dados[complete.cases(dados), ]

# 2. Normalize
direcao <- c(
  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,
  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,
  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,
  `Peso de espigas comerciais_ha`=1, Produtividade=1
)

normalizar_robusto <- function(x, maior_melhor = TRUE) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(5, length(x)))
  q <- quantile(x, probs = c(.05, .95), na.rm = TRUE, type = 8)
  if (diff(q) == 0) return(rep(5, length(x)))
  x2 <- (x - q[1]) / (q[2] - q[1])
  x2 <- pmin(pmax(x2, 0), 1) * 10
  if (!maior_melhor) x2 <- 10 - x2
  x2[is.na(x2)] <- 5
  x2
}

dados_norm <- dados %>%
  mutate(across(names(direcao), 
                ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),
                .names = "{.col}_norm"))

# 3. Fuzzy Systems (Simplified for calculation)
add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {
  x <- x[is.finite(x)]
  qs <- quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)
  L <- -2; U <- 12 
  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))
  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))
  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))
  fis
}

criar_fis_COS <- function(dn) {
  fis <- newfis("COS", defuzzMethod = "centroid")
  in_vars <- c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "COS", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras_orig <- rbind(c(2,2,1,1,1,3,3, 3, 1,1), c(2,2,2,2,2,2,2, 2, 1,1), c(1,1,3,3,3,1,1, 1, 1,1))
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

criar_fis_ARQ <- function(dn) {
  fis <- newfis("ARQ", defuzzMethod = "centroid")
  in_vars <- c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",
               "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "ARQ", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras_orig <- rbind(c(3,3,3,3,3,3, 3, 1,1), c(2,2,2,2,2,2, 2, 1,1), c(1,1,1,1,1,1, 1, 1,1))
  regras_fill <- matrix(0, nrow = 0, ncol = length(in_vars) + 3)
  for(i in 1:length(in_vars)) {
    r_low <- rep(0, length(in_vars)); r_low[i] <- 1
    regras_fill <- rbind(regras_fill, c(r_low, 1, 0.1, 1))
    r_med <- rep(0, length(in_vars)); r_med[i] <- 2
    regras_fill <- rbind(regras_fill, c(r_med, 2, 0.1, 1))
    r_high <- rep(0, length(in_vars)); r_high[i] <- 3
    regras_fill <- rbind(regras_fill, c(r_high, 3, 0.1, 1))
  }
  addrule(fis, rbind(regras_orig, regras_fill))
}

criar_fis_STAND <- function(dn) {
  fis <- newfis("STAND", defuzzMethod = "centroid")
  in_var <- "Número de plantas_ha_norm"
  fis <- addvar(fis, "input", in_var, c(0,10))
  fis <- add_tri_from_quartis(fis, 1, dn[[in_var]])
  fis <- addvar(fis, "output", "STAND", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))
  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))
  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(3, 3, 1,1), c(2, 2, 1,1), c(1, 1, 1,1))
  addrule(fis, regras)
}

fis_COS <- criar_fis_COS(dados_norm)
fis_ARQ <- criar_fis_ARQ(dados_norm)
fis_STAND <- criar_fis_STAND(dados_norm)

Xcos <- as.matrix(dados_norm[, c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")])
Xarq <- as.matrix(dados_norm[, c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",
                                 "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")])
Xstd <- as.matrix(dados_norm[, "Número de plantas_ha_norm", drop=FALSE])

dados_norm$COS <- apply(Xcos, 1, function(l) evalfis(matrix(l,1), fis_COS))
dados_norm$ARQ <- apply(Xarq, 1, function(l) evalfis(matrix(l,1), fis_ARQ))
dados_norm$STAND <- apply(Xstd, 1, function(l) evalfis(matrix(l,1), fis_STAND))

# 4. Calculate Means for Sunn hemp
# Identify the crop name
crops <- unique(dados_norm$Cultura)
sunn_hemp_name <- crops[grep("rotalaria|unn", crops, ignore.case=TRUE)][1]

cat("Crop Name found:", sunn_hemp_name, "\n")

df_sunn <- dados_norm %>% 
  filter(Cultura == sunn_hemp_name) %>%
  group_by(Parcela) %>%
  summarise(
    Mean_SC = mean(COS, na.rm=TRUE),
    Mean_MrP = mean(ARQ, na.rm=TRUE),
    Mean_STAND = mean(STAND, na.rm=TRUE),
    Mean_Yield = mean(Produtividade_norm, na.rm=TRUE)
  )

print(df_sunn)

# 5. Determine Top Activated Rule (Heuristic)
# Rules for Final Index:
# R1: High, High, High, High -> High (1.2)
# R2: Med, Med, Med, Med -> Med (1.0)
# R3: Low, Low, Low, Low -> Low (0.8)
# ... others ...

# We can infer the rule based on the mean values.
# Low: 0-3.3, Med: 3.4-6.6, High: 6.7-10
get_class <- function(x) {
  if(x < 3.4) return("Low")
  if(x < 6.7) return("Med")
  return("High")
}

df_sunn$Class_SC <- sapply(df_sunn$Mean_SC, get_class)
df_sunn$Class_MrP <- sapply(df_sunn$Mean_MrP, get_class)
df_sunn$Class_STAND <- sapply(df_sunn$Mean_STAND, get_class)
df_sunn$Class_Yield <- sapply(df_sunn$Mean_Yield, get_class)

print(df_sunn)
