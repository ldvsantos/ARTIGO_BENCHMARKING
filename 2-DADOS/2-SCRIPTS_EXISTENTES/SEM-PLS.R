# 1. Pacotes
library(plspm)
library(readxl)
library(dplyr)
library(DiagrammeR)
library(car)
library(cSEM)
library(ggplot2)


rm(list = ls())
gc()

# 2. Dados
dados <- read_excel("C:/Users/vidal/OneDrive/Documentos/ARTIGO_MA/3 - DADOS/bd.xlsx",
                    sheet = "MEE") |> 
  select(
    ER, EMACRO, PR, PWAC, PB, WSA, AMG, SOC, TN, ATP, PH, AK, BR, MBC, qmic, qco2, Q) |> 
  na.omit()

# 3. Modelo PLS-PM com plspm (constructo de 2ª ordem com repeated indicators)
inner_model <- matrix(c(
  0, 0, 0, 0,  # FISICO
  0, 0, 0, 0,  # QUIMICO
  0, 0, 0, 0,  # BIOLÓGICO
  1, 1, 1, 0   # INDICADOR_FUNCIONAL recebe de FISICO, QUIMICO, BIOLÓGICO
), nrow = 4, byrow = TRUE)

colnames(inner_model) <- rownames(inner_model) <- c("FISICO", "QUIMICO", "BIOLOGICO", "INDICADOR_FUNCIONAL")

outer_model <- list(
  c("EMACRO", "PR", "PWAC", "PB", "WSA", "Q"),
  c("AMG", "SOC", "ATP", "TN", "PH", "AK"),
  c("BR", "MBC", "qmic", "qco2"),
  c("EMACRO", "PR", "PWAC", "PB", "WSA", "AMG", "SOC", "TN", "ATP", "BR", "MBC", "qmic", "qco2")
)
modes <- c("A", "A", "A", "A")

modelo_pls <- plspm(dados, inner_model, outer_model, modes = modes)

# Resultados principais
summary(modelo_pls)
modelo_pls$path_coefs
modelo_pls$outer_model
modelo_pls$gof
plot(modelo_pls)



library(DiagrammeR)

grViz("
digraph modelo_funcional {
  graph [layout = dot, rankdir = TB]
  node [fontname = Helvetica, fontsize = 10, style = filled, shape = box, fillcolor = '#F0F8FF']

  // Indicadores FÍSICO
  subgraph cluster_fisico {
    label = 'Indicadores Físicos'
    style = dashed
    EMACRO [label = 'EMACRO\\n𝜆 = 0.6740']
    PR     [label = 'PR\\n𝜆 = -0.9322']
    PWAC   [label = 'PWAC\\n𝜆 = 0.8724']
    PB     [label = 'PB\\n𝜆 = 0.2250']
    WSA    [label = 'WSA\\n𝜆 = -0.6384']
    Q      [label = 'Q\\n𝜆 = 0.1155']
  }

  // Indicadores QUÍMICO
  subgraph cluster_quimico {
    label = 'Indicadores Químicos'
    style = dashed
    AMG    [label = 'AMG\\n𝜆 = 0.9998']
    SOC    [label = 'SOC\\n𝜆 = -0.8867']
    TN     [label = 'TN\\n𝜆 = -0.4276']
    PH     [label = 'PH\\n𝜆 = 0.8776']
    ATP    [label = 'ATP\\n𝜆 = 0.0060']
    AK     [label = 'AK\\n𝜆 = 0.3440']
  }

  // Indicadores BIOLÓGICO
  subgraph cluster_biologico {
    label = 'Indicadores Biológicos'
    style = dashed
    BR     [label = 'BR\\n𝜆 = 0.7166']
    MBC    [label = 'MBC\\n𝜆 = 0.7896']
    qmic   [label = 'qmic\\n𝜆 = -0.7617']
    qco2   [label = 'qCO₂\\n𝜆 = 0.8700']
  }

  // Construtos latentes
  FISICO     [label = 'Fator Físico', shape = ellipse, fillcolor = '#A9D0F5']
  QUIMICO    [label = 'Fator Químico', shape = ellipse, fillcolor = '#BCF5A9']
  BIOLOGICO  [label = 'Fator Biológico', shape = ellipse, fillcolor = '#F5A9D0']
  FUNCIONAL  [label = 'Indicador Funcional\\n(R² = 1.00)', shape = ellipse, fillcolor = '#F3E2A9']

  // Conexões FÍSICO
  EMACRO -> FISICO
  PR     -> FISICO
  PWAC   -> FISICO
  PB     -> FISICO
  WSA    -> FISICO
  Q      -> FISICO

  // Conexões QUÍMICO
  AMG    -> QUIMICO
  SOC    -> QUIMICO
  TN     -> QUIMICO
  PH     -> QUIMICO
  ATP    -> QUIMICO
  AK     -> QUIMICO

  // Conexões BIOLÓGICO
  BR     -> BIOLOGICO
  MBC    -> BIOLOGICO
  qmic   -> BIOLOGICO
  qco2   -> BIOLOGICO

  // Caminhos estruturais
  FISICO    -> QUIMICO   [label = '𝛽 = 0.922']
  FISICO    -> BIOLOGICO [label = '𝛽 = -0.197']
  FISICO    -> FUNCIONAL [label = '𝛽 = -0.665']
  QUIMICO   -> BIOLOGICO [label = '𝛽 = -0.763']
  QUIMICO   -> FUNCIONAL [label = '𝛽 = -0.235']
  BIOLOGICO -> FUNCIONAL [label = '𝛽 = 0.121']
}
")





#==========================
# AJUSTES DO MODELO
#==========================

# VIF (colinearidade entre construtos exógenos)
scores <- as.data.frame(modelo_pls$scores)
mod_vif <- lm(INDICADOR_FUNCIONAL ~ FISICO + QUIMICO + BIOLOGICO, data = scores)
vif(mod_vif)
#==========================
# AVALIAÇÃO PLS-PM com cSEM (modelo hierárquico - two-stage)
#==========================

# 1. SRMR
srmr_pls <- modelo_pls$gof["SRMR"]
df_srmr <- data.frame(Metrica = "SRMR", Valor = srmr_pls)

# 2. R² (se disponível)
if (length(modelo_pls$inner_model_R2) > 0) {
  r2_vals <- modelo_pls$inner_model_R2
  df_r2 <- data.frame(Metrica = paste0("R² - ", names(r2_vals)),
                      Valor = as.numeric(r2_vals))
} else {
  df_r2 <- data.frame(Metrica = character(0), Valor = numeric(0))
}

# 3. DG.rho
cr <- modelo_pls$unid[, "DG.rho"]
df_cr <- data.frame(Metrica = paste0("DG.rho - ", rownames(modelo_pls$unid)),
                    Valor = as.numeric(cr))

# 4. AVE
ave <- tapply(modelo_pls$outer_model$loading^2, 
              modelo_pls$outer_model$block, mean)
df_ave <- data.frame(Metrica = paste0("AVE - ", names(ave)),
                     Valor = as.numeric(ave))

# 5. HTMT
scores <- as.data.frame(modelo_pls$scores)
corr_mat <- cor(scores)
pairs <- combn(colnames(scores), 2)
htmt_vals <- apply(pairs, 2, function(p) {
  corr <- abs(corr_mat[p[1], p[2]])
  denom <- sqrt(
    mean(corr_mat[p[1], p[1]]) * mean(corr_mat[p[2], p[2]])
  )
  corr / denom
})
names(htmt_vals) <- apply(pairs, 2, paste, collapse = "_vs_")
df_htmt <- data.frame(Metrica = paste0("HTMT ", names(htmt_vals)),
                      Valor = as.numeric(htmt_vals))

# Unir tudo
tabela_metricas <- rbind(df_srmr, df_r2, df_cr, df_ave, df_htmt)

# Exibir no console
print(tabela_metricas, row.names = FALSE)

# Se estiver usando RMarkdown ou quer deixar mais bonito:
 knitr::kable(tabela_metricas, caption = "Métricas do Modelo PLS-PM")


 
 
 
 
 
 
 # =============================
 # VIP Plot – Critério de Wold
 # =============================
 
 
 vip_scores <- (modelo_pls$outer_model %>%
                  group_by(name) %>%
                  summarise(VIP = sqrt(sum(loading^2))) %>%
                  arrange(desc(VIP)))
 
 vip_scores$Elemento <- "VIP scores"  # adiciona uma coluna para a legenda dos pontos
 
 ggplot(vip_scores, aes(x = reorder(name, VIP), y = VIP)) +
   geom_segment(aes(xend = name, y = 0, yend = VIP), color = "#1f77b4", size = 0.7) +
   geom_point(aes(color = Elemento), size = 2.5) +
   geom_hline(aes(yintercept = 0.8, linetype = "VIP threshold"), color = "gray30") +
   scale_color_manual(name = "", values = c("VIP scores" = "#1f77b4")) +
   scale_linetype_manual(name = "", values = c("VIP threshold" = "dashed")) +
   labs(
     title = "Variable Importance according to VIP (Wold's Criterion)",
     x = "Predictor Variables",
     y = "VIP Score"
   ) +
   theme_minimal(base_size = 14) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     plot.title = element_text(face = "bold", hjust = 0.5),
     legend.position = "bottom"
   )
 
 
 # Tabela de VIPs
 tabela_vip <- vip_scores %>%
   rename(Variável = name) %>%         # renomeia a coluna "name"
   mutate(VIP = round(VIP, 3)) %>%     # arredonda apenas a coluna VIP
   arrange(desc(VIP))
 
 # Visualizar
 print("TABELA 4 – Importância das variáveis (VIP):")
 print(tabela_vip)
 
 
 #VIP > 1 → variável considerada importante para o modelo
 #VIP < 0.8 → pouco relevante
 #VIP entre 0.8 e 1 → relevância intermediária
 
 
 
 