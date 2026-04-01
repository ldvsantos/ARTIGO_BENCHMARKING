################################################################################
# SCRIPT UNIFICADO - CÁLCULO FUZZY, ANÁLISE ESTATÍSTICA E GRÁFICOS
#
# ETAPAS:
# 1. SETUP: Carrega todos os pacotes necessários e limpa o ambiente.
# 2. CÁLCULO FUZZY: Importa os dados brutos e calcula os subíndices e o IPACSA.
# 3. ANÁLISE E GRÁFICOS: Roda um loop para fazer a análise estatística (ANOVA +
#    Tukey) e gerar gráficos de barras com letras de comparação para cada variável.
# 4. ANÁLISES ADICIONAIS: Cria tabelas, boxplot e heatmap específicos para o IPACSA.
# 5. EXPORTAÇÃO: Salva todos os resultados (gráficos, tabelas .csv e .xlsx).
################################################################################
install.packages("httpgd")


## 1) SETUP: PACOTES E AMBIENTE ----

# Lista completa de pacotes necessários para todo o processo
req_pkgs <- c(
  "FuzzyR",         # Essencial para o cálculo Fuzzy
  "readxl",         # Para ler arquivos .xlsx
  "dplyr",          # Para manipulação de dados
  "tidyr",          # Para organizar dados (pivot_wider/longer)
  "forcats",        # Para trabalhar com fatores
  "readr",          # Para salvar arquivos .csv
  "ggplot2",        # Para criar os gráficos
  "ggpattern",      # Para as hachuras nos gráficos
  "RColorBrewer",   # Para as paletas de cores
  "car",            # Para a Anova (type II/III)
  "emmeans",        # Para médias marginais estimadas
  "multcomp",       # Para o teste de Tukey e letras (cld)
  "multcompView",   # Dependência do multcomp
  "openxlsx",        # Para salvar arquivos .xlsx
  "httpgd",          #Rodar grafico svg
  "radian"
)

# Bloco de código para verificar, instalar (se necessário) e carregar os pacotes
ins <- req_pkgs[!sapply(req_pkgs, require, character.only = TRUE)]
if(length(ins)) install.packages(ins, dependencies = TRUE)
invisible(lapply(req_pkgs, require, character.only = TRUE))

# Limpeza do ambiente de trabalho
rm(list = ls())
graphics.off()


################################################################################
# PARTE 1: CÁLCULO DOS ÍNDICES FUZZY
################################################################################

## 2) IMPORTAÇÃO DOS DADOS BRUTOS ----
# Carrega os dados da planilha Excel
dados_brutos <- readxl::read_xlsx("banco_dados.xlsx", sheet = "dados_010")
cat("Colunas disponíveis no banco de dados:\n")
print(names(dados_brutos))

# Verifica se todas as colunas necessárias estão presentes

# Checa e filtra colunas essenciais de forma compacta
colunas_necessarias <- c(
  "DMG","DMP","RMP","Densidade","Estoque de C","Na","ICV(%)",
  "Altura de Plantas","Diâmetro espiga","Comprimento espiga",
  "Número de plantas_ha","N total de espigas_ha","N de espigas comerciais_ha",
  "Peso de espigas comerciais_ha","Produtividade","Cultura","Parcela"
)
if(any(!colunas_necessarias %in% names(dados_brutos)))
  stop("Erro: Faltam colunas essenciais: ", paste(setdiff(colunas_necessarias, names(dados_brutos)), collapse=", "))
dados_brutos <- na.omit(dados_brutos[, colunas_necessarias])


## 3) DEFINIÇÃO DA DIREÇÃO DAS VARIÁVEIS E CHECAGENS ----
# 1 = quanto maior, melhor; -1 = quanto menor, melhor
direcao <- c(
  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,
  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,
  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,
  `Peso de espigas comerciais_ha`=1, Produtividade=1
)
if(any(dados_brutos$`ICV(%)` > 100, na.rm=TRUE)) warning("Atenção: Existem valores de ICV(%) > 100.")
if(any(dados_brutos$Densidade < 0.9 | dados_brutos$Densidade > 1.8, na.rm=TRUE)) message("Info: A variável 'Densidade' possui valores fora do intervalo típico.")


## 4) NORMALIZAÇÃO ROBUSTA (ESCALA 0–10) ----
normalizar_robusto <- function(x, maior_melhor = TRUE) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(5, length(x)))
  q <- stats::quantile(x, probs = c(.05, .95), na.rm = TRUE, type = 8)
  if (diff(q) == 0) return(rep(5, length(x)))
  x2 <- (x - q[1]) / (q[2] - q[1])
  x2 <- pmin(pmax(x2, 0), 1) * 10
  if (!maior_melhor) x2 <- 10 - x2
  x2[is.na(x2)] <- 5
  x2
}

# Aplica a normalização
dados_norm <- dados_brutos %>%
  dplyr::mutate(
    dplyr::across(
      names(direcao),
      ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),
      .names = "{.col}_norm"
    )
  )


## 5) DEFINIÇÃO DOS SISTEMAS DE INFERÊNCIA FUZZY (FIS) ----

# Função auxiliar para criar funções de pertinência triangulares
add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {
  x <- x[is.finite(x)]
  qs <- stats::quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)
  L <- 0; U <- 10
  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))
  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))
  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))
  fis
}

# Subíndice COS (Conservação do Solo)
criar_fis_COS <- function(dn) {
  fis <- newfis("COS", defuzzMethod = "centroid")
  in_vars <- c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10))
    fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "COS", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5)); fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5)); fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(2,2,1,1,1,3,3, 3, 1,1), c(2,2,2,2,2,2,2, 2, 1,1), c(1,1,3,3,3,1,1, 1, 1,1)); addrule(fis, regras)
}

# Subíndice ARQ (Arquitetura de Plantas)
criar_fis_ARQ <- function(dn) {
  fis <- newfis("ARQ", defuzzMethod = "centroid")
  in_vars <- c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm", "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10)); fis <- add_tri_from_quartis(fis, i, dn[[in_vars[i]]])
  }
  fis <- addvar(fis, "output", "ARQ", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5)); fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5)); fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(3,3,3,3,3,3, 3, 1,1), c(2,2,2,2,2,2, 2, 1,1), c(1,1,1,1,1,1, 1, 1,1)); addrule(fis, regras)
}

# Subíndice STAND (Estande de Plantas)
criar_fis_STAND <- function(dn) {
  fis <- newfis("STAND", defuzzMethod = "centroid")
  in_var <- "Número de plantas_ha_norm"
  fis <- addvar(fis, "input", in_var, c(0,10)); fis <- add_tri_from_quartis(fis, 1, dn[[in_var]])
  fis <- addvar(fis, "output", "STAND", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5)); fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5)); fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(3, 3, 1,1), c(2, 2, 1,1), c(1, 1, 1,1)); addrule(fis, regras)
}

# FIS FINAL (IPACSA)
criar_fis_FINAL <- function() {
  fis <- newfis("IPACSA_final", defuzzMethod = "centroid")
  in_vars <- c("COS","ARQ","STAND","Produtividade_norm")
  for (i in seq_along(in_vars)) {
    fis <- addvar(fis, "input", in_vars[i], c(0,10)); fis <- addmf(fis, "input", i, "baixa", "trimf", c(0,2.5,5)); fis <- addmf(fis, "input", i, "media", "trimf", c(2.5,5,7.5)); fis <- addmf(fis, "input", i, "alta",  "trimf", c(5,7.5,10))
  }
  fis <- addvar(fis, "output", "IPACSA", c(0,10))
  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5)); fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5)); fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))
  regras <- rbind(c(2,3,2,3, 3, 1.2, 1), c(2,2,2,2, 2, 1.0, 1), c(1,1,1,1, 1, 0.8, 1), c(3,2,2,2, 3, 1.1, 1), c(2,3,2,3, 3, 1.1, 1), c(2,2,3,2, 3, 1.1, 1), c(2,3,2,2, 3, 1.15,1)); addrule(fis, regras)
}


## 6) PIPELINE DE CÁLCULO (Subíndices + IPACSA) ----
fis_COS   <- criar_fis_COS(dados_norm)
fis_ARQ   <- criar_fis_ARQ(dados_norm)
fis_STAND <- criar_fis_STAND(dados_norm)
fis_FINAL <- criar_fis_FINAL()

calc_subindices <- function(df) {
  Xcos <- as.matrix(df[, c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")])
  Xarq <- as.matrix(df[, c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm", "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")])
  Xstd <- as.matrix(df[, "Número de plantas_ha_norm", drop=FALSE])
  df$COS   <- apply(Xcos, 1, function(l) evalfis(matrix(l,1), fis_COS))
  df$ARQ   <- apply(Xarq, 1, function(l) evalfis(matrix(l,1), fis_ARQ))
  df$STAND <- apply(Xstd, 1, function(l) evalfis(matrix(l,1), fis_STAND))
  df
}
dados_norm <- calc_subindices(dados_norm)

Xfinal <- as.matrix(dados_norm[, c("COS","ARQ","STAND","Produtividade_norm")])
dados_norm$IPACSA <- apply(Xfinal, 1, function(l) evalfis(matrix(l,1), fis_FINAL))

# Organização final dos dados para análises
dados_final <- dados_norm %>%
  dplyr::mutate(
    Parcela = factor(trimws(Parcela), levels = c("CC","CM","PD")),
    Cultura = as.factor(trimws(Cultura))
  ) %>%
  dplyr::filter(is.finite(IPACSA))


################################################################################
# PARTE 2: ANÁLISE ESTATÍSTICA E GRÁFICOS (EM LOOP)
################################################################################

## 7) DEFINIÇÕES DE ESTILO E VARIÁVEIS PARA ANÁLISE ----
cat("\n--- INICIANDO ANÁLISE ESTATÍSTICA E GERAÇÃO DE GRÁFICOS ---\n")

# Lista de variáveis-resposta que serão analisadas no loop
# Inclui o IPACSA, seus subíndices e a produtividade normalizada
variaveis_para_analise <- c("IPACSA", "COS", "ARQ", "STAND", "Produtividade_norm")

# Listas para armazenar os resultados do loop
resultados_estatisticos <- list()
tabelas_para_graficos <- list()

# --- Definições de Estilo Consistentes para os Gráficos ---
n_culturas <- length(unique(dados_final$Cultura))
culturas_unicas <- sort(unique(dados_final$Cultura))

# Paleta de cores em tons pastel
if (n_culturas < 3) {
  cores_base <- c("#FBB4AE", "#B3CDE3"); cores_pastel <- cores_base[1:n_culturas]
} else { cores_pastel <- RColorBrewer::brewer.pal(n = n_culturas, name = "Pastel1") }
names(cores_pastel) <- culturas_unicas

# Padrões de hachura
lista_de_padroes <- c("stripe", "crosshatch", "circle", "wave", "grid", "hexagon")
if (n_culturas > length(lista_de_padroes)) {
  padroes_hachura <- rep(lista_de_padroes, length.out = n_culturas)
} else { padroes_hachura <- lista_de_padroes[1:n_culturas] }
names(padroes_hachura) <- culturas_unicas


## 8) LOOP PARA ANÁLISE E GERAÇÃO DE GRÁFICOS DE BARRAS ----
for (var in variaveis_para_analise) {
  cat("\n\n#############################\n")
  cat("Analisando:", var, "\n")
  cat("#############################\n")
  
  # Modelo Linear (ANOVA de dois fatores)
  # Nota: usamos 'Parcela' como no primeiro script. Se sua coluna se chamar 'Manejo', ajuste aqui.
  formula_modelo <- as.formula(paste(var, "~ Parcela * Cultura"))
  modelo <- lm(formula_modelo, data = dados_final)
  print(Anova(modelo, type = "II"))
  
  # Médias marginais e teste post-hoc de Tukey para a interação
  emm <- emmeans(modelo, ~ Parcela * Cultura)
  cld_res <- multcomp::cld(emm, Letters = letters, adjust = "tukey", rev = TRUE) # 'a' = maior média
  
  resultados_estatisticos[[var]] <- list(modelo = modelo, anova = Anova(modelo, type="II"), emm = emm, cld = cld_res)
  
  # Tabela organizada para o gráfico
  tab <- as.data.frame(cld_res) %>%
    dplyr::rename(
      Media = emmean,
      Erro = SE,
      Grupo = .group
    ) %>%
    mutate(
      Variavel = var,
      Grupo = trimws(Grupo) # Remove espaços em branco das letras
    )
  tabelas_para_graficos[[var]] <- tab
  
  # Geração do Gráfico de Barras
  if (nrow(tab) > 0) {
    g <- ggplot(tab, aes(x = Parcela, y = Media, fill = Cultura, pattern = Cultura)) +
      geom_bar_pattern(
        stat = "identity", position = position_dodge(width = 0.9), colour = "black",
        pattern_fill = "gray20", pattern_density = 0.1, pattern_spacing = 0.02,
        pattern_key_scale_factor = 0.6
      ) +
      geom_errorbar(
        aes(ymin = Media - Erro, ymax = Media + Erro), width = 0.25,
        position = position_dodge(width = 0.9), linewidth = 0.5
      ) +
      geom_text(
        aes(y = Media + Erro, label = Grupo), position = position_dodge(width = 0.9),
        vjust = -0.5, size = 4, color = "black"
      ) +
      labs(
        title = paste("Médias ajustadas para", var),
        y = var,
        x = "Sistema de Cultivo"
      ) +
      scale_fill_manual(values = cores_pastel) +
      scale_pattern_manual(values = padroes_hachura) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
    
    print(g)
    ggsave(paste0("Grafico_Barras_", var, ".png"), plot = g, width = 8, height = 6, dpi = 300)
  }
}


################################################################################
# PARTE 3: ANÁLISES ADICIONAIS E GRÁFICOS ESPECÍFICOS DO IPACSA
################################################################################

## 9) GRÁFICO BOXPLOT DETALHADO PARA O IPACSA ----
cat("\n--- Gerando Boxplot detalhado para o IPACSA ---\n")
dados_final <- dados_final %>%
  mutate(Parcela_Cultura = interaction(Parcela, Cultura, sep = "_", drop = TRUE))

p_boxplot <- ggplot(dados_final, aes(x = Parcela_Cultura, y = IPACSA, fill = Cultura, pattern = Cultura)) +
  geom_boxplot_pattern(
    outlier.shape = NA, na.rm = TRUE, colour = "black",
    pattern_fill = "gray20", pattern_density = 0.1, pattern_spacing = 0.02
  ) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, na.rm = TRUE) +
  labs(
    title = "Distribuição do IPACSA por Sistema e Cultura",
    x = "Sistema de Cultivo_Cultura",
    y = "Índice IPACSA"
  ) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_fill_manual(values = cores_pastel) +
  scale_pattern_manual(values = padroes_hachura) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
print(p_boxplot)
ggsave("IPACSA_boxplot_detalhado.png", p_boxplot, width = 10, height = 7, dpi = 300)


## 10) TABELAS E HEATMAP DE CONSISTÊNCIA E COBERTURA (IPACSA) ----
cat("\n--- Gerando Tabelas e Heatmap de Consistência e Cobertura ---\n")

# Tabela 1: Valores médios
tabela1 <- dados_final %>%
  group_by(Parcela, Cultura) %>%
  summarise(media_IPACSA = mean(IPACSA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Cultura, values_from = media_IPACSA)

# Tabela 2: Consistência e Cobertura
dados_tabela2 <- dados_final %>% mutate(Alta_Sustentabilidade = ifelse(IPACSA >= 6.7, 1, 0))
tabela2_temp <- dados_tabela2 %>%
  group_by(Parcela, Cultura) %>%
  summarise(n = n(), n_alta = sum(Alta_Sustentabilidade, na.rm = TRUE), consistencia = n_alta / n, .groups = "drop")
total_alta <- sum(tabela2_temp$n_alta)
tabela2 <- tabela2_temp %>% mutate(cobertura_bruta = ifelse(total_alta > 0, n_alta / total_alta, 0))

# Heatmap a partir da Tabela 2
tabela_heatmap_long <- tabela2 %>%
  select(Parcela, Cultura, Consistência = consistencia, `Cobertura Bruta` = cobertura_bruta) %>%
  unite("Configuracao", Parcela, Cultura, sep = "–") %>%
  pivot_longer(cols = -Configuracao, names_to = "Indicador", values_to = "Valor")

p_heatmap <- ggplot(tabela_heatmap_long, aes(x = Indicador, y = Configuracao, fill = Valor)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Valor)), size = 3.5) +
  scale_fill_gradient(low = "lightyellow", high = "darkgreen", name = "Intensidade") +
  labs(
    title = "Heatmap de Consistência e Cobertura (IPACSA)",
    x = "Indicadores", y = "Configuração (Sistema–Cultura)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_heatmap)
ggsave("IPACSA_heatmap_consistencia.png", p_heatmap, width = 8, height = 7, dpi = 300)


################################################################################
# PARTE 4: EXPORTAÇÃO FINAL DOS RESULTADOS
################################################################################

## 11) EXPORTAÇÃO DE TODOS OS ARQUIVOS ----
cat("\n--- EXPORTANDO TODOS OS RESULTADOS ---\n")

# Exporta a tabela com todos os resultados do loop para Excel
tabela_final_loop <- bind_rows(tabelas_para_graficos)
write.xlsx(tabela_final_loop, "Resultados_Analise_Estatistica.xlsx", row.names = FALSE)

# Exporta as tabelas de resumo do IPACSA para CSV
write_csv(tabela1, "Tabela_Medias_IPACSA.csv")
write_csv(tabela2, "Tabela_Consistencia_Cobertura_IPACSA.csv")

# Exporta a base de dados final com todos os índices calculados
write_csv(dados_final, "Resultados_Completos_com_Indices.csv")

cat("\n\nProcesso finalizado com sucesso!\n")
cat("Arquivos gerados:\n")
for(var in variaveis_para_analise) { cat(paste0(" - Grafico_Barras_", var, ".png\n")) }
cat(" - IPACSA_boxplot_detalhado.png\n")
cat(" - IPACSA_heatmap_consistencia.png\n")
cat(" - Resultados_Analise_Estatistica.xlsx\n")
cat(" - Tabela_Medias_IPACSA.csv\n")
cat(" - Tabela_Consistencia_Cobertura_IPACSA.csv\n")
cat(" - Resultados_Completos_com_Indices.csv\n")