################################################################################

# SCRIPT - IPACSA COMPLETO

# 1. Cálculo do IPACSA (Fuzzy)

# 2. Geração de Gráficos de Resultados (COM ESTILO PASTEL E HACHURAS)

# 3. Geração de Tabelas de Análise

# 4. Geração de Heatmap de Consistência e Cobertura

#

# Requisitos: FuzzyR, readxl, dplyr, ggplot2, ggpattern, readr, Rmisc, tidyr, forcats

################################################################################



## 0) PACOTES ----

# Lista de pacotes necessários

# Lista de pacotes necessários para a análise

req_pkgs <- c("readxl", "dplyr", "ggplot2", "car", "emmeans", 

              "multcomp", "multcompView", "openxlsx", "ggpattern", "RColorBrewer", "FuzzyR")



# Bloco de código para verificar, instalar (se necessário) e carregar os pacotes

if (length(ins <- setdiff(req_pkgs, rownames(installed.packages())))) install.packages(ins, dependencies = TRUE)

invisible(lapply(req_pkgs, library, character.only = TRUE))





## 1) LIMPEZA DO AMBIENTE ----

rm(list = ls())

graphics.off()





## 2) IMPORTAÇÃO DOS DADOS ----

# Carrega os dados da planilha Excel

dados <- readxl::read_xlsx("banco_dados.xlsx", sheet = "dados_010")

cat("Colunas disponíveis no banco de dados:\n")

print(names(dados))



# Verifica se todas as colunas necessárias estão presentes

colunas_necessarias <- c(

  "DMG","DMP","RMP","Densidade","Estoque de C","Na","ICV(%)",

  "Altura de Plantas","Diâmetro espiga","Comprimento espiga",

  "Número de plantas_ha","N total de espigas_ha","N de espigas comerciais_ha",

  "Peso de espigas comerciais_ha","Produtividade","Cultura","Parcela"

)

faltando <- setdiff(colunas_necessarias, names(dados))

if(length(faltando)) {

  stop("Erro: Faltam colunas essenciais no banco de dados: ", paste(faltando, collapse=", "))

}

# Remove linhas com valores ausentes nas colunas necessárias

dados <- dados[complete.cases(dados[, colunas_necessarias]), ]





## 3) DEFINIÇÃO DA DIREÇÃO DAS VARIÁVEIS E CHECAGENS ----

# 1 = quanto maior, melhor; -1 = quanto menor, melhor

direcao <- c(

  DMG=1, DMP=1, RMP=-1, Densidade=-1, `Estoque de C`=1, Na=-1, `ICV(%)`=1,

  `Altura de Plantas`=1, `Diâmetro espiga`=1, `Comprimento espiga`=1,

  `Número de plantas_ha`=1, `N total de espigas_ha`=1, `N de espigas comerciais_ha`=1,

  `Peso de espigas comerciais_ha`=1, Produtividade=1

)

if(any(dados$`ICV(%)` > 100, na.rm=TRUE)) warning("Atenção: Existem valores de ICV(%) > 100. Verifique se a escala está correta (0–100).")

if(any(dados$Densidade < 0.9 | dados$Densidade > 1.8, na.rm=TRUE)) message("Info: A variável 'Densidade' possui valores fora do intervalo típico (0.9–1.8 g/cm³).")





## 4) NORMALIZAÇÃO ROBUSTA (ESCALA 0–10) ----

# Função para normalizar usando os percentis 5 e 95 como limites

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



# Aplica a normalização nas colunas definidas no vetor 'direcao'

dados_norm <- dados %>%

  dplyr::mutate(

    dplyr::across(

      names(direcao),

      ~ normalizar_robusto(.x, maior_melhor = direcao[cur_column()] == 1),

      .names = "{.col}_norm"

    )

  )





## 5) FUNÇÃO PARA CRIAR FUNÇÕES DE PERTINÊNCIA TRIANGULARES ----

add_tri_from_quartis <- function(fis, var_index, x, nomes = c("baixa","media","alta")) {

  x <- x[is.finite(x)]

  qs <- stats::quantile(x, c(.25,.5,.75), na.rm = TRUE, type = 8)

  # Ajuste: Estender os limites para garantir cobertura em 0 e 10 e evitar NAs
  L <- -2; U <- 12

  fis <- addmf(fis, "input", var_index, nomes[1], "trimf", c(L, qs[1], qs[2]))

  fis <- addmf(fis, "input", var_index, nomes[2], "trimf", c(qs[1], qs[2], qs[3]))

  fis <- addmf(fis, "input", var_index, nomes[3], "trimf", c(qs[2], qs[3], U))

  fis

}





## 6) CRIAÇÃO DOS SISTEMAS DE INFERÊNCIA FUZZY (FIS) - SUBÍNDICES ----

# Subíndice COS (Conservação do Solo)

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

  

  regras_orig <- rbind(

    c(2,2,1,1,1,3,3, 3, 1,1),

    c(2,2,2,2,2,2,2, 2, 1,1),

    c(1,1,3,3,3,1,1, 1, 1,1)

  )
  
  # Regras de Preenchimento (Main Effects) para evitar NAs
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



# Subíndice ARQ (Arquitetura de Plantas)

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

  

  regras_orig <- rbind(

    c(3,3,3,3,3,3, 3, 1,1),

    c(2,2,2,2,2,2, 2, 1,1),

    c(1,1,1,1,1,1, 1, 1,1)

  )
  
  # Regras de Preenchimento
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



# Subíndice STAND (Estande de Plantas)

criar_fis_STAND <- function(dn) {

  fis <- newfis("STAND", defuzzMethod = "centroid")

  in_var <- "Número de plantas_ha_norm"

  fis <- addvar(fis, "input", in_var, c(0,10))

  fis <- add_tri_from_quartis(fis, 1, dn[[in_var]])

  fis <- addvar(fis, "output", "STAND", c(0,10))

  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))

  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))

  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))

  

  regras <- rbind(

    c(3, 3, 1,1),

    c(2, 2, 1,1),

    c(1, 1, 1,1)

  )

  addrule(fis, regras)

}





## 7) CRIAÇÃO DO FIS FINAL (IPACSA) ----

criar_fis_FINAL <- function() {

  fis <- newfis("IPACSA_final", defuzzMethod = "centroid")

  in_vars <- c("COS","ARQ","STAND","Produtividade_norm")

  for (i in seq_along(in_vars)) {

    fis <- addvar(fis, "input", in_vars[i], c(0,10))

    fis <- addmf(fis, "input", i, "baixa", "trimf", c(0,2.5,5))

    fis <- addmf(fis, "input", i, "media", "trimf", c(2.5,5,7.5))

    fis <- addmf(fis, "input", i, "alta",  "trimf", c(5,7.5,10))

  }

  fis <- addvar(fis, "output", "IPACSA", c(0,10))

  fis <- addmf(fis, "output", 1, "baixa", "trimf", c(0,2.5,5))

  fis <- addmf(fis, "output", 1, "media", "trimf", c(2.5,5,7.5))

  fis <- addmf(fis, "output", 1, "alta",  "trimf", c(5,7.5,10))

  

  regras_orig <- rbind(

    c(2,3,2,3, 3, 1.2, 1),

    c(2,2,2,2, 2, 1.0, 1),

    c(1,1,1,1, 1, 0.8, 1),

    c(3,2,2,2, 3, 1.1, 1),

    c(2,3,2,3, 3, 1.1, 1),

    c(2,2,3,2, 3, 1.1, 1),

    c(2,3,2,2, 3, 1.15,1)

  )
  
  # Regras de Preenchimento
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





## 8) PIPELINE DE CÁLCULO (Subíndices + IPACSA) ----

fis_COS    <- criar_fis_COS(dados_norm)

fis_ARQ    <- criar_fis_ARQ(dados_norm)

fis_STAND  <- criar_fis_STAND(dados_norm)

fis_FINAL  <- criar_fis_FINAL()



calc_subindices <- function(df) {

  Xcos <- as.matrix(df[, c("DMG_norm","DMP_norm","RMP_norm","Densidade_norm","Na_norm","ICV(%)_norm","Estoque de C_norm")])

  Xarq <- as.matrix(df[, c("Altura de Plantas_norm","Diâmetro espiga_norm","Comprimento espiga_norm",

                           "N total de espigas_ha_norm","N de espigas comerciais_ha_norm","Peso de espigas comerciais_ha_norm")])

  Xstd <- as.matrix(df[, "Número de plantas_ha_norm", drop=FALSE])

  

  df$COS   <- apply(Xcos, 1, function(l) evalfis(matrix(l,1), fis_COS))

  df$ARQ   <- apply(Xarq, 1, function(l) evalfis(matrix(l,1), fis_ARQ))

  df$STAND <- apply(Xstd, 1, function(l) evalfis(matrix(l,1), fis_STAND))

  df

}

dados_norm <- calc_subindices(dados_norm)



Xfinal <- as.matrix(dados_norm[, c("COS","ARQ","STAND","Produtividade_norm")])

dados_norm$IPACSA <- apply(Xfinal, 1, function(l) evalfis(matrix(l,1), fis_FINAL))





## 9) ORGANIZAÇÃO FINAL DOS DADOS PARA GRÁFICOS E TABELAS ----

ordem_parcela <- c("CT","MT","NT")



dados_final <- dados_norm %>%

  dplyr::mutate(

    Parcela = factor(trimws(Parcela), levels = ordem_parcela),

    Cultura = as.factor(trimws(Cultura)),

    Parcela_Cultura = interaction(Parcela, Cultura, sep = "_", drop = TRUE),

    Parcela_Cultura = forcats::fct_inorder(Parcela_Cultura)

  ) %>%

  dplyr::filter(is.finite(IPACSA))



cat("\n--- RESUMO ESTATÍSTICO DO IPACSA ---\n")

print(summary(dados_final$IPACSA))





## 10) GERAÇÃO DE GRÁFICOS ----



# <--- INÍCIO DO BLOCO DE ESTILO --->

# --- Definições de Estilo Consistentes para os Gráficos ---



# Contar o número de culturas para tornar o script robusto

n_culturas <- length(unique(dados_final$Cultura))

culturas_unicas <- unique(dados_final$Cultura)



# 1. Paleta de cores em tons pastel

if (n_culturas < 3) {

  cores_base <- c("#FBB4AE", "#B3CDE3")

  cores_pastel <- cores_base[1:n_culturas]

} else {

  cores_pastel <- RColorBrewer::brewer.pal(n = n_culturas, name = "Pastel1")

}

names(cores_pastel) <- culturas_unicas



# 2. Padrões de hachura (um para cada cultura)

lista_de_padroes <- c("stripe", "crosshatch", "circle", "wave", "grid", "hexagon", "right45", "left45")

if (n_culturas > length(lista_de_padroes)) {

  warning(paste("Aviso: O número de culturas (", n_culturas, ") excede o número de padrões de hachura únicos. Os padrões serão repetidos.", sep=""))

  padroes_hachura <- rep(lista_de_padroes, length.out = n_culturas)

} else {

  padroes_hachura <- lista_de_padroes[1:n_culturas]

}

names(padroes_hachura) <- culturas_unicas

# <--- FIM DO BLOCO DE ESTILO --->





# Gráfico 1: Boxplot do IPACSA (com novo estilo)

p1 <- ggplot2::ggplot(dados_final, ggplot2::aes(x = Parcela_Cultura, y = IPACSA, fill = Cultura, pattern = Cultura)) +

  ggpattern::geom_boxplot_pattern(

    outlier.shape = NA, 

    na.rm = TRUE,

    colour = "black",

    pattern_fill = "gray20",

    pattern_density = 0.1,

    pattern_spacing = 0.02

  ) +

  ggplot2::geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, na.rm = TRUE) +

  ggplot2::labs(

    title = "IPACSA por Sistema de Cultivo e Cultura (0.0 - 0.10 m)", 

    x = "Sistema de Cultivo_Cultura", 

    y = "Índice IPACSA"

  ) +

  ggplot2::scale_y_continuous(limits = c(1, 10), breaks = 1:10) +

  # Aplica as cores e padrões definidos

  ggplot2::scale_fill_manual(values = cores_pastel) +

  scale_pattern_manual(values = padroes_hachura) + # <-- CORREÇÃO APLICADA AQUI

  ggplot2::theme_classic(base_size = 14) +

  ggplot2::theme(

    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),

    axis.text.y = element_text(color = "black"),

    plot.title = element_text(hjust = 0.5, face = "bold"),

    legend.position = "bottom",

    legend.title = element_text(face="bold")

  )

print(p1)

ggplot2::ggsave("IPACSA_boxplot.png", p1, width = 10, height = 7, dpi = 300)



# Gráfico 2: Gráfico de colunas com Média ± Erro Padrão (com novo estilo)

resumo_IPACSA <- dados_final %>%

  dplyr::group_by(Parcela, Cultura) %>%

  dplyr::summarise(

    media = mean(IPACSA, na.rm = TRUE),

    se = stats::sd(IPACSA, na.rm = TRUE)/sqrt(dplyr::n()),

    .groups = 'drop'

  ) %>%

  dplyr::mutate(Parcela = factor(Parcela, levels = ordem_parcela)) %>%

  dplyr::arrange(Parcela)



p2 <- ggplot2::ggplot(resumo_IPACSA, ggplot2::aes(x = Parcela, y = media, fill = Cultura, pattern = Cultura)) +

  ggpattern::geom_bar_pattern(

    stat = "identity",

    position = position_dodge(width = 0.9),

    colour = "black",

    pattern_fill = "gray20",

    pattern_density = 0.1,

    pattern_spacing = 0.02,

    pattern_key_scale_factor = 0.6

  ) +

  ggplot2::geom_errorbar(

    ggplot2::aes(ymin = media - se, ymax = media + se),

    width = 0.25,

    position = position_dodge(width = 0.9),

    linewidth = 0.5

  ) +

  ggplot2::labs(

    x = "Sistema de Cultivo", 

    y = "IPACSA (média ± EP)",

    title = "Média do IPACSA por Sistema de Cultivo e Cultura"

  ) +

  # Aplica as cores e padrões definidos

  ggplot2::scale_fill_manual(values = cores_pastel) +

  scale_pattern_manual(values = padroes_hachura) + # <-- CORREÇÃO APLICADA AQUI

  ggplot2::theme_classic(base_size = 14) +

  ggplot2::theme(

    plot.title = element_text(face = "bold", hjust = 0.5),

    axis.text = element_text(color = "black"),

    legend.position = "bottom",

    legend.title = element_text(face="bold")

  )

print(p2)

ggplot2::ggsave("IPACSA_colunas.png", p2, width = 8, height = 6, dpi = 300)



################################################################################

# INÍCIO DA SEÇÃO 2: GERAÇÃO DAS TABELAS PRINCIPAIS

################################################################################



## 11) ---- TABELA 1: Valores médios do IPACSA por Sistema e Cultura ----

tabela1 <- dados_final %>%

  dplyr::group_by(Parcela, Cultura) %>%

  dplyr::summarise(

    media_IPACSA = mean(IPACSA, na.rm = TRUE),

    .groups = "drop"

  ) %>%

  tidyr::pivot_wider(names_from = Cultura, values_from = media_IPACSA)



# Exportar e mostrar Tabela 1

readr::write_csv(tabela1, "Tabela1_IPACSA_Sistema_Cultura.csv")

cat("\n--- TABELA 1: Média do IPACSA por Sistema e Cultura ---\n")

print(tabela1)





## 12) ---- TABELA 2: Análise de Consistência e Cobertura ----

# Definir o desfecho: alta sustentabilidade (IPACSA >= 6.7)

dados_tabela2 <- dados_final %>%

  dplyr::mutate(Alta_Sustentabilidade = ifelse(IPACSA >= 6.7, 1, 0))



# Agregar por sistema–cultura para calcular consistência

tabela2_temp <- dados_tabela2 %>%

  dplyr::group_by(Parcela, Cultura) %>%

  dplyr::summarise(

    n = n(),

    n_alta = sum(Alta_Sustentabilidade, na.rm = TRUE),

    consistencia = n_alta / n,

    .groups = "drop"

  )



# Calcular coberturas

total_alta <- sum(tabela2_temp$n_alta)



tabela2 <- tabela2_temp %>%

  dplyr::mutate(

    cobertura_bruta = ifelse(total_alta > 0, n_alta / total_alta, 0),

    cobertura_unica = cobertura_bruta

  )



# Exportar e mostrar Tabela 2

readr::write_csv(tabela2, "Tabela2_IPACSA_Consistencia_Cobertura.csv")

cat("\n--- TABELA 2: Análise de Consistência e Cobertura (IPACSA >= 6.7) ---\n")

print(tabela2)





# <--- NOVO BLOCO: GERAÇÃO DO HEATMAP A PARTIR DA TABELA 2 --->

## 13) ---- HEATMAP DE CONSISTÊNCIA E COBERTURA ----

cat("\n--- Gerando Heatmap de Consistência e Cobertura ---\n")



# Preparar dados para o heatmap a partir da 'tabela2' já calculada

tabela_heatmap <- tabela2 %>%

  # Selecionar apenas as colunas de interesse

  dplyr::select(Parcela, Cultura, consistencia, cobertura_bruta, cobertura_unica) %>%

  # Criar uma única coluna para a configuração (ex: "CC-Caupi")

  tidyr::unite("Configuracao", Parcela, Cultura, sep = "–") %>%

  # Renomear colunas para ficarem mais bonitas no gráfico

  dplyr::rename(

    Consistência = consistencia,

    `Cobertura Bruta` = cobertura_bruta,

    `Cobertura Única` = cobertura_unica

  )



# Transformar os dados para o formato "longo", ideal para o ggplot

tabela_heatmap_long <- tabela_heatmap %>%

  tidyr::pivot_longer(

    cols = -Configuracao,

    names_to = "Indicador",

    values_to = "Valor"

  )



# Plotar o heatmap

p_heatmap <- ggplot(tabela_heatmap_long, aes(x = Indicador, y = Configuracao, fill = Valor)) +

  geom_tile(color = "white", linewidth = 0.5) +

  geom_text(aes(label = sprintf("%.2f", Valor)), size = 3.5) +

  scale_fill_gradient(low = "lightyellow", high = "darkgreen", name = "Intensidade") +

  labs(

    title = "Heatmap dos Indicadores de Consistência e Cobertura (IPACSA)",

    x = "Indicadores",

    y = "Configuração (Sistema–Cultura)"

  ) +

  theme_minimal(base_size = 12) +

  theme(

    axis.text.x = element_text(angle = 30, hjust = 1),

    plot.title = element_text(hjust = 0.5, face = "bold")

  )



# Exibir e salvar o heatmap

print(p_heatmap)

ggplot2::ggsave("IPACSA_heatmap_consistencia.png", p_heatmap, width = 8, height = 7, dpi = 300)





## 14) ---- EXPORTAÇÃO FINAL DOS DADOS ----

# Exporta a tabela completa com todos os resultados para possível reanálise

readr::write_csv(dados_final, "IPACSA_resultados_completos.csv")



cat("\n\nProcesso finalizado com sucesso!\n")

cat("Arquivos gerados:\n")

cat(" - IPACSA_boxplot.png\n")

cat(" - IPACSA_colunas.png\n")

cat(" - Tabela1_IPACSA_Sistema_Cultura.csv\n")

cat(" - Tabela2_IPACSA_Consistencia_Cobertura.csv\n")

cat(" - IPACSA_heatmap_consistencia.png\n") # <--- ARQUIVO NOVO

cat(" - IPACSA_resultados_completos.csv (dados brutos e calculados)\n")











################################################################################

# NOVO BLOCO – ANÁLISE ESTATÍSTICA POR GLM (IPACSA ~ Sistema * Cultura)

################################################################################



# --- CARREGAR PACOTES ---

# Instale os pacotes se ainda não tiver:

 install.packages("car")

 install.packages("emmeans")

 install.packages("multcomp")

 install.packages("multcompView") # Pacote que estava faltando

 install.packages("ggplot2")



library(car)         # ANOVA tipo III (Wald)

library(emmeans)     # Pós-teste múltiplo e letras compactas

library(multcomp)    # cld()

library(multcompView)# Dependência para a função cld() funcionar

library(ggplot2)     # Para construção de gráficos (labs, theme, ggsave)





# --- CÓDIGO DA ANÁLISE (sem alterações) ---



# Ajustar modelo GLM (usando distribuição Gamma para acomodar IPACSA > 0)

modelo_glm <- glm(

  IPACSA ~ Parcela * Cultura,      # Efeito principal e interação

  data    = dados_final,

  family  = Gamma(link = "identity")

)



# ANOVA tipo III (teste de Wald)

anova_glm <- car::Anova(modelo_glm, type = 3, test = "Wald")

print(anova_glm)



# Estatísticas de ajuste

cat("\n--- QUALIDADE DO AJUSTE ---\n")

cat("AIC:", AIC(modelo_glm), "\n")

cat("BIC:", BIC(modelo_glm), "\n")

cat("Deviance/df:", deviance(modelo_glm)/df.residual(modelo_glm), "\n")



# ───────────────────────────────────────────────────────────────

# 1. Estimativas marginais por sistema

# ───────────────────────────────────────────────────────────────

emm_sistema <- emmeans(modelo_glm, ~ Parcela)

cat("\n--- ESTIMATIVAS MARGINAIS (médias ajustadas por Sistema) ---\n")

print(summary(emm_sistema))



# Comparações entre sistemas

comparacoes_sistema <- pairs(emm_sistema, adjust = "bonferroni")

cat("\n--- PÓS-TESTE Sistemas (Bonferroni) ---\n")

print(comparacoes_sistema)



# Letras compactas para sistemas

letras_sistema <- multcomp::cld(emm_sistema, Letters = letters, adjust = "bonferroni")

print(letras_sistema)



# ───────────────────────────────────────────────────────────────

# 2. Estimativas marginais por cultura

# ───────────────────────────────────────────────────────────────

emm_cultura <- emmeans(modelo_glm, ~ Cultura)

cat("\n--- ESTIMATIVAS MARGINAIS (médias ajustadas por Cultura) ---\n")

print(summary(emm_cultura))



# Comparações entre culturas

comparacoes_cultura <- pairs(emm_cultura, adjust = "bonferroni")

cat("\n--- PÓS-TESTE Culturas (Bonferroni) ---\n")

print(comparacoes_cultura)



# Letras compactas para culturas

letras_cultura <- multcomp::cld(emm_cultura, Letters = letters, adjust = "bonferroni")

print(letras_cultura)





