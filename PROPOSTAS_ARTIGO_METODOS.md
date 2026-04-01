# Propostas de Artigo Metodológico — Benchmarking Estatístico para Dados Ambientais

**Data:** 01/04/2026  
**Status:** Em desenvolvimento  

---

## PROPOSTA 1 (SELECIONADA) — Benchmarking Multi-Paradigma para Índice de Qualidade do Solo

### Título provisório
> *"Classical, Fuzzy, and Machine Learning Approaches to Integrated Soil Quality Assessment: A Multi-Paradigm Benchmarking on the Same Agronomic Dataset"*

### Premissa
A literatura publica, via de regra, UM método para construir índices de qualidade do solo (SQI). Ninguém compara ANOVA+MDS *vs.* Fuzzy Inference *vs.* SEM-PLS *vs.* ML (Random Forest / MARS) **no mesmo banco de dados real**. Pacci et al. (2026, Int. J. Environ. Sci. Tech.) e Venkateswarlu et al. (2025, Sci. Reports) apontam essa lacuna mas não a preenchem.

### Dados disponíveis
- `6-FUZZI_AGRONOMY/banco_dados.xlsx` — 200 obs., 15+ variáveis solo+planta, 3 manejos × 4 culturas
- `IPACSA_resultados_completos.csv` — índices fuzzy já calculados
- `DADOS_GLM.xlsx` — resultados GLM já processados

### Códigos existentes
| Paradigma | Script existente | Status |
|-----------|-----------------|--------|
| ANOVA + GLM | `GLM.R`, `GLM_GRÁFICOS.R` | Pronto |
| Fuzzy (triangular/gaussiana) | `Código Fuzzy completo.R` | Pronto |
| SEM-PLS (modelo estrutural) | `SEM-PLS.R` | Pronto |
| MARS (ablação de variáveis) | `calc_mars.R` | Pronto |
| Validação rankings | `Validacao_Ablation_Rank.R` | Pronto |

### O que falta codificar
- Random Forest para SQI (adaptar do projeto 7-REVISÃO_ESCOPO_ML)
- Métricas de concordância entre paradigmas (Kendall's W, ICC, Bland-Altman)
- Análise de sensibilidade / simulação Monte Carlo para robustez com n pequeno

### Estrutura do artigo
1. Introdução — lacuna: ausência de benchmarking no mesmo dataset
2. Material: descrição do banco (sem detalhar o experimento, foco nas variáveis)
3. Métodos: 5 paradigmas aplicados ao mesmo dataset
4. Resultados: concordância/discordância entre métodos, rankings, diagnósticos
5. Discussão: recomendações de uso por tipo de dado/objetivo
6. Conclusão: fluxograma decisório para pesquisadores

### Revistas-alvo
- *Ecological Indicators* (IF 7.0)
- *Geoderma* (IF 6.1)
- *Computers and Electronics in Agriculture* (IF 8.3)

---

## PROPOSTA 2 — Framework de Confiabilidade para Degradação de Fibras Naturais

### Título provisório
> *"Integrating Weibull Reliability, Bootstrap Inference, and Meta-Analytic Synthesis for Predicting Natural Fiber Degradation in Soil Bioengineering: A Methodological Framework"*

### Premissa
Toth et al. (2025, Polymer Composites) mostram que Weibull é usado para fibras mas NUNCA integrado com meta-análise de múltiplos estudos. Marasović et al. (2025, Sci. Reports) estudam degradação de mulches mas sem modelagem de confiabilidade formal.

### Dados disponíveis (3 projetos combinados)
- `1-ARTIGO_LC_K/` — degradação Taboa com NaOH (4 tratamentos × 6 timepoints × 3 réplicas, Weibull censurado)
- `4 - DEGRADAÇÃO FORÇADA/` — tração + punção ao longo de 120 dias (12 timepoints × 15 réplicas, Weibull 2P/3P)
- `ARTIGO GEOTEXTIL TABOA/` + `ARTIGO NAOH TABOA/` — dados publicados de degradação

### Códigos existentes
- `ANALISE_WEIBULL.r` — Weibull 2P/3P com Anderson-Darling e K-S
- `modelar_LC_k_VUF.py` — Weibull censurado (lifelines), bootstrap IC
- `efeito_aleatorio_biologicas.R` — meta-análise com imputação múltipla (mice + Rubin's rules)

### O que falta
- Meta-regresar parâmetros Weibull (β, η) agregando os 3+ estudos
- Forest plot de taxas de degradação (k) por tratamento
- Análise de sensibilidade para censura à direita

### Revistas-alvo
- *Geotextiles and Geomembranes* (IF 5.3)
- *Journal of Cleaner Production* (IF 11.1)
- *Composites Part B* (IF 13.1)

---

## PROPOSTA 3 — Review Crítico: Estatística Clássica vs. Computacional para Dados Ambientais com Amostras Pequenas

### Título provisório
> *"When n Is Small and Variables Are Many: A Critical Review of Statistical Paradigms for Environmental Soil Data in Agronomic Experiments"*

### Premissa
Experimentos de campo em solo/bioengenharia tipicamente têm n=3–15 réplicas mas 10–30 variáveis — uma violação estrutural dos pressupostos de métodos clássicos. Nenhuma review sistematiza as alternativas (bootstrap, fuzzy, Bayesiano, ML com regularização) para esse cenário específico.

### Base empírica (dados existentes como cases)
- **n=3:** LC-K (3 espécimes/tratamento)
- **n=15:** Degradação forçada
- **n≈200:** Fuzzy Agronomy (cenário de amostra "grande" para comparação)
- **n=5–50 estudos:** Meta-análise (cenário de agregação)

### Diferencial
Não é só review — inclui simulação Monte Carlo para demonstrar empiricamente os limites de cada abordagem em função de n. Usa os dados reais como semente para simulações.

### Material dos scripts que alimenta a review
- Demonstra o que muda quando aplica ANOVA vs. bootstrap vs. Weibull vs. Fuzzy nos MESMOS dados
- Quantifica inflação de erro tipo I e perda de poder com n pequeno
- Propõe decision tree para escolha de método

### Revistas-alvo
- *Environmental Modelling & Software* (IF 4.9)
- *PeerJ* (IF 2.7, rápida)
- *Methods in Ecology and Evolution* (IF 6.3)

---

## Comparativo

| Critério | Proposta 1 | Proposta 2 | Proposta 3 |
|----------|-----------|-----------|-----------|
| % código pronto | ~80% | ~60% | ~40% |
| Originalidade | Alta | Alta | Muito alta |
| Dados necessários | 1 projeto | 3 projetos | 4+ projetos |
| Tempo de execução | Menor | Médio | Maior |
| Risco de rejeição | Baixo | Médio | Médio |
| Impacto potencial | Alto (IF 6–8) | Alto (IF 5–13) | Muito alto (IF 5–6) |
| Independe do experimento? | Sim | Sim | Sim |
