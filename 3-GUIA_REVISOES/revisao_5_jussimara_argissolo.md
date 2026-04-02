# Revisões Jussimara — Mesmo experimento 22 anos (Argissolo, Tabuleiros Costeiros)

**Fonte:** Artigos 02 e 03 da Jussimara sobre qualidade estrutural do Argissolo  
**Relevância:** Mesmo experimento de longa duração usado no benchmarking (22 anos, 3 sistemas de preparo × 4 coberturas)

---

## PADRÕES DE ERRO RECORRENTES EM REVISÕES Q1

### 1. MECANISMOS INSUFICIENTES (Crítica #1 mais comum)

**Erro típico:** Descrever o QUE aconteceu sem explicar POR QUÊ.

**Exemplo ruim:**
> "A leguminosa promoveu maior estabilidade de agregados que a gramínea."

**Correção exigida:**
> "A diferenciação entre coberturas reflete distintos mecanismos de agregação: resíduos de gramíneas (C/N 40–80:1) estimulam hifas fúngicas que promovem macroagregados (>250 μm) por emaranhamento físico (Six et al. 2004), enquanto leguminosas (C/N 10–25:1) favorecem decomposição bacteriana produtora de polissacarídeos que cimentam microagregados (<250 μm) em hierarquia ascendente (Bronick and Lal 2005)."

**Checklist mecanístico:**
- [ ] C/N ratio quantificado para cada cobertura
- [ ] Via microbiológica especificada (fungos vs. bactérias)
- [ ] Faixa de tamanho de agregados indicada
- [ ] Citação de mecanismo fundamental (Six, Tisdall & Oades)

---

### 2. FIGURAS SEM INFORMAÇÃO QUANTITATIVA

**Erro típico:** Legenda genérica tipo "Blocos de solo sob diferentes tratamentos."

**Correção exigida:**
- Dimensões físicas (ex.: "blocos 0,30 × 0,40 × 0,25 m")
- Valores quantitativos por tratamento (ex.: "CT — clods >10 cm, SQ 2,5–2,7; NT — agregados <3 cm, SQ 1,0–1,9")
- Escala visual indicada na legenda

**Para PCA/Biplot:**
> "PCA biplot (**PC1 = 42,0%**; **PC2 = 28,7%**; **cumulativo = 70,7%**). KMO = 0,52; Bartlett p < 0,001."

---

### 3. TABELAS DESORGANIZADAS E NOTAÇÃO INCONSISTENTE

**Erros identificados:**
- Dados não delineados por linhas claras
- Letras de significância (Aa/Bb) sem correspondência com p-values
- β coefficients com erros de formatação numérica
- "Sig." repetindo valores errados (ex.: "0.70" em múltiplas células)
- Mistura de convenções (algumas tabelas com letras, outras sem)

**Padrão exigido:**
- Toda tabela com mesmo sistema de letras
- p-values explícitos ou em nota de rodapé
- "*p < 0,05; **p < 0,01; ***p < 0,001" padronizado
- VIF, R², SE em colunas próprias

---

### 4. HIERARQUIA LÓGICA DOS RESULTADOS

**Erro típico:** Apresentar NT, MT, CT simultaneamente sem estrutura.

**Estrutura exigida para cada seção de resultados:**
1. Efeito principal do preparo (tillage)
2. Efeito principal da cobertura
3. Interação preparo × cobertura
4. Comparação por profundidade (0–10 cm vs. 10–20 cm)
5. Mecanismo explicativo (1–2 frases)

---

### 5. COMPARAÇÕES REGIONAIS AUSENTES

**Revisores exigem:**
- Comparação com Cerrado brasileiro (Oxisols gibbsíticos)
- Comparação com solos tropicais do Sudeste Asiático
- Discussão de universalidade vs. particularidade regional
- Papel das mudanças climáticas (sequestro de C, resistência à seca)

**Citações úteis:**
- Schiebelbein et al. 2025 (Cerrado Oxisols)
- Pheap et al. 2025; Briliawan et al. 2022 (Southeast Asia)
- Marengo et al. 2020 (climate projections NE Brazil)

---

### 6. RECOMENDAÇÕES PRÁTICAS VAGAS

**Erro típico:** "O plantio direto é recomendado para agricultores familiares."

**Correção exigida — valores específicos:**
> "Recomendações para agricultores familiares:
> - NT ou MT como preparo primário
> - Consórcio crotalária 10,5 kg ha⁻¹ + milheto 6,0 kg ha⁻¹ (70:30) sob NT
> - Ou guandu 17,5 kg ha⁻¹ + milheto 6,0 kg ha⁻¹ (75:25) sob MT
> - Semeadura 60 dias antes do milho
> - Inoculantes: *Bradyrhizobium* + *Azospirillum brasilense*"

---

### 7. PARADOXO NT + MILHETO NÃO EXPLICADO

**Achado:** Melhor SQ (1,00) e maior ICV (87%), mas produtividade inferior.

**Mecanismo exigido:**
> "Imobilização transiente de N: resíduos de milheto (C/N 40–80:1) decompõem lentamente; assimilação microbiana de N mineral coincide com demanda crítica do milho (V4–V8), restringindo crescimento vegetativo inicial (Aita & Giacomini 2003). Leguminosas (C/N 10–25:1) liberam N rapidamente, fornecendo pulso de fertilização biológica."

**Quantificação:** "−5 a −15 kg N ha⁻¹ nos primeiros 30 dias" (Ambrosano et al. 2009)

---

### 8. CONCLUSÃO VERBOSA E REPETITIVA

**Erro típico:** 6 parágrafos repetindo introdução e resultados.

**Estrutura exigida (4 parágrafos):**
1. Achado central sobre preparo (NT/MT vs. CT)
2. Trade-off entre coberturas (gramínea vs. leguminosa)
3. Recomendações específicas com taxas de semeadura
4. Contribuição metodológica (framework replicável)

---

## CHECKLIST PRÉ-SUBMISSÃO (COMPILADO DAS REVISÕES)

### Metodologia
- [ ] Profundidade de amostragem justificada (por que 0–20 cm e não 0–40 cm?)
- [ ] Protocolo de peneiramento úmido detalhado (pré-tratamento, tempo de oscilação)
- [ ] Fórmulas de DMG/DMP com fonte
- [ ] Critérios de seleção de variáveis para PCA justificados
- [ ] Threshold de F para regressão stepwise especificado
- [ ] Versões de software com trademark (IBM® SPSS® v.26)

### Figuras
- [ ] Variância explicada em eixos de PCA
- [ ] KMO e Bartlett reportados
- [ ] Dimensões físicas em fotos de campo
- [ ] Escalas de cor com unidades

### Tabelas
- [ ] Sistema de letras consistente em todas as tabelas
- [ ] p-values explícitos ou padronizados em nota
- [ ] Sem erros de digitação em colunas numéricas
- [ ] VIF para multicolinearidade

### Discussão
- [ ] Mecanismo para cada achado principal
- [ ] Comparação regional (Cerrado, SE Asia)
- [ ] Contexto de mudanças climáticas
- [ ] Limitações reconhecidas (amostragem única vs. série temporal)

### Conclusão
- [ ] Máximo 4 parágrafos
- [ ] Valores específicos de manejo
- [ ] Incerteza reportada (±)
- [ ] Sem repetição de estatísticas

---

## ERROS DE FORMATAÇÃO FREQUENTES

| Erro | Correção |
|------|----------|
| "Figure 01" | "Figure 1" |
| "p < 0.000" | "p < 0.001" |
| "ICV" em texto EN | "VCI" |
| "*Sunn hemp juncea*" | "*Crotalaria juncea*" |
| "enviroNECnt" (corrupção) | "environment" |
| "Ambrosano et al., 20009" | "2009" |
| "Mauad et at" | "Mauad et al." |
| "Angers e Caron" | "Angers and Caron" |
| "Northeast" | "northeastern" |
| "No tillage" | "no-tillage" |

---

## EXPRESSÕES A EVITAR (MARCADORES DE IA)

- "It is worth noting that"
- "It is evident, therefore"
- "It is noteworthy that"
- "remarkably"
- "preliminary hypothesis" → "hypothesis"

---

## REFERÊNCIAS-CHAVE PARA MECANISMOS

**Agregação:**
- Six et al. 2004 (fungal enmeshment)
- Tisdall & Oades 1982 (hierarquia de agregados)
- Bronick & Lal 2005 (polysaccharide binding)

**Bioporos:**
- Peth et al. 2008 (root channels)
- Schlüter et al. 2018 (earthworm galleries)
- Pires et al. 2017 (X-ray CT in tropical soils)

**Imobilização N:**
- Aita & Giacomini 2003
- Ambrosano et al. 2009
- Teixeira et al. 2009

**Comparação regional:**
- Schiebelbein et al. 2025 (Cerrado)
- Pheap et al. 2025 (SE Asia)
- Cintra et al. 2004 (camada coesa Tabuleiros)
