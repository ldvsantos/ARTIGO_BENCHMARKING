---
title: "Response to Reviewers"
subtitle: "Manuscript: Weibull-reliability framework linking chemical architecture to Functional Service Life in natural-fiber geotextiles for tropical soil erosion control"
author: "Luiz Diego Vidal Santos et al."
date: 2026-04-XX
submission-id: "1afb8146-66f6-47bc-bc43-6218c8190fd1"
journal: "Journal of Umm Al-Qura University for Applied Sciences"
---

Dear Dr. Basim Asghar,

We appreciate the editorial handling and the reviewers' detailed feedback, which helped us identify genuine weaknesses in the original submission. Below we reply point by point. All significant changes are marked in blue (additions) and ~~strikethrough~~ (deletions) in the revised manuscript.

---

# Response to Editor

> *Please ensure the results are accurately reported, any overstated conclusions are rewritten and the limitations of the work fully explained.*

**Response:**
We conducted a full numerical audit of all tables, in-text values, and the underlying Python scripts. A systematic bug in the P₁₀ computation was identified and corrected (see R2.2). Conclusions that went beyond the experimental evidence were toned down throughout — in particular, L/C is no longer presented as a universal predictor, but rather as a comparative recalcitrance indicator within the studied species pair. A limitations discussion was added at the end of Section 3.3 and expanded in Section 3.4. The Conclusions now explicitly state that the findings should be regarded as exploratory and that extension to other species, climates, and fabric architectures requires independent calibration and pilot-scale field validation.

---

# Response to Reviewer 1

We are grateful for the encouraging assessment and take the suggestions seriously.

---

**Comment R1.1 — Text excessively long and repetitive, especially in the Discussion.**

> *"the text is excessively long and occasionally repetitive, especially in the discussion section, where literature review and interpretation sometimes overlap."*

**Response:**
The Introduction was cut from ~820 words (15 paragraphs) to ~420 words (6 paragraphs), roughly a 49% reduction. Redundant paragraphs on general soil mechanics and microplastic statistics were removed. In the Discussion, we merged four paragraphs on lignin 3-D organization into one, deleted repeated restatements of L/C and k values across subsections, and shortened Section 3.4 by referencing Table 3 instead of restating all parameter values.

---

**Comment R1.2 — Separate experimental results from literature-based interpretation.**

> *"A clearer separation between experimental results and literature-based interpretation would improve readability."*

**Response:**
We verified that each subsection in Section 3 already opens with the experimental finding — statistical test, effect size, or measured value — before any interpretive paragraph. Where this order was inconsistent, we relocated the data paragraph to the opening position. Hedging markers ("suggests", "is consistent with", "may") were added to distinguish data-supported statements from literature-based inference.

---

**Comment R1.3 — L/C ratio as a predictive parameter should be presented more cautiously.**

> *"the role of the L/C ratio as a predictive parameter should be presented more cautiously. While the correlation with degradation rate is plausible, the current dataset is relatively limited and does not fully demonstrate general applicability across lignocellulosic fibers."*

**Response:**
We agree that two species cannot support a general predictive model. The framing was revised everywhere it appeared:

- Abstract: "constrains biodegradation kinetics" → "appears to modulate biodegradation kinetics, although the two-species dataset limits generalization."
- Hypothesis (end of Introduction): "acts as a predictive variable" → "may act as a comparative recalcitrance indicator."
- Section 3.2: the sentence "the two-species dataset precludes fitting a general functional relationship" was retained and reinforced with "generalization to other lignocellulosic fibers would require multi-species calibration with independent validation."
- Figure 9 caption: an explicit caveat on the need for independent calibration was added.

---

**Comment R1.4 — Weibull reliability analysis: parameter estimation and uncertainty.**

> *"the statistical modelling and Weibull reliability analysis should be described more clearly, including explicit explanation of parameter estimation procedures and uncertainty bounds."*

**Response:**
We added a new subsection to Section 2 titled "Weibull reliability analysis and Functional Service Life" that specifies:

1. The failure criterion: UTS < 50% of the group-specific initial mean ($\sigma_{\text{crit}} = 0.5\,\bar{\sigma}_0$).
2. How failure times are extracted, with right-censoring for specimens that had not yet reached the threshold by their last observation.
3. Maximum Likelihood Estimation (MLE) via `WeibullFitter` from the Python *lifelines* library (v0.27), which handles censored data via partial-likelihood contributions.
4. 95% confidence intervals for β and η obtained from the Fisher information matrix.
5. Propagation of those intervals to P₁₀ through the analytical formula.
6. Goodness-of-fit assessed by the Anderson–Darling statistic adapted for the Weibull family.

Limitations of the two-stage procedure (OLS for tensile trends + MLE for failure times) are now discussed alongside results in Section 3.3.

---

**Comment R1.5 — Simplify figures.**

> *"several figures could be simplified to improve clarity, as some graphical elements contain excessive information."*

**Response:**
Figures 4 and 5 were each split into two panels (a/b), separating MCA and NaOH datasets with fewer overlapping annotations per panel. Figure 8 was reorganized into a 2×4 grid with consistent scale bars and a unified legend for the triple overlay. Full analytical detail is preserved in Supplementary Figures for interested readers.

---

# Response to Reviewer 2

We acknowledge the serious methodological issues raised by Reviewer 2 and address each one below.

---

### Major Concerns

**Comment R2.1 — Eq. (1) is tautological (two-point fit).**

> *"k = 0.032×e⁻²·¹×⁽L/C⁾ is fitted on only two species (two L/C values). A two-parameter model perfectly fits two points by construction—this is tautological, not predictive."*

**Response:**
The reviewer is correct. Eq. (1) was removed from the manuscript. The L/C–k relationship is now reported descriptively as an observed association between two species. The text explicitly states that "the two-species dataset precludes fitting a general functional relationship" (Section 3.2) and that L/C is framed only as a "comparative recalcitrance indicator."

---

**Comment R2.2 — Critical numerical inconsistencies.**

> *"L/C of Syagrus is 0.67 (Table 1) vs. 0.80 (Fig. 9); FSL P₁₀ is 38 d (Table 1) vs. 55 d (Table 3); η is 95 d (Table 3) vs. 85 d (Fig. 9); β is 2.1 (Table 3) vs. 2.8 (Fig. 9)."*

**Response:**
We performed a complete numerical audit across all tables, in-text values, and the analysis script (`modelar_LC_k_VUF.py`). The root cause was a label swap: the *lifelines* library (v0.27) returns `lambda_` for the scale parameter and `rho_` for the shape parameter, but the script had inverted these when computing P₁₀ ($P_{10} = \eta[-\ln(0.90)]^{1/\beta}$). This inflated all tabulated FSL values by 35–69%.

Corrections applied:

- **Table 1 (*Syagrus coronata*):** Cellulose corrected from 48% (back-calculated from L/C = 0.67) to 40% (gravimetric Van Soest measurement). L/C updated accordingly from 0.67 to 0.80 (= 32/40).
- **Table 3 (P₁₀ column):** All five rows recalculated analytically: *Typha* untreated 42 → 26 d; NaOH 6% 95 → 64 d; NaOH 9% 108 → 75 d; *Syagrus* untreated 55 → 33 d; *Syagrus* + MCA mono 92 → 63 d. The β and η parameters, estimated by MLE, remain unchanged.
- **Table 3 (k for *Syagrus* untreated):** Corrected from 0.0095 to 0.0082 day⁻¹ to match Table 1.
- All in-text references to P₁₀, FSL, and L/C were updated.
- Figure 9 was regenerated from the corrected parameter set.

The script bug was fixed in the source code. Weibull shape and scale estimates remain robust because they derive from MLE on the observed failure-time data; only the downstream P₁₀ derivation was affected.

---

**Comment R2.3 — Weibull methodology is missing.**

> *"No failure criterion, estimation method (MLE vs. regression), censoring treatment, confidence intervals, or goodness-of-fit tests are reported."*

**Response:**
All of these elements are now documented in the new Weibull subsection of Section 2. See response to R1.4 for the full list.

---

**Comment R2.4 — Fiber vs. geotextile conflation.**

> *"Tests are on individual fibers/bundles, yet conclusions extrapolate to field-deployed geotextiles. Fabric architecture effects are unaddressed."*

**Response:**
We clarified that mechanical tests were performed on geotextile strip specimens (≈ 200 × 200 mm) cut from field-exposed sections — not on isolated fibers. Section 2.3 now states: "All mechanical results reported in this study refer to the geotextile specimen scale; microstructural characterization (SEM, FTIR, XRD) was conducted on individual fiber bundles extracted from the same sections." We also added an explicit limitation in Section 3.4: "translation from specimen-scale testing to full-scale field deployment requires validation of fabric-architecture effects (weave density, junction integrity, and edge unraveling) that are not captured by the present experimental design." To address this gap constructively, Section 3.4 now proposes a pilot-scale validation protocol: deployment of full rolls (≥ 2 m width) on instrumented slopes with embedded soil-moisture and pore-pressure sensors, periodic in-situ tensile sampling, and concurrent vegetation-cover monitoring over at least two rainy seasons. Section 4 includes: "Full-scale validation is needed to assess how fabric architecture modulates the specimen-level trends reported here."

---

**Comment R2.5 — Typha–Ramie entry in Table 3.**

> *"Never introduced in Methods, no data presented. Must be removed or fully documented."*

**Response:**
Removed. That row corresponded to a parallel study not documented in this manuscript and should not have been included.

---

**Comment R2.6 — Misattributed references.**

> *"Ibáñez & McCarthy-Neumann (2014) and Konvalinková et al. (2015) are forest-ecology papers, not geotextile studies."*

**Response:**
Both citations were removed entirely. They appeared in a passage about geosynthetic recalcitrance and microplastic generation in the Introduction, which was itself condensed during the revision. The claims are now supported by Bai (2022) and other relevant sources.

---

### Minor Concerns

**Comment R2.7 — Manuscript length (~35 pp, reduce 30–40%).**

**Response:**
The Introduction was reduced by ~49% (see R1.1). Discussion repetitions were removed (see R1.1 and R3.7). The revised manuscript body (Abstract through Conclusions, excluding references, tables, and figure captions) totals approximately 7 200 words, down from an estimated 11 500 words in the original submission — a reduction of approximately 37%, within the requested 30–40% range.

---

**Comment R2.8 — Units mix N/mm and N·mm⁻².**

**Response:**
All mechanical results now use N/mm (force per unit width, ISO 10319:2015) consistently. Five occurrences of N·mm⁻² were corrected. Section 2.3 explains the normalization conventions for tensile (ISO 10319:2015) and puncture (ASTM D6241, peak force in N).

---

**Comment R2.9 — MCA identified only by trade name (Hydronorth®).**

**Response:**
The chemical class is now specified in Section 2: "a styrene-acrylic copolymer emulsion used as Morphochemical Consolidation Agent (MCA; commercial name Hydronorth®, solids content ≈ 50%, pH 8.0–9.0)."

---

**Comment R2.10 — Graphical abstract is generic.**

**Response:**
The graphical abstract was redesigned to reflect the actual workflow of the study, from species collection through treatments, field exposure, mechanical and microstructural characterization, to Weibull modeling with FSL output at P₁₀. Representative data from the results are now embedded in the diagram.

---

**Comment R2.11 — SEM image-processing lacks sensitivity analysis.**

**Response:**
We conducted a sensitivity analysis varying the Otsu-based binarization threshold by ±10% (factors 0.63, 0.70, 0.77). Fracture counts changed by ≤17% for the treated conditions (NaOH 6% and 9%); the untreated condition showed higher sensitivity (up to 41%), expected given its more heterogeneous surface. Porosity estimates varied within 2–4 percentage points. The ordinal ranking of treatments remained invariant. These results are reported in Supplementary Figure S2 and referenced in Section 2.5.

---

**Comment R2.12 — Environmental data not correlated with degradation curves.**

**Response:**
A temporal overlay of daily precipitation, cumulative precipitation, and mean UTS per treatment was added as Supplementary Figure S3. The wettest 30-day window (cumulative ≈ 434 mm, ending at day 141) coincides with UTS reductions of 54–72% across treatments. The sampling protocol does not allow causal inference at the event scale, and this limitation is stated in Section 3.1.

---

**Comment R2.13 — *Juncus* sp. in Table 1 not introduced.**

**Response:**
The *Juncus* sp. row was removed from Table 1. The table now contains only the two species investigated in this study (*Typha domingensis* and *Syagrus coronata*).

---

# Response to Reviewer 3

We appreciate the thorough methodological scrutiny, especially regarding the Weibull framework. The concerns were well-founded and led to substantial improvements.

---

**Comment R3.1 — Weibull framework lacks rigor and transparency.**

> *"the Weibull reliability framework [...] is not described with sufficient rigor and transparency."*

**Response:**
This was addressed by adding a complete new subsection to Section 2. Each sub-point is answered below.

---

**Comment R3.1a — How failure is defined.**

> *"the manuscript does not specify in a reproducible way: how failure is defined"*

**Response:**
A specimen is classified as having failed at exposure time *t* when its measured UTS falls below 50% of the group-specific initial mean ($\sigma_{\text{crit}} = 0.5\,\bar{\sigma}_0$). This threshold represents loss of half the original load-bearing capacity, aligned with requirements for temporary erosion-control elements whose support window typically ranges from 90 to 180 days. To assess sensitivity, an exploratory recalculation was performed at alternative thresholds of 40% and 60% of $\bar{\sigma}_0$: the ordinal ranking of treatments and qualitative conclusions remained unchanged in both cases, with FSL varying by approximately ±8–15 days. This analysis is now reported in the Weibull subsection of Section 2.

---

**Comment R3.1b — How failure times are obtained.**

> *"how failure times are obtained from the experimental data"*

**Response:**
For each species-by-treatment combination, the exposure time at which a given specimen first satisfied the failure criterion was retained as the failure time. Specimens that had not reached the threshold by their last observation were treated as right-censored at that time. This procedure is now documented in the new Weibull subsection of Section 2.

---

**Comment R3.1c — Weibull parameter estimation method.**

> *"how the Weibull parameters are estimated"*

**Response:**
Parameters were estimated by MLE using `WeibullFitter` from the Python *lifelines* library (v0.27), which handles right-censored observations via partial-likelihood contributions. 95% confidence intervals for β and η were obtained from the Fisher information matrix inverted at the MLE solution.

---

**Comment R3.1d — Goodness-of-fit and uncertainty analysis.**

> *"whether any goodness-of-fit assessment or uncertainty analysis has been performed"*

**Response:**
Goodness-of-fit is now assessed by the Anderson–Darling statistic adapted for the Weibull family. Confidence intervals for P₁₀ are propagated by evaluating the analytical formula at the parameter-level bounds. Limitations — threshold sensitivity, reduced late-time sample sizes, and the stationarity assumption — are discussed in Section 3.3.

---

**Comment R3.2 — FSL definition not operationally clear.**

> *"the definition of FSL is not operationally clear [...] the critical threshold defining 'loss of function' is not explicitly defined."*

**Response:**
FSL is now defined as P₁₀ = η[−ln(0.90)]^{1/β}, i.e., the exposure time at which 10% cumulative failure probability is reached (equivalently, 90% of deployed elements still satisfy the load-bearing criterion). The failure threshold is 50% of the group-specific initial mean UTS. This definition appears in the new Weibull subsection and is recalled at first use in Section 3.3. The choice of the 50% threshold and the P₁₀ reliability level follows recommendations for temporary erosion-control elements. For critical-risk applications (e.g., highway embankments), the manuscript now notes that a more conservative P₅ (95% survival) may be warranted; within the present dataset, P₅ estimates are approximately 15–25% shorter than P₁₀ but preserve the same treatment hierarchy.

---

**Comment R3.3 — Separate measured vs. calculated vs. literature values.**

> *"it is not always clearly indicated which values are directly measured in this study and which are adopted or inferred."*

**Response:**
Tables 1 and 3 now include superscript annotations: ^m^ for measured in this study and ^c^ for calculated from measured inputs. A footnote explains the notation and data provenance.

---

**Comment R3.4 — OLS on time-unbalanced dataset.**

> *"the manuscript does not sufficiently justify the suitability of this approach for such a dataset, nor does it discuss its limitations."*

**Response:**
A paragraph at the end of the Weibull subsection in Section 2 now acknowledges the time-unbalanced design and discusses its implications. MLE via *lifelines* accommodates right-censored and unequally spaced observations. We note that reduced late-time sample sizes may widen confidence intervals and that the stationarity assumption has not been independently verified. These limitations are framed as acceptable for preliminary design guidance pending future monitoring campaigns.

---

**Comment R3.5 — Definition and units of mechanical variables.**

> *"Tensile strength is reported in units that are not fully explained (e.g., N/mm)"*

**Response:**
All mechanical variables are now reported in units consistent with their respective standards: tensile strength as force per unit width (N/mm, ISO 10319:2015 for wide-width strip testing) and puncture resistance as peak force (N, ASTM D6241 CBR method). These conventions are stated in Section 2.3.

---

**Comment R3.6 — Distinguish results from literature-based interpretations.**

> *"The manuscript would benefit from a clearer distinction between results directly supported by the experimental data and interpretations based on existing literature."*

**Response:**
Each subsection in Section 3 opens with the experimental finding (data, test, effect size), followed by interpretive text that invokes supporting literature. Hedging markers ("may", "suggests", "is consistent with") were added systematically to flag interpretive claims.

---

**Comment R3.7 — Presentation too dense, Introduction too long.**

> *"The introduction is longer than necessary and includes repetitive arguments."*

**Response:**
The Introduction was cut from ~820 words (15 paragraphs) to ~420 words (6 paragraphs). Redundant contextual material was removed. The revised Introduction follows a direct path: erosion problem → limitations of synthetic geosynthetics → performance gap of biodegradable alternatives → study objective and hypothesis.

---

**Comment R3.8 — Title: "Weibull reliability project" is inadequate.**

> *"The term 'Weibull reliability project' is not technically appropriate in this context."*

**Response:**
We agree. The title was revised to:

> **"Weibull-reliability framework linking chemical architecture to Functional Service Life in natural-fiber geotextiles for tropical soil erosion control"**

---

We trust these revisions address the concerns raised. We remain available for further clarification.

Sincerely,

Luiz Diego Vidal Santos, on behalf of all authors
