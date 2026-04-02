"""
Integrar dados da Sara Assunção (2019, 17º ano) ao dataset master (22º ano, n=108).
Estratégia: médias por tratamento (Sistema × Cultura) — mesma lógica usada para pH/P/K do Jaelson.
"""
import csv
from collections import defaultdict
import statistics

# ── Paths ──────────────────────────────────────────────────────────
BASE = r"c:\Users\vidal\OneDrive\Documentos\13 - CLONEGIT\artigo-posdoc\7-ARTIGO_BENCHMARKING\2-DADOS\1-DADOS_BRUTOS"
SARA_CSV    = f"{BASE}\\Sara2019_dataset_completo.csv"
IPACSA_CSV  = f"{BASE}\\IPACSA_resultados_completos.csv"
IQS_CSV     = f"{BASE}\\IQS_todos_paradigmas_18ind.csv"
OUT_MASTER  = f"{BASE}\\MASTER_38ind.csv"
OUT_SARA_MEANS = f"{BASE}\\Sara2019_medias_tratamento.csv"

# ── Variables to extract from Sara (20 new, non-overlapping with current 18) ──
SARA_VARS = [
    "Ca_cmolc_dm3", "Mg_cmolc_dm3", "Al_cmolc_dm3", "HAl_cmolc_dm3",
    "SB_cmolc_dm3", "T_cmolc_dm3", "V_pct", "m_pct", "MOS_g_kg",
    "EstN_kg_ha", "MI_pct", "MA_pct", "VIB_mm_h", "RP_KPa", "AD_PT",
    "qMIC", "qCO2", "CCO2_g_kg", "CMIC_ug_g", "NMIC_mg_kg"
]

# ── Name mapping: Sara → current dataset ──
SISTEMA_MAP = {"CC": "CT", "CM": "MT", "PD": "NT"}
CULTURA_MAP = {"Caupi": "Cowpea", "Guandu": "Pigeon pea",
               "Crotalaria": "Sunn hemp", "Milheto": "Millet"}

# ── Step 1: Read Sara, compute treatment means ──
print("=" * 60)
print("STEP 1: Reading Sara 2019 dataset and computing treatment means")
print("=" * 60)

sara_data = defaultdict(lambda: defaultdict(list))

with open(SARA_CSV, encoding="utf-8") as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row["Cultura"] == "Mata":
            continue  # skip forest reference
        sistema = row["Sistema"]
        cultura = row["Cultura"]
        key = (sistema, cultura)
        for var in SARA_VARS:
            val = row.get(var, "")
            if val.strip():
                sara_data[key][var].append(float(val))

# Compute means
sara_means = {}
for (sistema, cultura), vars_dict in sara_data.items():
    parcela = SISTEMA_MAP[sistema]
    cult_en = CULTURA_MAP[cultura]
    key_out = (parcela, cult_en)
    sara_means[key_out] = {}
    for var in SARA_VARS:
        vals = vars_dict.get(var, [])
        if vals:
            sara_means[key_out][var] = round(statistics.mean(vals), 4)
        else:
            sara_means[key_out][var] = None

print(f"\nTreatment means computed for {len(sara_means)} combinations:")
for (p, c), vals in sorted(sara_means.items()):
    n_vars = sum(1 for v in vals.values() if v is not None)
    print(f"  {p:3s} × {c:12s}: {n_vars} variables with data")

# Save Sara means to CSV
with open(OUT_SARA_MEANS, "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["Parcela", "Cultura"] + SARA_VARS)
    for (p, c), vals in sorted(sara_means.items()):
        writer.writerow([p, c] + [vals.get(v, "") for v in SARA_VARS])
print(f"\nSara treatment means saved to: {OUT_SARA_MEANS}")

# ── Step 2: Read IQS_todos_paradigmas (main working file) ──
print("\n" + "=" * 60)
print("STEP 2: Reading IQS_todos_paradigmas_18ind.csv")
print("=" * 60)

with open(IQS_CSV, encoding="utf-8") as f:
    reader = csv.DictReader(f)
    iqs_rows = list(reader)
    iqs_fields = reader.fieldnames

print(f"  Rows: {len(iqs_rows)}")
print(f"  Columns: {len(iqs_fields)}")
print(f"  Parcela values: {sorted(set(r['Parcela'] for r in iqs_rows))}")
print(f"  Cultura values: {sorted(set(r['Cultura'] for r in iqs_rows))}")

# ── Step 3: Merge Sara means into IQS dataset ──
print("\n" + "=" * 60)
print("STEP 3: Merging Sara means into master dataset")
print("=" * 60)

new_fields = iqs_fields + SARA_VARS
merged_rows = []
matched = 0
unmatched = 0

for row in iqs_rows:
    parcela = row["Parcela"].strip().strip('"')
    cultura = row["Cultura"].strip().strip('"')
    key = (parcela, cultura)
    
    new_row = dict(row)
    
    if key in sara_means:
        for var in SARA_VARS:
            new_row[var] = sara_means[key].get(var, "")
        matched += 1
    else:
        for var in SARA_VARS:
            new_row[var] = ""
        unmatched += 1
        print(f"  WARNING: No Sara data for {parcela} × {cultura}")

    merged_rows.append(new_row)

print(f"\n  Matched: {matched}/{len(iqs_rows)} observations")
if unmatched:
    print(f"  Unmatched: {unmatched}")

# ── Step 4: Save master CSV ──
print("\n" + "=" * 60)
print("STEP 4: Saving master dataset")
print("=" * 60)

with open(OUT_MASTER, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=new_fields, quoting=csv.QUOTE_MINIMAL)
    writer.writeheader()
    for row in merged_rows:
        writer.writerow(row)

print(f"  Output: {OUT_MASTER}")
print(f"  Rows: {len(merged_rows)}")
print(f"  Columns: {len(new_fields)}")
print(f"  Original indicators: {len(iqs_fields)}")
print(f"  New Sara indicators: {len(SARA_VARS)}")
print(f"  Total columns: {len(new_fields)}")

# ── Step 5: Summary statistics for new variables ──
print("\n" + "=" * 60)
print("STEP 5: Summary statistics for new Sara variables (treatment means)")
print("=" * 60)

for var in SARA_VARS:
    vals = [sara_means[k][var] for k in sara_means if sara_means[k].get(var) is not None]
    if vals:
        print(f"  {var:20s}: min={min(vals):8.2f}  max={max(vals):8.2f}  "
              f"mean={statistics.mean(vals):8.2f}  sd={statistics.stdev(vals):8.2f}  n={len(vals)}")

# ── Step 6: Collinearity flags ──
print("\n" + "=" * 60)
print("STEP 6: Collinearity flags")
print("=" * 60)
print("  PERFECT DEPENDENCIES (will require exclusion in PLS-SEM and ANOVA/PCA):")
print("    SB ≈ Ca + Mg + K        → drop SB from analysis")
print("    T(CTC) = SB + H+Al      → keep CTC, drop H+Al OR keep both and let VIF decide")
print("    V% = SB/T × 100         → drop V% if Ca, Mg, K, CTC included")
print("    m% = Al/(SB+Al) × 100   → drop m% if Al included")
print("")
print("  RECOMMENDED ANALYSIS SET (excluding perfect collinearities):")
print("    Chemical new (5): Ca, Mg, CTC, MOS, Al")
print("    Physical new (5): MI, MA, VIB, RP, AD_PT")
print("    Nutrient (1):     EstN")
print("    Micro (5):        qMIC, qCO2, CCO2, CMIC, NMIC")
print("    → Drop SB, H+Al, V%, m% from models")
print("    → Total: 18 existing + 16 new = 34 indicators (n/p = 3.2)")
print("")
print("  ALL 20 Sara variables stored in CSV for full flexibility.")

print("\n✓ Integration complete.")
