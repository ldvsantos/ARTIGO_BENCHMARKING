import re
import urllib.request
import json
import urllib.error
import urllib.parse
import time

def check_doi(key, doi, title):
    url = f"https://api.crossref.org/works/{urllib.parse.quote(doi)}"
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "mailto:test@ldvsantos.com"})
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode())
            cr_title = data["message"]["title"][0] if data.get("message", {}).get("title") else "NO TITLE IN CROSSREF"
            print(f"[OK] {key} | DOI VÁLIDO: {doi}")
            time.sleep(0.1) 
            return True
    except urllib.error.HTTPError as e:
        if e.code == 404:
            print(f"\n[ALERTA - 404] O DOI '{doi}' da ref '{key}' NÃO FUNCIONA!\n     Original: {title}\n")
        else:
            print(f"\n[ERRO API {e.code}] Falha na verificação de {doi}\n")
        return False
    except Exception as e:
        print(f"\n[ERRO EXCEÇÃO] {doi}: {str(e)}\n")
        return False

with open(r"c:\Users\vidal\OneDrive\Documentos\13 - CLONEGIT\artigo-posdoc\7-ARTIGO_BENCHMARKING\1-MANUSCRITO\EN\sn-bibliography.bib", encoding="utf-8") as f:
    content = f.read()

entries = re.findall(r"@\w+\s*\{\s*([^,]+),([^@]*)", content, flags=re.MULTILINE|re.IGNORECASE)

print(f"Total de referências encontradas na bibliografia: {len(entries)}\n")

c_ok = 0
c_err = 0
c_nodoi = 0

for key, entry_text in entries:
    doi_match = re.search(r"doi\s*=\s*(?:\{|\")([^{}\",]+)", entry_text, re.IGNORECASE)
    title_match = re.search(r"title\s*=\s*(?:\{|\")([^{}\"]+)", entry_text, re.IGNORECASE)
    title = title_match.group(1).replace('\n', ' ').strip() if title_match else "Sem título"
    
    if doi_match:
        doi = doi_match.group(1).strip()
        is_ok = check_doi(key.strip(), doi, title)
        if is_ok: c_ok += 1
        else: c_err += 1
    else:
        print(f"[NO DOI] Ignorando {key.strip()} ({title[:40]}...)")
        c_nodoi += 1

print(f"\n--- RESUMO ---")
print(f"Válidos: {c_ok}")
print(f"Erros/Alucinações: {c_err}")
print(f"Sem DOI: {c_nodoi}")

