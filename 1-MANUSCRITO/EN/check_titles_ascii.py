import re
import urllib.request
import json
import urllib.error
import urllib.parse
from difflib import SequenceMatcher

def similarity(a, b):
    return SequenceMatcher(None, a.lower(), b.lower()).ratio()

with open(r"c:\Users\vidal\OneDrive\Documentos\13 - CLONEGIT\artigo-posdoc\7-ARTIGO_BENCHMARKING\1-MANUSCRITO\EN\sn-bibliography.bib", encoding="utf-8") as f:
    content = f.read()

entries = re.findall(r"@\w+\s*\{\s*([^,]+),([^@]*)", content, flags=re.MULTILINE|re.IGNORECASE)

print("--- AUDITORIA DE TITULOS CROSSREF ---")
for key, entry_text in entries:
    doi_match = re.search(r"doi\s*=\s*(?:\{|\")([^{}\",]+)", entry_text, re.IGNORECASE)
    title_match = re.search(r"title\s*=\s*(?:\{|\")([^{}\"]+)", entry_text, re.IGNORECASE)
    title = title_match.group(1).replace("\n", " ").strip() if title_match else ""
    
    if doi_match and title:
        doi = doi_match.group(1).strip()
        url = f"https://api.crossref.org/works/{urllib.parse.quote(doi)}"
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "mailto:teste@ldvsantos.com"})
            with urllib.request.urlopen(req) as response:
                data = json.loads(response.read().decode())
                cr_title = data["message"]["title"][0] if data.get("message", {}).get("title") else ""
                
                # remover chaves e pontua??o extra para confer?ncia
                clean_title = re.sub(r"[{}\[\]'\-:,]+", "", title).strip()
                clean_cr = re.sub(r"[{}\[\]'\-:,]+", "", cr_title).strip()
                
                sim = similarity(clean_title, clean_cr)
                if sim < 0.7:
                    print(f"\n[ALERTA DISCREP?NCIA] {key} ({sim*100:.1f}%)")
                    print(f"  - BIB: {title[:100]}...")
                    print(f"  - CR : {cr_title[:100]}...")
        except:
            pass
print("--- FIM ---")
