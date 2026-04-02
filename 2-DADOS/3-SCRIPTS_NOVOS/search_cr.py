import urllib.request
import json
import urllib.parse

def search(query):
    url = f"https://api.crossref.org/works?query={urllib.parse.quote(query)}&rows=1"
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "mailto:teste@ldvsantos.com"})
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode())
            if data["message"]["items"]:
                cr = data["message"]["items"][0]
                print(f"Buscado: {query}\n -> Achou: {cr.get(\"title\", [\"\"])[0]}\n -> DOI: {cr.get(\"DOI\", \"\")}\n")
            else:
                print(f"Nada para: {query}")
    except Exception as e:
        print("Erro", e)

search("Compound and cascading drought impacts on key ecosystem services in Europe")
search("The soil management assessment framework a quantitative soil quality evaluation method")
search("Building and estimating structural equation models with the")
search("Fuzzy soil quality indexing for evaluating rice-based cropping systems under conservation agriculture in")
search("Soil quality index of an")
search("Soil quality assessment using erosion-sensitive indices and fuzzy membership under different croppin")
search("Comparison of three soil quality index approaches coupled with a machine learning approach for asses")
search("Shifts in microbial and physicochemical parameters associated with increasing soil quality in a trop")
search("Do fallow in the off-season and crop succession promote differences in soil aggregation in no-tillag")
search("Towards a standard technique for soil quality assessment")
search("Machine learning versus fuzzy inference comparing soil quality index approaches for")
search("Connectivity and percolation of 8 years of rain by preferential flow in a clay soil")

