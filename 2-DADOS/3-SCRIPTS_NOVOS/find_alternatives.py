import urllib.request
import json
import urllib.parse
def s(q):
    url = "https://api.crossref.org/works?query=" + urllib.parse.quote(q) + "&rows=5"
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "test@test.com"})
        with urllib.request.urlopen(req) as response:
            items = json.loads(response.read().decode())["message"]["items"]
            for cr in items:
                title = cr.get("title",[""])[0][:80]
                doi = cr.get("DOI","")
                print(f"{title} ({doi})")
    except BaseException as e:
        print("Erro: ", e)

print("== ML vs Fuzzy SQI ==")
s("machine learning fuzzy soil quality index")
print("== Fuzzy SQI rice ==")
s("fuzzy soil quality index rice Cambodia")
print("== ML SQI overfitting ==")
s("soil quality index machine learning overfit")
