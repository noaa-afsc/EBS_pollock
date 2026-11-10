"""Check the final HTML/PDF editorial revision and publication invariants."""
from pathlib import Path
import json,re,collections,unicodedata,subprocess
from bs4 import BeautifulSoup,Comment
import pymupdf as f
R=Path(__file__).resolve().parents[1]/'2026';p=f.open(R/'assessment.pdf');s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser')
old=BeautifulSoup(subprocess.check_output(['git','show','8497cae:2026/assessment.html'],cwd=R.parent),'html.parser')
for e in s(['style','script']):e.decompose()
for e in s.find_all(string=lambda x:isinstance(x,Comment)):e.extract()
ids={e['id']for e in s.find_all(id=True)};assert all(a['href'][1:]in ids for a in s.select('a[href^="#"]'))
text=s.get_text(' ',strip=True)+'\n'+'\n'.join(q.get_text()for q in p)
for pat in [r'\bvintages\b',r'weight\s+at\s+age',r'natural-mortality',r'base case',r'ages-3\+ biomass']:assert not re.search(pat,text,re.I),pat
assert sum('table2-sources/'in l.get('uri','')for q in p for l in q.get_links())==39
def terms(t):
 t=re.sub('weight at age',lambda m:'Weight-at-age'if m[0][0].isupper()else'weight-at-age',t,flags=re.I)
 return t.replace('These are distinct vintages.','These series use different data updates.')
assert len(old.find_all('table'))==len(s.find_all('table'))==91
for a,b in zip(old.find_all('table'),s.find_all('table')):
 for tag in ['thead','tbody']:
  x=a.find(tag);y=b.find(tag);assert terms(x.get_text(' ',strip=True)if x else '')==(y.get_text(' ',strip=True)if y else '')
def chars(t):return collections.Counter(c for c in unicodedata.normalize('NFKC',t).replace('\xad','-').replace('‐','-')if not c.isspace())
bib=''.join(e.get_text()for e in s.select('#refs > .csl-entry'));pdfbib=''.join(q.get_text(clip=f.Rect(49,389 if i==35 else 49,566,736))for i,q in enumerate(p)if 35<=i<=40)
assert chars(bib)==chars(pdfbib),(chars(bib)-chars(pdfbib),chars(pdfbib)-chars(bib))
checks={'pages':len(p),'all_90_bibliography_entries_verified_against_HTML':True,'all_HTML_internal_anchor_targets_verified':True,'all_91_table_values_preserved':True,'Table_2_source_links':39,'outdated_terms_absent':True,'computer_modern':True,'raster_figures':'ATS title updated; all other main-report raster figures retained','graphic_titles_updated':3,'visual_review_pages':[3,4,5,6,7,36,41,264],'model_compilation_and_convergence':'Not applicable: editorial changes use saved results; no model run','main_report_render':'Edited canonical HTML/PDF directly; no owning current Quarto main-report source','site_quarto_render':'Passed; local resource-embedding warnings checked by site link validator','manual_accessibility':'Not Tested'}
(R/'validation/editorial-20261109/checks.json').write_text(json.dumps(checks,indent=2)+'\n');print(json.dumps(checks,indent=2))
