"""Check the published report, its homepage links, and local assets."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit, unquote
import hashlib, json
ROOT=Path(__file__).resolve().parents[1]
class Links(HTMLParser):
    def __init__(self):
        super().__init__(); self.urls=[]; self.tables=0; self.images=0
    def handle_starttag(self,tag,attrs):
        a=dict(attrs)
        for k in ('href','src'):
            if k in a:self.urls.append(a[k])
        self.tables+=tag=='table'; self.images+=tag=='img'
class Table2Links(HTMLParser):
    def __init__(self):
        super().__init__(); self.depth=0; self.urls=[]
    def handle_starttag(self,tag,attrs):
        a=dict(attrs)
        if tag=='div' and a.get('id')=='tbl-management-measures':self.depth=1
        elif tag=='div' and self.depth:self.depth+=1
        if self.depth and tag=='a' and 'href' in a:self.urls.append(a['href'])
    def handle_endtag(self,tag):
        if tag=='div' and self.depth:self.depth-=1
for name in ['index.html','development.html','2026/assessment.html','2026/weight-at-age-bts-cv-sensitivity.html','2026/projection-empirical-weight-comparison.html','2026/projection-re-weight-comparison.html']:
    p=ROOT/'docs'/name; s=p.read_text(); parser=Links();parser.feed(s)
    assert '/Users/' not in s and 'file:///' not in s,name
    for url in parser.urls:
        u=urlsplit(url)
        if u.scheme or u.netloc or not u.path:continue
        target=(ROOT/'docs'/unquote(u.path).lstrip('/')) if u.path.startswith('/') else p.parent/unquote(u.path)
        assert target.exists(),(name,url)
    if name=='2026/assessment.html':
        assert parser.tables==89 and parser.images==89,(parser.tables,parser.images)
        table2=Table2Links();table2.feed(s)
        assert len(table2.urls)==39,len(table2.urls)
        assert all(u.startswith('table2-sources/') for u in table2.urls)
        assert 'This approach was adopted in the 2024 assessment' in s
for name in ['assessment.html','assessment.pdf']:
    assert (ROOT/'2026'/name).read_bytes()==(ROOT/'docs/2026'/name).read_bytes(),name
manifest=json.loads((ROOT/'2026/release.json').read_text())
for name,sha in manifest['files'].items():
    assert hashlib.sha256((ROOT/'2026'/name).read_bytes()).hexdigest()==sha,name
print('PASS: homepage, development links, 89 tables, 89 images, 39 Table 2 source links, report assets, matching release hashes')
