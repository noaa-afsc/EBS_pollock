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
for name in ['index.html','development.html','2026/assessment.html','2026/weight-at-age-bts-cv-sensitivity.html','2026/projection-empirical-weight-comparison.html','2026/projection-re-weight-comparison.html']:
    p=ROOT/'docs'/name; s=p.read_text(); parser=Links();parser.feed(s)
    assert '/Users/' not in s and 'file:///' not in s,name
    for url in parser.urls:
        u=urlsplit(url)
        if u.scheme or u.netloc or not u.path:continue
        target=(ROOT/'docs'/unquote(u.path).lstrip('/')) if u.path.startswith('/') else p.parent/unquote(u.path)
        assert target.exists(),(name,url)
    if name=='2026/assessment.html':
        assert parser.tables==91 and parser.images==88,(parser.tables,parser.images)
        assert 'This approach was adopted in the 2024 assessment' in s
for name in ['assessment.html','assessment.pdf']:
    assert (ROOT/'2026'/name).read_bytes()==(ROOT/'docs/2026'/name).read_bytes(),name
manifest=json.loads((ROOT/'2026/release.json').read_text())
for name,sha in manifest['files'].items():
    assert hashlib.sha256((ROOT/'2026'/name).read_bytes()).hexdigest()==sha,name
print('PASS: homepage, development links, 91 tables, 88 images, report assets, matching release hashes')
