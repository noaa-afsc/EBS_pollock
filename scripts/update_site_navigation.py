"""Refresh navigation in historical rendered pages without rerunning analyses."""
from pathlib import Path
import re
r=Path(__file__).resolve().parents[1]/'docs'
pattern=r'<nav id="quarto-sidebar".*?</nav>'
s=re.search(pattern,(r/'development.html').read_text(),re.S).group()
s='\n'.join(line.rstrip() for line in s.splitlines())
s=s.replace(' sidebar-item-text sidebar-link active',' sidebar-item-text sidebar-link').replace('aria-current="page"','')
s=re.sub(r'href="\./([^\"]*)"',lambda m:'href="../'+m[1]+'"',s)
for p in (r/'doc').glob('*.html'):
 text=p.read_text()
 if re.search(pattern,text,re.S):p.write_text(re.sub(pattern,lambda m:s,text,count=1,flags=re.S))
p=r/'sitemap.xml';p.write_text(p.read_text().replace('https://nmfs-opensci.github.io/NOAA-quarto-simple/','https://noaa-afsc.github.io/EBS_pollock/'))
