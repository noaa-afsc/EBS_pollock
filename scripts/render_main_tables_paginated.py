"""Render the canonical main tables with fresh-page starts and readable type."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as f,re,subprocess,tempfile,json,html
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';work=Path('/tmp/assessment-main-tables-pagination');work.mkdir(exist_ok=True)
s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser');table_section=s.find(id='tables');fontdir=Path('/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode');font=f.Font(fontfile=str(fontdir/'cmunrm.otf'));PUB='https://noaa-afsc.github.io/EBS_pollock/2026/'
items=[];sections=[]
for label in table_section.select('.safe-caption-label'):
 m=re.fullmatch(r'Table\s+(\d+):',label.get_text(' ',strip=True))
 if not m:continue
 n=int(m[1]);root=next(p for p in label.parents if p.get('id','').startswith('tbl-') and '-caption-'not in p['id']);clone=BeautifulSoup(str(root),'html.parser').find();table=clone.find('table');assert table
 for el in clone.select('script, details, .code-copy-button'):el.decompose()
 cap=clone.find(['caption','figcaption']);caption=cap.decode_contents();cap.decompose()
 for a in clone.select('a[href]'):
  href=a['href']
  if href.startswith('#'):a['href']=PUB+'assessment.html'+href
  elif not re.match(r'^[a-zA-Z][\w+.-]*:',href) and not href.startswith('//'):a['href']=PUB+href
 # Estimate minimum widths from unbroken cell tokens; choose landscape only when needed.
 widths=[]
 for tr in table.find_all('tr'):
  col=0
  for cell in tr.find_all(['th','td'],recursive=False):
   span=int(cell.get('colspan',1));tokens=re.split(r'[\s\-–—]+',cell.get_text(' ',strip=True));need=max([font.text_length(t,fontsize=9.5)for t in tokens]+[0])+4
   while len(widths)<col+span:widths.append(0)
   if span==1:widths[col]=max(widths[col],need)
   col+=span
 estimated=sum(widths);landscape=estimated>550
 klass=(' landscape' if landscape else '')+(' compact' if 40<=len(table.select('tbody tr'))<=52 else '')
 heading='<h1>Tables</h1>'if n==1 else ''
 sections.append(f'<section class="pdf-table{klass}" id="pdf-table-{n}">{heading}<div class="pdf-caption">{caption}</div>{str(clone)}</section>')
 items.append({'number':n,'id':root['id'],'landscape':landscape,'estimated_min_width_pt':round(estimated,1),'body_rows':len(table.select('tbody tr')),'expected_text':BeautifulSoup(caption+str(clone),'html.parser').get_text(' ',strip=True)})
assert [x['number']for x in items]==list(range(1,55))
css=f'''
@font-face {{font-family:CMU;src:url("{(fontdir/'cmunrm.otf').as_uri()}");font-weight:400;font-style:normal}}
@font-face {{font-family:CMU;src:url("{(fontdir/'cmunbx.otf').as_uri()}");font-weight:700;font-style:normal}}
@font-face {{font-family:CMU;src:url("{(fontdir/'cmunti.otf').as_uri()}");font-weight:400;font-style:italic}}
@font-face {{font-family:CMU;src:url("{(fontdir/'cmunbi.otf').as_uri()}");font-weight:700;font-style:italic}}
@page {{size:letter portrait;margin:42pt 36pt 48pt}}
@page wide {{size:letter landscape;margin:42pt 36pt 48pt}}
* {{box-sizing:border-box;font-family:CMU,serif!important}}
body {{margin:0;font-size:9.5pt;line-height:1.12;color:#222}}
.pdf-table {{break-before:page;break-inside:auto}}
.pdf-table:first-child {{break-before:auto}}
.landscape {{page:wide}}
h1 {{font-size:15pt;margin:0 0 12pt;break-after:avoid}}
.pdf-caption {{font-size:9.5pt;line-height:1.15;margin:0 0 10pt;break-inside:avoid;break-after:avoid}}
.safe-caption-label {{font-weight:bold}}
figure {{margin:0!important}}
.pdf-table div {{overflow:visible!important;max-height:none!important;height:auto!important;padding-top:0!important;padding-bottom:0!important}}
table {{border-collapse:collapse!important;width:100%!important;max-width:100%!important;table-layout:auto!important;font-size:9.5pt!important;line-height:1.12!important;break-inside:auto;margin:0!important}}
thead {{display:table-header-group}}
tfoot {{display:table-row-group}}
tr {{break-inside:avoid-page}}
th,td {{font-size:9.5pt!important;line-height:1.12!important;padding:1.1pt 2pt!important;vertical-align:top!important;overflow:visible!important;height:auto!important}}
th {{text-align:left}}
thead th,thead td {{background:#e8edf0!important}}
.compact th,.compact td {{font-size:9pt!important;line-height:1.10!important;padding-top:.7pt!important;padding-bottom:.7pt!important}}
tbody tr:nth-child(odd) td,tbody tr:nth-child(odd) th {{background:#f4f6f7}}
.gt_title {{font-size:10.5pt!important;padding:3pt!important}}
.gt_sourcenote,.gt_footnote,small {{font-size:8pt!important;line-height:1.15!important}}
.gt_sourcenote,.gt_footnote {{padding-top:5pt!important}}
.gt_right {{text-align:right!important}} .gt_left {{text-align:left!important}} .gt_center {{text-align:center!important}}
#tbl-prohibited-species-catch th,#tbl-prohibited-species-catch td {{text-align:right!important}}
sub,sup {{font-size:70%;line-height:0}}
a {{color:#0645ad;text-decoration:underline}}
p {{margin:5pt 0}}
'''
file=work/'tables.html';file.write_text('<!doctype html><html><head><meta charset="utf-8"><title>Assessment tables</title><style>'+css+'</style></head><body>'+''.join(sections)+'</body></html>')
(work/'table-manifest.json').write_text(json.dumps(items,indent=2)+'\n')
cmd=['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome','--headless','--disable-gpu','--no-pdf-header-footer','--allow-file-access-from-files','--user-data-dir='+str(work/'chrome-profile'),'--virtual-time-budget=3000','--print-to-pdf='+str(work/'tables.pdf'),file.as_uri()]
r=subprocess.run(cmd,capture_output=True,text=True,timeout=120);(work/'chrome.log').write_text(r.stdout+r.stderr);assert r.returncode==0,(r.returncode,r.stderr[-2000:]);assert (work/'tables.pdf').is_file()
p=f.open(work/'tables.pdf');caps={}
for i,page in enumerate(p):
 for b in page.get_text('blocks'):
  m=re.match(r'Table\s+(\d+):',b[4].strip())
  if m:caps.setdefault(int(m[1]),i)
assert sorted(caps)==list(range(1,55)),caps
for j,item in enumerate(items):
 start=caps[item['number']];end=caps.get(item['number']+1,len(p));item['first_page']=start;item['page_count']=end-start
 assert start==0 or caps[item['number']]!=caps.get(item['number']-1),'Two tables share a page'
 assert '/tmp/'not in ''.join(p[i].get_text()for i in range(start,end))
(work/'table-manifest.json').write_text(json.dumps(items,indent=2)+'\n')
for n in [1,2,8,21,22,29,30,31,37,38,39,53,54]:p[caps[n]].get_pixmap(matrix=f.Matrix(1.3,1.3)).save(str(work/f'table-{n}.png'))
print(json.dumps({'pages':len(p),'tables':len(items),'landscape_tables':[x['number']for x in items if x['landscape']],'multipage_tables':[{k:x[k]for k in ['number','page_count']}for x in items if x['page_count']>1]},indent=2))
