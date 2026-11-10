"""Resolve same-report web fragments to internal PDF GoTo annotations."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as f,re,unicodedata,json,hashlib,collections
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';BASE='https://noaa-afsc.github.io/EBS_pollock/2026/assessment.html#'
s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser');p=f.open(R/'assessment.pdf')
def norm(t):return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKC',t).casefold())
lines=[];pages=[]
for i,page in enumerate(p):
 groups=[]
 for block in page.get_text('dict')['blocks']:
  for line in block.get('lines',[]):
   if tuple(line['dir'])!=(1.,0.):continue
   for sp in line['spans']:
    y=sp['origin'][1];g=next((g for g in groups if abs(g[0]-y)<1.5),None)
    if g is None:g=[y,[]];groups.append(g)
    g[1].append(sp)
 page_lines=[]
 for y,spans in sorted(groups):
  text=' '.join(sp['text']for sp in sorted(spans,key=lambda sp:sp['origin'][0]));top=min(sp['bbox'][1]for sp in spans);record=(i,top,text,norm(text));lines.append(record);page_lines.append(record)
 joined='';positions=[]
 for rec in page_lines:
  joined+=rec[3];positions.extend([rec[1]]*len(rec[3]))
 pages.append((joined,positions))
caption_map={}
for i,y,text,_ in lines:
 match=re.match(r'^\s*(Table|Figure)\s+(\d+)\s*:',text)
 if match:caption_map.setdefault((match[1],int(match[2])),(i,max(0,y-5),text))
def locate_text(text):
 target=norm(text)
 # Prefer an exact heading/entry-start line over an in-text mention.
 for i,y,t,n in lines:
  if n==target:return i,max(0,y-5),t
 for i,(text,positions)in enumerate(pages):
  j=text.find(target)
  if j>=0:return i,max(0,positions[j]-5),target
 return None
ids=sorted({l['uri'][len(BASE):]for page in p for l in page.get_links()if l.get('uri','').startswith(BASE)})
destinations={};unresolved=[]
for id in ids:
 e=s.find(id=id);assert e,id
 cap=e.find(['caption','figcaption']);hit=None
 if cap:
  label=re.match(r'(Table|Figure)\s+(\d+)\s*:',cap.get_text(' ',strip=True))
  if label:hit=caption_map.get((label[1],int(label[2])))
 if hit is None:
  heading=e.find(['h1','h2','h3','h4','h5']) if e.name not in ['h1','h2','h3','h4','h5'] else e
  if heading:hit=locate_text(heading.get_text(' ',strip=True))
 if hit is None and id=='advice-model260':hit=locate_text('Model 26.0 advice from revised fits')
 if hit is None:
  text=e.get_text(' ',strip=True) if id.startswith('ref-') else (e.find('p')or e).get_text(' ',strip=True)
  for length in [110,90,70,55]:
   prefix=norm(text)[:length]
   if len(prefix)>=35:
    hit=locate_text(prefix)
    if hit:break
 if hit:destinations[id]=hit
 else:unresolved.append((id,e.get_text(' ',strip=True)[:150]))
assert not unresolved,unresolved
original_text=[page.get_text()for page in p];original_images=[[x['digest'].hex()for x in page.get_image_info(hashes=True)]for page in p];original_toc=p.get_toc();before_links=[(i,l)for i,page in enumerate(p)for l in page.get_links()]
external_before=collections.Counter(l['uri']for _,l in before_links if l['kind']==f.LINK_URI and not l['uri'].startswith(BASE));old_goto=sum(l['kind']==f.LINK_GOTO for _,l in before_links);audit=[]
for i,page in enumerate(p):
 for link in page.get_links():
  uri=link.get('uri','')
  if not uri.startswith(BASE):continue
  id=uri[len(BASE):];dest,y,title=destinations[id]
  page.update_link({'xref':link['xref'],'kind':f.LINK_GOTO,'from':link['from'],'page':dest,'to':f.Point(36,y),'zoom':0})
  audit.append({'source_page':i+1,'anchor':id,'destination_page':dest+1,'destination_y':round(y,2),'destination_text':title[:180]})
tmp=R/'assessment-internal-links-tmp.pdf';p.save(tmp,garbage=4,deflate=True);p.close();q=f.open(tmp)
assert len(q)==len(original_text) and q.get_toc()==original_toc
for i,page in enumerate(q):
 assert page.get_text()==original_text[i],('text changed',i+1)
 assert [x['digest'].hex()for x in page.get_image_info(hashes=True)]==original_images[i],('figure changed',i+1)
links=[(i,l)for i,page in enumerate(q)for l in page.get_links()];assert not any(l.get('uri','').startswith(BASE)for _,l in links)
assert collections.Counter(l['uri']for _,l in links if l['kind']==f.LINK_URI)==external_before
assert sum(l['kind']==f.LINK_GOTO for _,l in links)==old_goto+len(audit)
assert all(l['kind']!=f.LINK_GOTO or 0<=l['page']<len(q)for _,l in links)
assert sum('table2-sources/'in l.get('uri','')for _,l in links)==39
checks={'converted_same_report_links':len(audit),'unique_destinations':len(destinations),'remaining_same_report_web_links':0,'all_page_text_unchanged':True,'all_raster_figures_unchanged':True,'all_existing_external_links_preserved':True,'all_existing_bookmarks_preserved':True,'Table_2_external_source_links_preserved':39,'Table_38_destination':next(a for a in audit if a['anchor']=='tbl-model262-current-parameter-families'),'pages':len(q),'manual_accessibility':'Not Tested'}
q.close();tmp.replace(R/'assessment.pdf');v=R/'validation/internal-pdf-links-20261109';v.mkdir(exist_ok=True);(v/'destinations.json').write_text(json.dumps(audit,indent=2)+'\n');(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-internal-pdf-links';m['internal_PDF_links']=checks;m['files']['assessment.pdf']=hashlib.sha256((R/'assessment.pdf').read_bytes()).hexdigest();(R/'release.json').write_text(json.dumps(m,indent=2)+'\n');print(json.dumps(checks,indent=2))
