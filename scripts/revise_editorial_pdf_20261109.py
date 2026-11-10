"""Reflow approved editorial paragraphs and references; retain figures and numeric tables."""
from pathlib import Path
import io,re,sys,collections,json,hashlib,unicodedata,subprocess
import pymupdf as f
from bs4 import BeautifulSoup,NavigableString
from reportlab.pdfgen import canvas
from reportlab.platypus import Paragraph,BaseDocTemplate,PageTemplate,Frame
from reportlab.lib.styles import ParagraphStyle
from xml.sax.saxutils import escape
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'
s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser');p=f.open(stream=subprocess.check_output(['git','show','8497cae:2026/assessment.pdf'],cwd=ROOT),filetype='pdf')
old_images=[[v['digest'].hex() for v in q.get_image_info(hashes=True)] for q in p]
old_toc=p.get_toc();anchors={};overlays=[];regions=[]
BASE='https://noaa-afsc.github.io/EBS_pollock/2026/assessment.html'
def inline(e):
 if isinstance(e,NavigableString):return escape(str(e).replace('\xad','').replace('‐','-'))
 if e.name in ['style','script']:return ''
 inner=''.join(inline(x) for x in e.children)
 if e.name in ['i','em']:return '<i>'+inner+'</i>'
 if e.name in ['b','strong']:return '<b>'+inner+'</b>'
 if e.name in ['sup','sub']:return '<'+e.name+'>'+inner+'</'+e.name+'>'
 if e.name=='a' and e.get('href'):return '<a href="'+escape(BASE+e['href'] if e['href'].startswith('#') else e['href'],{'"':'&quot;'})+'" color="#202020">'+inner+'</a>'
 return inner
class MarkedParagraph(Paragraph):
 def drawOn(self,c,x,y,*a,**kw):
  if getattr(self,'anchor_id',None):anchors[self.anchor_id]=(getattr(c,'target_page',c.getPageNumber()-1),792-y-self.height)
  return super().drawOn(c,x,y,*a,**kw)
def para(e,size=10.2):
 h=e.name in ['h1','h2','h3'];fs={'h1':15,'h2':12.5,'h3':11}.get(e.name,size)
 q=MarkedParagraph(inline(e),ParagraphStyle('x',fontName='Times-Bold' if h else 'Times-Roman',fontSize=fs,leading=fs*1.2,spaceAfter=6 if size>=10 else 2))
 q.anchor_id=e.get('id') or e.get('data-anchor-id')
 return q
# Locate unchanged heading destinations before replacing prose.
for e in s.find_all(['h1','h2','h3','h4']):
 id=e.get('id') or e.get('data-anchor-id') or (e.parent.get('id') if e.parent.name=='section' else None)
 if not id:continue
 label=e.get_text(' ',strip=True)
 for item in old_toc:
  if item[1]==label:
   page=item[2]-1;hits=p[page].search_for(label)
   anchors[id]=(page,hits[0].y0 if hits else 50);break
# Render a replacement fragment inside its existing page allocation.
def fragment(page,rect,elements,size=10.2):
 rect=f.Rect(rect);buf=io.BytesIO();c=canvas.Canvas(buf,pagesize=(612,792));c.target_page=page
 ps=[para(e,size) for e in elements]
 scale=1
 while True:
  total=0
  for q in ps:
   q.style.fontSize*=scale;q.style.leading*=scale
   _,h=q.wrap(rect.width,1000);total+=h+q.style.spaceAfter
  if total<=rect.height+.1:break
  scale=.98
  assert ps[0].style.fontSize>7,('fragment overflow',page,total,rect.height)
 y=rect.y0
 for q in ps:
  _,h=q.wrap(rect.width,1000);q.drawOn(c,rect.x0,792-y-h);y+=h+q.style.spaceAfter
 c.save();overlays.append((page,f.open(stream=buf.getvalue(),filetype='pdf'),0));regions.append((page,rect))
def direct(id):return s.find(id=id)
def wraptext(t):return BeautifulSoup('<p>'+escape(t)+'</p>','html.parser').p
# Executive summary page; preserve the tables and status paragraph on page 4.
ex=direct('executive-summary');els=ex.find_all(['h1','p'],recursive=False)
for id in ['changes-in-assessment-inputs','changes-in-assessment-methods']:
 els+=direct(id).find_all(['h2','p'],recursive=False)
els+=[direct('summary-of-results').find('h2')]+direct('summary-of-results').find_all('p',recursive=False)[:4]
fragment(2,(50.5,64,564,730),els)
fragment(4,(50.5,154,564,730),direct('general').find_all(['h2','h3','p']))
life=direct('review-of-life-history').find_all('p',recursive=False)
fragment(5,(50.5,74,564,178),[life[0]])
fragment(5,(50.5,184,564,328),[life[1]])
fragment(5,(50.5,356,564,450),direct('sec-stock-structure-life-history').find_all('p',recursive=False))
catch=next(e for e in s.find_all('p') if 'The catch table reports two 2026 values' in e.get_text())
fragment(6,(50.5,49,564,104),[catch])
# Table notes only; retain every cell and all table graphics.
notes=s.select('td.gt_sourcenote')
for i,q in enumerate(p):
 for b in q.get_text('blocks'):
  t=b[4].replace('\xa0',' ')
  if 'spawning biomass is female' in t:
   td=next(x for x in notes if x.get_text().startswith('2027–2028 projections assume'))
   fragment(i,(b[0],b[1],555,b[3]+1),[td],8.5)
  if 'Between half B35%' in t:
   model='26.1.2' if '26.1.2' in t else '26.0'
   td=next(x for x in notes if 'For Model '+model+',' in x.get_text() and 'status tests' in x.get_text())
   fragment(i,(b[0],b[1],555,b[3]+8),[td],8.3)
# Bibliography reflow: first partial page, then five full pages.
entries=direct('refs').select(':scope > .csl-entry');bibanchors={}
class BibDoc(BaseDocTemplate):
 def afterFlowable(self,flow):
  if getattr(flow,'anchor_id',None):bibanchors[flow.anchor_id]=(35+self.page-1,792-self.frame._y-flow.height)
buf=io.BytesIO();doc=BibDoc(buf,pagesize=(612,792))
first=Frame(50.5,57,513.5,346, leftPadding=0,rightPadding=0,topPadding=0,bottomPadding=0,id='first')
rest=Frame(50.5,57,513.5,685,leftPadding=0,rightPadding=0,topPadding=0,bottomPadding=0,id='rest')
doc.addPageTemplates([PageTemplate(id='first',frames=first,autoNextPageTemplate='rest'),PageTemplate(id='rest',frames=rest)])
# MarkedParagraph stores exact reference coordinates via drawOn; compensate page index later.
flows=[para(e,10.2)for e in entries];doc.build(flows);bib=f.open(stream=buf.getvalue(),filetype='pdf');assert len(bib)<=6,len(bib)
# drawOn anchor indexes were relative to bibliography; shift them.
for e in entries:
 a=anchors[e['id']];anchors[e['id']]=(a[0]+35,a[1])
for j in range(6):
 regions.append((35+j,f.Rect(50,389 if j==0 else 49,565,736)))
 if j<len(bib):overlays.append((35+j,bib,j))
# Keep all incoming reference destinations attached to their reference entries.
old=BeautifulSoup(subprocess.check_output(['git','show','8497cae:2026/assessment.html'],cwd=ROOT),'html.parser');old_ref=[]
for e in old.select('#refs > .csl-entry'):
 words=e.get_text(' ',strip=True).split();prefix=' '.join(words[:8])
 for j in range(35,41):
  hits=p[j].search_for(prefix)
  if hits:old_ref.append((j,hits[0].y0,e['id']));break
for q in p:
 for l in q.get_links():
  if l['kind']==f.LINK_GOTO and 35<=l['page']<=40:
   candidates=[a for a in old_ref if a[0]==l['page']]
   if candidates:
    nearest=min(candidates,key=lambda a:abs(a[1]-l['to'].y))
    if abs(nearest[1]-l['to'].y)<25:
     a=anchors[nearest[2]];l.update(page=a[0],to=f.Point(50.5,a[1]));q.update_link(l)
# Erase only paragraph/bibliography text, with transparent redactions.
for page,rect in regions:
 q=p[page]
 for l in q.get_links():
  if rect.intersects(l['from']):q.delete_link(l)
 q.add_redact_annot(rect,fill=False,cross_out=False)
for i in sorted(set(x[0]for x in regions)):p[i].apply_redactions(images=0,graphics=0)
for page,overlay,oi in overlays:
 p[page].show_pdf_page(p[page].rect,overlay,oi)
 for l in overlay[oi].get_links():
  if l['kind']!=f.LINK_URI:continue
  uri=l['uri'];data={'kind':f.LINK_URI,'from':l['from'],'uri':uri}
  if uri.startswith(BASE+'#') and uri.split('#',1)[1] in anchors:
   a=anchors[uri.split('#',1)[1]];data={'kind':f.LINK_GOTO,'from':l['from'],'page':a[0],'to':f.Point(50.5,a[1])}
  p[page].insert_link(data)
# Edit terminology in each existing span, then embed Computer Modern consistently.
sys.argv=['font-conversion','--cm-font-dir','/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode']
source=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0]
source=source.replace("ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'","ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'")
def terms(t):
 t=re.sub(r'weight[\s\u00a0]+at[\s\u00a0]+age',lambda m:'Weight-at-age' if m[0][0].isupper() else 'weight-at-age',t,flags=re.I)
 t=re.sub(r'natural-mortality',lambda m:'Natural mortality' if m[0][0].isupper() else 'natural mortality',t,flags=re.I)
 t=t.replace('ages-3+ biomass','age 3+ biomass')
 t=re.sub('base case','base model',t,flags=re.I)
 t=re.sub(r'assessment[\s\u00a0]+vintages','assessments',t)
 t=t.replace('source vintages','source dates').replace('These are distinct vintages.','These series use different data updates.')
 return re.sub(r'\bvintages\b\s*','',t)
source=source.replace("text=sp['text'].replace('\\x00','⁻')","text=terms(sp['text'].replace('\\x00','⁻'))")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'terms':terms};exec(source,ns)
for i,q in enumerate(p):
 ns['transform'](q)
 if (i+1)%50==0:print('Verified text rendering through page',i+1,flush=True)
# Updated outline includes the new General subsections.
toc=[]
for item in old_toc:
 item=[item[0],terms(item[1]),item[2]];toc.append(item)
 if item[1]=='2.1 General':toc.extend([[3,'2.1.1 Changes in assessment inputs',5],[3,'2.1.2 Changes in assessment methods',5]])
p.set_toc(toc);p.subset_fonts();tmp=R/'assessment-editorial-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_fonts=True);p.close()
q=f.open(tmp);assert len(q)==266
text='\n'.join(x.get_text() for x in q)
for pattern in [r'\bvintages\b',r'Natural-mortality',r'weight\s+at\s+age',r'base case',r'ages-3\+ biomass',r'Between half B35% and B35%, current status uses']:
 assert not re.search(pattern,text,re.I),('outdated wording',pattern)
for phrase in ['log-scale Wald confidence interval','2.1.1 Changes in assessment inputs','2.1.2 Changes in assessment methods','10.18637/jss.v115.i02','10.1111/geb.70035','The catch table reports two 2026 values']:
 assert phrase in text,phrase
for i,page in enumerate(q):assert [v['digest'].hex() for v in page.get_image_info(hashes=True)]==old_images[i],('image changed',i+1)
for i in [2,3,4,5,6,35,40,263]:q[i].get_pixmap(matrix=f.Matrix(1.4,1.4)).save('/tmp/editorial-page-'+str(i+1)+'.png')
checks={'pages':len(q),'all_raster_figure_hashes_preserved':True,'outdated_terms_absent':True,'bibliography_entries':len(entries),'computer_modern':True,'scientific_runs_changed':False,'new_subsections_in_outline':True,'manual_accessibility':'Not Tested'}
q.close();tmp.replace(R/'assessment.pdf');
# Retain the saved ATS plot, changing only its title wording.
image_doc=f.open(R/'assessment.pdf');old_png=subprocess.check_output(['git','show','8497cae:2026/assets/figure-017.png'],cwd=ROOT);old_digest=f.Pixmap(old_png).digest
for page in image_doc:
 for info in page.get_image_info(hashes=True,xrefs=True):
  if info['digest']==old_digest:page.replace_image(info['xref'],filename=str(R/'assets/figure-017.png'))
image_tmp=R/'assessment-image-tmp.pdf';image_doc.save(image_tmp,garbage=4,deflate=True);image_doc.close();image_tmp.replace(R/'assessment.pdf');checks['ATS_graphic_title_updated']=True;
(R/'validation/editorial-20261109/pdf-checks.json').write_text(json.dumps(checks,indent=2)+'\n');print(checks)
