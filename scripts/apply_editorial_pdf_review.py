"""Reflow reviewed SAFE prose and captions; preserve current plots, tables and navigation."""
from pathlib import Path
from lxml import html
import re,json,unicodedata,subprocess,statistics,hashlib,io,copy
from urllib.parse import urljoin
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/editorial-review-20261110';base='448ae3f';BASE='https://noaa-afsc.github.io/EBS_pollock/2026/assessment.html'
p=f.open(stream=subprocess.check_output(['git','show',base+':2026/assessment.pdf'],cwd=ROOT),filetype='pdf');oldtoc=p.get_toc(False);original_images=[[x['digest'].hex()for x in q.get_image_info(hashes=True)]for q in p];oldexternal={x['uri']for q in p for x in q.get_links()if x['kind']==f.LINK_URI}
dom=html.parse(str(r/'assessment.html'),parser=html.HTMLParser(huge_tree=True));caps=json.loads((out/'main-caption-edits.json').read_text());ops=json.loads((out/'prose-edits.json').read_text());fontdir=Path('/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode');frames=[];linklog=[]
def norm(t):return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKC',t).casefold())
def flat(t):return ' '.join(unicodedata.normalize('NFKC',t).replace('\xad','-').split())
def page_lines(q):
 groups=[]
 for block in q.get_text('dict')['blocks']:
  for line in block.get('lines',[]):
   if tuple(line['dir'])!=(1.,0.):continue
   for sp in line['spans']:
    if not sp['text'].strip():continue
    y=sp['origin'][1];g=next((g for g in groups if abs(g[0]-y)<1.5),None)
    if g is None:g=[y,[]];groups.append(g)
    g[1].append(sp)
 return [(min(s['bbox'][1]for s in ss),' '.join(s['text']for s in sorted(ss,key=lambda s:s['origin'][0])))for y,ss in sorted(groups)]
caption_map={}
for i,q in enumerate(p):
 for y,t in page_lines(q):
  m=re.match(r'^Figure\s+(\d+)\s*:',t)
  if m:caption_map.setdefault(m[1],[]).append((i,y))
oldheadings={}
for i in [7,8]:
 for y,t in page_lines(p[i]):
  if re.match(r'3\.1\.[123]\s',t):oldheadings[t]=(i,y)
def css(size):
 return '@font-face{font-family:CMU;src:url(cmunrm.otf)}@font-face{font-family:CMU;src:url(cmunbx.otf);font-weight:bold}@font-face{font-family:CMU;src:url(cmunti.otf);font-style:italic}body{font-family:CMU;font-size:'+str(size)+'pt;line-height:1.2;margin:0;color:#111}p{margin:0 0 5pt 0}h3{font-family:CMU;font-size:10.5pt;font-weight:bold;margin:7pt 0 6pt 0}a{color:#111;text-decoration:none}table{border-collapse:collapse;width:100%}td{padding:0;vertical-align:top}td.label{width:44pt;padding-right:3pt}'
def prepare_markup(markup):
 e=html.fragment_fromstring('<div>'+markup+'</div>')
 for a in e.xpath('.//a'):
  h=a.get('href','')
  if h.startswith('#'):a.set('href',BASE+h)
 for a in e.xpath('.//a[contains(@class,"anchorjs-link")]'):a.getparent().remove(a)
 return ''.join(html.tostring(x,encoding='unicode',with_tail=True)for x in e)
def frame(i,rect,markup,size=10.2,preserve=False,reason=''):
 rect=f.Rect(rect);q=p[i];old=[]
 for l in q.get_links():
  if rect.intersects(l['from']):
   label=flat(q.get_textbox(l['from']));old.append((dict(l),label));q.delete_link(l)
 assert not any(rect.intersects(f.Rect(x['bbox']))for x in q.get_image_info()),('figure overlap',i+1,rect)
 q.add_redact_annot(rect,fill=False,cross_out=False);q.apply_redactions(images=0,graphics=0)
 spare,scale=q.insert_htmlbox(rect,prepare_markup(markup),css=css(size),archive=f.Archive(str(fontdir)),scale_low=1)
 assert spare>=0 and scale==1,('frame overflow',i+1,reason,spare)
 q=p[i]
 if preserve:
  for l,label in old:
   hits=[h for h in q.search_for(label)if rect.intersects(h)]if label else[]
   if not hits:
    labels=[label]+re.findall(r'\([^()]{3,120}\)',label)
    if l.get('uri'):labels+=[flat(e.text_content())for e in dom.xpath('//a[@href]')if urljoin(BASE,e.get('href'))==l['uri']]
    words=[w for w in q.get_text('words',sort=True)if rect.intersects(f.Rect(w[:4]))]
    for candidate in sorted(set(labels),key=len,reverse=True):
     hits=[h for h in q.search_for(candidate)if rect.intersects(h)]
     if not hits:
      target=norm(candidate)
      for start in range(len(words)):
       joined='';bounds=f.Rect(words[start][:4])
       for word in words[start:start+20]:
        joined+=norm(word[4]);bounds|=f.Rect(word[:4])
        if joined==target:hits.append(f.Rect(bounds));break
        if len(joined)>len(target):break
     if hits:break
   assert hits,('link relocation',i+1,label,reason,l.get('uri'))
   h=min(hits,key=lambda h:abs(h.y0-l['from'].y0));data={k:v for k,v in l.items()if k in ['kind','page','to','uri','file','zoom','name']};data['from']=h;q.insert_link(data)
 frames.append({'page':i+1,'reason':reason,'font_size':size,'rect':list(rect),'spare':spare})
def inner(e):return html.tostring(e,encoding='unicode',with_tail=False)
# Management narrative stays in its original page allocation.
frame(6,(50.49,109.5,565,616.5),''.join('<p>'+t+'</p>'for t in ops['management_paragraphs']),reason='management context')
sec=dom.xpath('//*[@id="sec-fishery-conditions"]')[0];pars=sec.xpath('./p');subs=sec.xpath('./section');assert len(subs)>=3
content=''.join(inner(e)for e in pars[1:])+''.join(inner(e)for e in subs[0]if e.tag in ['h3','p'])
frame(7,(50.49,49.5,565,618),content,reason='catch and spatial narrative')
content=''.join(inner(e)for sub in subs[1:3]for e in sub if e.tag in ['h3','p'])
frame(8,(50.49,49.5,565,445),content,reason='fleet dispersion and fish size')
# Captions are located by their actual figure labels, independent of asset numbering.
for n,text in caps.items():
 assert len(caption_map.get(n,[]))==1,(n,caption_map.get(n))
 i,y=caption_map[n][0];q=p[i];top=max([y-.5]+[x['bbox'][3]+.1 for x in q.get_image_info()if x['bbox'][3]<=y+2]);rect=(50.49,top,565,740)
 frame(i,rect,'<table><tr><td class="label">Figure '+n+':</td><td>'+__import__('html').escape(text)+'</td></tr></table>',size=8.5,preserve=True,reason='Figure '+n+' caption')
# Smaller prose and table-note edits outside the already reflowed pages.
def edit_text(t):
 t=flat(t);new=t
 for a,b in ops['text_replacements']:
  new=new.replace(flat(a),b)
 new=new.replace('2025, because sample collections','2025 because sample collections')
 return re.sub(r'\s+([.,;])',r'\1',re.sub(' {2,}',' ',new)).strip()
skip={6,7,8}|{caption_map[n][0][0]for n in caps}
for i in range(len(p)):
 if i in skip:continue
 q=p[i];blocks=list(q.get_text('blocks'))
 for b in blocks:
  old=flat(b[4]);new=edit_text(b[4])
  if old==new:continue
  # Apply only actual lexical edits, rather than incidental spacing changes.
  if not any(flat(a)in old for a,_ in ops['text_replacements']):continue
  size_values=[sp['size']for bl in q.get_text('dict')['blocks']for ln in bl.get('lines',[])for sp in ln['spans']if f.Rect(b[:4]).intersects(f.Rect(sp['bbox']))and sp['text'].strip()]
  size=round(statistics.median(size_values),1)if size_values else 10.2
  next_tops=[v[1] for v in blocks if v[1]>b[3]+1 and v[1]<740]+[v['bbox'][1] for v in q.get_image_info()if v['bbox'][1]>b[3]+1]
  bottom=min([b[3]+40,740]+[v-2 for v in next_tops]);right=max(b[2]+.8,q.rect.width-50.5)
  frame(i,(b[0]-.3,b[1]-.4,right,bottom),'<p>'+__import__('html').escape(new)+'</p>',size=size,preserve=True,reason='prose/note cleanup');q=p[i]
# Revised graphic assets are matched to their original PDF images by pixel identity.
from PIL import Image
changed_assets=[1,2,4,7,8,9,10,11,23,24,25,26,48,54,55,64,79,80,81];image_index={}
for i,q in enumerate(p):
 for info in q.get_image_info(hashes=True,xrefs=True):image_index.setdefault(info['digest'].hex(),[]).append((i,info['xref']))
image_records=[]
for n in changed_assets:
 data=subprocess.check_output(['git','show',base+f':2026/assets/figure-{n:03}.png'],cwd=ROOT);im=Image.open(io.BytesIO(data)).convert('RGB');hits=image_index.get(hashlib.md5(im.tobytes()).hexdigest(),[]);assert len(hits)==1,(n,hits)
 i,xref=hits[0];p[i].replace_image(xref,filename=str(r/f'assets/figure-{n:03}.png'));image_records.append({'asset':n,'page':i+1})
# Resolve new same-report links to native PDF destinations.
line_index=[];page_index=[]
for i,q in enumerate(p):
 lines=page_lines(q);joined='';positions=[]
 for y,t in lines:
  z=norm(t);joined+=z;positions.extend([y]*len(z));line_index.append((i,y,t,z))
 page_index.append((joined,positions))
def locate(text):
 t=norm(text)
 for i,y,raw,z in line_index:
  if z==t:return i,max(0,y-4)
 for i,(joined,pos)in enumerate(page_index):
  j=joined.find(t)
  if j>=0:return i,max(0,pos[j]-4)
 return None
anchors={}
for i,q in enumerate(p):
 for l in list(q.get_links()):
  uri=l.get('uri','')
  if not uri.startswith(BASE+'#'):continue
  ident=uri.split('#',1)[1]
  if ident not in anchors:
   els=dom.xpath('//*[@id="'+ident+'"]');assert len(els)==1,ident;e=els[0];heading=e.xpath('./h1|./h2|./h3|./h4');c=e.xpath('.//figcaption|.//caption')
   text=(heading[0]if heading else c[0]if c else e).text_content();hit=None
   for length in [140,110,90,70,55]:
    target=norm(text)[:length]
    if len(target)>=25:hit=locate(target)
    if hit:break
   assert hit,('unresolved anchor',ident,text[:100]);anchors[ident]=hit
  page,y=anchors[ident];q.update_link({'xref':l['xref'],'kind':f.LINK_GOTO,'from':l['from'],'page':page,'to':f.Point(50.5,y),'zoom':0});linklog.append({'source_page':i+1,'anchor':ident,'destination_page':page+1})
# Update destinations for the subsection headings moved during reflow.
shifts=[]
for name,(i,oldy)in oldheadings.items():
 hit=locate(name);assert hit and hit[0]==i,(name,hit);newy=hit[1]+4;shifts.append((i,oldy,newy))
for q in p:
 for l in q.get_links():
  if l['kind']!=f.LINK_GOTO:continue
  for i,oldy,newy in shifts:
   if l['page']==i and abs(l['to'].y-oldy)<10:l['to']=f.Point(l['to'].x,newy);q.update_link(l);break
for item in oldtoc:
 d=item[3]
 if 'to'not in d:continue
 for i,oldy,newy in shifts:
  if item[2]==i+1 and abs(d['to'].y-oldy)<10:d['to']=f.Point(d['to'].x,newy);break
p.set_toc(oldtoc)
tmp=out/'reviewed-assessment.pdf';p.save(tmp,garbage=4,deflate=True);p.close();q=f.open(tmp)
assert len(q)==280
alltext=' '.join(flat(x.get_text(sort=True))for x in q)
for phrase in ['Council recommendations, voluntary programs and regulatory implementation retain their separate dates','Salmon accounting and genetic stock identification serve different purposes','one year before the requested 2026 catch and survey updates','These seasonal groupings','The annual and seasonal extracts have different coverage']:
 assert phrase not in alltext,phrase
for n,text in caps.items():
 i=caption_map[n][0][0];assert norm(text)in norm(q[i].get_text(sort=True)),('caption text',n)
newexternal={l['uri']for page in q for l in page.get_links()if l['kind']==f.LINK_URI};assert oldexternal<=newexternal,('external links lost',oldexternal-newexternal)
for i in [6,7,8,9,101,102,104,124,131,143,164]:q[i].get_pixmap(matrix=f.Matrix(1,1)).save(str(out/f'page-{i+1}.png'))
(out/'PDF-editorial-checks.json').write_text(json.dumps({'pages':280,'frames':frames,'graphic_replacements':image_records,'native_links_created':linklog,'all_original_external_link_targets_preserved':True,'font':'Computer Modern Unicode Serif','manual_accessibility':'Not Tested'},indent=2)+'\n')
q.close();tmp.replace(r/'assessment.pdf');print('PASS: matching PDF prose, captions and graphic labels; 280 pages, Computer Modern, external links preserved')
