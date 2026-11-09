from pathlib import Path
import re,json,html
import pymupdf as f
root=Path(__file__).resolve().parents[1];r=root/'2026';out=r/'tables/weight-age-revision-20261109'
old=f.open('/tmp/fishery-restructure-before/assessment.pdf');flow=f.open(out/'restructured-text-pages.pdf');cap=json.loads((out/'captions.json').read_text());N=len(flow)
assert 9 <= N <= 11
# Map every retained original page; pages 10–19 are replaced by the reflowed text.
seq=list(range(10))+[('text',i) for i in range(N)]+list(range(20,109))+['heatmap']+list(range(109,len(old)))
page_map={x:i for i,x in enumerate(seq) if isinstance(x,int)};new=f.open()
for x in seq:
 if isinstance(x,int):new.insert_pdf(old,from_page=x,to_page=x,links=False)
 elif isinstance(x,tuple):new.insert_pdf(flow,from_page=x[1],to_page=x[1],links=False)
 else:new.new_page(width=612,height=792)
assert len(new)==len(old)+N-9
textpages=range(10,10+N);heatidx=seq.index('heatmap');page=new[heatidx]
fig=f.open(r/'tables/waa-omit-bts2026.pdf');page.show_pdf_page(f.Rect(36,70,576,376),fig,0)
captionhtml=f'<p style="padding-left:56pt;text-indent:-56pt"><b>Figure 23:</b> {html.escape(cap["new23"])}</p>'
page.insert_htmlbox(f.Rect(36,390,576,565),captionhtml,css='body {font-family:sans-serif;font-size:10pt;line-height:1.25}');page.insert_text((36,584),'Figure data: tables/waa-omit-bts2026-figure.csv',fontname='helv',fontsize=9)
# Replace old Figure33 graphic and its caption; it is numbered34 in the final document.
bioidx=page_map[120];bio=new[bioidx];imgs=bio.get_images();assert len(imgs)==1
bio.replace_image(imgs[0][0],filename=str(r/'assets/figure-034.png'))
blocks=bio.get_text('blocks');captionblocks=[b for b in blocks if 'biological and selectivity schedules' in b[4].replace('\xa0',' ')];assert captionblocks
bounds=f.Rect(captionblocks[0][:4]);bio.add_redact_annot(bounds,fill=(1,1,1));bio.apply_redactions(images=0,graphics=0)
bio.insert_htmlbox(f.Rect(bounds.x0,bounds.y0,576,730),'<p style="padding-left:50pt;text-indent:-50pt"><b>Figure 34:</b> '+html.escape(cap['new34'])+'</p>',css='body {font-family:serif;font-size:9pt;line-height:1.15}')
# Change existing figure numbers at the character level, retaining nearby prose.
# Reflowed text already carries the new numbers. Figure34 caption above is also final.
number_edits=0
for i,page in enumerate(new):
 if i in textpages or i==heatidx:continue
 edits=[]
 for b in page.get_text('rawdict')['blocks']:
  for line in b.get('lines',[]):
   chars=[];styles=[]
   for span in line['spans']:
    for ch in span.get('chars',[]):chars.append(ch);styles.append(span)
   text=''.join(c['c'] for c in chars)
   for m in re.finditer(r'Figure[\s\xa0]+(\d+)(?![\dA-Za-z])',text):
    n=int(m.group(1))
    if n<23 or (i==bioidx and n==34):continue
    a,b=m.span(1);rect=f.Rect(chars[a]['bbox'])
    for c in chars[a+1:b]:rect|=f.Rect(c['bbox'])
    span=styles[a];edits.append((rect,chars[a]['origin'],str(n+1),span['size'],span['font']))
 for rect,*_ in edits:page.add_redact_annot(rect,fill=(1,1,1))
 if edits:
  page.apply_redactions(images=0,graphics=0)
  for rect,origin,text,size,font in edits:
   page.insert_text(origin,text,fontsize=size,fontname='helv' if any(x in font.lower() for x in ['arial','helvetica']) else 'tiro')
  number_edits+=len(edits)
# Restore original internal/external links on preserved pages with remapped destinations.
headings=json.loads((out/'text-page-anchors.json').read_text())['headings']
heading_page={x['text']:10+x['page']-1 for x in headings}
def old_target(target,point=None):
 if target in page_map:return page_map[target]
 if target==18:return heading_page['4.1.3 Weight-at-age']
 # Find the closest old text block to the old destination, then match that text.
 if point is not None:
  blocks=old[target].get_text('blocks');candidates=sorted(blocks,key=lambda b:abs(b[1]-point.y))
  for b in candidates[:3]:
   phrase=' '.join(b[4].replace('\xa0',' ').replace('\xad','').split())[:90]
   for k in textpages:
    norm=' '.join(new[k].get_text().replace('\xa0',' ').replace('\xad','').split())
    if phrase and phrase in norm:return k
 return 10+min(N-1,max(0,target-10))
restored=0
for src,dst in page_map.items():
 for l in old[src].get_links():
  if l['kind']==f.LINK_GOTO and l.get('page',-1)>=0:
   target=old_target(l['page'],l.get('to'));point=l.get('to',f.Point(36,36)) if l['page'] in page_map else f.Point(36,36)
   new[dst].insert_link({'kind':f.LINK_GOTO,'from':l['from'],'page':target,'to':point});restored+=1
  elif l['kind']==f.LINK_URI:new[dst].insert_link({'kind':f.LINK_URI,'from':l['from'],'uri':l['uri']})
# Add references from newly typeset text pages to the matching tables and figures.
def norm(t):return ' '.join(t.replace('\xa0',' ').replace('\xad','').replace('–','-').replace('−','-').split())
texts=[norm(p.get_text()) for p in new]
figure_targets={};table_targets={}
for i,t in enumerate(texts):
 for typ,store in [('Figure',figure_targets),('Table',table_targets)]:
  for m in re.finditer(r'\b'+typ+r'\s+(\d+|[A-J]\d+)\s*:',t):store.setdefault(m.group(1),i)
for i in textpages:
 p=new[i]
 for typ,store in [('Figure',figure_targets),('Table',table_targets)]:
  for m in re.finditer(r'\b'+typ+r'\s+(\d+|[A-J]\d+)\b',p.get_text()):
   target=store.get(m.group(1))
   if target is not None:
    for rect in p.search_for(m.group()):p.insert_link({'kind':f.LINK_GOTO,'from':rect,'page':target,'to':f.Point(36,36)})
# Repair Contents bookmarks to actual heading pages; introduce the new4.1 subsections.
toc=[]
for level,title,oldno in old.get_toc():
 if title.startswith('4.3.2 '):continue
 if title.startswith('4.1 '):
  for lev,text in [(2,'4.1 Fishery data'),(3,'4.1.1 Catch'),(3,'4.1.2 Age compositions'),(3,'4.1.3 Weight-at-age')]:toc.append([lev,text,heading_page[text]+1])
  continue
 if title in heading_page:dest=heading_page[title]
 else:
  needle=norm(title);matches=[i for i,t in enumerate(texts) if re.search(r'(?<!\w)'+re.escape(needle)+r'(?!\w)',t)]
  dest=min(matches,key=lambda x:abs(x-old_target(oldno-1))) if matches else old_target(oldno-1)
 toc.append([level,title,dest+1])
new.set_toc(toc)
# Every page now uses its physical page number; erase only centered footer labels.
for i,page in enumerate(new):
 footer=[]
 for b in page.get_text('blocks'):
  if b[1]>page.rect.height-48 and .4*page.rect.width<b[0]<.6*page.rect.width and re.fullmatch(r'\s*\d+a?\s*',b[4]):footer.append(f.Rect(b[:4]))
 for rect in footer:page.add_redact_annot(rect,fill=(1,1,1))
 if footer:page.apply_redactions(images=0,graphics=0)
 label=str(i+1);size=10;page.insert_text((page.rect.width/2-f.get_text_length(label,fontname='tiro',fontsize=size)/2,page.rect.height-30),label,fontname='tiro',fontsize=size)
path=r/'assessment-structure-tmp.pdf';new.save(path,garbage=3,deflate=True);new.close();path.replace(r/'assessment.pdf')
p=f.open(r/'assessment.pdf');p[heatidx].get_pixmap(matrix=f.Matrix(1.4,1.4)).save('/tmp/waa-moved-figure.png');p[bioidx].get_pixmap(matrix=f.Matrix(1.4,1.4)).save('/tmp/biological-current-figure.png')
assert '4.1.3 Weight-at-age' in p[heading_page['4.1.3 Weight-at-age']].get_text()
assert '4.3.2 Annual fishery' not in ''.join(x.get_text() for x in p)
(out/'PDF-restructure-checks.json').write_text(json.dumps({'pages':len(p),'rebuilt_text_pages':N,'restored_preserved_page_internal_links':restored,'renumbered_figure_reference_labels':number_edits,'new_figure23_page':heatidx+1,'biological_figure34_page':bioidx+1,'manual_accessibility':'Not Tested'},indent=2))
print('PDF reorganized:',len(p),'pages; figure23 at',heatidx+1,'; renumbered labels',number_edits)
