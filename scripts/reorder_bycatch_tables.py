"""Order bycatch tables consistently and consolidate their linked Source footers."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as fitz,csv,json,re,hashlib
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';PUB='https://noaa-afsc.github.io/EBS_pollock/2026/'
ids=['tbl-prohibited-species-catch','tbl-fmp-species-in-pollock','tbl-pollock-in-other-fisheries','tbl-nontarget-species-bycatch'];mapping={6:8,7:6,8:7}
# Supply the PSC column dictionary using the existing aggregation recipe.
cols=[('Year','Calendar year','year','', 'Year'),('Bairdi','Bairdi Tanner crab catch','individuals','Bairdi Tanner Crab','PSCNQ Estimate (*)'),('Chinook','Chinook salmon catch','individuals','Chinook Salmon','PSCNQ Estimate (*)'),('Halib','Halibut catch','metric tons','Halibut','PSCNQ Estimate (*)'),('Halib.M','Halibut mortality','metric tons','Halibut','Halibut Mortality (mt)'),('Herring','Herring catch','metric tons','Herring','PSCNQ Estimate (*)'),('NonChin','Non-Chinook salmon catch','individuals','Non-Chinook Salmon','PSCNQ Estimate (*)'),('Snow Crab','Opilio Tanner (snow) crab catch','individuals','Opilio Tanner (Snow) Crab','PSCNQ Estimate (*)'),('Red King','Red king crab catch','individuals','Red King Crab','PSCNQ Estimate (*)'),('Blue King','Blue king crab catch','individuals','Blue King Crab','PSCNQ Estimate (*)'),('Golden King','Golden (brown) king crab catch','individuals','Golden (Brown) King Crab','PSCNQ Estimate (*)')]
with (R/'tables/prohibited-species-column-map.csv').open('w',newline='') as out:
 w=csv.writer(out,lineterminator='\n');w.writerow(['Column','Definition','Units','Source species group','Source field']);w.writerows(cols)
file=R/'assessment.html';text=file.read_text();s=BeautifulSoup(text,'html.parser');sections=[s.find(id=id) for id in ids];footer_html={};table_data={}
for i,sec in enumerate(sections):
 table_data[ids[i]]=sec.find('table').get_text(' ',strip=True).split('Year',1)[1]
 small=sec.find('small');assert small and small.get_text().startswith('Source:')
 download=next(p for p in sec.find_all('p',recursive=False) if p.find('a') and 'Download annual' in p.get_text())
 if i==0:
  download.append(' · ');a=s.new_tag('a',href='tables/prohibited-species-column-map.csv');a.string='Column definitions (CSV)';download.append(a)
 small.append(' ')
 for node in list(download.contents):small.append(node.extract())
 small.append('.');download.decompose();footer_html[ids[i]]=small.decode_contents()
 assert len([a for a in small.find_all('a') if a['href'].endswith('.csv')])==2
 assert sec.find('table').get_text(' ',strip=True).split('Year',1)[1]==table_data[ids[i]]
def bounds(id):
 start=text.rfind('<section',0,text.index('id="'+id+'"'));depth=0
 for m in re.finditer(r'</?section\b[^>]*>',text[start:]):
  depth+=-1 if m.group().startswith('</') else 1
  if not depth:return start,start+m.end()
bs=[bounds(id) for id in ids]
for (_,end),(start,_) in zip(bs,bs[1:]):assert not text[end:start].strip()
text=text[:bs[0][0]]+'\n'.join(str(sections[i]) for i in [1,2,0,3])+text[bs[-1][1]:]
rename=lambda m:m[1]+str(mapping[int(m[2])])
text=re.sub(r'(Table[\s\xa0]+)(6|7|8)(?![0-9A-Za-z])',rename,text);file.write_text(text)
# PDF pages50–52 contain the three complete table blocks.
p=fitz.open(R/'assessment.pdf');original_links=[(i,l.copy()) for i,page in enumerate(p) for l in page.get_links()];toc=p.get_toc();N=len(p)
old_data={};source_bounds={};data_regions={}
for i in range(49,53):
 blocks=[b for b in p[i].get_text('blocks') if 'Source:' in b[4]];assert len(blocks)==1
 source_bounds[i]=fitz.Rect(blocks[0][:4]);top=min(x.y0 for x in p[i].search_for('Year'))-1;data_regions[i]=fitz.Rect(0,top,612,blocks[0][1]-2);old_data[i]=p[i].get_text(clip=data_regions[i])
# Renumber labels and references using character bounds, preserving all numeric cells.
for page in p:
 edits=[]
 for block in page.get_text('rawdict')['blocks']:
  for line in block.get('lines',[]):
   chars=[];styles=[]
   for sp in line['spans']:
    for ch in sp.get('chars',[]):chars.append(ch);styles.append(sp)
   txt=''.join(ch['c'] for ch in chars)
   for m in re.finditer(r'Table\s+(6|7|8)(?![0-9A-Za-z])',txt):
    j=m.start(1);ch=chars[j];sp=styles[j];edits.append((fitz.Rect(ch['bbox']),ch['origin'],str(mapping[int(m[1])]),sp['size'],sp['font']))
 for rect,*_ in edits:page.add_redact_annot(rect,fill=(1,1,1))
 if edits:
  page.apply_redactions(images=0,graphics=0)
  for rect,origin,t,size,font in edits:page.insert_text(origin,t,fontsize=size,fontname='helv' if any(x in font.lower() for x in ['arial','helvetica']) else 'tiro')
# Render small Source paragraphs, including links, in their established locations.
for i,id in zip(range(49,53),ids):
 page=p[i];b=source_bounds[i];page.add_redact_annot(b,fill=(1,1,1));page.apply_redactions(images=0,graphics=0)
 foot=BeautifulSoup(footer_html[id],'html.parser')
 for a in foot.find_all('a'):a['href']=PUB+a['href']
 fit=page.insert_htmlbox(fitz.Rect(56,b.y0,556,730),'<p>'+str(foot)+'</p>',css='body {font-family:serif;font-size:8pt;line-height:1.15} p {margin:0} a {color:#0645ad;text-decoration:underline}')
 assert fit[0]>=0,fit
 # Ensure URI annotations exist even if the local rendering library omits them.
 for a in foot.find_all('a'):
  label=a.get_text(' ',strip=True);rects=page.search_for(label,clip=fitz.Rect(56,b.y0,556,730),flags=fitz.TEXT_DEHYPHENATE|fitz.TEXT_PRESERVE_WHITESPACE|fitz.TEXT_MEDIABOX_CLIP);assert rects or any(l.get('uri')==a['href'] for l in page.get_links()),(i,label)
  for rect in rects:
   if not any(l.get('uri')==a['href'] and fitz.Rect(l['from']).intersects(rect) for l in page.get_links()):page.insert_link({'kind':fitz.LINK_URI,'from':rect,'uri':a['href']})
 after_data=page.get_text(clip=data_regions[i])
 if after_data!=old_data[i]:
  import difflib
  print('DATA_DIFFERENCE',i,list(difflib.unified_diff(old_data[i].splitlines(),after_data.splitlines()))[:25])
 assert after_data==old_data[i]
# Move oldPSC Table6 after oldTable8 and remap every existing internal link.
order=list(range(N));order[49:52]=[50,51,49];mp={old:new for new,old in enumerate(order)};p.select(order)
for page in p:
 for link in page.get_links():
  if link['kind']==fitz.LINK_GOTO:page.delete_link(link)
for src,link in original_links:
 if link['kind']!=fitz.LINK_GOTO or link.get('page',-1)<0:continue
 p[mp[src]].insert_link({'kind':fitz.LINK_GOTO,'from':link['from'],'page':mp[link['page']],'to':link.get('to',fitz.Point(0,0))})
p.set_toc([[level,re.sub(r'(Table\s+)(6|7|8)(?!\d)',rename,title),mp[page-1]+1] for level,title,page in toc])
for i in range(49,52):
 page=p[i];footer=[b for b in page.get_text('blocks') if b[1]>745 and 250<b[0]<350 and b[4].strip().isdigit()];assert len(footer)==1
 page.add_redact_annot(fitz.Rect(footer[0][:4]),fill=(1,1,1));page.apply_redactions(images=0,graphics=0)
 label=str(i+1);page.insert_text((306-fitz.get_text_length(label,fontname='tiro',fontsize=10)/2,762),label,fontname='tiro',fontsize=10)
tmp=R/'assessment-bycatch-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_images=True,deflate_fonts=True);p.close();tmp.replace(R/'assessment.pdf')
p=fitz.open(R/'assessment.pdf');assert len(p)==N
for old in range(49,53):assert p[mp[old]].get_text(clip=data_regions[old])==old_data[old]
# Check the four HTML tables and their PDF pages agree on numbering and sources.
s=BeautifulSoup(file.read_text(),'html.parser');actual=[n['id'] for n in s.find(id='tables').find_all('section',recursive=False) if n.get('id') in ids];assert actual==[ids[1],ids[2],ids[0],ids[3]]
for n,i in enumerate([50,51,49,52],6):assert re.search(r'Table\s+'+str(n)+':',p[mp[i]].get_text(sort=True))
assert len(s.find(id='tbl-management-measures').find_all('a'))==39
for a in s.find_all('a',href=True):
 m=re.fullmatch(r'Table\s+(6|7|8)',a.get_text(' ',strip=True))
 if m and a['href'].startswith('#'):
  target=s.find(id=a['href'][1:]);cap=target.find(['caption','figcaption']);assert re.match(r'Table\s+'+m[1]+':',cap.get_text(' ',strip=True)),(m[1],a['href'])
v=R/'validation/bycatch-order-20261109';v.mkdir(exist_ok=True)
for i in range(49,53):p[i].get_pixmap(matrix=fitz.Matrix(1.3,1.3)).save(v/f'pdf-page-{i+1}.png')
checks={'old_table_order':[7,8,6,9],'new_numbers':[6,7,8,9],'numeric_cells_unchanged':True,'download_links_in_small_source_footers':8,'PDF_pages':N,'Table2_links_preserved':39,'old_to_new_number':mapping}
(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-table-links-notation-order';m['bycatch_table_order_and_sources']=checks;m['files']={n:hashlib.sha256((R/n).read_bytes()).hexdigest() for n in ['assessment.html','assessment.pdf']};(R/'release.json').write_text(json.dumps(m,indent=2)+'\n')
print(json.dumps(checks))
