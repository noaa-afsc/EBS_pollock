"""Remove the requested ATS allocation and complete likelihood tables; renumber main tables."""
from pathlib import Path
from bs4 import BeautifulSoup,Comment
import pymupdf as fitz,re,sys,json,hashlib
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';path=R/'assessment.html';s=BeautifulSoup(path.read_text(),'html.parser')
s.find(id='fig-model262-recruitment-deviations').decompose();s.find(id='tbl-ats-biomass-allocation').decompose();s.find(id='tbl-model262-current-likelihood-complete').decompose()
for a in list(s.select('a[href="#tbl-ats-biomass-allocation"], a[href="#tbl-model262-current-likelihood-complete"]')):
 item=a.find_parent('li') or a.find_parent('p') or a;item.decompose()
projection_figure=s.find(id='fig-projection-vintages').extract();s.find(id='fig-recruitment-2024-current').insert_after(projection_figure)
figure_mapping={int(m[1]):i+1 for i,e in enumerate([e for e in s.select('.safe-caption-label')if re.fullmatch(r'Figure\s+\d+:',e.get_text(' ',strip=True))]) if (m:=re.fullmatch(r'Figure\s+(\d+):',e.get_text(' ',strip=True)))}
for a in list(s.select('a[href="#fig-model262-recruitment-deviations"]')):(a.find_parent('li') or a.find_parent('p') or a).decompose()
def renumber_figure(text):return re.sub(r'(Figure[\s\xa0]+)(\d+)(?!\d)',lambda m:m[1]+str(figure_mapping.get(int(m[2]),int(m[2]))),text)
for a in s.select('a[href^="#fig-"]'):
 for text in list(a.find_all(string=True)):text.replace_with(renumber_figure(str(text)))
for e in s.select('.safe-caption-label'):
 for text in list(e.find_all(string=True)):text.replace_with(renumber_figure(str(text)))
for a in s.select('a.lightbox[title]'):
 a['title']=re.sub(r'^(Figure[\s\xa0]+)(\d+)(?!\d)',lambda m:m[1]+str(figure_mapping.get(int(m[2]),int(m[2]))),a['title'])
# All local table anchors, captions and list entries follow the table series.
def renumber(text):
 return re.sub(r'(Table[\s\xa0]+)(2[4-9]|[3-5][0-9])(?!\d)',lambda m:m[1]+str(int(m[2])-1-(int(m[2])>41)),text)
for a in s.select('a[href^="#"]'):
 if a['href'].startswith('#tbl-'):
  for text in list(a.find_all(string=True)):text.replace_with(renumber(str(text)))
for e in s.select('.safe-caption-label'):
 for text in list(e.find_all(string=True)):text.replace_with(renumber(str(text)))
# One caption contains a plain local cross-reference; historical SAFE Table 27 stays fixed.
for text in list(s.find_all(string=True)):
 if text.parent.name not in ['style','script'] and 'AVO indices and sampling relative errors for 2009–2026 match Table 26' in text:text.replace_with(str(text).replace('match Table 26','match Table 25'))
path.write_text('\n'.join(line.rstrip()for line in str(s).splitlines())+'\n')
p=fitz.open(R/'assessment.pdf');old_toc=p.get_toc();images=[[x['digest'].hex()for x in page.get_image_info(hashes=True)]for page in p]
# Table 23 occupies the lower part of the shared Table 21–23 page.
page=p[65];area=fitz.Rect(45,388.6,570,521)
for l in page.get_links():
 if area.intersects(l['from']):page.delete_link(l)
page.add_redact_annot(area,fill=(1,1,1),cross_out=False);page.apply_redactions(images=0,graphics=2)
# Preserve and move the shared-page residual table after removing the complete likelihood table.
residual_source=fitz.open();residual_source.insert_pdf(p,from_page=80,to_page=80)
page=p[80];erase=fitz.Rect(45,45,570,505)
for l in page.get_links():
 if erase.intersects(l['from']):page.delete_link(l)
page.add_redact_annot(erase,fill=(1,1,1),cross_out=False);page.apply_redactions(images=0,graphics=2)
page.show_pdf_page(fitz.Rect(45,50,570,205),residual_source,0,clip=fitz.Rect(45,350,570,505),keep_proportion=False)
for l in residual_source[0].get_links():
 if l['from'].y0>=350:
  data={k:v for k,v in l.items()if k in ['kind','from','page','to','uri','file','zoom','name']};data['from']=data['from']+(0,-300,0,-300);page.insert_link(data)
pdf_bytes=p.tobytes(garbage=4,deflate=True);p.close();p=fitz.open(stream=pdf_bytes,filetype='pdf')
for page in p:
 for l in page.get_links():
  if l['kind']==fitz.LINK_GOTO and l['page']==80 and l['to'].y>=345:l['to']=fitz.Point(l['to'].x,l['to'].y-300);page.update_link(l)
# Change only the characters in local two-digit table labels/references.
# Keep citations to the published 2024 SAFE Table 27 and Tables 26/32 unchanged.
def renumber_data(data):
 count=0;groups=[]
 for block in data['blocks']:
  context=' '.join(''.join(sp['text']for sp in line['spans'])for line in block.get('lines',[])).replace('\xa0',' ')
  for line in block.get('lines',[]):
   if tuple(line['dir'])!=(1.,0.):continue
   for sp in line['spans']:
    y=sp['origin'][1];group=next((g for g in groups if abs(g['y']-y)<1.5),None)
    if group is None:group={'y':y,'items':[]};groups.append(group)
    group['items'].append((sp,context))
 for group in groups:
  items=sorted(group['items'],key=lambda a:a[0]['origin'][0]);chars=[];locations=[]
  for i,(sp,_) in enumerate(items):
   if chars:chars.append(' ');locations.append(None)
   for j,ch in enumerate(sp['text']):chars.append(ch);locations.append((i,j))
  text=''.join(chars);context=' '.join(c for _,c in items);replacement={}
  for m in re.finditer(r'(Table|Figure)\s+([1-9][0-9]?)(?!\d)',text):
   kind=m[1];n=int(m[2])
   if kind=='Table' and not 24<=n<=57:continue
   if kind=='Figure' and figure_mapping.get(n,n)==n:continue
   if kind=='Figure' and (re.match(r'\s+(?:of|in)\s+(?:the\s+)?2024\s+SAFE',text[m.end():]) or re.search(r'2024\s+SAFE\s*$',text[:m.start()])):continue
   if kind=='Table' and n==27 and re.search(r'(?:official\s+(?:2024\s+)?SAFE|2024\s+SAFE)\s+Table\s+27',text+' '+context):continue
   for k,ch in enumerate(str(figure_mapping.get(n,n) if kind=='Figure' else n-1-(n>41)),m.start(2)):
    if locations[k] is not None:replacement[locations[k]]=ch
   count+=1
  for i,(sp,_) in enumerate(items):sp['text']=''.join(replacement.get((i,j),ch)for j,ch in enumerate(sp['text']))
 return count
sys.argv=['fonts','--cm-font-dir','/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode']
source=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0]
source=source.replace("for block in page.get_text('dict')['blocks']:","data=page.get_text('dict');renumber_data(data)\n for block in data['blocks']:")
source=source.replace("'assessmentCM'+k","'cleanupDisplays20261109'+k")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'renumber_data':renumber_data};exec(source,ns)
changed=[];count=0
for page in p:
 n=renumber_data(page.get_text('dict'))
 if n:ns['transform'](page);changed.append(page.number);count+=n
pdf_bytes=p.tobytes(garbage=4,deflate=True);p.close();p=fitz.open(stream=pdf_bytes,filetype='pdf')
# Remove the dedicated recruitment-deviations page and map every internal link.
removed_page=128;links=[(i,dict(l))for i,page in enumerate(p)for l in page.get_links()];order=[i for i in range(len(p))if i!=removed_page];order.remove(154);order.insert(order.index(118)+1,154);page_map={old:new for new,old in enumerate(order)}
p.select(order)
pdf_bytes=p.tobytes(garbage=4,deflate=True);p.close();p=fitz.open(stream=pdf_bytes,filetype='pdf')
for page in p:
 for l in page.get_links():
  if l['kind']==fitz.LINK_GOTO:page.delete_link(l)
for old_src,l in links:
 if l['kind']==fitz.LINK_GOTO and old_src in page_map and l.get('page')in page_map:
  p[page_map[old_src]].insert_link({'kind':fitz.LINK_GOTO,'from':l['from'],'page':page_map[l['page']],'to':l.get('to',fitz.Point(0,0)),'zoom':l.get('zoom',0)})
pdf_bytes=p.tobytes(garbage=4,deflate=True);p.close();p=fitz.open(stream=pdf_bytes,filetype='pdf')
# Update printed page numbers while retaining Computer Modern and all body text.
footer_source=source.replace("text=sp['text'].replace('\\x00','⁻')", "text=sp['text'].replace('\\x00','⁻')\n    if sp['bbox'][1]>page.rect.height-47 and abs(sp['bbox'][0]-page.rect.width/2)<36 and text.strip().isdigit():text=str(page.number+1)")
footer_source=footer_source.replace('data=page.get_text(\'dict\');renumber_data(data)','data=page.get_text(\'dict\')')
footer_source=footer_source.replace("'cleanupDisplays20261109'+k","'cleanupFooters20261109'+k")
foot_ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py')};exec(footer_source,foot_ns)
for page in list(p)[119:]:foot_ns['transform'](page)
p.set_toc([[level,renumber_figure(renumber(title)),page_map[page-1]+1]for level,title,page in old_toc if page-1 in page_map]);p.subset_fonts();tmp=R/'assessment-table-removed-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_fonts=True);p.close()
q=fitz.open(tmp);assert len(q)==265
for i,page in enumerate(q):assert [x['digest'].hex()for x in page.get_image_info(hashes=True)]==images[order[i]]
for i in [14,65,66,70,227]:q[i].get_pixmap(matrix=fitz.Matrix(1.3,1.3)).save('/tmp/table-removed-page-'+str(i+1)+'.png')
q.close();tmp.replace(R/'assessment.pdf')
m=json.loads((R/'release.json').read_text());m['tables']=89;m['images']=89;m['pages']=265;m['moved_figure']={'id':'fig-projection-vintages','after_id':'fig-recruitment-2024-current','new_number':33,'new_PDF_page':120,'narrative_moved_to_recruitment_discussion':True};m['removed_figure']={'id':'fig-model262-recruitment-deviations','old_number':42,'old_PDF_page':129,'subsequent_figures_renumbered':True};m['removed_table']={'ids':['tbl-ats-biomass-allocation','tbl-model262-current-likelihood-complete'],'old_numbers':[23,41],'subsequent_main_tables_renumbered':True,'PDF_references_and_labels_updated':count,'pages':265}
for name in m['files']:m['files'][name]=hashlib.sha256((R/name).read_bytes()).hexdigest()
(R/'release.json').write_text(json.dumps(m,indent=2)+'\n');print('Removed ATS allocation table; updated',count,'PDF table labels/references')
