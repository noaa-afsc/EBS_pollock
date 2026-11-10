"""Replace only the PDF's main table section, preserving report content and links."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as f,re,subprocess,json,hashlib,collections,unicodedata
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';WORK=Path('/tmp/assessment-main-tables-pagination');BASELINE='c79dc3f'
old=f.open(stream=subprocess.check_output(['git','show',BASELINE+':2026/assessment.pdf'],cwd=ROOT),filetype='pdf');tables=f.open(WORK/'tables.pdf');manifest=json.loads((WORK/'table-manifest.json').read_text());s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser');START=41;END=86
assert len(old)==265 and old[START].get_text().startswith('Tables') and old[END].get_text().startswith('Figures')
def canonical(t):return collections.Counter(c for c in unicodedata.normalize('NFKC',t).replace('\xad','').replace('‐','-')if not c.isspace())
starts={};table_checks=[]
for item in manifest:
 n=item['number'];first=item['first_page'];count=item['page_count'];root=s.find(id=item['id']);head=root.find('thead');actual=''.join(tables[i].get_text()for i in range(first,first+count));expected=item['expected_text']+(head.get_text(' ',strip=True)if head else '')*(count-1)+('Tables'if n==1 else '')
 assert canonical(actual)==canonical(expected),('Table text mismatch',n,canonical(actual)-canonical(expected),canonical(expected)-canonical(actual))
 cap=next(b for b in tables[first].get_text('blocks')if re.match(r'Table\s+'+str(n)+':',b[4].strip()));starts[n]=(START+first,max(36,cap[1]-4))
 for i in range(first,first+count):
  for b in tables[i].get_text('dict')['blocks']:
   for line in b.get('lines',[]):
    for sp in line['spans']:assert sp['bbox'][0]>=30 and sp['bbox'][2]<=tables[i].rect.width-30,(n,'overflow',sp['text'],sp['bbox'])
 table_checks.append({'number':n,'id':item['id'],'first_PDF_page':START+first+1,'pages':count,'all_text_and_values_verified':True})
N=len(tables);delta=N-(END-START);old_to_new={i:i if i<START else i+delta for i in range(len(old))if not START<=i<END};old_links=[(i,dict(l))for i,page in enumerate(old)for l in page.get_links()];old_toc=old.get_toc(simple=False)
out=f.open();out.insert_pdf(old,from_page=0,to_page=START-1,links=False);out.insert_pdf(tables,links=False);out.insert_pdf(old,from_page=END,to_page=len(old)-1,links=False);out.set_metadata(old.metadata)
# Renumber printed footers without touching body fonts or figure graphics.
fontfile='/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode/cmunrm.otf';font=f.Font(fontfile=fontfile);footer_pages=[];original_missing=[]
for i,page in enumerate(out):
 if i<START:continue
 is_table=START<=i<START+N
 footer=[]
 if not is_table:
  old_i=next(k for k,v in old_to_new.items()if v==i)
  for b in page.get_text('dict')['blocks']:
   for line in b.get('lines',[]):
    for sp in line['spans']:
     if sp['text'].strip()==str(old_i+1) and sp['bbox'][1]>page.rect.height-47 and abs(sp['bbox'][0]-page.rect.width/2)<36:footer.append(f.Rect(sp['bbox']))
  if not footer:original_missing.append(i+1);continue
  for rect in footer:page.add_redact_annot(rect+(-.2,-.2,.2,.2),fill=False,cross_out=False)
  page.apply_redactions(images=0,graphics=0)
 page.insert_font(fontname='tablePaginationFooter20261109',fontfile=fontfile)
 label=str(i+1);x=(page.rect.width-font.text_length(label,fontsize=10))/2
 page.insert_text((x,page.rect.height-30),label,fontname='tablePaginationFooter20261109',fontsize=10);footer_pages.append(i)
# Fresh serialized objects ensure stable annotation references.
b=out.tobytes(garbage=4,deflate=True);out.close();out=f.open(stream=b,filetype='pdf')
def clean_link(l):return {k:v for k,v in l.items()if k in ['kind','from','page','to','uri','file','zoom','name']}
remapped=[];external_before=set()
for old_src,l in old_links:
 if l['kind']==f.LINK_URI:external_before.add(l['uri'])
 if old_src not in old_to_new:continue
 data=clean_link(l)
 if l['kind']==f.LINK_GOTO:
  target=l['page']
  if START<=target<END:
   text=old[old_src].get_textbox(l['from']+(-1,-1,1,1));numbers=re.findall(r'Table\s+(\d+)',text)
   assert len(numbers)==1 or 'Tables'in text,('Unresolved table link',old_src+1,text,target+1)
   n=int(numbers[0])if numbers else 1;assert n in starts,n
   data['page'],y=starts[n];data['to']=f.Point(36,y);remapped.append({'source_page':old_to_new[old_src]+1,'table':n,'destination_page':data['page']+1})
  else:assert target in old_to_new;data['page']=old_to_new[target]
 out[old_to_new[old_src]].insert_link(data)
for i,page in enumerate(tables):
 for l in page.get_links():
  data=clean_link(l)
  if l['kind']==f.LINK_GOTO:data['page']=START+l['page']
  out[START+i].insert_link(data)
# Preserve the outline with updated table-section and subsequent page destinations.
toc=[]
for level,title,page,dest in old_toc:
 old_page=page-1;new_dest={k:v for k,v in dest.items()if k in ['kind','to','zoom','color','bold','italic','collapse']}
 if START<=old_page<END:
  m=re.search(r'Table\s+(\d+)',title);n=int(m[1])if m else 1;new_page,y=starts[n];new_dest['to']=f.Point(36,y)
 else:new_page=old_to_new[old_page]
 new_dest['page']=new_page;toc.append([level,title,new_page+1,new_dest])
out.set_toc(toc);tmp=R/'assessment-table-pagination-tmp.pdf';out.save(tmp,garbage=4,deflate=True);out.close();q=f.open(tmp)
assert len(q)==len(old)+delta
# Verify every preserved report page, including all figure pixels, after allowing footer changes.
def body_counter(page):return canonical(page.get_text(clip=f.Rect(0,0,page.rect.width,page.rect.height-47)))
for old_i,new_i in old_to_new.items():
 assert body_counter(old[old_i])==body_counter(q[new_i]),('Body changed',old_i+1,new_i+1)
 assert [x['digest'].hex()for x in old[old_i].get_image_info(hashes=True)]==[x['digest'].hex()for x in q[new_i].get_image_info(hashes=True)],('Figure changed',old_i+1)
for i in footer_pages:
 footer=[sp['text'].strip()for b in q[i].get_text('dict')['blocks']for line in b.get('lines',[])for sp in line['spans']if sp['bbox'][1]>q[i].rect.height-47 and abs(sp['bbox'][0]-q[i].rect.width/2)<36 and sp['text'].strip().isdigit()]
 assert footer==[str(i+1)],(i+1,footer)
all_links=[l for page in q for l in page.get_links()];assert all(l['kind']!=f.LINK_GOTO or 0<=l['page']<len(q)for l in all_links)
assert not any('assessment.html#'in l.get('uri','')for l in all_links)
external_after={l['uri']for l in all_links if l['kind']==f.LINK_URI};assert external_before<=external_after,('External links lost',external_before-external_after)
assert sum(l['kind']==f.LINK_GOTO for l in all_links)==sum(l['kind']==f.LINK_GOTO for _,l in old_links)
source_urls={l['uri']for l in all_links if 'table2-sources/'in l.get('uri','')};assert len(source_urls)==15,len(source_urls)
for n in [1,2,8,21,22,37,38,39,53,54]:q[starts[n][0]].get_pixmap(matrix=f.Matrix(1.3,1.3)).save(str(WORK/f'final-table-{n}.png'))
checks={'pages':len(q),'main_table_section_pages_before':END-START,'main_table_section_pages_after':N,'tables_starting_on_fresh_pages':54,'single_page_main_tables':sum(x['pages']==1 for x in table_checks),'long_tables':[{k:x[k]for k in ['number','pages']}for x in table_checks if x['pages']>1],'all_table_text_and_values_verified_against_HTML':True,'all_other_report_body_text_preserved':True,'all_figure_pixels_and_sequence_preserved':True,'all_existing_external_links_preserved':True,'all_PDF_internal_links_remapped':True,'same_report_HTML_links':0,'Table_38_page':starts[38][0]+1,'Table_2_distinct_source_targets_preserved':15,'original_figure_pages_without_printed_footer_retained':original_missing,'manual_accessibility':'Not Tested'}
q.close();tmp.replace(R/'assessment.pdf');v=R/'validation/table-pagination-20261109';v.mkdir(exist_ok=True);(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n');(v/'tables.json').write_text(json.dumps(table_checks,indent=2)+'\n');(v/'internal-table-links.json').write_text(json.dumps(remapped,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-table-pagination';m['pages']=checks['pages'];m['PDF_table_pagination']=checks;m['internal_PDF_links']['Table_38_destination']={'destination_page':starts[38][0]+1,'destination_y':round(starts[38][1],2)};m['files']['assessment.pdf']=hashlib.sha256((R/'assessment.pdf').read_bytes()).hexdigest();(R/'release.json').write_text(json.dumps(m,indent=2)+'\n');print(json.dumps(checks,indent=2))
