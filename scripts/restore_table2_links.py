"""Restore Table2 source links from the reviewed management timeline register.

Usage: python restore_table2_links.py PRIVATE_2026_SOURCE_ROOT
Copies public source documents into the publication tree; no model operations.
"""
from pathlib import Path
import csv,hashlib,html,json,re,shutil,sys
from collections import Counter
from urllib.parse import urlsplit,urlunsplit,urljoin
from bs4 import BeautifulSoup
import pymupdf as fitz
ROOT=Path(__file__).resolve().parents[1];REPORT=ROOT/'2026'
SOURCE=Path(sys.argv[1]);TIMELINE=SOURCE/'source/management-timeline-20261026'
OUT=REPORT/'table2-sources';OUT.mkdir(exist_ok=True)
PUBLIC='https://noaa-afsc.github.io/EBS_pollock/2026/'
rows=list(csv.DictReader((TIMELINE/'management-measures.csv').open()))
assert len(rows)==23
extract=lambda text:re.findall(r'\[([^]]+)\]\(([^)]+)\)',text)
links={label:target for row in rows for label,target in extract(row['measure_sources'])}
notes={'Supplied CSV':'source/management-timeline-20261026/originals/ebs_pollock_management_measures.csv','supplied Markdown':'source/management-timeline-20261026/originals/ebs_pollock_management_measures.md','row-by-row verification and corrections':'source/management-timeline-20261026/row-review.json'}
downloads={x['name']:x for x in json.loads((TIMELINE/'source-downloads.json').read_text()) if x.get('status')=='saved'}
manifest=[];targets={}
for label,original in (links|notes).items():
 parsed=urlsplit(original);src=SOURCE/parsed.path;assert src.is_file(),original
 dest=OUT/src.name
 if src.suffix=='.html':
  # Self-contained reading copy: preserve document text and resolve its links.
  record=downloads[src.name];official=record['url'];soup=BeautifulSoup(src.read_text(),'html.parser')
  title=soup.title.get_text(' ',strip=True) if soup.title else label
  content=soup.find('pre') if 'govinfo.gov' in official else soup.find('main') or soup.find('article') or soup.body
  assert content and len(content.get_text(strip=True))>500
  for tag in content.find_all(['script','style','nav','header','footer','form','iframe','img','svg','button','link']):tag.decompose()
  for tag in content.find_all(True):
   href=tag.get('href');tag.attrs={}
   if href and urlsplit(urljoin(official,href)).scheme in ['http','https']:tag['href']=urljoin(official,href)
  date=record.get('retrieved_UTC','2026-10-26')[:10]
  page=f'''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>{html.escape(title)}</title><style>body{{font-family:system-ui,sans-serif;max-width:70rem;margin:2rem auto;padding:0 1rem;line-height:1.6}}pre{{white-space:pre-wrap;overflow-wrap:anywhere}}a{{color:#0645ad}}.provenance{{padding:1rem;background:#f2f5f7}}</style></head><body><aside class="provenance">Archived source for assessment Table 2. <a href="{html.escape(official,quote=True)}">Official source</a>. Retrieved {date}. Navigation and scripts omitted from this reading copy.</aside><h1>{html.escape(title)}</h1>{content}</body></html>'''
  dest.write_text(page)
 else:shutil.copy2(src,dest)
 if src.suffix=='.pdf':
  doc=fitz.open(dest);assert len(doc)>0
  if parsed.fragment.startswith('page='):assert 1<=int(parsed.fragment[5:])<=len(doc)
  doc.close()
 relative='table2-sources/'+dest.name+('#'+parsed.fragment if parsed.fragment else '')
 targets[label]=relative
 manifest.append({'label':label,'original_report_target':original,'published_target':relative,'original_sha256':hashlib.sha256(src.read_bytes()).hexdigest(),'published_sha256':hashlib.sha256(dest.read_bytes()).hexdigest(),'official_url':downloads.get(src.name,{}).get('url'),'reading_copy':src.suffix=='.html'})
# Restore the row and note anchors, preserving report text and every other table.
path=REPORT/'assessment.html';source=path.read_text();soup=BeautifulSoup(source,'html.parser');table=soup.find(id='tbl-management-measures');assert table
cells=table.select('td[headers="measure_sources"]');assert len(cells)==len(rows)
for cell,row in zip(cells,rows):
 pairs=extract(row['measure_sources']);assert cell.get_text(' ',strip=True)=='; '.join(label for label,_ in pairs)
 cell.clear()
 for i,(label,_) in enumerate(pairs):
  if i:cell.append('; ')
  a=soup.new_tag('a',href=targets[label]);a.string=label;cell.append(a)
for label in notes:
 candidates=[x for x in table.find_all(string=True) if label in str(x)];assert len(candidates)==1,(label,candidates)
 node=candidates[0];before,after=str(node).split(label,1);a=soup.new_tag('a',href=targets[label]);a.string=label;node.replace_with(before,a,after)
# Replace only the Table2 wrapper, keeping the rest of the saved HTML byte-identical.
marker='id="tbl-management-measures"';start=source.rfind('<div',0,source.index(marker));depth=0
for m in re.finditer(r'</?div\b[^>]*>',source[start:]):
 depth+=-1 if m.group().startswith('</') else 1
 if depth==0:end=start+m.end();break
new=source[:start]+str(table)+source[end:];path.write_text(new)
# Add PDF URI annotations and visible underlines without reflowing the table.
pdfpath=REPORT/'assessment.pdf';pdf=fitz.open(pdfpath)
before_text=hashlib.sha256(''.join(p.get_text() for p in pdf).encode()).hexdigest();old_pages=len(pdf);old_links=sum(len(p.get_links()) for p in pdf)
page_indexes=[i for i,p in enumerate(pdf) if 'Historical pollock management and monitoring measures' in p.get_text().replace('\xa0',' ')]
assert len(page_indexes)==3,page_indexes
expected=Counter(label for row in rows for label,_ in extract(row['measure_sources']));found=Counter();annotations=[]
for label in links:
 for i in page_indexes:
  page=pdf[i];max_y=180 if i==page_indexes[-1] else 735
  rects=page.search_for(label,clip=fitz.Rect(410,50,580,max_y))
  for rect in rects:
   page.insert_link({'kind':fitz.LINK_URI,'from':rect,'uri':PUBLIC+targets[label]})
   page.draw_line((rect.x0,rect.y1+.3),(rect.x1,rect.y1+.3),color=(0.02,.25,.65),width=.45,overlay=True)
   annotations.append({'label':label,'page':i+1,'uri':PUBLIC+targets[label],'rect':list(rect)})
   found[label]+=1
assert found==expected,(found,expected)
page=pdf[page_indexes[-1]]
for label in notes:
 rects=page.search_for(label);assert rects,(label,'missing in PDF')
 for rect in rects:
  page.insert_link({'kind':fitz.LINK_URI,'from':rect,'uri':PUBLIC+targets[label]})
  page.draw_line((rect.x0,rect.y1+.3),(rect.x1,rect.y1+.3),color=(0.02,.25,.65),width=.45,overlay=True)
  annotations.append({'label':label,'page':page.number+1,'uri':PUBLIC+targets[label],'rect':list(rect)})
tmp=REPORT/'assessment-table2-tmp.pdf';pdf.save(tmp,garbage=4,deflate=True,deflate_images=True,deflate_fonts=True);pdf.close();tmp.replace(pdfpath)
result=fitz.open(pdfpath);assert len(result)==old_pages
assert hashlib.sha256(''.join(p.get_text() for p in result).encode()).hexdigest()==before_text
assert sum(len(p.get_links()) for p in result)==old_links+len(annotations)
for entry in annotations:
 assert any(l.get('uri')==entry['uri'] and fitz.Rect(l['from']).intersects(fitz.Rect(entry['rect'])) for l in result[entry['page']-1].get_links())
# Every restored HTML target is backed by a real local file and every PDF page fragment is valid.
check=BeautifulSoup(path.read_text(),'html.parser').find(id='tbl-management-measures');anchors=check.find_all('a');assert len(anchors)==39
for a in anchors:assert (REPORT/urlsplit(a['href']).path).is_file()
validation=REPORT/'validation/table2-links-20261109';validation.mkdir(exist_ok=True)
for i in page_indexes:result[i].get_pixmap(matrix=fitz.Matrix(1.5,1.5)).save(validation/f'pdf-page-{i+1}.png')
(OUT/'source-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
checks={'table_rows':23,'HTML_row_links':36,'HTML_note_links':3,'PDF_links_added':len(annotations),'PDF_link_details':annotations,'PDF_text_unchanged':True,'PDF_pages_unchanged':old_pages,'all_local_targets_exist':True,'PDF_page_fragments_valid':True,'manual_accessibility':'Not Tested'}
(validation/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
release=json.loads((REPORT/'release.json').read_text());release['version']='2026-11-09-table2-links';release['files']={n:hashlib.sha256((REPORT/n).read_bytes()).hexdigest() for n in ['assessment.html','assessment.pdf']};release['table2_source_links']={'row_links':36,'note_links':3,'PDF_annotations':len(annotations),'source_manifest':'table2-sources/source-manifest.json','validation':'validation/table2-links-20261109/checks.json','PDF_text_and_page_count_unchanged':True};(REPORT/'release.json').write_text(json.dumps(release,indent=2)+'\n')
print('Restored39 HTML links and',len(annotations),'PDF link annotations; table text and page count unchanged.')
