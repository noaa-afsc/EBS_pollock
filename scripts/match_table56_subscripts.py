"""Copy Table55 event-label typography into Table56; preserve probabilities."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as fitz,json,hashlib,re,sys
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'
path=R/'assessment.html';text=path.read_text();s=BeautifulSoup(text,'html.parser');source=s.find(id='tbl-current-decision-definitions');target=s.find(id='tbl-current-decision-model26')
get_cells=lambda t:[row.find(['th','td'],recursive=False) for row in t.select('tbody tr')]
a=get_cells(source);b=get_cells(target);assert len(a)==len(b)==9
before_values=[[c.get_text() for c in row.find_all('td',recursive=False)] for row in target.select('tbody tr')]
for x,y in zip(a,b):
 assert x.get_text()==y.get_text()
 y.clear();y.append(BeautifulSoup(x.decode_contents(),'html.parser'))
assert [x.decode_contents() for x in a]==[y.decode_contents() for y in b]
assert before_values==[[c.get_text() for c in row.find_all('td',recursive=False)] for row in target.select('tbody tr')]
start=text.rfind('<div',0,text.index('id="tbl-current-decision-model26"'));depth=0
for m in re.finditer(r'</?div\b[^>]*>',text[start:]):
 depth+=-1 if m.group().startswith('</') else 1
 if not depth:end=start+m.end();break
path.write_text(text[:start]+str(target)+text[end:])
p=fitz.open(sys.argv[1] if len(sys.argv)>1 else R/'assessment.pdf');page=next(x for x in p if 'Table 55:' in x.get_text(sort=True).replace('\xa0',' '));old_links=sum(len(x.get_links()) for x in p);old_pages=len(p)
source_lines=[];target_lines=[]
for block in page.get_text('dict')['blocks']:
 for line in block.get('lines',[]):
  label=''.join(sp['text'] for sp in line['spans'])
  if label.startswith('P(') and line['bbox'][0]<160:
   (source_lines if line['bbox'][1]<450 else target_lines).append(line)
assert len(source_lines)==len(target_lines)==9
region=fitz.Rect(170,535,580,680);numeric=page.get_text(clip=region)
background=page.get_pixmap(colorspace=fitz.csRGB)
for src,dst in zip(source_lines,target_lines):
 assert ''.join(x['text'] for x in src['spans']).replace(' ','')==''.join(x['text'] for x in dst['spans']).replace(' ','')
 colour=tuple(c/255 for c in background.pixel(150,int(dst['spans'][0]['origin'][1]-2)))
 page.add_redact_annot(fitz.Rect(dst['bbox']),fill=colour)
page.apply_redactions(images=0,graphics=0)
page.insert_font(fontname='table56regular',fontfile='/System/Library/Fonts/Supplemental/Times New Roman.ttf')
page.insert_font(fontname='table56italic',fontfile='/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf')
for src,dst in zip(source_lines,target_lines):
 src_origin=src['spans'][0]['origin'];dst_origin=dst['spans'][0]['origin']
 for span in src['spans']:
  point=(dst_origin[0]+span['origin'][0]-src_origin[0],dst_origin[1]+span['origin'][1]-src_origin[1])
  font='table56italic' if 'Italic' in span['font'] else 'table56regular'
  page.insert_text(point,span['text'],fontsize=span['size'],fontname=font)
assert page.get_text(clip=region)==numeric
tmp=R/'assessment-subscript-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_images=True,deflate_fonts=True);page.get_pixmap(matrix=fitz.Matrix(1.7,1.7)).save('/tmp/table56-subscripts.png');p.close();tmp.replace(R/'assessment.pdf')
p=fitz.open(R/'assessment.pdf');assert len(p)==old_pages and sum(len(x.get_links()) for x in p)==old_links
v=R/'validation/table56-subscripts-20261109';v.mkdir(exist_ok=True)
checks={'event_labels_matched':9,'HTML_subscript_elements':len(target.find_all('sub')),'numeric_cells_unchanged':72,'PDF_numeric_region_unchanged':True,'PDF_pages':len(p),'PDF_link_count_unchanged':old_links}
(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-table2-links-table56-subscripts';m['files']={n:hashlib.sha256((R/n).read_bytes()).hexdigest() for n in ['assessment.html','assessment.pdf']};m['table56_subscripts']=checks;(R/'release.json').write_text(json.dumps(m,indent=2)+'\n')
print(json.dumps(checks))
