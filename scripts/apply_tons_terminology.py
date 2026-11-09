from pathlib import Path
import json,re,shutil
from bs4 import BeautifulSoup
import pymupdf as f
root=Path(__file__).resolve().parents[1];r=root/'2026';out=r/'tables/terminology-tons'
shutil.copy2(r/'assessment.pdf','/tmp/assessment-before-tons.pdf')
mapping={'figure-008.png':'fsh_wt_frq_yr.png','figure-009.png':'fsh_wt_freq_week_A.png','figure-010.png':'fsh_wt_freq_week_B.png','figure-023.png':'data_input_effects.png','figure-027.png':'model23_0_vs23_2_uncertainty.png','figure-028.png':'model23_2_vs26_2_uncertainty.png','figure-029.png':'population-uncertainty-tons.png','figure-033.png':'lnRS.png'}
s=(r/'assessment.html').read_text();soup=BeautifulSoup(s,'html.parser');p=f.open(r/'assessment.pdf');replaced=[]
for asset,source in mapping.items():
 img=soup.find('img',src='assets/'+asset);assert img,asset
 wrapper=img.find_parent('figure');caption=wrapper.find('figcaption').get_text(' ',strip=True);num=int(re.search(r'Figure\s+(\d+)',caption).group(1))
 pages=[i for i,page in enumerate(p) if re.search(r'Figure\s+'+str(num)+r'\s*:',page.get_text(sort=True).replace('\xa0',' '))]
 assert len(pages)==1,(asset,num,pages)
 page=p[pages[0]];images=page.get_images();assert len(images)==1,(asset,images)
 shutil.copy2(out/source,r/'assets'/asset);page.replace_image(images[0][0],filename=str(r/'assets'/asset));replaced.append({'asset':asset,'figure':num,'PDF_page':pages[0]+1})
# Replace every editable occurrence, retaining original span styling and data.
count=0
for page in p:
 edits=[]
 for b in page.get_text('dict')['blocks']:
  for line in b.get('lines',[]):
   for span in line['spans']:
    if re.search(r'\btonnes\b',span['text'],re.I):
     count+=len(re.findall(r'\btonnes\b',span['text'],re.I))
     text=re.sub(r'\btonnes\b',lambda m:'Tons' if m.group()[0].isupper() else 'tons',span['text'],flags=re.I)
     edits.append((span,text))
 for span,text in edits:page.add_redact_annot(f.Rect(span['bbox']),fill=(1,1,1))
 if edits:
  page.apply_redactions(images=0,graphics=0)
  for span,text in edits:
   font=span['font'].lower();sans=any(x in font for x in ['arial','helvetica','sans']);bold=bool(span['flags']&16);italic=bool(span['flags']&2)
   name=('hebi' if bold and italic else 'hebo' if bold else 'heit' if italic else 'helv') if sans else ('tibi' if bold and italic else 'tibo' if bold else 'tiit' if italic else 'tiro')
   page.insert_text(span['origin'],text,fontsize=span['size'],fontname=name,color=f.sRGB_to_pdf(span['color']))
# Replace the whole biological caption area, which previously spanned separate blocks.
bioidx=json.loads((r/'tables/weight-age-revision-20261109/PDF-restructure-checks.json').read_text())['biological_figure34_page']-1;bio=p[bioidx];bio.add_redact_annot(f.Rect(36,380,580,735),fill=(1,1,1));bio.apply_redactions(images=0,graphics=0)
cap=json.loads((r/'tables/weight-age-revision-20261109/captions.json').read_text())['new34']
import html
fit=bio.insert_htmlbox(f.Rect(36,386,576,720),'<p style="padding-left:50pt;text-indent:-50pt"><b>Figure 34:</b> '+html.escape(cap)+'</p>',css='body {font-family:serif;font-size:9pt;line-height:1.15}');assert fit[0]>=0
path=r/'assessment-tons-tmp.pdf';p.save(path,garbage=3,deflate=True);p.close();path.replace(r/'assessment.pdf')
s=re.sub(r'\btonnes\b',lambda m:'Tons' if m.group()[0].isupper() else 'tons',s,flags=re.I);(r/'assessment.html').write_text(s)
# Report-owned labels and source recipes also use the selected wording.
textfiles=[]
for area in [r/'tables',root/'scripts']:
 for path in area.rglob('*'):
  if path.suffix.lower() not in ['.csv','.svg','.html','.qmd','.md','.R','.py','.json']:continue
  try:t=path.read_text()
  except UnicodeDecodeError:continue
  new=re.sub(r'\btonnes\b',lambda m:'Tons' if m.group()[0].isupper() else 'tons',t,flags=re.I)
  if new!=t:path.write_text(new);textfiles.append(str(path.relative_to(root)))
new=f.open(r/'assessment.pdf');assert not any(re.search(r'\btonnes\b',x.get_text(),re.I) for x in new)
new[bioidx].get_pixmap(matrix=f.Matrix(1.4,1.4)).save('/tmp/biological-current-final.png')
(out/'terminology-checks.json').write_text(json.dumps({'editable_PDF_occurrences_changed':count,'graphic_labels_replaced':replaced,'report_source_files_relabelled':textfiles,'HTML_remaining_occurrences':len(re.findall(r'\btonnes\b',s,re.I)),'numeric_conversion':False},indent=2))
print('Changed',count,'editable PDF occurrences and',len(replaced),'graphic assets; zero remaining editable occurrences.')
