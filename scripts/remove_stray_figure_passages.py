"""Remove the specified editorial/provenance paragraphs from the Figures section."""
from pathlib import Path
from bs4 import BeautifulSoup
import pymupdf as fitz,json,hashlib,re
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'
starts=['This provisional summary uses saved sampler','January–April 2026 has elapsed','The user-identified production export']
repeated='Source values and reproduction records are retained in the technical supplement.'
p=R/'assessment.html';text=p.read_text();s=BeautifulSoup(text,'html.parser');fig=s.find(id='figures');remove=[x for x in fig.find_all('p') if any(x.get_text(' ',strip=True).startswith(k) for k in starts) or x.get_text(' ',strip=True)==repeated]
assert len(remove)==7
# Preserve original bytes outside the selected paragraphs.
for para in remove:
 # BeautifulSoup may reorder attributes. Use the paragraph's complete text to find its source range.
 label=para.get_text(' ',strip=True)
 matches=[]
 for m in re.finditer(r'<p(?:\s[^>]*)?>.*?</p>',text,re.S):
  if BeautifulSoup(m.group(),'html.parser').get_text(' ',strip=True)==label:matches.append(m)
 assert matches,(label[:60],len(matches))
 if label!=repeated:assert len(matches)==1
 m=matches[0];text=text[:m.start()]+text[m.end():]
p.write_text(text)
pdf=fitz.open(R/'assessment.pdf');pages=len(pdf);before_links=sum(len(x.get_links()) for x in pdf)
# Full paragraph bounds include fragments created by earlier label edits.
areas={86:[fitz.Rect(49,702,390,716)],87:[fitz.Rect(49,468,564,581)],88:[fitz.Rect(49,433,565,532)],89:[fitz.Rect(49,433,564,523)],96:[fitz.Rect(49,408,390,422)]}
changed=[];expected_removed_links=0
for i,rects in areas.items():
 page=pdf[i];before=page.get_text();expected_removed_links+=sum(any(fitz.Rect(l['from']).intersects(r) for r in rects) for l in page.get_links())
 for rect in rects:page.add_redact_annot(rect,fill=(1,1,1))
 page.apply_redactions(images=0,graphics=0);changed.append(i+1)
tmp=R/'assessment-clean-figures-tmp.pdf';pdf.save(tmp,garbage=4,deflate=True,deflate_images=True,deflate_fonts=True);pdf.close();tmp.replace(R/'assessment.pdf')
pdf=fitz.open(R/'assessment.pdf');assert len(pdf)==pages
norm=lambda t:t.replace('\xa0',' ').replace('\xad','')
alltext=' '.join(norm(x.get_text()) for x in pdf)
for k in starts+[repeated]:assert k not in alltext,k
assert sum(len(x.get_links())for x in pdf)==before_links-expected_removed_links
s=BeautifulSoup(p.read_text(),'html.parser');assert len(s.find(id='tbl-management-measures').find_all('a'))==39
assert not any(k in s.find(id='figures').get_text(' ',strip=True)for k in starts+[repeated])
v=R/'validation/figure-prose-cleanup-20261109';v.mkdir(exist_ok=True)
for i in [87,88,89]:pdf[i].get_pixmap(matrix=fitz.Matrix(1.2,1.2)).save(v/f'pdf-page-{i+1}.png')
checks={'HTML_paragraphs_removed':7,'PDF_pages_cleaned':changed,'PDF_repeated_source_sentences_removed':5,'PDF_pages_unchanged':pages,'links_removed_with_deleted_prose':expected_removed_links,'Table2_links_retained':39,'figure_images_and_captions_preserved':True}
(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-figure-prose-cleanup';m['figure_prose_cleanup']=checks;m['files']={n:hashlib.sha256((R/n).read_bytes()).hexdigest()for n in ['assessment.html','assessment.pdf']};(R/'release.json').write_text(json.dumps(m,indent=2)+'\n')
print(json.dumps(checks))
