"""Remove redundant 'recorded' wording throughout the published SAFE products."""
from pathlib import Path
import re,sys,json,hashlib
import pymupdf as fitz
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';out=R/'validation/remove-recorded-20261110';out.mkdir(exist_ok=True)
def revise(t):
 for a,b in [(r'recorded as collection errors','classified as collection errors'),(r'recorded as positive definite','reported as positive definite'),(r'recorded separately','documented separately'),(r'recorded with','supplied with'),(r'recorded in','in'),(r'source-recorded','source')]:
  t=re.sub(a+r'\b',b,t,flags=re.I)
 t=re.sub(r'\bRecorded[-\s]+(?=[A-Z])','',t)
 t=re.sub(r'\bRecorded[-\s]+([a-z])',lambda m:m[1].upper(),t)
 t=re.sub(r'\brecorded(?:[-\s]+|\b)','',t)
 return t
s=(R/'assessment.html').read_text();updated=revise(s);assert not re.search(r'\brecorded\b',updated,re.I)
p=fitz.open(R/'assessment.pdf');before=p.get_toc();links=[len(q.get_links())for q in p];imgs=[[x['digest'].hex()for x in q.get_image_info(hashes=True)]for q in p];targets=[]
# Reuse the established Computer Modern span renderer, preserving line geometry.
sys.argv=['font-conversion','--cm-font-dir','/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode']
source=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0]
source=source.replace("text=sp['text'].replace('\\x00','⁻')","text=revise(sp['text'].replace('\\x00','⁻'))")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'revise':revise};exec(source,ns)
for i,q in enumerate(p):
 if re.search(r'\brecorded\b',q.get_text(),re.I):targets.append(i+1);ns['transform'](q)
tmp=out/'assessment.pdf';p.save(tmp,garbage=4,deflate=True);p.close();p=fitz.open(tmp)
assert p.get_toc()==before
assert [len(q.get_links())for q in p]==links
assert [[x['digest'].hex()for x in q.get_image_info(hashes=True)]for q in p]==imgs
assert not any(re.search(r'\brecorded\b',q.get_text(),re.I)for q in p)
for i in [8,17,186,196,203,279]:
 if i in targets:p[i-1].get_pixmap(matrix=fitz.Matrix(1,1)).save(str(out/f'page-{i}.png'))
(out/'checks.json').write_text(json.dumps({'edited_pages':targets,'pages':len(p),'HTML_occurrences_removed':len(re.findall(r'\brecorded\b',s,re.I)),'HTML_PDF_remaining_occurrences':0,'links_bookmarks_figure_pixels_preserved':True,'font':'Computer Modern Unicode Serif','manual_accessibility':'Not Tested'},indent=2))
p.close();tmp.replace(R/'assessment.pdf');(R/'assessment.html').write_text(updated);print('PASS',len(targets),'pages edited')
