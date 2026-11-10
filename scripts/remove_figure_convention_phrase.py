"""Remove the redundant Figure 3 convention phrase without changing numeric content."""
from pathlib import Path
import re,sys,json,subprocess
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/remove-recorded-20261110'
def revise(t):return re.sub(r',\s*matching\s+the\s+original\s+figure\s+convention','',t)
pth=r/'assessment.html';s=pth.read_text();assert s.count('matching the original figure convention')==2;s=revise(s)
# Preserve stable anchors and file identifiers: the wording request concerns reader text.
old=subprocess.check_output(['git','show','HEAD:2026/assessment.html'],cwd=ROOT,text=True)
for match in re.finditer(r'(?:id|href|src|data-anchor-id)="[^"]*\brecorded\b[^"]*"',old,re.I):
 attr=match[0];new=re.sub(r'\brecorded[-\s]*','',attr,flags=re.I);s=s.replace(new,attr)
p=f.open(r/'assessment.pdf');links=[len(q.get_links())for q in p];toc=p.get_toc()
sys.argv=['font-conversion','--cm-font-dir','/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode']
source=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0].replace("text=sp['text'].replace('\\x00','⁻')","text=revise(sp['text'].replace('\\x00','⁻'))")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'revise':revise};exec(source,ns);ns['transform'](p[103])
tmp=out/'caption-update.pdf';p.save(tmp,garbage=4,deflate=True);p.close();q=f.open(tmp)
assert [len(x.get_links())for x in q]==links and q.get_toc()==toc
assert not any('matching the original figure convention' in ' '.join(x.get_text().split())for x in q)
assert not any(re.search(r'\brecorded\b',x.get_text(),re.I)for x in q)
q[103].get_pixmap(matrix=f.Matrix(1,1)).save(str(out/'caption-page-104.png'));q.close();tmp.replace(r/'assessment.pdf');pth.write_text(s)
