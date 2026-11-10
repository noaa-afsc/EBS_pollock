"""Revise a saved vector map footnote and render its unchanged map geometry."""
from pathlib import Path
import sys,re
import pymupdf as f
from PIL import Image
p=f.open(sys.argv[1]);q=p[0];edits=[]
for b in q.get_text('dict')['blocks']:
 for l in b.get('lines',[]):
  for s in l['spans']:
   if re.search(r'\bsupplied\b',s['text'],re.I):edits.append(s);q.add_redact_annot(f.Rect(s['bbox']),fill=False,cross_out=False)
assert len(edits)==1;q.apply_redactions(images=0,graphics=0)
for s in edits:q.insert_text(s['origin'],re.sub(r'\bsupplied\s+','',s['text']),fontname='helv',fontsize=s['size'],color=f.sRGB_to_pdf(s['color']))
out=Path('2026/validation/prose-figure-cleanup-20261110/map-footnote.pdf');p.save(out,garbage=4,deflate=True);p.close()
import subprocess
subprocess.run(['pdftoppm','-f','1','-singlefile','-scale-to-x','1870','-scale-to-y','3036','-png',str(out),'2026/assets/figure-012'],check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
