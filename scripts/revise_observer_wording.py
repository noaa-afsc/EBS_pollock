"""Apply two approved attribution changes to the current published SAFE products."""
from pathlib import Path
import hashlib,json,re,html,unicodedata
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/observer-wording-20261110'
changes=[('The assessment authors report good fishing','Available observer data suggest good fishing'),('The assessment author’s observations','The observer data')]
s=(r/'assessment.html').read_text()
for a,b in changes:
 assert s.count(a)==1,a
 s=s.replace(a,b)
p=f.open(r/'assessment.pdf');old_toc=p.get_toc();old_links=[len(q.get_links()) for q in p];old_imgs=[[v['digest'].hex() for v in q.get_image_info(hashes=True)] for q in p];targets=[]
fontdir=Path('/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode')
css='@font-face {font-family:CMU;src:url(cmunrm.otf)} body {font-family:CMU;font-size:10.2pt;line-height:1.2;margin:0} p {margin:0}'
for a,b in changes:
 matches=[]
 for i,q in enumerate(p):
  for block in q.get_text('blocks'):
   t=' '.join(block[4].split()).replace('Salmon- avoidance','Salmon-avoidance')
   if a in t:matches.append((i,block,t))
 assert len(matches)==1,(a,len(matches))
 i,block,t=matches[0];q=p[i];rect=f.Rect(50.49,block[1]-.4,565,block[3]+1)
 assert not any(rect.intersects(l['from']) for l in q.get_links())
 q.add_redact_annot(f.Rect(block[:4]),fill=False,cross_out=False);q.apply_redactions(images=0,graphics=0)
 spare,scale=q.insert_htmlbox(rect,'<p>'+html.escape(t.replace(a,b))+'</p>',css=css,archive=f.Archive(str(fontdir)),scale_low=1)
 assert spare>=0 and scale==1,(spare,scale)
 targets.append(i+1)
tmp=out/'assessment.pdf';p.save(tmp,garbage=4,deflate=True);p.close();p=f.open(tmp)
assert p.get_toc()==old_toc
assert [len(q.get_links()) for q in p]==old_links
assert [[v['digest'].hex() for v in q.get_image_info(hashes=True)] for q in p]==old_imgs
text=unicodedata.normalize('NFKC',' '.join(' '.join(q.get_text().split()) for q in p))
for a,b in changes:assert a not in text and b in text
for i in targets:
 p[i-1].get_pixmap(matrix=f.Matrix(1.4,1.4)).save(str(out/f'page-{i}.png'))
fonts=sorted({sp['font'] for q in p for bl in q.get_text('dict')['blocks'] for ln in bl.get('lines',[]) for sp in ln['spans']})
assert any('CMU' in name for name in fonts),fonts
(out/'checks.json').write_text(json.dumps({'pages':len(p),'edited_pages':targets,'fonts':fonts,'all_links_bookmarks_and_figure_pixels_preserved':True,'both_wording_changes_verified':True,'manual_accessibility':'Not Tested'},indent=2))
p.close();tmp.replace(r/'assessment.pdf');(r/'assessment.html').write_text(s)
print('Verified both changes; preserved',len(old_links),'pages.')
