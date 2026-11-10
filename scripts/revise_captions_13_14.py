"""Apply author-supplied captions to Figures 13–14 in the current SAFE products."""
from pathlib import Path
import re,json,html,unicodedata
from lxml import html as lh
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/captions-13-14-20261110';caps=json.loads((out/'captions.json').read_text())
s=(r/'assessment.html').read_text();dom=lh.fromstring(s,parser=lh.HTMLParser(huge_tree=True));p=f.open(r/'assessment.pdf');toc=p.get_toc();images=[[x['digest'].hex()for x in q.get_image_info(hashes=True)]for q in p];pages=[]
fontdir=Path('/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode');css='@font-face {font-family:CMU;src:url(cmunrm.otf)} body {font-family:CMU;font-size:8.5pt;line-height:1.2;margin:0} table {border-collapse:collapse;width:100%} td {padding:0;vertical-align:top} td.label {width:43pt;padding-right:3pt}'
for n,text in caps.items():
 found=[e for e in dom.xpath('//figcaption')if re.match(r'Figure\s+'+n+r'[:.]',' '.join(e.text_content().split()))];assert len(found)==1,n
 cap=found[0];ident=cap.get('id');old=''.join(cap.xpath('.//span[contains(@class,"safe-caption-text")]')[0].itertext()).strip()
 pattern=r'(<figcaption\b[^>]*id="'+re.escape(ident)+r'"[^>]*>.*?<span class="safe-caption-text">).*?(</span>\s*</figcaption>)'
 s,count=re.subn(pattern,lambda m:m[1]+html.escape(text)+m[2],s,flags=re.S);assert count==1,n
 # Lightbox titles mirror the reader caption.
 s=re.sub(r'title="Figure(?:\xa0| )'+n+r': [^"]*"',lambda m:'title="'+html.escape('Figure\xa0'+n+': '+text,quote=True)+'"',s)
 hits=[]
 for i,q in enumerate(p):
  for b in q.get_text('blocks'):
   if re.match(r'Figure\s+'+n+r'[:.]',' '.join(b[4].split())):hits.append((i,b))
 assert len(hits)==1,(n,hits)
 i,b=hits[0];q=p[i];top=b[1]-.5
 # Caption occupies all body text below this figure; footer stays intact.
 blocks=[x for x in q.get_text('blocks')if x[1]>=top and x[3]<740]
 rect=f.Rect(50.49,top,565,min(740,max(x[3]for x in blocks)+15))
 for x in blocks:q.add_redact_annot(f.Rect(x[:4]),fill=False,cross_out=False)
 q.apply_redactions(images=0,graphics=0)
 spare,scale=q.insert_htmlbox(rect,'<table><tr><td class="label">Figure '+n+':</td><td>'+html.escape(text)+'</td></tr></table>',css=css,archive=f.Archive(str(fontdir)),scale_low=1)
 assert spare>=0 and scale==1,(n,spare,scale)
 pages.append(i+1)
tmp=out/'assessment.pdf';p.save(tmp,garbage=4,deflate=True);p.close();q=f.open(tmp);assert len(q)==280 and q.get_toc()==toc
assert [[x['digest'].hex()for x in page.get_image_info(hashes=True)]for page in q]==images
norm=lambda t:re.sub(r'\s+','',unicodedata.normalize('NFKC',t))
for n,text in caps.items():assert norm(text) in norm(q[pages[int(n)-13]-1].get_text()),n
for i in pages:q[i-1].get_pixmap(matrix=f.Matrix(1.2,1.2)).save(str(out/f'page-{i}.png'))
(out/'checks.json').write_text(json.dumps({'caption_numbers':list(caps),'PDF_pages':pages,'total_pages':280,'all_caption_text_verified':True,'all_figure_pixels_and_bookmarks_preserved':True,'font':'Computer Modern Unicode Serif','manual_accessibility':'Not Tested'},indent=2))
q.close();tmp.replace(r/'assessment.pdf');(r/'assessment.html').write_text(s);print('PASS: captions 13–14 updated in HTML and PDF')
