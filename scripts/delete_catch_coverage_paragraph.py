"""Delete the requested catch-coverage paragraph and close its PDF page space."""
from pathlib import Path
import re,json
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/prose-figure-cleanup-20261110'
a='These summaries describe the available observed catch, including catcher-processors, motherships and observed catcher vessels. The extract omits electronic-monitoring catcher-vessel records, so changes in fleet coverage can also affect the mapped patterns. The maps show catch concentration; the updated fleet-dispersion index below measures the separation among observed tows.'
s=(r/'assessment.html').read_text();assert s.count('<p>'+a+'</p>')==1;s=s.replace('<p>'+a+'</p>\n','')
p=f.open(r/'assessment.pdf');counts=[len(q.get_links())for q in p];toc=p.get_toc(False);source=f.open(stream=p.tobytes(),filetype='pdf');q=p[8];links=[dict(l)for l in q.get_links()];shift=58.5
for l in q.get_links():q.delete_link(l)
q.add_redact_annot(f.Rect(0,45,612,742),fill=False,cross_out=False);q.apply_redactions(images=0,graphics=0)
q.show_pdf_page(f.Rect(0,50.5,612,683.5),source,8,clip=f.Rect(0,109,612,742))
for l in links:
 data={k:v for k,v in l.items()if k in ['kind','from','page','to','uri','file','zoom','name']};data['from']=data['from']+(0,-shift,0,-shift);q.insert_link(data)
for page in p:
 for l in page.get_links():
  if l['kind']==f.LINK_GOTO and l['page']==8 and 109<=l['to'].y<=742:
   l['to']=f.Point(l['to'].x,l['to'].y-shift);page.update_link(l)
for item in toc:
 if item[2]==9 and 'to' in item[3] and 109<=item[3]['to'].y<=742:item[3]['to']=f.Point(item[3]['to'].x,item[3]['to'].y-shift)
p.set_toc(toc)
# Synchronize final SVG typography corrections into their existing figure positions.
for n in [19,27,28,29,34,38]:
 mapping=json.loads((out/'asset-to-PDF-page.json').read_text());q=p[mapping[str(n)]-1];info=q.get_image_info(xrefs=True);assert len(info)==1;q.replace_image(info[0]['xref'],filename=str(r/f'assets/figure-{n:03}.png'))
tmp=out/'final-cleanup.pdf';p.save(tmp,garbage=4,deflate=True);p.close();source.close();q=f.open(tmp)
assert [len(x.get_links())for x in q]==counts
assert 'These summaries describe the available observed catch' not in q[8].get_text()
q[8].get_pixmap(matrix=f.Matrix(1.1,1.1)).save(str(out/'final-page-9.png'))
for n in [12,19,27,28,29,34,38,64]:q[json.loads((out/'asset-to-PDF-page.json').read_text())[str(n)]-1].get_pixmap(matrix=f.Matrix(.75,.75)).save(str(out/f'final-figure-{n}.png'))
q.close();tmp.replace(r/'assessment.pdf');(r/'assessment.html').write_text(s)
print('PASS: deleted coverage paragraph; page space closed; links retained')
