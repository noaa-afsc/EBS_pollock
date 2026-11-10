"""Apply authorized prose deletions and report-wide 'supplied' wording cleanup."""
from pathlib import Path
import re,sys,json,html,unicodedata,shutil,subprocess,io,hashlib
from PIL import Image
from lxml import html as lh
import pymupdf as f
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/prose-figure-cleanup-20261110'
a1='Following the approach described in Section 3.1.1 of the 2024 assessment, we summarize the mean weight of pollock within each source record and weight that record by its pollock catch.'
a2='The source extract extends through September 18, 2026, so the B-season results describe the available portion of the season. Catcher-vessel observations exclude electronic-monitoring records and represent only part of the shoreside fleet. Annual panels use a source-week boundary at week 24; the weekly panels cover weeks 2–15 and 25–40. These plotting windows differ from the regulatory season dates. Ridge heights are scaled to reveal distribution shape; catch totals should be compared using the underlying bin totals.'
def clean(t):
 for a,b in [('and supplied as','and used as'),('supplied privately','provided privately'),('remain to be supplied','remain to be documented'),('normalization supplied with','normalization in'),('mappings are supplied in','mappings are in'),('Index units as supplied','Index units')]:t=t.replace(a,b)
 t=re.sub(r'\bSupplied\s+([a-z])',lambda m:m[1].upper(),t)
 return re.sub(r'\bsupplied\s*','',t,flags=re.I)
s=(r/'assessment.html').read_text()
for a in [a1,a2]:assert s.count(a)==1;s=s.replace(a+' ','').replace(a,'')
s=clean(s);dom=lh.fromstring(s,parser=lh.HTMLParser(huge_tree=True))
p=f.open(r/'assessment.pdf');old_toc=p.get_toc();old_links=[len(q.get_links())for q in p];original_images=[[x['digest'].hex()for x in q.get_image_info(hashes=True)]for q in p]
asset_pages={};image_index={}
for i,page in enumerate(p):
 for info in page.get_image_info(hashes=True):image_index.setdefault(info['digest'].hex(),[]).append(i+1)
for n in [12,16,17,19,27,28,29,34,38,64]:
 data=subprocess.check_output(['git','show',f'HEAD:2026/assets/figure-{n:03}.png'],cwd=ROOT);image=Image.open(io.BytesIO(data)).convert('RGB');matches=image_index.get(hashlib.md5(image.tobytes()).hexdigest(),[])
 if n==12 and not matches:
  matches=[i+1 for i,page in enumerate(p)if re.search(r'Figure\s+12:',page.get_text()) and any((v['width'],v['height'])==image.size for v in page.get_image_info())]
 assert len(matches)==1,(n,matches);asset_pages[n]=matches[0]
(out/'asset-to-PDF-page.json').write_text(json.dumps(asset_pages,indent=2))
# Reflow the shortened three-paragraph section, preserving its three figure links.
q=p[8];area=f.Rect(50.49,382.5,565,591.5);saved=[dict(l)for l in q.get_links()if area.intersects(l['from'])];assert len(saved)==3
q.add_redact_annot(area,fill=False,cross_out=False);q.apply_redactions(images=0,graphics=0)
paras=dom.xpath('//*[@id="seasonal-changes-in-fish-size"]/p');assert len(paras)==3
content=''.join('<p>'+html.escape(''.join(e.itertext()).strip())+'</p>'for e in paras)
fontdir=Path('/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode');css='@font-face{font-family:CMU;src:url(cmunrm.otf)}body{font-family:CMU;font-size:10.2pt;line-height:1.2;margin:0}p{margin:0 0 8pt 0}'
spare,scale=q.insert_htmlbox(area,content,css=css,archive=f.Archive(str(fontdir)),scale_low=1);assert spare>=0 and scale==1
for l in saved:
 n={108:8,109:9,110:10}[l['page']];hits=[x for x in q.search_for('Figure '+str(n))if area.intersects(x)];assert len(hits)==1,(n,hits)
 data={k:v for k,v in l.items()if k in ['kind','page','to','zoom']};data['from']=hits[0];q.insert_link(data)
# Retain page geometry while removing 'supplied' from the remaining PDF text.
sys.argv=['font-conversion','--cm-font-dir',str(fontdir)]
source=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0].replace("text=sp['text'].replace('\\x00','⁻')","text=cleanup_text(sp['text'].replace('\\x00','⁻'))")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'cleanup_text':clean};exec(source,ns);edited=[]
for i,q in enumerate(p):
 if re.search(r'\bsupplied\b',q.get_text(),re.I):ns['transform'](q);edited.append(i+1)
# Apply the revised labels from saved data or unchanged vector plot geometry.
shutil.copy2(out/'ats-age-composition.png',r/'assets/figure-016.png')
changed_figures=[12,16,17,19,27,28,29,34,38,64];replaced=[]
for i,q in enumerate(p):
 for info in q.get_image_info(xrefs=True):
  for n in changed_figures:
   old=r/'assets'/f'figure-{n:03}.png'
   # Figure n occurs on its dedicated main figure page (101+n).
   if i+1==asset_pages[n]:
    assert len(q.get_image_info())==1,(n,i)
    q.replace_image(info['xref'],filename=str(old));replaced.append(n)
assert sorted(replaced)==sorted(changed_figures),replaced
tmp=out/'assessment.pdf';p.save(tmp,garbage=4,deflate=True);p.close();q=f.open(tmp)
assert len(q)==280 and q.get_toc()==old_toc
assert [len(x.get_links())for x in q]==old_links
alltext=unicodedata.normalize('NFKC',' '.join(' '.join(x.get_text().split())for x in q))
assert a1 not in alltext and 'The source extract extends through September 18' not in alltext
assert not re.search(r'\bsupplied\b',alltext,re.I)
for i in [8,116,117]:q[i].get_pixmap(matrix=f.Matrix(1,1)).save(str(out/f'page-{i+1}.png'))
q.close();tmp.replace(r/'assessment.pdf');(r/'assessment.html').write_text(s)
(out/'checks.json').write_text(json.dumps({'prose_deletions':2,'supplied_remaining_in_HTML_PDF_text':0,'changed_figures':changed_figures,'pages':280,'all_links_and_bookmarks_preserved':True,'manual_accessibility':'Not Tested'},indent=2))
print('PASS: prose deletions, wording cleanup and ten revised graphics')
