"""Shorten labels in identified vector graphics, preserving their plotted geometry."""
from pathlib import Path
from lxml import etree
from PIL import Image
import argparse,re,json,subprocess
import pymupdf as f
ap=argparse.ArgumentParser();ap.add_argument('--sources',type=Path,required=True);args=ap.parse_args();root=Path(__file__).resolve().parents[1];out=root/'2026/validation/editorial-review-20261110';m=json.loads(args.sources.read_text());m['2']=str(Path(m['2']).with_suffix('.pdf'))
def change(n,t):
 original=t
 if n==1:
  t=t.replace('Qualifying CV records','Catcher-vessel records').replace('source hauls','hauls').replace('Source: inseas26.csv. ','').replace('the preliminary terminal year','preliminary 2026 observations')
 if n==2 and 'Provisional summary of saved sampler' in t:return ''
 if n==4:
  if 'October completeness is unverified' in t:return ''
  t=t.replace('by calendar season group','by season')
  if 'A: January-May. B: June-December. Open markers' in t:t='A: January-May. B: June-December.'
 if n==11:
  if 'Annual bars avoid interpolating historical splits' in t:return ''
  if 'Seasons use week-ending dates:' in t:t='2026 catch is partial through October 17; the annual completion forecast is excluded.'
 if n==23:
  t=t.replace('Each fit inherits the preceding updates; model formulation is retained.','Each fit includes the preceding data updates; all use Model 23.0.')
  t=t.replace('Input releases combine added observations, historical revisions and stated uncertainty updates.','Data updates include new observations, historical revisions and revised observation errors.')
 if n in [24,25,26]:
  t=t.replace('Final saved fits','Fits').replace('saved point projections','point projections').replace('saved local normal approximation','local normal approximation')
  t=t.replace(' Each saved projection retains its verified assumptions.','')
  if 'Selected stages omit separate weight-at-age' in t or 'Separate weight-at-age and A-season weight stages' in t:return ''
 if n==48:
  if 'native OSA values and assessment weights are preserved' in t:return ''
  t=t.replace('pseudo sample','placeholder sample')
 if n==54:
  t=t.replace('the selected assessment sample sizes remain unchanged','assessment sample sizes are unchanged').replace('retained substitute','age-composition placeholder')
 if n==55:t=t.replace('Retained 2020 substitute','2020 ATS placeholder').replace('retained substitute','age-composition placeholder')
 t=t.replace('tonnes','tons')
 return t
records=[]
for n,path in m.items():
 n=int(n);source=Path(path);asset=root/f'2026/assets/figure-{n:03}.png';w,h=Image.open(asset).size;count=0
 if source.suffix=='.svg':
  tree=etree.parse(str(source));before=[etree.tostring(e)for e in tree.iter()if etree.QName(e).localname not in ['text','svg','g','defs','clipPath']]
  for e in list(tree.iter()):
   if etree.QName(e).localname!='text' or not e.text:continue
   new=change(n,e.text)
   if new==e.text:continue
   count+=1
   if not new:e.getparent().remove(e)
   else:e.text=new;e.attrib.pop('textLength',None);e.attrib.pop('lengthAdjust',None)
  after=[etree.tostring(e)for e in tree.iter()if etree.QName(e).localname not in ['text','svg','g','defs','clipPath']];assert before==after,n
  assert count,(n,source)
  dst=out/f'label-asset-{n:03}.svg';tree.write(str(dst),encoding='utf-8',xml_declaration=True)
  subprocess.run(['rsvg-convert','-w',str(w),'-h',str(h),'-o',str(asset),str(dst)],check=True)
 else:
  p=f.open(source);q=p[0];edits=[];drawings=q.get_drawings()
  for block in q.get_text('dict')['blocks']:
   for line in block.get('lines',[]):
    for sp in line['spans']:
     new=change(n,sp['text'])
     if new!=sp['text']:edits.append((sp,new,tuple(line['dir'])));q.add_redact_annot(f.Rect(sp['bbox']),fill=False,cross_out=False)
  assert edits,n;q.apply_redactions(images=0,graphics=0)
  for sp,new,direction in edits:
   if new:q.insert_text(sp['origin'],new,fontname='hebo'if sp['flags']&16 else'helv',fontsize=sp['size'],color=f.sRGB_to_pdf(sp['color']),rotate=90 if direction==(0.0,-1.0)else 0)
  assert q.get_drawings()==drawings,('vector geometry',n)
  dst=out/f'label-asset-{n:03}.pdf';p.save(dst,garbage=4,deflate=True);p.close();count=len(edits)
  subprocess.run(['pdftoppm','-f','1','-singlefile','-scale-to-x',str(w),'-scale-to-y',str(h),'-png',str(dst),str(asset.with_suffix(''))],check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
 records.append({'asset':n,'changed_text_elements':count,'vector_plot_geometry_preserved':True})
(out/'graphic-vector-checks.json').write_text(json.dumps(records,indent=2));print('PASS: edited',len(records),'vector graphics without changing their plotted geometry')
