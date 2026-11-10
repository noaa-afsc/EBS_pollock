"""Revise vector plot text, retaining all non-text SVG elements and plot geometry."""
from pathlib import Path
from lxml import etree
from PIL import Image
import re,subprocess,json,argparse
ROOT=Path(__file__).resolve().parents[1];out=ROOT/'2026/validation/prose-figure-cleanup-20261110';out.mkdir(exist_ok=True)
ap=argparse.ArgumentParser();ap.add_argument('--private-source',type=Path,required=True);ap.add_argument('--residual-svg',type=Path,required=True);args=ap.parse_args();source_root=args.private_source
items={19:source_root/'surveys/wiki/assets/ats/ats-biomass-series.svg',27:ROOT/'2026/tables/terminology-tons/model23_0_vs23_2_uncertainty.svg',28:ROOT/'2026/tables/terminology-tons/model23_2_vs26_2_uncertainty.svg',29:ROOT/'2026/tables/terminology-tons/population-uncertainty-tons.svg',34:ROOT/'2026/tables/weight-age-revision-20261109/model26_2_biological_schedules.svg',38:args.residual_svg}
records=[]
for n,source in items.items():
 tree=etree.parse(str(source));geometry=[etree.tostring(e)for e in tree.iter()if etree.QName(e).localname not in ['text','svg','g','defs','clipPath']]
 count=0
 for e in tree.iter():
  if etree.QName(e).localname!='text' or not e.text:continue
  old=e.text;new=re.sub(r'\bSupplied\s+([a-z])',lambda m:m[1].upper(),old);new=re.sub(r'\bsupplied\s+','',new,flags=re.I)
  if old.startswith('Supplied '):new=new[:1].upper()+new[1:]
  if n==19:new=new.replace('tonnes','tons')
  if old!=new:
   e.text=new;e.attrib.pop('textLength',None);e.attrib.pop('lengthAdjust',None);count+=1
 assert count,n
 assert geometry==[etree.tostring(e)for e in tree.iter()if etree.QName(e).localname not in ['text','svg','g','defs','clipPath']]
 svg=out/f'figure-{n:03}.svg';tree.write(str(svg),encoding='utf-8',xml_declaration=True)
 asset=ROOT/f'2026/assets/figure-{n:03}.png';w,h=Image.open(asset).size
 subprocess.run(['rsvg-convert','-w',str(w),'-h',str(h),'-o',str(asset),str(svg)],check=True)
 records.append({'figure':n,'edited_text_nodes':count,'all_nontext_SVG_elements_identical':True})
(out/'SVG-checks.json').write_text(json.dumps(records,indent=2))
