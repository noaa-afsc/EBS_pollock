"""Use embedded Computer Modern Unicode while retaining the saved page layout.

The existing line positions, table cell widths, superscripts/subscripts, figures,
links and bookmarks are retained. Each text span is fitted to its existing width.
Rare mathematical symbols use the companion math/Unicode font, not missing glyphs.
"""
from pathlib import Path
import argparse,collections,json,hashlib,math,re,unicodedata
import pymupdf as fitz
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026'
parser=argparse.ArgumentParser();parser.add_argument('--cm-font-dir',required=True);parser.add_argument('--preview',action='store_true');args=parser.parse_args()
fontdir=Path(args.cm_font_dir)
paths={k:fontdir/n for k,n in [('regular','cmunrm.otf'),('bold','cmunbx.otf'),('italic','cmunti.otf'),('bolditalic','cmunbi.otf')]}
paths['math']=Path('/Users/jim/Library/Fonts/latinmodern-math.otf');paths['unicode']=Path('/System/Library/Fonts/Supplemental/Arial Unicode.ttf')
fonts={k:fitz.Font(fontfile=str(v)) for k,v in paths.items()};aliases={k:'assessmentCM'+k for k in paths}
def style(sp):
 name=sp['font'].lower();bold=bool(sp['flags']&16) or 'bold' in name;italic=bool(sp['flags']&2) or 'italic' in name
 return 'bolditalic' if bold and italic else 'bold' if bold else 'italic' if italic else 'regular'
def runs(text,key):
 out=[]
 for ch in text:
  use=key if fonts[key].has_glyph(ord(ch)) else 'math' if fonts['math'].has_glyph(ord(ch)) else 'unicode'
  assert fonts[use].has_glyph(ord(ch)),(repr(ch),hex(ord(ch)))
  if out and out[-1][0]==use:out[-1]=(use,out[-1][1]+ch)
  else:out.append((use,ch))
 return out
stats=collections.Counter();scales=[]
def transform(page):
 spans=[]
 for block in page.get_text('dict')['blocks']:
  for line in block.get('lines',[]):
   for sp in line['spans']:
    if not sp['text']:continue
    text=sp['text'].replace('\x00','⁻') # Four missing superscript-minus mappings, confirmed against current HTML units.
    stats['unit_superscript_minus_mappings_recovered']+=sp['text'].count('\x00')
    direction=tuple(round(v,4)for v in line['dir']);assert direction in [(1.,0.),(0.,-1.)],direction
    groups=runs(text,style(sp));lengths=[fonts[k].text_length(t,fontsize=sp['size']) for k,t in groups];length=sum(lengths)
    width=(sp['bbox'][2]-sp['origin'][0]) if direction==(1.,0.) else sp['origin'][1]-sp['bbox'][1]
    scale=width/length if length>0 and width>0 else 1
    assert math.isfinite(scale) and .2<scale<5,(text,scale)
    spans.append((sp,direction,groups,lengths,scale));scales.append(scale)
 links=[dict(l) for l in page.get_links()]
 page.add_redact_annot(page.rect,fill=False,cross_out=False);page.apply_redactions(images=0,graphics=0)
 for l in page.get_links():page.delete_link(l)
 used={k for _,_,gs,_,_ in spans for k,_ in gs}
 for key in used:page.insert_font(fontname=aliases[key],fontfile=str(paths[key]))
 for sp,direction,gs,lengths,scale in spans:
  offset=0
  for (key,text),length in zip(gs,lengths):
   origin=(sp['origin'][0]+offset*scale,sp['origin'][1]) if direction==(1.,0.) else (sp['origin'][0],sp['origin'][1]-offset*scale)
   matrix=fitz.Matrix(scale,1) if direction==(1.,0.) else fitz.Matrix(1,scale)
   page.insert_text(origin,text,fontname=aliases[key],fontsize=sp['size'],color=fitz.sRGB_to_pdf(sp['color']),rotate=0 if direction==(1.,0.) else 90,morph=(fitz.Point(origin),matrix))
   stats['characters_'+key]+=len(text);offset+=length
  stats['spans_replaced']+=1
 for l in links:
  clean={k:v for k,v in l.items() if k in ['kind','from','page','to','uri','file','zoom','name']}
  page.insert_link(clean)
 page.clean_contents(sanitize=True)

p=fitz.open(R/'assessment.pdf');preview=[2,11,42,85,227]
if args.preview:
 for i in preview:
  transform(p[i]);p[i].get_pixmap(matrix=fitz.Matrix(1.3,1.3)).save('/tmp/cm-preview-'+str(i+1)+'.png')
 print('Computer Modern preview pages:',[i+1 for i in preview]);raise SystemExit
# Preserve the current artifact outside the publication resources while validating.
backup=Path('/tmp/assessment-before-computer-modern.pdf');backup.write_bytes((R/'assessment.pdf').read_bytes())
old_toc=p.get_toc();old_links=[[(l['kind'],tuple(l['from']),l.get('page'),str(l.get('to')),l.get('uri'))for l in page.get_links()]for page in p]
original_text=[page.get_text() for page in p]
original_images=[[v['digest'].hex()for v in page.get_image_info(hashes=True)]for page in p]
for i,page in enumerate(p):
 transform(page)
 if (i+1)%25==0:print('Converted',i+1,'pages',flush=True)
p.subset_fonts();tmp=R/'assessment-computer-modern-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_fonts=True,deflate_images=True);p.close()
q=fitz.open(tmp);assert q.get_toc()==old_toc and len(q)==len(original_text)
def canonical(text):
 text=unicodedata.normalize('NFKC',text.replace('\x00','⁻')).replace('\xad','-').replace('‐','-')
 return collections.Counter(ch for ch in text if not ch.isspace())
used_fonts=collections.Counter()
for i,page in enumerate(q):
 assert canonical(page.get_text())==canonical(original_text[i]),('text difference',i+1,canonical(page.get_text())-canonical(original_text[i]),canonical(original_text[i])-canonical(page.get_text()))
 assert [v['digest'].hex()for v in page.get_image_info(hashes=True)]==original_images[i],('raster figure difference',i+1)
 current=[(l['kind'],tuple(l['from']),l.get('page'),str(l.get('to')),l.get('uri'))for l in page.get_links()];assert current==old_links[i],('link difference',i+1)
 for block in page.get_text('dict')['blocks']:
  for line in block.get('lines',[]):
   for sp in line['spans']:used_fonts[sp['font']]+=len(sp['text'])
for i in preview:q[i].get_pixmap(matrix=fitz.Matrix(1.3,1.3)).save('/tmp/cm-final-'+str(i+1)+'.png')
checks={'pages':len(q),'all_page_character_content_verified':True,'all_links_and_bookmarks_verified':True,'all_raster_figure_pixel_hashes_verified':True,'used_fonts_characters':dict(used_fonts),'font_family':'Computer Modern Unicode Serif','math_symbols':'Latin Modern Math','rare_unicode_symbols':'Unicode fallback','source_line_and_table_geometry_retained':True,'horizontal_fit_range':[min(scales),max(scales)],'counts':dict(stats),'manual_accessibility':'Not Tested'}
q.close();tmp.replace(R/'assessment.pdf');v=R/'validation/computer-modern-20261109';v.mkdir(exist_ok=True);(v/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-computer-modern';m['PDF_typography']=checks;m['files']['assessment.pdf']=hashlib.sha256((R/'assessment.pdf').read_bytes()).hexdigest();(R/'release.json').write_text(json.dumps(m,indent=2)+'\n');print(json.dumps(checks,indent=2))
