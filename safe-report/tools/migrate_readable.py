"""One-time migration of the released HTML to authored Markdown and CSV tables.

This is a migration utility, never part of the report build. Running it again
would replace subsequent author edits. The normal entry point is EBSpollock.qmd.
"""
from copy import deepcopy
import csv
from hashlib import sha256
import json
from pathlib import Path
import re
import subprocess
from lxml import etree, html

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parent / '2026/assessment.html'
assert sha256(SOURCE.read_bytes()).hexdigest() == 'a3d4258b607040ab2a90d215085e5ed84d0bc97fe0fd2497bc09c5442c14b082', 'This migration utility requires the November 10 baseline; do not overwrite authored QMD.'
WORK = ROOT / '_build/migration'
WORK.mkdir(parents=True, exist_ok=True)
dom = html.parse(str(SOURCE), parser=html.HTMLParser(huge_tree=True))
main = deepcopy(dom.xpath('//main')[0])
fields = []
replacements = {}
math_tokens = {}
link_ids = {a.get('href')[1:] for a in main.xpath('.//a[starts-with(@href,"#")]')}

def add_field(element):
    copy = deepcopy(element)
    copy.tag = 'div'
    copy.attrib.clear()
    clean(copy)
    copy.set('id', 'field-' + str(len(fields)))
    fields.append(copy)
    return len(fields) - 1

def clean(root):
    for e in list(root.xpath('.//script | .//style | .//button')):
        e.drop_tree()
    for e in list(root.xpath('.//math')):
        tex = e.xpath('.//annotation[@encoding="application/x-tex"]/text()')
        assert len(tex) == 1, html.tostring(e)
        tex[0]=re.sub(r'\n\s*\n','\n',tex[0].strip())
        token='SAFEMATH'+str(len(math_tokens))+'END'
        math_tokens[token]=('$$\n'+tex[0]+'\n$$') if e.get('display')=='block' else ('$'+tex[0]+'$')
        replacement=html.Element('span');replacement.text=token
        replacement.tail = e.tail
        e.getparent().replace(e, replacement)
    for e in list(root.iterdescendants()):
        if not isinstance(e.tag, str):
            e.drop_tree()
            continue
        keep = {k:v for k,v in e.attrib.items() if k in ('id','href','src','alt','colspan','rowspan','start')}
        if e.tag == 'img' and 'esp2026-logo' in e.get('class',''):
            keep['class'] = 'esp2026-logo'
        if e.tag == 'div' and e.get('id','').startswith('ref-'):
            keep['class'] = 'csl-entry'
        if e.tag in ('div','span') and keep.get('id') not in link_ids and not keep.get('id','').startswith('ref-'):
            keep.pop('id',None)
        e.attrib.clear(); e.attrib.update(keep)
    for e in reversed(list(root.xpath('.//span | .//div'))):
        if not e.attrib:
            e.drop_tag()

# The title and authors belong in YAML, rather than generated title-page markup.
header = main.xpath('./header')[0]
title = ' '.join(header.xpath('.//h1')[0].text_content().split())
authors = [' '.join(e.text_content().split()) for e in header.xpath('.//p[@class="author"]')]
affiliation = ' '.join(header.xpath('.//p[@class="affiliation"]')[0].text_content().split())
header.drop_tree()
for nav in main.xpath('./nav'):
    nav.drop_tree()
for callout in main.xpath('./div[contains(@class,"callout")]'):
    label = callout.get('title')
    body = callout.xpath('.//div[contains(@class,"callout-body-container")]')[0]
    token = 'SAFECALLOUT' + str(len(replacements))
    replacements[token] = ('callout', label, add_field(body))
    placeholder = html.Element('p'); placeholder.text = token; placeholder.tail = callout.tail
    callout.getparent().replace(callout, placeholder)

# Keep merged header cells, numerical strings and table notes as authored data.
tables = []
single_table_figures = set(main.xpath('.//figure[count(.//table)=1]'))
for group in main.xpath('.//figure[count(.//table)>1]'):
    group.tag='div'
    for caption in group.xpath('./figcaption'):
        caption.tag='p'; group.insert(0,caption)
for number, table in enumerate(list(main.xpath('.//table')), 1):
    owner = next((e for e in table.iterancestors() if e.get('id','').startswith(('tbl-','advice-','status-'))), None)
    identifier = owner.get('id') if owner is not None else table.get('id')
    identifier = identifier or f'table-{number:03d}'
    stem = f'{number:03d}-' + re.sub(r'[^a-zA-Z0-9_-]', '-', identifier)
    figure = next((e for e in table.iterancestors() if e in single_table_figures), None)
    captions = figure.xpath('./figcaption') if figure is not None else table.xpath('./caption')
    caption = add_field(captions[0]) if captions else None
    extra=None
    if figure is not None:
        extra_content=deepcopy(figure)
        for node in extra_content.xpath('.//table | ./figcaption | .//style'):node.drop_tree()
        if extra_content.text_content().strip():extra=add_field(extra_content)
    target = figure if figure is not None else table
    # Transfer the public anchor to the replacement so links keep working.
    if owner is not None:
        owner.attrib.pop('id', None)
    target.attrib.pop('id', None)
    rows = []; spans = {}; classes = {}; aligns = {}; ncols = 0
    groups = [('head', table.xpath('./thead/tr')), ('body', table.xpath('./tbody/tr')), ('foot', table.xpath('./tfoot/tr'))]
    for kind, group in groups:
        for row in group:
            cells = []
            total = 0
            for cnum, cell in enumerate(row.xpath('./th | ./td'), 1):
                cells.append(add_field(cell))
                key = f'{len(rows)+1}:{cnum}'
                cs, rs = int(cell.get('colspan',1)), int(cell.get('rowspan',1))
                if cs != 1 or rs != 1:
                    spans[key] = [rs,cs]
                total += cs
                original_class = cell.get('class','')
                selected = [x for x in original_class.split() if x in ('gt_title','gt_subtitle','gt_sourcenote','gt_footnote','gt_row_group')]
                if selected: classes[key] = ' '.join(selected)
                alignment = next((x[3:] for x in ('gt_right','gt_center','gt_left') if x in original_class), None)
                if alignment: aligns[key] = alignment
                # Explicit emphasis is retained in Markdown; highlight remains a style.
                if 'background' in cell.get('style',''):
                    classes[key] = (classes.get(key,'') + ' highlighted-cell').strip()
            rows.append(cells); ncols = max(ncols,total)
    layout = {'columns':ncols, 'header_rows':len(groups[0][1]), 'footer_rows':len(groups[2][1]),
              'spans':spans, 'classes':classes, 'align':aligns}
    # Compress the common column alignment into defaults.
    default_align = []
    for c in range(1,ncols+1):
        values=[v for k,v in aligns.items() if int(k.split(':')[1])==c]
        default_align.append(max(set(values),key=values.count) if values else 'left')
    layout['column_align']=default_align
    layout['align']={k:v for k,v in aligns.items() if v != default_align[int(k.split(':')[1])-1]}
    tables.append((stem, rows, layout))
    token = f'SAFETABLE{number:03d}'
    replacements[token] = ('table', identifier, stem, caption, extra)
    placeholder = html.Element('p'); placeholder.text = token; placeholder.tail = target.tail
    target.getparent().replace(target,placeholder)

# Figure panels, notes and captions stay together as an authored division.
for number, figure in enumerate(list(main.xpath('.//figure[.//img]')), 1):
    captions = figure.xpath('./figcaption')
    caption = add_field(captions[0]) if captions else None
    owner = next((e for e in figure.iterancestors() if e.get('id','').startswith(('fig-','esp2026-fig-'))), None)
    identifier = owner.get('id') if owner is not None else figure.get('id', f'figure-{number:03d}')
    if owner is not None: owner.attrib.pop('id',None)
    content = deepcopy(figure)
    for cap in content.xpath('./figcaption'):cap.drop_tree()
    images=[]
    for index,img in enumerate(content.xpath('.//img')):
        images.append((img.get('src'),img.get('alt','')))
        if img.getparent().tag=='a':img.getparent().drop_tag()
        marker=html.Element('span');marker.text=f'SAFEIMAGE{index}';marker.tail=img.tail
        img.getparent().replace(img,marker)
    field=add_field(content)
    token = f'SAFEFIGURE{number:03d}'
    replacements[token] = ('figure',identifier,images,caption,field)
    placeholder=html.Element('p');placeholder.text=token;placeholder.tail=figure.tail
    figure.getparent().replace(figure,placeholder)

# Standalone illustrations keep alt text without printing it as a caption.
for number,img in enumerate(list(main.xpath('.//img'))):
    token=f'SAFESTANDALONE{number}'
    replacements[token]=('image',img.get('src'),img.get('alt',''),'esp2026-logo' if 'esp2026-logo' in img.get('class','') else '')
    marker=html.Element('span');marker.text=token;marker.tail=img.tail
    img.getparent().replace(img,marker)

# A section's anchor belongs on its Markdown heading.
for section in list(main.xpath('.//section')):
    headings=section.xpath('./h1 | ./h2 | ./h3 | ./h4 | ./h5 | ./h6')
    if headings and section.get('id'):
        headings[0].set('id',section.get('id'))
        section.attrib.pop('id',None)
    if not section.get('id'):
        section.drop_tag()
for detail in main.xpath('.//details'):
    detail.tag='div';detail.set('class','supplementary-data')
    for summary in detail.xpath('./summary'):summary.tag='p'
clean(main)
# Retain supplementary-data as an authored division, without HTML details tags.
for e in list(main.xpath('.//div')):
    if e.get('id','').startswith('ref-'):continue
    if e.get('id'):
        marker=html.Element('span',id=e.get('id'));e.insert(0,marker)
    e.drop_tag()
main.tag='div';main.attrib.clear()
for heading in main.xpath('.//h1|.//h2|.//h3|.//h4|.//h5|.//h6'):
    if heading.get('id','').startswith('sec-prior-ssc') or heading.getparent().get('id')=='appendices':
        pass

field_html=html.Element('div')
for field in fields:field_html.append(field)
(WORK/'fields.html').write_bytes(html.tostring(field_html))
(WORK/'body.html').write_bytes(html.tostring(main))
for name in ['fields','body']:
    subprocess.run(['quarto','pandoc',str(WORK/(name+'.html')),'-f','html','-t','markdown','--wrap=none','-o',str(WORK/(name+'.md'))],check=True)
field_text=(WORK/'fields.md').read_text()
parsed={int(n):s.strip() for n,s in re.findall(r'::: \{#field-(\d+)\}\n(.*?)\n:::',field_text,re.S)}
assert len(parsed)==len(fields),(len(parsed),len(fields))
for key in parsed:
    if parsed[key]=='---':parsed[key]='—'
    if parsed[key]=='--':parsed[key]='–'
    for token,tex in math_tokens.items():parsed[key]=parsed[key].replace(token,tex)
for stem,rows,layout in tables:
    with (ROOT/'tables'/(stem+'.csv')).open('w',newline='') as output:
        writer=csv.writer(output);writer.writerow([f'cell_{i+1}' for i in range(max(map(len,rows)))])
        for row in rows:writer.writerow([parsed[i] for i in row])
    (ROOT/'tables'/(stem+'.json')).write_text(json.dumps(layout,ensure_ascii=False,indent=2)+'\n')
body=(WORK/'body.md').read_text()
lines=body.strip().splitlines()
if re.fullmatch(r':{3,} \{\}',lines[0]):
    assert re.fullmatch(r':{3,}',lines[-1]);body='\n'.join(lines[1:-1])
body=re.sub(r'\[(SAFEMATH\d+END)\]\{#([^}]+)\}',r'[]{#\2}\n\n\1',body)
for token,tex in math_tokens.items():body=body.replace(token,tex)
for token,spec in replacements.items():
    kind=spec[0]
    if kind=='callout':
        _,label,field=spec
        value=f'::: {{.callout-note title={json.dumps(label)}}}\n{parsed[field]}\n:::'
    elif kind=='table':
        _,identifier,stem,caption,extra=spec
        value=f'::: {{#{identifier} .report-table .unnumbered data-table="tables/{stem}.csv"}}\n'
        if caption is not None:value+='\n'+parsed[caption]+'\n'
        value+='\n:::'
        if extra is not None:value+='\n\n::: {.table-notes}\n'+parsed[extra]+'\n:::'
    elif kind=='image':
        _,path,alt,klass=spec
        value=f'![]({path})'+'{fig-alt='+json.dumps(alt,ensure_ascii=False)+(' .'+klass if klass else '')+'}'
    else:
        _,identifier,images,caption,field=spec
        content=parsed[field]
        for i,(path,alt) in enumerate(images):
            content=content.replace(f'SAFEIMAGE{i}',f'![]({path})'+'{fig-alt='+json.dumps(alt,ensure_ascii=False)+'}')
        value=f':::: {{#{identifier} .report-figure}}\n\n'+content+'\n'
        if caption is not None:value+='\n::: {.figure-caption}\n'+parsed[caption]+'\n:::\n'
        value+='\n::::'
    assert body.count(token)==1,(token,body.count(token))
    body=body.replace(token,value)
# Single-purpose spans from generated output add no authoring value.
body=re.sub(r'\n{3,}','\n\n',body)
import textwrap
paragraphs=[]
for paragraph in body.split('\n\n'):
    if '\n' not in paragraph and len(paragraph)>110 and not paragraph.startswith((':','!','[','<','#','$')):
        paragraph=textwrap.fill(paragraph,width=100,break_long_words=False,break_on_hyphens=False)
    paragraphs.append(paragraph)
body='\n\n'.join(paragraphs)
body='::: {.assessment-links}\n[Assessment homepage](../index.html) | [Download assessment PDF](assessment.pdf)\n:::\n\n'+body
yaml='---\ntitle: '+json.dumps(title)+'\nlang: en\nlightbox: true\nauthor:\n'
for author in authors:
    yaml+='  - name: '+json.dumps(author)+'\n'
    if author==authors[0]:yaml+='    affiliation: '+json.dumps(affiliation)+'\n'
yaml+='''output-file: assessment.html
format:
  html:
    toc: true
    toc-depth: 3
    number-sections: false
    html-math-method: mathml
    embed-resources: false
    css: report.css
    fig-cap-location: bottom
    tbl-cap-location: top
    code-fold: true
crossref: false
filters:
  - path: tables.lua
    at: pre-ast
---

<!-- Edit prose, captions and links here. Table values are in tables/*.csv;
     grouped headers and cell spans are in the matching tables/*.json files.
     Build both formats with: bash safe-report/render.sh -->

'''
(ROOT/'EBSpollock.qmd').write_text(yaml+body+'\n')
print('Created EBSpollock.qmd and',len(tables),'CSV tables from',len(fields),'editable fields')
