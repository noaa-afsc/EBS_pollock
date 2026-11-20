"""Compare readable-source HTML with the published report at content level."""
from collections import Counter
from hashlib import sha256
import json
from pathlib import Path
import re
import unicodedata
from lxml import html

ROOT=Path(__file__).resolve().parent

def norm(value):
    value=unicodedata.normalize('NFKC',value)
    value=value.translate(str.maketrans({'’':"'",'‘':"'",'“':'"','”':'"','–':'-','—':'-','‑':'-','−':'-','\u00ad':''}))
    return re.sub(r'\s+','',value)

def load(path):
    dom=html.parse(str(path),parser=html.HTMLParser(huge_tree=True))
    main=dom.xpath('//main')[0]
    # Render time varies across machines and builds; compare authored content.
    for element in main.xpath('.//script | .//style | .//*[@class="assessment-timestamp"]'):
        element.drop_tree()
    return main

def table_rows(table):
    return [[(int(cell.get('rowspan',1)),int(cell.get('colspan',1)),norm(cell.text_content()))
             for cell in row.xpath('./th | ./td')]
            for row in table.xpath('./thead/tr | ./tbody/tr | ./tfoot/tr')]

def verify():
    baseline=load(ROOT.parent/'2026/assessment.html')
    candidate=load(ROOT/'_build/2026/assessment.html')
    whole=norm(candidate.text_content())
    paragraphs=[e for e in baseline.xpath('.//p | .//figcaption | .//h1 | .//h2 | .//h3 | .//h4 | .//h5 | .//li')
                if not e.xpath('.//math')]
    missing=[e.text_content().strip() for e in paragraphs if norm(e.text_content()) and norm(e.text_content()) not in whole]
    before=baseline.xpath('.//table');after=candidate.xpath('.//table')
    differences=[]
    if len(before)!=len(after):differences.append({'table_count':[len(before),len(after)]})
    else:
        for number,(a,b) in enumerate(zip(before,after),1):
            x,y=table_rows(a),table_rows(b)
            if x!=y:
                changed=[i+1 for i,(u,v) in enumerate(zip(x,y)) if u!=v]
                differences.append({'table':number,'rows':[len(x),len(y)],'changed_rows':changed,
                                    'example_before':x[changed[0]-1] if changed else None,
                                    'example_after':y[changed[0]-1] if changed else None})
    images_before=[(e.get('src'),norm(e.get('alt',''))) for e in baseline.xpath('.//img')]
    images_after=[(e.get('src'),norm(e.get('alt',''))) for e in candidate.xpath('.//img')]
    equations_before=Counter(norm(e.text_content()) for e in baseline.xpath('.//annotation[@encoding="application/x-tex"]'))
    equations_after=Counter(norm(e.text_content()) for e in candidate.xpath('.//annotation[@encoding="application/x-tex"]'))
    # Ignore generated image-viewer links: their images and descriptions have their own check.
    links_before=Counter((e.get('href'),norm(e.text_content())) for e in baseline.xpath('.//a[@href][not(.//img)]'))
    links_after=Counter((e.get('href'),norm(e.text_content())) for e in candidate.xpath('.//a[@href][not(.//img)]'))
    lost_links=list((links_before-links_after).elements())
    ids=Counter(candidate.xpath('.//@id'))
    broken=sorted(set(e.get('href') for e in candidate.xpath('.//a[starts-with(@href,"#")]') if e.get('href')[1:] not in ids))
    duplicate_ids=[key for key,value in ids.items() if value>1]
    result={'source':'EBSpollock.qmd','source_sha256':sha256((ROOT/'EBSpollock.qmd').read_bytes()).hexdigest(),
      'reference_html_sha256':sha256((ROOT.parent/'2026/assessment.html').read_bytes()).hexdigest(),
      'reference_version':json.loads((ROOT.parent/'2026/release.json').read_text())['version'],
      'html_sha256':sha256((ROOT/'_build/2026/assessment.html').read_bytes()).hexdigest(),
      'paragraphs_and_headings_checked':len(paragraphs),'missing_text':missing,
      'tables':[len(before),len(after)],'table_differences':differences,
      'images':[len(images_before),len(images_after)],'image_content_and_alt_match':images_before==images_after,
      'missing_images':list((Counter(images_before)-Counter(images_after)).elements()),
      'equations':[sum(equations_before.values()),sum(equations_after.values())],
      'equations_match':equations_before==equations_after,'missing_equations':list((equations_before-equations_after).elements()),
      'missing_links':lost_links,'broken_fragment_links':broken,'duplicate_ids':duplicate_ids}
    result['passed']=not(missing or differences or lost_links or broken or duplicate_ids) and result['image_content_and_alt_match'] and result['equations_match']
    (ROOT/'_build/readable-source-comparison.json').write_text(json.dumps(result,ensure_ascii=False,indent=2)+'\n')
    print(json.dumps({k:result[k] for k in ['passed','tables','images','equations','image_content_and_alt_match','equations_match']},indent=2))
    print('Missing text:',len(missing),'table differences:',len(differences),'missing links:',len(lost_links),'broken anchors:',len(broken))
    return result

if __name__=='__main__':
    raise SystemExit(0 if verify()['passed'] else 1)
