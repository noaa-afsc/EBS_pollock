"""Insert saved-result weight figure into canonical HTML and PDF; no estimation."""
from pathlib import Path
import json, hashlib, shutil, html
from bs4 import BeautifulSoup
import pymupdf as fitz
root=Path(__file__).resolve().parents[1]; report=root/'2026'
audit=report/'validation/waa-figure-20261109';audit.mkdir(exist_ok=True)
backup=Path('/tmp/waa-figure-before-20261109');backup.mkdir(exist_ok=True)
for name in ['assessment.html','assessment.pdf','release.json']:
    dest=backup/('before-'+name)
    if not dest.exists():shutil.copy2(report/name,dest)
caption=('Weight at age after omitting 2026 BTS weight observations from the standalone random-effects fit. '
         '(A) Full history; (B) recent years, with cell labels in kg per fish. Ages 3–10 are shown. '
         'Fishery and BTS observations end in 2025. The right-hand panels show fishery-scale fitted weights, '
         '2026 estimates without weight observations, and 2027–2028 forecasts. The solid line marks 2026; '
         'dashed boxes mark the forecast years. Colours show proportional deviations from each age’s '
         'mean through 2025 within its own panel; the scale saturates at ±50%. Blank cells indicate absent observations. '
         'The 2020 BTS gap is retained. This display describes the selected fit; a comparison of fits is required to isolate the effect of omitting 2026 BTS weights.')
alt=('Two sets of three heatmaps compare observed fishery weights, observed BTS weights, and predicted fishery-scale weights. '
     'The historical view spans 1970–2028; the labelled detail spans 2014–2028. Observations stop in 2025. '
     'The predicted 2026 age-3 weight is 0.37 kg; that cohort reaches 0.48 kg at age 4 in 2027 and 0.60 kg at age 5 in 2028. '
     'Solid and dashed boundaries distinguish current-year estimates and forecasts.')
intro=('Figure 22a summarizes the observed weights and the selected standalone fit after omitting 2026 BTS weight observations. '
       'It shows the historical patterns and recent-year detail; the forecasts track the 2026 age-3 cohort from 0.368 kg to '
       '0.482 kg at age 4 in 2027 and 0.601 kg at age 5 in 2028.')
s=(backup/'before-assessment.html').read_text();soup=BeautifulSoup(s,'html.parser');section=soup.find(id='sec-fishery-weights');assert section
fragment=f'''<p>{intro.replace('Figure 22a','<a href="#fig-waa-omit-bts2026">Figure 22a</a>')}</p>
<div id="fig-waa-omit-bts2026" class="quarto-figure quarto-figure-center"><figure class="figure">
<a href="assets/waa-omit-bts2026.png" class="lightbox" data-gallery="waa-omit-bts2026" title="Figure 22a: Weight at age after omitting 2026 BTS weights"><img src="assets/waa-omit-bts2026.png" class="img-fluid figure-img" alt="{html.escape(alt,quote=True)}" style="width:100%"></a>
<figcaption class="figure-caption" data-safe-caption-layout="label-hanging-text"><span class="safe-caption-label">Figure 22a:</span> <span class="safe-caption-text">{caption}</span></figcaption></figure></div>
<p><a href="tables/waa-omit-bts2026-figure.csv">Download the figure data, including mean weights and anomaly reference means</a>.</p>'''
# Preserve original bytes outside the exact subsection.
start=s.index('id="sec-fishery-weights"');end=s.index('</section>',start)
s=s[:end]+fragment+'\n'+s[end:];(report/'assessment.html').write_text(s)
pdf=fitz.open(backup/'before-assessment.pdf');n=len(pdf);old_toc=pdf.get_toc();insert_at=18
# Capture internal link destinations to verify the insertion preserves targets.
before_links=[(i,l['page']) for i,p in enumerate(pdf) for l in p.get_links() if l.get('kind')==fitz.LINK_GOTO and l.get('page',-1)>=0]
pdf.new_page(pno=insert_at,width=612,height=792);page=pdf[insert_at]
page.insert_text((36,34),'4.3.2 Annual fishery weight at age (continued)',fontsize=10,fontname='hebo')
fig=fitz.open(report/'tables/waa-omit-bts2026.pdf');page.show_pdf_page(fitz.Rect(32,42,580,663),fig,0)
# HTML caption used verbatim; PDF uses Unicode-capable HTML text and hanging label layout.
css='body {font-family: sans-serif; font-size: 8.5pt; line-height:1.16} p {margin:0 0 5pt 0} .caption {padding-left:49pt;text-indent:-49pt}'
r=page.insert_htmlbox(fitz.Rect(36,663,576,740),f'<p class="caption"><b>Figure 22a:</b> {caption}</p>',css=css);assert r[0]>=0,r
page.insert_text((295,762),'18a',fontsize=10)
pdf.set_toc([[level,title,pno+(pno>insert_at)] for level,title,pno in old_toc])
tmp=report/'assessment-waa-tmp.pdf';pdf.save(tmp,garbage=3,deflate=True);pdf.close();tmp.replace(report/'assessment.pdf')
new=fitz.open(report/'assessment.pdf');assert len(new)==n+1
# Compare every original internal link with the corresponding shifted page and target.
link_checks=[]
for source,target in before_links:
    now_source=source+(source>=insert_at);now_target=target+(target>=insert_at)
    assert any(l.get('kind')==fitz.LINK_GOTO and l.get('page')==now_target for l in new[now_source].get_links()),(source,target)
    link_checks.append(1)
new[insert_at].get_pixmap(matrix=fitz.Matrix(1.5,1.5)).save(audit/'inserted-page.png')
assert 'Figure 22a' in new[insert_at].get_text()
r=json.loads((report/'release.json').read_text());r['pages']=len(new);r['images']=len(BeautifulSoup(s,'html.parser').find_all('img'))
r['version']='2026-11-09-weight-figure';r['files']={name:hashlib.sha256((report/name).read_bytes()).hexdigest() for name in ['assessment.html','assessment.pdf']}
r['weight_at_age_figure']={'id':'fig-waa-omit-bts2026','number':'22a','section':'4.3.2','fit':'omit_bts_2026_20261109/attempt-001','data':'tables/waa-omit-bts2026-figure.csv','caption':caption,'PDF_printed_page':'18a','preserved_internal_PDF_links':len(link_checks),'manual_accessibility':'Not Tested'}
(report/'release.json').write_text(json.dumps(r,indent=2)+'\n')
(audit/'checks.json').write_text(json.dumps({'pages':len(new),'original_internal_links_preserved':len(link_checks),'image_alt':alt,'caption':caption,'models_rerun':False,'manual_accessibility':'Not Tested'},indent=2)+'\n')
print('Inserted figure; PDF pages:',len(new),'internal links preserved:',len(link_checks))
