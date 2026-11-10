"""Apply approved assessment editorial revisions to the published HTML baseline."""
from pathlib import Path
from bs4 import BeautifulSoup,NavigableString,Comment
import re,json,subprocess
R=Path(__file__).resolve().parents[1]/'2026';path=R/'assessment.html'
s=BeautifulSoup(subprocess.check_output(['git','show','8497cae:2026/assessment.html'],cwd=R.parent),'html.parser')
def content(el,html):
 el.clear()
 for x in list(BeautifulSoup(html,'html.parser').contents):el.append(x)
def link(id,label):return f'<a href="#{id}">{label}</a>'
def cite(id,label):return f'<span class="citation" data-cites="{id}"><a href="#ref-{id}" role="doc-biblioref">{label}</a></span>'
# Global terminology in visible text and accessibility labels; retain stable IDs/URLs.
def terms(t):
 t=re.sub(r'\bweight at age\b',lambda m:'Weight-at-age' if m[0][0].isupper() else 'weight-at-age',t,flags=re.I)
 t=re.sub(r'\bnatural-mortality\b',lambda m:'Natural mortality' if m[0][0].isupper() else 'natural mortality',t,flags=re.I)
 t=re.sub(r'\bbase case\b',lambda m:'Base model' if m[0][0].isupper() else 'base model',t,flags=re.I)
 t=t.replace('ages-3+ biomass','age 3+ biomass')
 t=t.replace('assessment vintages','assessments').replace('earlier source vintages','earlier source dates').replace('These are distinct vintages.','These series use different data updates.')
 return re.sub(r'\bvintages\b\s*','',t,flags=re.I)
for t in list(s.find_all(string=True)):
 if not isinstance(t,Comment) and t.parent.name not in ['style','script']:t.replace_with(terms(str(t)))
for e in s.find_all(True):
 for a in ['alt','title','aria-label']:
  if e.has_attr(a):e[a]=terms(e[a])
for id,html in [
 ('changes-in-assessment-inputs','<p>Catch, fishery age compositions, survey indices and age compositions, and weight-at-age schedules have been updated since 2024. '+link('changes-inputs-detail','Section 2.1.1')+' describes the years, sources and data treatments.</p>'),
 ('changes-in-assessment-methods','<p>Model 26.0 implements the age-structured assessment in Rceattle and uses weight-at-age schedules estimated by a separate random-effects model. '+link('changes-methods-detail','Section 2.1.2')+' describes these changes from Model 23.0.</p>')]:
 e=s.find(id=id)
 for x in list(e.children):
  if getattr(x,'name',None)!='h2':x.extract()
 for x in list(BeautifulSoup(html,'html.parser').contents):e.append(x)
e=s.find(id='summary-of-results');ps=e.find_all('p',recursive=False)
content(ps[0],'Estimated total biomass in 2026 is 12.162 million t (approximate 95% log-scale Wald confidence interval 9.534–15.515 million t). Female spawning biomass (SSB) is 3.057 million t (2.489–3.754 million t, using the same interval method). The largest fishing mortality rate among ages is 0.3 per year.')
content(ps[1],'Scenario 2, the expected-catch projection scenario, assumes catches of 1.375 million t in each of 2027 and 2028, equal to the specified 2027 annual TAC. The projections use mean 2023–2025 fishery selectivity, fixed biological inputs and recruitment equal to the fitted 1978–2023 mean ('+link('sec-projections','Section 7')+').')
content(ps[3],'The final specifications for 2026 and 2027, issued in the absence of a 2025 stock assessment, specified the Bering Sea subarea pollock OFL at 2,496,000 t and ABC at 2,036,000 t; specified annual TAC is 1,375,000 t. These are the adopted specifications, separate from the recommendations from this assessment.')
g=s.find(id='general');first=g.find('p',recursive=False)
for x in list(first.next_siblings):x.extract()
html='''<section id="changes-inputs-detail" class="level3" data-number="2.1.1"><h3 data-number="2.1.1"><span class="header-section-number">2.1.1</span> Changes in assessment inputs</h3>
<p>The 2026 assessment updates the accepted December 2024 assessment; the 2025 document was a catch report. Updated inputs include annual catches from 1991 onward, fishery age compositions for 2024–2025, BTS indices and age compositions for 2025–2026, the 2026 ATS index and provisional age composition, and AVO indices through 2026. Historical BTS estimates were also revised. Spatiotemporal survey estimation continues the approach adopted in 2024; the current biomass-index and abundance-at-age products use sdmTMB (CITE1) and tinyVAST (CITE2), respectively.</p>
<p>The 2024–2025 fishery age compositions use year-specific FT-NIRS-to-traditional-microscopic-age conversions, with likelihood sample sizes of 578 and 530. The provisional 2026 ATS age composition applies the BTS age–length key to ATS lengths; FT-NIRS ages from the ATS samples are expected in 2027.</p>
<p>Fishery weight-at-age retains the fixed 1964–1990 schedule and annual observation means for 1991–2025. A separate random-effects model fitted to fishery and BTS weights through 2025 supplies 2026 predictions and 2027–2028 forecasts; its fit omits the 2026 BTS observations. Population weights use BTS means through 2026, including revised historical estimates. Spawning weights at ages 3–15+ use fitted 2024–2025 A-season weights and a 2026 projection, retaining the previous ages 1–2 schedule. DATAREF and APPREF describe the data treatments and their uncertainty.</p></section>
<section id="changes-methods-detail" class="level3" data-number="2.1.2"><h3 data-number="2.1.2"><span class="header-section-number">2.1.2</span> Changes in assessment methods</h3>
<p>The main assessment-method change is the transition from the ADMB implementation of Model 23.0 to the Rceattle implementation of Model 26.0. Weight-at-age is estimated in a separate random-effects model and supplied as fixed schedules, whereas Model 23.0 estimates cohort and year effects within the assessment. Model 26.0 uncertainty therefore conditions on the estimated weights. Natural mortality and catchability assumptions retain the previous treatment. The configuration comparisons, diagnostics and sensitivities evaluate the effects of the data updates and implementation change (METHODREF).</p></section>'''
html=html.replace('CITE1',cite('anderson2025sdmtmb','Anderson et al. 2025')).replace('CITE2',cite('thorson2025tinyvast','Thorson et al. 2025')).replace('DATAREF',link('sec-data','Section 4')).replace('APPREF',link('sec-ftnir-appendix','Appendix G')+' and '+link('sec-bts-spatiotemporal-appendix','Appendix H')).replace('METHODREF',link('sec-approach','Section 5')+', '+link('sec-results','Section 6')+' and '+link('review-and-source-record','Appendix I'))
for x in list(BeautifulSoup(html,'html.parser').contents):g.append(x)
# Correct life-history phrasing while retaining citation targets.
e=s.find(id='review-of-life-history');p=e.find_all('p',recursive=False)[0]
h=str(p).replace('In the EBS pollock','In the EBS, pollock').replace('March-May','March–May')
h=h.replace('Bailey (<a href="#ref-bailey2000" role="doc-biblioref">2000</a>)</span> )','<a href="#ref-bailey2000" role="doc-biblioref">Bailey 2000</a></span>)')
p.replace_with(BeautifulSoup(h,'html.parser').p)
p=e.find_all('p',recursive=False)[1];h=str(p).replace('showed spatial patterns of pollock foraging varies by size of predators.','showed that spatial patterns of pollock foraging, including cannibalism, vary with the size of the foraging pollock.').replace('For example, the northern part of the shelf region between the 100 and 200 m isobaths (closest to the shelf break) tends to be more piscivorous than pollock found in more near-shore shallow areas.','For example, pollock in the northern shelf region between the 100 and 200 m isobaths (closest to the shelf break) tend to be more piscivorous than pollock in shallow nearshore areas.')
p.replace_with(BeautifulSoup(h,'html.parser').p)
for t in list(s.find_all(string=True)):
 if 'the Aleutian Islands and Bogoslof “stocks”' in t:t.replace_with(str(t).replace('the Aleutian Islands and Bogoslof “stocks” showed subtle differentiation from the Gulf of Alaska management group.','The Aleutian Islands and Bogoslof stocks showed subtle differentiation from the Gulf of Alaska stocks.'))
 if 'The two 2026 rows distinguish an official catch' in t:t.replace_with(str(t).replace('The two 2026 rows distinguish an official catch','The catch table reports two 2026 values: reported catch'))
 if 'from the annual catch estimate of 1,312,504.58' in t:t.replace_with(str(t).replace('from the annual catch estimate','and the annual catch estimate'))
# Concise complete table notes.
for td in s.select('td.gt_sourcenote'):
 text=td.get_text(' ',strip=True)
 if 'spawning biomass is female biomass at spawning' in text:
  content(td,text.replace('; spawning biomass is female biomass at spawning','.').replace('..','.'))
 if 'Between half B35%' in text:
  prefix=text.split(' Between half B35%')[0]
  content(td,prefix+' '+link('sec-status-determination','Section 9')+' describes the status tests. Each two-year column pair reports overfishing for the earlier catch year and stock status for the later assessment year; cells outside those year-specific determinations are marked n/a. NMFS issues the final status determinations.')
# Published software-methods references.
refs=s.find(id='refs')
entries=[('anderson2025sdmtmb','Anderson, S. C., E. J. Ward, P. A. English, L. A. K. Barnett, and J. T. Thorson. 2025. “sdmTMB: An R Package for Fast, Flexible, and User-Friendly Generalized Linear Mixed Effects Models with Spatial and Spatiotemporal Random Fields.” <em>Journal of Statistical Software</em> 115 (2): 1–46. <a href="https://doi.org/10.18637/jss.v115.i02">https://doi.org/10.18637/jss.v115.i02</a>.'),('thorson2025tinyvast','Thorson, J. T., S. C. Anderson, P. Goddard, and C. N. Rooper. 2025. “tinyVAST: R package with an expressive interface to specify lagged and simultaneous effects in multivariate spatio-temporal models.” <em>Global Ecology and Biogeography</em> 34 (4): e70035. <a href="https://doi.org/10.1111/geb.70035">https://doi.org/10.1111/geb.70035</a>.')]
for id,text in entries:
 e=s.new_tag('div',id='ref-'+id);e['class']=['csl-entry'];e['role']='listitem';content(e,text);refs.append(e)
# Keep alphabetical order, with software references adjacent to their authors.
for e in sorted(refs.select(':scope > .csl-entry'),key=lambda x:x.get_text(' ',strip=True).casefold()):refs.append(e.extract())
# Add subsection links to generated Contents lists where General occurs.
for a in list(s.select('a[href="#general"]')):
 if a.find_parent('nav') or a.find_parent(id='sec-contents-appendix'):
  li=a.find_parent('li')
  if li:
   ul=s.new_tag('ul')
   for id,label in [('changes-inputs-detail','2.1.1 Changes in assessment inputs'),('changes-methods-detail','2.1.2 Changes in assessment methods')]:
    item=s.new_tag('li');content(item,link(id,label));ul.append(item)
   li.append(ul)
path.write_text(str(s))
v=R/'validation/editorial-20261109';v.mkdir(exist_ok=True);(v/'methods-references.json').write_text(json.dumps({'sdmTMB':'https://doi.org/10.18637/jss.v115.i02','tinyVAST':'https://doi.org/10.1111/geb.70035','interval_method':'Log-scale Wald confidence interval, estimate * exp(+/- 1.959964 * se_log)','interval_source':'results/catch-csv-20261104/code/extract_products.R in assessment-model repository','scientific_inputs_changed':False},indent=2)+'\n')
print('Updated HTML editorial baseline')
