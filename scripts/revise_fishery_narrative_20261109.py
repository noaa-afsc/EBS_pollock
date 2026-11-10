"""Apply requested fishery narrative removals, table ordering and alignment."""
from pathlib import Path
from bs4 import BeautifulSoup,Comment
import subprocess,re,json,hashlib,sys,collections,unicodedata
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';BASELINE='1620917'
s=BeautifulSoup(subprocess.check_output(['git','show',BASELINE+':2026/assessment.html'],cwd=ROOT),'html.parser')
removals=[
'Only the 1998 records included catch without an area; this catch is allocated between NW and SE in proportion to reported catch within each disposition.',
'Qualifying CV records are absent in 2025–2026 under the selection filter, sablefish has a reporting gap in 2014, and 2026 B season is partial through September 18.',
'The 2024–2025 estimates use FT-NIRS ages converted to align with traditional microscopic ages (TMA).',
'These distributions describe mean fish weights for each observer record.',
'Mean-one display normalization remains separate from the assessment inputs in million square metres.',
'The historical 2024 advice is retained for reference.',
'The plotted points describe the fitted and deterministic projected states; uncertainty is reported in the accompanying projection figures and decision tables.',
'These curves describe relative age patterns rather than absolute abundance. They include model estimates for unsampled years, including 2026 for the fishery.',
'Peel-specific ABC and OFL remain outside this calculation.',
'The retained 2020 ATS age-1 placeholder is excluded from observed-index summaries; the 2020 USV biomass estimate remains an observation.',
'They describe estimation uncertainty under the base model. Weight, maturity, female fraction and natural mortality remain fixed inputs. Their uncertainty and alternative model assumptions remain separate questions.',
'These ranges use each model’s own covariance; each interval describes uncertainty around its fitted estimate.',
'These models retain their recorded fixed annual process-SD settings.',
'Biomass uses common population weights, and fishing mortality is the maximum realized rate across ages. Positive annual quantities use the stated first-order log-scale intervals.',
'They describe local estimation uncertainty and are separate from posterior inference.',
'The proposed Tier 3 handoff used its documented fishery forecast and carried-forward weights. The projection inputs and catch recommendations are presented in the projection and harvest-advice sections.',
'Unsupported age–length cells leave approximately 0.495% and 0.358% of expansion-input biomass unallocated in 2024 and 2025, respectively; these are sampling-support limits, rather than missing months or deductions from assessment catch.',
'The conversion preserves total numbers; the selected likelihood input remains 578. Key uncertainty is additional to the sampling precision represented by that input weight.'
]
for t in list(s.find_all(string=True)):
 if isinstance(t,Comment) or t.parent.name in ['style','script']:continue
 text=str(t)
 for x in removals:text=text.replace(x,'')
 text=text.replace('; compare catch amounts using the reference columns.','.').replace('largest named bycatch categories','largest bycatch categories').replace('; they do not isolate the influence of these two survey years.','.').replace('The preliminary 2026 core-area estimate','The 2026 core-area estimate')
 if text!=str(t):t.replace_with(text)
for e in list(s.find_all('p')):
 t=e.get_text(' ',strip=True)
 if t.startswith('Cumulative catch through April'):
  e.insert(0,'The cumulative monthly depiction shows whether A-season fishing activity has shifted toward earlier or later months within the year. ')
 if t.startswith('A-season roe production'):e.insert(0,'As expected, ');e.contents[1].replace_with(str(e.contents[1]).replace('A-season','A-season',1))
 if t.startswith('The historical source discrepancy extends beyond jellyfish') or 'Annual catch inputs from 1991 onward use the catch estimates.' in t or t.startswith('FT-NIRS age estimation and survey developments are documented under') or t.startswith('Model 26.0 uses observations through 2026 where available') or t.startswith('The 2024 fishery weights use the same year-specific conversion') or t.startswith('We retained the fixed 1964–1990 schedule and annual WtAgeRE observation means') or t.startswith('The 2027 and 2028 rows contain the standalone WtAgeRE fishery-weight forecasts.') or t.startswith('The technical supplement documents data sources and preparation, model configurations and run identities') or t.startswith('These ATS age estimates are provisional and will be updated') or t.startswith('The assessment used 20 biomass observations through 2026.') or t.startswith('The western region contained the greatest biomass and abundance') or t.startswith('The conditional alternative fixes Foreign and Modern process SDs') or t.startswith('We calculated these comparisons from each model’s fitted age proportions') or t.startswith('Catch and spawning weights use the standalone WtAgeRE 2027') or t.startswith('Both calculations include the age-15+ group once'):e.decompose()
e=s.find(id='sec-bycatch');ps=e.find_all('p',recursive=False)
# Discuss groundfish, pollock in other fisheries, prohibited species, then nontarget species.
for i in [1,2,0,3]:e.append(ps[i].extract())
table=s.find(id='tbl-prohibited-species-catch').find('table')
for cell in table.select('thead th, tbody th, tbody td'):cell['style']=(cell.get('style','').rstrip(';')+';text-align:right;').lstrip(';')
weights=s.find(id='sec-fishery-weights');figure_paragraphs=[e for e in weights.find_all('p',recursive=False)if e.get_text().startswith(('Figure 23 shows','Figure 34 distinguishes'))]
merged=BeautifulSoup('<p>Cohort growth and annual changes in condition link observed weights to the schedules used for catch and spawning projections. Observed fishery and BTS weights and the selected fishery-scale estimates for 2014–2028 are shown in <a href="#fig-waa-omit-bts2026">Figure 23</a>. The forecast follows the 2026 age-3 cohort from 0.368 kg to 0.482 kg at age 4 in 2027 and 0.601 kg at age 5 in 2028. The 2026 biological weight schedules are compared with the selected 2027 catch and spawning projection weights in <a href="#fig-model262-current-biological-schedules">Figure 34</a>.</p>','html.parser').p
figure_paragraphs[0].replace_with(merged);figure_paragraphs[1].decompose()
ats_intro=next(e for e in s.find_all('p')if e.get_text().startswith('Acoustic-trawl surveys (ATS) are typically'))
ats_effort=next(e for e in s.find_all('p')if e.get_text().startswith('Historical and acoustic-trawl sampling effort'))
ats_merged=BeautifulSoup('<p>'+ats_intro.decode_contents()+' '+ats_effort.decode_contents()+'</p>','html.parser').p
ats_effort.replace_with(ats_merged);ats_intro.decompose()
ats_biomass=next(e for e in s.find_all('p')if e.get_text().startswith('Estimated pollock biomass for the EBS shelf has averaged'))
ats_regions=next(e for e in s.find_all('p')if e.get_text().startswith('The 2026 regional estimates included'))
regional_merged=BeautifulSoup('<p>'+ats_biomass.decode_contents()+' '+ats_regions.decode_contents()+' The SCA is a traditionally reported subset of the survey used to evaluate pollock abundance in areas considered sensitive to Steller sea lions.</p>','html.parser').p
ats_biomass.replace_with(regional_merged);ats_regions.decompose()
allocation=next(e for e in s.find_all('p')if e.get_text().startswith('We applied the same age keys to biomass'))
allocation.clear()
for node in list(BeautifulSoup('We applied the same age keys to biomass and divided biomass by abundance to estimate mean weights at age. The resulting mean weights are presented in <a href="#tbl-ats-tma-weights">Table 24</a> and <a href="#fig-ats-derived-weights">Figure 17</a>.','html.parser').contents):allocation.append(node)
uncertainty=next(e for e in s.find_all('p')if e.get_text().startswith('For assessment use, we increased the survey errors'))
shortened=str(uncertainty).split(' We retained the established geostatistical method')[0]+'</p>'
uncertainty.replace_with(BeautifulSoup(shortened,'html.parser').p)
north=next(e for e in s.find_all('p')if e.get_text().startswith('The Northern Extension contained'))
heatmap=next(e for e in s.find_all('p')if e.get_text().startswith('The ATS index used in the assessment covered'))
flipped=BeautifulSoup('<p>'+heatmap.decode_contents()+' '+north.decode_contents()+'</p>','html.parser').p
north.replace_with(flipped);heatmap.decompose()
population_weights=next(e for e in s.find_all('p')if e.get_text().startswith('Population weights use BTS observations'))
spawning_weights=next(e for e in s.find_all('p')if e.get_text().startswith('For spawning biomass, we replaced the older weights'))
weights_merged=BeautifulSoup('<p>'+population_weights.decode_contents()+' '+spawning_weights.decode_contents()+'</p>','html.parser').p
population_weights.replace_with(weights_merged);spawning_weights.decompose()
base_model=next(e for e in s.find_all('p')if e.get_text().startswith('Model 26.0 (Rceattle) is the adopted model'))
model_sensitivity=next(e for e in s.find_all('p')if e.get_text().startswith('Model 26.1.2 is the Early-late fishery split sensitivity.'))
models_merged=BeautifulSoup('<p>'+base_model.decode_contents()+' '+model_sensitivity.decode_contents()+'</p>','html.parser').p
base_model.replace_with(models_merged);model_sensitivity.decompose()
sequence=next(e for e in s.find_all('p')if e.get_text().startswith('The saved 2027 spawning-biomass and young-abundance projections'))
sequence.replace_with(BeautifulSoup(str(sequence).split(' The selected fishery information weights remain')[0]+'</p>','html.parser').p)
for a in list(s.select('a[href="#sec-weight-age-change"]')):
 if a.find_parent('p'):a['href']='#sec-fishery-weights';a.string='Section 4.1.3'
 else:(a.find_parent('li') or a).decompose()
for a in list(s.select('a[href="#estimation-and-numerical-checks"]')):(a.find_parent('li') or a).decompose()
s.find(id='sec-weight-age-change').decompose();s.find(id='estimation-and-numerical-checks').decompose()
lnrs_intro=next(e for e in s.find_all('p')if e.get_text().startswith('The log recruits-per-spawner diagnostic'))
lnrs_series=next(e for e in s.find_all('p')if e.get_text().startswith('The series shows substantial variation among year classes.'))
lnrs=BeautifulSoup('<p>'+lnrs_intro.decode_contents().replace('birth year','year of hatching')+' '+lnrs_series.decode_contents()+'</p>','html.parser').p
lnrs_intro.replace_with(lnrs);lnrs_series.decompose()
exploitation=next(e for e in s.find_all('p')if e.get_text().startswith('Biological schedules and selectivity retain their age, sex and year definitions'))
for node in list(exploitation.find_all(string=True)):
 text=str(node)
 for old,new in [('13.6793%','13.7%'),('3.3260 million t','3.326 million t'),('3.8531 million t','3.853 million t'),('14.5333%','14.5%'),('11.2075–18.3700%','11.2–18.4%')]:text=text.replace(old,new)
 if text!=str(node):node.replace_with(text)
parameters=next(e for e in s.find_all('p')if e.get_text().startswith('The parameter-family summary identifies fitted parameter groups'))
parameters.clear()
for node in list(BeautifulSoup('The parameter-family summary identifies fitted parameter groups and their asymptotic uncertainty (<a href="#tbl-model262-current-parameter-families">Table 39</a>). Complete estimates and the largest parameter correlations are retained in <a href="#sec-current-output-appendix">the detailed estimates appendix</a>. The summary likelihood contributions are in <a href="#tbl-model262-current-likelihood-summary">Table 40</a>.','html.parser').contents):parameters.append(node)
selectivity=next(e for e in s.find_all('p')if e.get_text().startswith('The fishery selectivity curves show substantial changes'))
selectivity.replace_with(BeautifulSoup(str(selectivity).split(' The latest age-1 estimates have wider uncertainty ranges')[0]+'</p>','html.parser').p)
profile=next(e for e in s.find_all('p')if e.get_text().startswith('These intervals propagate fitted-parameter uncertainty using the inverse fixed-q'))
profile.replace_with(BeautifulSoup(str(profile).split('; the recruitment-only projection bands')[0]+'.</p>','html.parser').p)
likelihood=next(e for e in s.find_all('p')if e.get_text().startswith('The likelihood comparison separates data-likelihood'))
projection_comparison=next(e for e in s.find_all('p')if e.get_text().startswith('The projections use the revised ordinary fits'))
short_projection=projection_comparison.decode_contents().split(' Model 26.1.2 is the sensitivity analysis;')[0]
comparison=BeautifulSoup('<p>'+likelihood.decode_contents()+' '+short_projection+'</p>','html.parser').p
likelihood.replace_with(comparison);projection_comparison.decompose()
projection_paragraph=next(e for e in s.find_all('p')if e.get_text().startswith('The two assessments project different spawning-biomass paths'))
recruitment_paragraph=next(e for e in s.find_all('p')if e.get_text().startswith('The recruitment comparison uses calendar age-1 year'))
recruitment_paragraph.insert_after(projection_paragraph.extract())
for a in list(s.select('a[href="#assessment-source-archive"]')):(a.find_parent('li') or a.find_parent('p') or a).decompose()
s.find(id='assessment-source-archive').decompose()
(R/'assessment.html').write_text('\n'.join(line.rstrip()for line in str(s).splitlines())+'\n')
# Reuse the proven saved-page paragraph renderer, with this revision's baseline.
source=(ROOT/'scripts/revise_editorial_pdf_20261109.py').read_text().split('# Executive summary page;')[0].replace('8497cae',BASELINE)
exec(source)
# Page 8: reflow its complete narrative, using space freed by the deletions.
es=s.find_all(['p','h3']);start=next(i for i,e in enumerate(es)if e.get_text().startswith('Catch by area and season'))
fragment(7,(50.5,50,564,730),es[start:start+11])
# Observer-weight paragraph and reordered Section 3.2.
weight=next(e for e in s.find_all('p')if e.get_text().startswith('Following the approach described in Section 3.1.1'))
fragment(8,(50.5,382,564,435),[weight])
fragment(9,(50.5,52,564,340),s.find(id='sec-bycatch').find_all(['h2','p'],recursive=False))
# Remove the two requested standalone notes in the PDF, and the repeated Table 4 wording.
for i,q in enumerate(p):
 for b in q.get_text('blocks'):
  text=b[4].replace('\xa0',' ').replace('\n',' ')
  if 'Annual catch inputs from 1991 onward use the catch estimates.' in text or text.startswith('FT-NIRS age estimation and survey developments are documented under') or text.startswith('Model 26.0 uses observations through 2026 where available') or text.startswith('The 2024 fishery weights use the same year-specific conversion') or text.startswith('We retained the fixed 1964–1990 schedule and annual WtAgeRE observation means') or text.startswith('The 2027 and 2028 rows contain the standalone WtAgeRE fishery-weight forecasts.') or text.startswith('The technical supplement documents data sources and preparation, model configurations and run identities') or text.startswith('These ATS age estimates are provisional and will be updated') or text.startswith('The assessment used 20 biomass observations through 2026.') or text.startswith('The western region contained the greatest biomass and abundance') or text.startswith('The conditional alternative fixes Foreign and Modern process SDs') or text.startswith('We calculated these comparisons from each model’s fitted age proportions') or text.startswith('Catch and spawning weights use the standalone WtAgeRE 2027') or text.startswith('Both calculations include the age-15+ group once'):
   regions.append((i,f.Rect(b[:4])+(-.5,-.5,.5,.5)))
  if i==46 and 'Only the 1998 records' in text:
   text=re.sub(r'Only the 1998 records included catch without an area[;;] this catch is allocated between NW and SE in proportion to reported catch within each disposition\.\s*','',text)
   fragment(i,(b[0],b[1],560,b[3]+1),[wraptext(text)],9)
# Reflow the remaining portions of the two shortened age-composition paragraphs.
agepara=next(e for e in s.find_all('p') if e.get_text().startswith('We estimate fishery age compositions from biological samples'))
continued=str(agepara);continued='<p>'+continued[continued.index('and age estimates'):]
fragment(11,(42,42,570,184),[BeautifulSoup(continued,'html.parser').p])
conversion=next(e for e in s.find_all('p')if e.get_text().startswith('The 2024 fishery age composition is expressed'))
fragment(11,(42,365,570,432),[conversion])
fragment(12,(42,202,570,339),[next(e for e in s.find_all('p')if e.get_text().startswith('Cohort growth and annual changes in condition'))])
fragment(13,(42,367,570,408),[next(e for e in s.find_all('p')if e.get_text().startswith('The missing 2020 survey and subsequent 2021 observation'))])
fragment(13,(42,466,570,526),[next(e for e in s.find_all('p')if e.get_text().startswith('Acoustic-trawl surveys (ATS) are typically'))])
fragment(13,(42,510,570,642),[next(e for e in s.find_all('p')if e.get_text().startswith('Estimated pollock biomass for the EBS shelf has averaged'))])
fragment(14,(42,192,570,245),[next(e for e in s.find_all('p')if e.get_text().startswith('The 2026 core-area estimate'))])
fragment(14,(42,465,570,507),[next(e for e in s.find_all('p')if e.get_text().startswith('We applied the same age keys to biomass'))])
fragment(14,(42,589,570,642),[next(e for e in s.find_all('p')if e.get_text().startswith('For assessment use, we increased the survey errors'))])
spatial_heading=next(e for e in s.find_all(['h3','h4'])if e.get_text().strip()=='Spatial and historical patterns')
spatial_heading=BeautifulSoup(str(spatial_heading),'html.parser').find(['h3','h4']);spatial_heading.name='h3'
fragment(14,(42,629,570,647),[spatial_heading])
fragment(14,(42,648,570,730),[next(e for e in s.find_all('p')if e.get_text().startswith('The ATS index used in the assessment covered'))])
regions.append((15,f.Rect(42,42,570,95)))
fragment(16,(42,96,570,163),[next(e for e in s.find_all('p')if e.get_text().startswith('The AVO assessment index is 2.01 million'))])
fragment(16,(42,568,570,641),[next(e for e in s.find_all('p')if e.get_text().startswith('Population weights use BTS observations'))])
fragment(17,(42,42,570,96),[next(e for e in s.find_all('p')if 'described the rationale for changing weight estimation' in e.get_text())])
fragment(17,(42,368,570,455),[next(e for e in s.find_all('p')if e.get_text().startswith('Model 26.0 (Rceattle) is the adopted model'))])
sequence=next(e for e in s.find_all('p')if e.get_text().startswith('The saved 2027 spawning-biomass and young-abundance projections'))
continued=str(sequence);continued='<p>'+continued[continued.index('estimates above use the revised catch inputs'):]
fragment(18,(42,42,570,134),[BeautifulSoup(continued,'html.parser').p])
fragment(18,(42,137,570,179),[next(e for e in s.find_all('p')if e.get_text().startswith('The input sequence and configuration comparison answer different questions'))])
fragment(18,(42,182,570,262),[next(e for e in s.find_all('p')if e.get_text().startswith('Approximate distributions compare the change in spawning biomass'))])
fragment(18,(42,382,570,461),[next(e for e in s.find_all('p')if e.get_text().startswith('Model 23.2 is a structural bridge between'))])
fragment(18,(42,486,570,566),[next(e for e in s.find_all('p')if e.get_text().startswith('Model 26.0 uses one fishery and fixed weight-at-age schedules.'))])
fragment(19,(50.5,72,564,152),[next(e for e in s.find_all('p')if e.get_text().startswith('The split-fishery alternatives distinguish Foreign fishing'))])
regions.append((19,f.Rect(50,155,565,413)))
fragment(19,(50.5,594,564,647),[next(e for e in s.find_all('p')if e.get_text().startswith('The local asymptotic 95% ranges in the tables'))])
fragment(34,(50.5,381,564,422),[next(e for e in s.find_all('p')if e.get_text().startswith('Additional age-1 BTS selectivity and BTS data-influence tests'))])
fragment(239,(50.5,95,564,149),[next(e for e in s.find_all('p')if e.get_text().startswith('Scope. This appendix carries forward'))])
fragment(20,(50.5,322,564,475),[next(e for e in s.find_all('p')if e.get_text().startswith('The log recruits-per-spawner diagnostic'))])
fragment(20,(50.5,480,564,599),[next(e for e in s.find_all('p')if e.get_text().startswith('Biological schedules and selectivity retain their age, sex and year definitions'))])
fragment(20,(50.5,625,564,680),[next(e for e in s.find_all('p')if e.get_text().startswith('The parameter-family summary identifies fitted parameter groups'))])
index_fits=next(e for e in s.find_all('p')if e.get_text().startswith('The fitted indices follow the main rises and falls'))
continued=str(index_fits);continued='<p>'+continued[continued.index('and 2009 and positive departures'):]
fragment(21,(50.5,50,564,117),[BeautifulSoup(continued,'html.parser').p])
fragment(21,(50.5,122,564,215),[next(e for e in s.find_all('p')if e.get_text().startswith('The fishery selectivity curves show substantial changes'))])
fragment(22,(50.5,443,564,510),[next(e for e in s.find_all('p')if e.get_text().startswith('These intervals propagate fitted-parameter uncertainty using the inverse fixed-q'))])
fragment(22,(50.5,537,564,591),[next(e for e in s.find_all('p')if e.get_text().startswith('Across the full nine peels ending in 2017–2025'))])
fragment(23,(50.5,624,564,726),[next(e for e in s.find_all('p')if e.get_text().startswith('The likelihood comparison separates data-likelihood'))])
fragment(26,(50.5,517,564,733),[next(e for e in s.find_all('p')if e.get_text().startswith('The projection tables retain all seven scenarios with recruitment fixed'))])
# Reflow the recruitment discussion with the relocated projection comparison.
overlays=[item for item in overlays if item[0]!=20];regions=[item for item in regions if item[0]!=20]
page_elements=s.find_all(['p','h3','h4']);start=next(i for i,e in enumerate(page_elements)if e.get_text().startswith('Numbers-at-age track cohorts through the population'))
end=next(i for i in range(start,len(page_elements))if page_elements[i].get_text().startswith('The fitted indices follow the main rises and falls'))
flow=[]
for e in page_elements[start:end]:
 clone=BeautifulSoup(str(e),'html.parser').find(['p','h3','h4'])
 if clone.name=='h4':clone.name='h3'
 flow.append(clone)
first_part=str(page_elements[end]).split(' and 2009 and positive departures')[0]+'</p>';flow.append(BeautifulSoup(first_part,'html.parser').p)
def recruitment_fragment(elements):
 rect=f.Rect(42,50,570,734);buf=io.BytesIO();c=canvas.Canvas(buf,pagesize=(612,792));c.target_page=20
 ps=[para(e,10.2)for e in elements]
 for q in ps:q.style.leading=q.style.fontSize*1.10;q.style.spaceAfter=3
 scale=1
 while True:
  total=0
  for q in ps:q.style.fontSize*=scale;q.style.leading*=scale;_,h=q.wrap(rect.width,1000);total+=h+q.style.spaceAfter
  if total<=rect.height:break
  scale=.99
  assert ps[0].style.fontSize>9,'Recruitment page needs more room'
 y=rect.y0
 for q in ps:
  _,h=q.wrap(rect.width,1000);q.drawOn(c,rect.x0,792-y-h);y+=h+q.style.spaceAfter
 c.save();overlays.append((20,f.open(stream=buf.getvalue(),filetype='pdf'),0));regions.append((20,f.Rect(42,49,570,736)))
recruitment_fragment(flow)
regions.append((154,f.Rect(50,49,565,88)))
regions.append((35,f.Rect(50,223,565,354)))
# Remove orphaned overview-table footnote markers.
overview=s.find(id='sec-data').find('table')
for text in list(overview.find_all(string=True)):
 if '*' in text or '†' in text:text.replace_with(str(text).replace('*','').replace('†',''))
(R/'assessment.html').write_text('\n'.join(line.rstrip()for line in str(s).splitlines())+'\n')
# Retain the original link targets for all figure and table references in rewritten prose.
for id in ['fig-cumulative-catch-tac','fig-roe-production','fig-nominal-seasonal-cpue','fig-catch-numbers-sex-season','fig-spatial-catch-a','fig-spatial-catch-b','fig-seasonal-mean-weight','tbl-catch-by-area-season','tbl-catch-retained-discarded','tbl-fmp-species-in-pollock','tbl-pollock-in-other-fisheries','tbl-prohibited-species-catch','tbl-nontarget-species-bycatch']:
 e=s.find(id=id)
 if not e:continue
 # Existing links can be mapped by the displayed reference number.
 label=e.find('caption') or e.find('figcaption')
 if not label:continue
 m=re.search(r'(Figure|Table)\s*(\d+)',label.get_text(' ',strip=True))
 if not m:continue
 for i,page in enumerate(p):
  for b in page.get_text('blocks'):
   norm=b[4].replace('\xa0',' ')
   if norm.startswith(m[1]) and re.search(r'\b'+m[2]+r'\b',norm[:35]) and i>35:
    anchors[id]=(i,b[1]);break
# Explicit saved table destinations, independent of split caption number spans.
anchors.update({'fig-projection-vintages':(154,90),'fig-recruitment-2024-current':(118,36)})
anchors.update({'tbl-fmp-species-in-pollock':(49,36),'tbl-pollock-in-other-fisheries':(50,36),'tbl-prohibited-species-catch':(51,36),'tbl-nontarget-species-bycatch':(52,36)})
for page,rect in regions:
 q=p[page]
 for l in q.get_links():
  if rect.intersects(l['from']):q.delete_link(l)
 q.add_redact_annot(rect,fill=False,cross_out=False)
for i in sorted(set(x[0]for x in regions)):p[i].apply_redactions(images=0,graphics=0)
for page,overlay,oi in overlays:
 p[page].show_pdf_page(p[page].rect,overlay,oi)
 for l in overlay[oi].get_links():
  if l['kind']!=f.LINK_URI:continue
  uri=l['uri'];data={'kind':f.LINK_URI,'from':l['from'],'uri':uri}
  if uri.startswith(BASE+'#') and uri.split('#',1)[1]in anchors:
   a=anchors[uri.split('#',1)[1]];data={'kind':f.LINK_GOTO,'from':l['from'],'page':a[0],'to':f.Point(50.5,a[1])}
  p[page].insert_link(data)
# Commit annotation objects before moving their rectangles.
pdf_bytes=p.tobytes(garbage=4,deflate=True);p.close();p=f.open(stream=pdf_bytes,filetype='pdf')
# Close spaces left by removed paragraphs, with matching link destinations.
def vertical_shift(page,y):
 if page==10 and 420<y<740:return -190
 if page==35 and 365<y<740:return -140
 if page==23 and 95<y<740:return -45
 if page==24 and 598<y<740:return -60
 if page==19 and 420<y<740:return -250
 if page==15 and 95<y<740:return -50
 if page==12 and 150<y<340:return -75
 if page==12 and 340<y<740:return -150
 return 0
for page in p:
 for l in page.get_links():
  changed_link=False;delta=vertical_shift(page.number,l['from'].y0)
  if delta:l['from']=l['from']+(0,delta,0,delta);changed_link=True
  if l['kind']==f.LINK_GOTO:
   delta=vertical_shift(l['page'],l['to'].y)
   if delta:l['to']=f.Point(l['to'].x,l['to'].y+delta);changed_link=True
  if changed_link:page.update_link(l)
# Retain Computer Modern and right-align Table 8 headings. Its data already align right.
sys.argv=['fonts','--cm-font-dir','/usr/local/texlive/2014/texmf-dist/fonts/opentype/public/cm-unicode']
fontsource=(ROOT/'scripts/set_pdf_computer_modern.py').read_text().split("p=fitz.open(R/'assessment.pdf')")[0]
fontsource=fontsource.replace("text=sp['text'].replace('\\x00','⁻')", """text=sp['text'].replace('\\x00','⁻')
    if page.number==10 and sp['bbox'][1]<215:text=text.replace('*','').replace('†','')
    delta=vertical_shift(page.number,sp['bbox'][1])
    if delta:
     sp['origin']=(sp['origin'][0],sp['origin'][1]+delta);sp['bbox']=(sp['bbox'][0],sp['bbox'][1]+delta,sp['bbox'][2],sp['bbox'][3]+delta)
    if page.number==51 and 67<sp['bbox'][1]<80:
     rights=[100,148,192,224,262,300,348,401,445,488,535];right=min(rights,key=lambda x:abs(x-sp['bbox'][2]));delta=right-sp['bbox'][2]
     sp['origin']=(sp['origin'][0]+delta,sp['origin'][1]);sp['bbox']=(sp['bbox'][0]+delta,sp['bbox'][1],sp['bbox'][2]+delta,sp['bbox'][3])""")
fontsource=fontsource.replace("'assessmentCM'+k","'cleanupMain20261109'+k")
ns={'__file__':str(ROOT/'scripts/set_pdf_computer_modern.py'),'vertical_shift':vertical_shift};exec(fontsource,ns)
changed=sorted(set([51]+[i for i,_ in regions]))
for i in changed:ns['transform'](p[i])
p.set_toc([item for item in old_toc if not item[1].startswith(('5.2 Weight-at-age treatment','5.3 Estimation and numerical checks','14 Assessment source archive'))]);p.subset_fonts();tmp=R/'assessment-fishery-tmp.pdf';p.save(tmp,garbage=4,deflate=True,deflate_fonts=True);p.close()
q=f.open(tmp);assert len(q)==266
for i,page in enumerate(q):assert [v['digest'].hex()for v in page.get_image_info(hashes=True)]==old_images[i],('Figure changed',i+1)
for i in changed:q[i].get_pixmap(matrix=f.Matrix(1.3,1.3)).save('/tmp/fishery-revision-page-'+str(i+1)+'.png')
q.close();tmp.replace(R/'assessment.pdf')
v=R/'validation/fishery-narrative-20261109';v.mkdir(exist_ok=True)
(v/'checks.json').write_text(json.dumps({'pages':266,'all_main_report_raster_figures_preserved':True,'section_3_2_table_order':[6,7,8,9],'Table_8_alignment':'right','Computer_Modern_retained':True,'scientific_inputs_changed':False,'manual_accessibility':'Not Tested'},indent=2)+'\n')
m=json.loads((R/'release.json').read_text());m['version']='2026-11-09-fishery-narrative';m['fishery_narrative_revision']={'ordered_bycatch_discussion':[6,7,8,9],'Table_8_right_aligned':True,'requested_narrative_removals':True,'cumulative_catch_purpose':'Detect earlier or later within-year A-season activity'}
for name in m['files']:m['files'][name]=hashlib.sha256((R/name).read_bytes()).hexdigest()
(R/'release.json').write_text(json.dumps(m,indent=2)+'\n');print('Updated fishery narrative and table alignment; PDF pages',changed)
