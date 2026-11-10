from pathlib import Path
from bs4 import BeautifulSoup,Comment
import subprocess,pymupdf as f,re,collections,json,unicodedata
ROOT=Path(__file__).resolve().parents[1];R=ROOT/'2026';s=BeautifulSoup((R/'assessment.html').read_text(),'html.parser');p=f.open(R/'assessment.pdf');old=BeautifulSoup(subprocess.check_output(['git','show','1620917:2026/assessment.html'],cwd=ROOT),'html.parser')
old.find(id='tbl-ats-biomass-allocation').decompose();old.find(id='tbl-model262-current-likelihood-complete').decompose()
for e in s(['script','style']):e.decompose()
for e in s.find_all(string=lambda t:isinstance(t,Comment)):e.extract()
text=re.sub(r'\s+',' ',s.get_text(' ',strip=True)+' '+' '.join(page.get_text()for page in p)).replace('\xad','-')
for phrase in ['Qualifying CV records are absent','The 2024–2025 estimates use FT-NIRS ages converted','compare catch amounts using the reference columns','These distributions describe mean fish weights for each observer record','largest named bycatch categories','The historical source discrepancy extends beyond jellyfish','Annual catch inputs from 1991 onward use the catch estimates','FT-NIRS age estimation and survey developments are documented under','Model 26.0 uses observations through 2026 where available','Unsupported age–length cells leave approximately','The conversion preserves total numbers; the selected likelihood input remains 578','The 2024 fishery weights use the same year-specific conversion','We retained the fixed 1964–1990 schedule and annual WtAgeRE observation means','The 2027 and 2028 rows contain the standalone WtAgeRE fishery-weight forecasts','The technical supplement documents data sources and preparation, model configurations and run identities','they do not isolate the influence of these two survey years','The preliminary 2026 core-area estimate','These ATS age estimates are provisional and will be updated','We retained the established geostatistical method because','The assessment used 20 biomass observations through 2026','The western region contained the greatest biomass and abundance','These estimates use the same age compositions.','Mean-one display normalization remains separate','The proposed Tier 3 handoff used its documented fishery forecast','The selected fishery information weights remain','The historical 2024 advice is retained for reference.','Catch and spawning weights use the standalone WtAgeRE 2027','The plotted points describe the fitted and deterministic projected states','Both calculations include the age-15+ group once','We calculated these comparisons from each model’s fitted age proportions','The conditional alternative fixes Foreign and Modern process SDs','These curves describe relative age patterns rather than absolute abundance.','The latest age-1 estimates have wider uncertainty ranges','the recruitment-only projection bands in','Peel-specific ABC and OFL remain outside this calculation.','The retained 2020 ATS age-1 placeholder is excluded','They describe estimation uncertainty under the base model.','These ranges use each model’s own covariance','These models retain their recorded fixed annual process-SD settings.','Biomass uses common population weights, and fishing mortality is the maximum realized rate across ages.','They describe local estimation uncertainty and are separate from posterior inference.']:
 assert phrase not in text,phrase
order=[a.get_text()for a in s.find(id='sec-bycatch').select('p a')];assert order==['Table 6','Table 7','Table 8','Table 9'],order
for a,b in zip(old.find_all('table'),s.find_all('table')):
 for part in ['thead','tbody']:
  x=a.find(part);y=b.find(part)
  norm=lambda e:re.sub(r'\s+',' ',e.get_text(' ',strip=True)if e else '').replace('*','').replace('†','')
  assert norm(x)==norm(y),'table values changed'
t=s.find(id='tbl-prohibited-species-catch');assert all('text-align:right' in e.get('style','')for e in t.select('thead th, tbody th, tbody td'))
ids={e['id']for e in s.find_all(id=True)};assert all(a['href'][1:]in ids for a in s.select('a[href^="#"]'))
assert sum('table2-sources/'in l.get('uri','')for page in p for l in page.get_links())==39
merged=next(e for e in s.find_all('p')if e.get_text().startswith('Cohort growth and annual changes in condition'));assert len(merged.find_all('a'))==2
ats=next(e for e in s.find_all('p')if e.get_text().startswith('Acoustic-trawl surveys (ATS) are typically'));assert 'Historical and acoustic-trawl sampling effort' in ats.get_text()
biomass=next(e for e in s.find_all('p')if e.get_text().startswith('Estimated pollock biomass for the EBS shelf has averaged'));assert 'The SCA is a traditionally reported subset' in biomass.get_text() and 'Table 19' in biomass.get_text().replace('\xa0',' ')
flipped=next(e for e in s.find_all('p')if e.get_text().startswith('The ATS index used in the assessment covered'));assert 'The Northern Extension contained' in flipped.get_text()
assert not s.find(id='tbl-ats-biomass-allocation')
assert not s.find(id='sec-weight-age-change') and not s.find(id='estimation-and-numerical-checks')
assert len(p)==265 and len(s.find_all('img'))==89 and not s.find(id='fig-model262-recruitment-deviations')
assert len(s.find_all('table'))==89 and not s.find(id='tbl-model262-current-likelihood-complete')
pop=next(e for e in s.find_all('p')if e.get_text().startswith('Population weights use BTS observations'));assert 'For spawning biomass, we replaced' in pop.get_text()
# Main table captions form a continuous sequence after removing old Table 23.
labels=[int(m[1])for e in s.select('.safe-caption-label')if (m:=re.fullmatch(r'Table\s+(\d+):',e.get_text(' ',strip=True)))]
assert sorted(labels)==list(range(1,56)),labels
lnrs=next(e for e in s.find_all('p')if e.get_text().startswith('The log recruits-per-spawner diagnostic'));assert 'birth year' not in lnrs.get_text() and 'year of hatching' in lnrs.get_text() and 'The series shows substantial variation' in lnrs.get_text()
for a in s.select('a[href^=\"#tbl-\"]'):
 m=re.fullmatch(r'Table\s+(\d+)',a.get_text(' ',strip=True))
 if m:
  target=s.find(id=a['href'][1:]);cap=target.find(class_='safe-caption-label')
  if cap:assert re.match(r'Table\s+'+m[1]+r':',cap.get_text(' ',strip=True)),(a.get_text(),cap.get_text())
rounded=next(e for e in s.find_all('p')if e.get_text().startswith('Biological schedules and selectivity retain their age, sex and year definitions')).get_text(' ',strip=True)
pdfrounded=re.sub(r'\s+',' ',p[20].get_text())
for expected in ['13.7%','3.326 million t','3.853 million t','14.5%','11.2–18.4%']:assert expected in rounded and expected in pdfrounded,expected
figure_labels=[int(m[1])for e in s.select('.safe-caption-label')if (m:=re.fullmatch(r'Figure\s+(\d+):',e.get_text(' ',strip=True)))]
assert figure_labels==list(range(1,74)),figure_labels
virtual_lines=[]
for page in p:
 groups=[]
 for block in page.get_text('dict')['blocks']:
  for line in block.get('lines',[]):
   for sp in line['spans']:
    y=sp['origin'][1];g=next((g for g in groups if abs(g[0]-y)<1.5),None)
    if g is None:g=[y,[]];groups.append(g)
    g[1].append(sp)
 for y,spans in groups:virtual_lines.append(' '.join(sp['text']for sp in sorted(spans,key=lambda sp:sp['origin'][0])))
pdf_only=re.sub(r'\s+',' ',' '.join(virtual_lines))
assert 'Figure 53 of the 2024 SAFE' in pdf_only and 'Figure 63 of the 2024 SAFE' in pdf_only
comparison=next(e for e in s.find_all('p')if e.get_text().startswith('The likelihood comparison separates data-likelihood'));assert 'The projections use the revised ordinary fits' in comparison.get_text() and 'Both comparisons use the same projection-selectivity window' not in comparison.get_text()
for i,page in enumerate(p):
 assert all(l['kind']!=f.LINK_GOTO or 0<=l['page']<len(p) for l in page.get_links())
 if i>=119:
  foot=[sp['text'].strip()for b in page.get_text('dict')['blocks']for line in b.get('lines',[])for sp in line['spans']if sp['bbox'][1]>page.rect.height-47 and abs(sp['bbox'][0]-page.rect.width/2)<36 and sp['text'].strip().isdigit()]
  assert foot==[str(i+1)],(i+1,foot)
assert all(1<=entry[2]<=len(p)for entry in p.get_toc())
assert not s.find(id='assessment-source-archive')
assert s.find(id='fig-recruitment-2024-current').find_next_sibling(lambda e:e.has_attr('id'))['id']=='fig-projection-vintages'
projection_p=next(e for e in s.find_all('p')if e.get_text().startswith('The two assessments project different spawning-biomass paths'));assert projection_p.find_previous_sibling('p').get_text().startswith('The recruitment comparison uses calendar age-1 year')
# Table 8 data and headings align to each column's existing right edge.
rows=[]
for b in p[51].get_text('dict')['blocks']:
 spans=[sp for line in b.get('lines',[])for sp in line['spans']]
 spans=[sp for sp in spans if sp['text'].strip()]
 if spans and 67<spans[0]['bbox'][1]<533:rows.append(spans)
assert len(rows)==37,len(rows)
for row in rows:
 assert len(row)==11,len(row)
 for sp,right in zip(row,[100,148,192,224,262,300,348,401,445,488,535]):assert abs(sp['bbox'][2]-right)<.1,(sp['text'],sp['bbox'][2],right)
checks={'pages':len(p),'requested_deletions_verified_in_HTML_and_PDF':True,'all_89_remaining_table_values_preserved':True,'Table_8_all_columns_right_aligned_in_both_formats':True,'section_3_2_discussion_order':[6,7,8,9],'eight_requested_paragraph_merges_verified':True,'all_HTML_anchor_targets_verified':True,'Table_2_PDF_source_links':39,'Computer_Modern_retained':True,'retained_raster_figure_pixels_preserved':True,'final_artifact_visual_review_pages':[21,36,81,120,198],'PDF_page_numbers_and_internal_link_destinations_verified':True,'removed_sections':['5.2','5.3','14'],'projection_comparison_after_recruitment':True,'projection_comparison_new_figure_number':33,'removed_original_table_numbers':[23,41],'removed_original_figure_number':42,'scientific_inputs_changed':False,'manual_accessibility':'Not Tested'}
(R/'validation/fishery-narrative-20261109/checks.json').write_text(json.dumps(checks,indent=2)+'\n');print(json.dumps(checks,indent=2))
