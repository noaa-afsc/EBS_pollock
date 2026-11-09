from pathlib import Path
from bs4 import BeautifulSoup
import re,json,shutil
root=Path(__file__).resolve().parents[1];r=root/'2026'
backup=Path('/tmp/fishery-restructure-before');backup.mkdir(exist_ok=True)
for name in ['assessment.html','assessment.pdf','release.json']:shutil.copy2(r/name,backup/name)
s=(r/'assessment.html').read_text();soup=BeautifulSoup(s,'html.parser')
sec=soup.find(id='sec-data');fish=sec.find(id='fishery-catch-and-age-compositions');old=fish.find_all('p',recursive=False)
h=fish.h2;h.clear();h.append(BeautifulSoup('<span class="header-section-number">4.1</span> Fishery data','html.parser'));fish['data-number']='4.1'
def subsection(id,num,title):
 t=soup.new_tag('section',id=id,attrs={'class':'level3','data-number':num});head=soup.new_tag('h3',attrs={'data-number':num,'class':'anchored','data-anchor-id':id});head.append(BeautifulSoup(f'<span class="header-section-number">{num}</span> {title}','html.parser'));t.append(head);return t
catch=subsection('sec-fishery-catch','4.1.1','Catch');age=subsection('sec-fishery-age-compositions','4.1.2','Age compositions')
for i in [0,2]:catch.append(old[i].extract())
for i in [1,3,4,5,6]:age.append(old[i].extract())
weights=sec.find(id='sec-fishery-weights').extract();weights['data-number']='4.1.3';weights.h3['data-number']='4.1.3';weights.h3.clear();weights.h3.append(BeautifulSoup('<span class="header-section-number">4.1.3</span> Weight-at-age','html.parser'))
weights.insert(3,old[7].extract())
other=sec.find(id='sec-other-data');fh=next(p for p in other.find_all('p',recursive=False) if p.get_text(strip=True)=='Fishery weights');detail=fh.find_next_sibling('p').extract();fh.decompose()
model=next(p for p in weights.find_all('p',recursive=False) if p.get_text().startswith('The weight-model approach'));model.insert_after(detail)
fig=weights.find(id='fig-waa-omit-bts2026').extract()
for p in list(weights.find_all('p',recursive=False)):
 if 'Download the figure data' in p.get_text():p.extract()
intro=next(p for p in weights.find_all('p',recursive=False) if 'Figure 22a' in p.get_text());intro.clear();intro.append(BeautifulSoup('<a href="#fig-waa-omit-bts2026" class="quarto-xref">Figure 22a</a> shows observed fishery and BTS weights and the selected fishery-scale estimates for 2014–2028. The forecast follows the 2026 age-3 cohort from 0.368 kg to 0.482 kg at age 4 in 2027 and 0.601 kg at age 5 in 2028.','html.parser'))
fish.extend([catch,age,weights])
# Save an editable editorial source for this revised section.
(r/'tables/weight-age-revision-20261109/fishery-data.html').write_text(str(fish))
def section_range(text,id):
 start=text.rfind('<section',0,text.index('id="'+id+'"'));depth=0
 for m in re.finditer(r'</?section\b[^>]*>',text[start:]):
  depth+= -1 if m.group().startswith('</') else 1
  if depth==0:return start,start+m.end()
 raise ValueError(id)
a,b=section_range(s,'sec-data');s=s[:a]+str(sec)+s[b:]
caption=('Observed and predicted weight at age, 2014–2028, after omitting 2026 BTS weight observations from the standalone random-effects fit. '
 'Cell labels give mean weight in kg per fish for ages 3–10. Fishery and BTS observations end in 2025; the right-hand panel includes 2026 estimates without weight observations and 2027–2028 forecasts. '
 'The solid line marks 2026; dashed boxes mark forecast years. Colours show proportional deviations from each age’s mean through 2025 within its own panel, saturating at ±50%. '
 'Blank cells indicate absent observations, including the 2020 BTS gap. This figure describes the selected fit; it provides no direct comparison with the fit that included 2026 BTS weights.')
alt='Three heatmaps for 2014–2028 compare observed fishery weights, observed BTS weights, and predicted fishery-scale weights. Observations end in 2025. The 2026 age-3 cohort has predicted weights of 0.37 kg in 2026, 0.48 kg at age 4 in 2027, and 0.60 kg at age 5 in 2028. Boundaries identify 2026 and forecast years.'
fig.img['alt']=alt;fig.find('a')['title']='Figure 22a: '+caption
fig.find(class_='safe-caption-text').string=caption
fig.find('figure').append(BeautifulSoup('<p><a href="tables/waa-omit-bts2026-figure.csv">Download the figure data and anomaly reference means</a>.</p>','html.parser'))
# Place after the AVO Figure 22 in the main Figures section.
ss=BeautifulSoup(s,'html.parser');old22=ss.find(id='fig-avo-all-years');assert old22
needle=str(old22)
# Source block boundaries: stop at the next top-level figure wrapper.
start=s.index('id="fig-avo-all-years"');start=s.rfind('<div',0,start)
# Match balanced div tags.
depth=0
for m in re.finditer(r'</?div\b[^>]*>',s[start:]):
 depth+= -1 if m.group().startswith('</') else 1
 if depth==0:end=start+m.end();break
s=s[:end]+'\n'+str(fig)+s[end:]
# Update the sidebar hierarchy.
ss=BeautifulSoup(s,'html.parser');oldnav=ss.find('a',id='toc-fishery-catch-and-age-compositions');oldli=oldnav.parent;nav=ss.new_tag('ul',attrs={'class':'collapse'})
oldnav.clear();oldnav.append(BeautifulSoup('<span class="header-section-number">4.1</span> Fishery data','html.parser'))
for id,num,title in [('sec-fishery-catch','4.1.1','Catch'),('sec-fishery-age-compositions','4.1.2','Age compositions'),('sec-fishery-weights','4.1.3','Weight-at-age')]:
 nav.append(BeautifulSoup(f'<li><a href="#{id}" class="nav-link" id="toc-{id}" data-scroll-target="#{id}"><span class="header-section-number">{num}</span> {title}</a></li>','html.parser'))
oldweightnav=ss.find('a',id='toc-sec-fishery-weights');oldweightnav.parent.decompose();oldli.append(nav)
# Serialize only the sidebar, preserving report bytes elsewhere.
old_soup=BeautifulSoup(s,'html.parser');old_toc=old_soup.find(id='TOC');new_toc=ss.find(id='TOC');tocstart=s.index('<nav',s.index('id="TOC"')-100);tocend=s.index('</nav>',tocstart)+6;s=s[:tocstart]+str(new_toc)+s[tocend:]
# Ordinary figure numbering; appendix letters remain unchanged.
def renumber(m):
 n=m.group(2)
 return m.group(1)+('23' if n=='22a' else str(int(n)+1) if int(n)>=23 else n)
s=re.sub(r'(Figure[\s\xa0]+)(22a|\d+)(?![\dA-Za-z])',renumber,s)
s=s.replace('4.3.2 Annual fishery weight at age','4.1.3 Weight-at-age').replace('4.1 Fishery catch and age compositions','4.1 Fishery data')
# Figure 33 becomes 34 after inserting Figure 23.
ss=BeautifulSoup(s,'html.parser');bio=ss.find(id='fig-model262-current-biological-schedules')
bcap=('Model 26.0 biological and selectivity schedules. Maturity, female fraction and natural mortality are supplied inputs. '
 'The weight panel distinguishes the 2026 population (BTS), spawning and fishery schedules from the selected 2027 fishery forecast used for both catch and spawning in projections. '
 'The 2026 fishery estimates and 2027 forecast come from the standalone WtAgeRE fit omitting 2026 BTS weight observations; population and historical spawning weights retain their separate treatments. '
 'Fishing and survey selectivity curves have approximate 95% asymptotic ranges; ATS and AVO share their estimated curve. Supplied schedules have no fitted-parameter bands.')
oldcap=bio.find(class_='safe-caption-text').get_text();s=s.replace(oldcap,bcap+'\n')
oldalt=bio.img['alt'];s=s.replace(oldalt,bcap)
(r/'assessment.html').write_text(s)
shutil.copy2(r/'tables/weight-age-revision-20261109/model26_2_biological_schedules.png',r/'assets/figure-034.png')
(r/'tables/weight-age-revision-20261109/captions.json').write_text(json.dumps({'new23':caption,'new34':bcap,'alt23':alt},indent=2))
# Fragment for repaginating the affected PDF text pages: existing Data table onward,
# followed by the analytic approach up to the end of 5.1.3.
ss=BeautifulSoup(s,'html.parser');data=ss.find(id='sec-data');children=list(data.find_all(recursive=False));fragment=''.join(str(x) for x in children[2:])
approach=ss.find(id='sec-approach');fragment+=str(approach.find('h1',recursive=False));models=approach.find(id='main-and-comparison-models')
for x in models.find_all(recursive=False):
 if x.get('id')=='sec-model261-configuration':break
 fragment+=str(x)
(r/'tables/weight-age-revision-20261109/pdf-text-fragment.html').write_text(fragment)
print('Reorganized 4.1; moved and numbered Figure23; refreshed biological Figure34.')
