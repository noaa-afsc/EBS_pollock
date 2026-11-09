"""Compare annual nontarget summaries across two 2024 revisions."""
from pathlib import Path
import csv,subprocess,io,sys
repo=sys.argv[1]
def revision(rev):
    raw=subprocess.check_output(['git','-C',repo,'show',rev+':doc/data/byc_nontarg.csv']).decode('utf-8-sig')
    return {r['Year']:r for r in csv.DictReader(io.StringIO(raw))}
a=revision('c29d1e0');b=revision('d38de13')
current={r['Year']:r for r in csv.DictReader(open('2026/tables/nontarget-species-bycatch.csv'))}
rows=[]
for year in ['2019','2020','2021','2022']:
    for col in a[year]:
        if col=='Year':continue
        av=float(a[year][col]);bv=float(b[year][col]);cv=float(current[year][col.replace('Sea pen','Sea.pen')])
        rows.append(dict(Year=year,historical_column=col,September2024_mt=av,November2024_mt=bv,November_over_September=bv/av if av else '',current_named_group_mt=cv))
with open('2026/validation/nontarget-history-column-audit.csv','w') as f:
    w=csv.DictWriter(f,fieldnames=rows[0]);w.writeheader();w.writerows(rows)
assert all(abs(r['November_over_September']-2)<.001 for r in rows if r['historical_column'] in ['Jellies','MiscFish','SeaStar','Osmerid','Grenadier','Eelpouts'])
print('Verified near-twofold change in six same-named historical columns, all four years.')
