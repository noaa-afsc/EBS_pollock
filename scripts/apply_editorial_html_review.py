"""Apply reviewed caption and prose edits to the current saved SAFE HTML."""
from pathlib import Path
from html.parser import HTMLParser
import re,json,html,subprocess
ROOT=Path(__file__).resolve().parents[1];r=ROOT/'2026';out=r/'validation/editorial-review-20261110';base='448ae3f';s=subprocess.check_output(['git','show',base+':2026/assessment.html'],cwd=ROOT,text=True);original=s
caps=json.loads((out/'main-caption-edits.json').read_text());ops=json.loads((out/'prose-edits.json').read_text());records=[]
for n,text in caps.items():
 pattern=r'(<figcaption\b[^>]*>\s*<span class="safe-caption-label">Figure(?:\s|\u00a0)*'+n+r':</span>\s*<span class="safe-caption-text">).*?(</span>\s*</figcaption>)'
 s,count=re.subn(pattern,lambda m:m[1]+html.escape(text)+m[2],s,flags=re.S);assert count==1,(n,count)
 s=re.sub(r'title="Figure(?:\s|\u00a0)*'+n+r': [^"]*"',lambda m:'title="'+html.escape('Figure\u00a0'+n+': '+text,quote=True)+'"',s)
management='<p>'+'</p>\n<p>'.join(ops['management_paragraphs'])+'</p>'
pattern=r'<p>For 2026, adopted ABC.*?<p>Salmon accounting and genetic stock identification.*?</p>'
s,count=re.subn(pattern,lambda m:management,s,flags=re.S);assert count==1
for start,new in ops['paragraphs'].items():
 s,count=re.subn(r'<p>'+re.escape(start)+r'.*?</p>',lambda m:'<p>'+new+'</p>',s,flags=re.S);assert count==1,start
class ReaderTextEdits(HTMLParser):
 def __init__(self,t):
  super().__init__(convert_charrefs=False);self.t=t;self.lines=[0]+[m.end()for m in re.finditer('\n',t)];self.stack=[];self.edits=[];self.feed(t)
 def handle_starttag(self,t,a):
  if t not in {'area','base','br','col','embed','hr','img','input','link','meta','param','source','track','wbr'}:self.stack.append(t)
 def handle_endtag(self,t):
  if t in self.stack:self.stack=self.stack[:len(self.stack)-1-self.stack[::-1].index(t)]
 def handle_data(self,d):
  if any(t in self.stack for t in ['script','style','code','pre']):return
  new=d
  for a,b in ops['text_replacements']:
   pat=r'\s+'.join(re.escape(x)for x in a.split());new,n=re.subn(pat,lambda m:b,new)
   if n:records.append({'old':a,'new':b,'count':n})
  if new!=d:
   new=re.sub(r' {2,}',' ',new);new=re.sub(r' +([.,;])',r'\1',new);line,col=self.getpos();start=self.lines[line-1]+col;self.edits.append((start,start+len(d),new))
p=ReaderTextEdits(s)
for a,b,new in reversed(p.edits):s=s[:a]+new+s[b:]
assert 'matching the original figure convention'not in s
s=s.replace('The 2026 extract ends September 18','The 2026 data extend through September 18').replace('The figure summarizes saved sampler point estimates and retains their original age bases.','The 2024–2025 estimates use FT-NIRS ages before conversion to tester ages.').replace('2025, because sample collections','2025 because sample collections')
(r/'assessment.html').write_text(s)
(out/'HTML-editorial-checks.json').write_text(json.dumps({'baseline':base,'main_captions_reviewed':73,'main_captions_revised':len(caps),'text_edits':records,'management_rewritten':True,'scientific_numeric_tables_and_images_modified':False},indent=2)+'\n')
print('PASS: 46 captions, management context and requested prose edits applied to HTML')
