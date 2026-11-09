"""Reflow edited report paragraphs using saved HTML; never runs assessment code."""
from pathlib import Path
from bs4 import BeautifulSoup,NavigableString
import re,json
from reportlab.platypus import SimpleDocTemplate,Paragraph,Spacer,Table,TableStyle,KeepTogether
from reportlab.lib.styles import getSampleStyleSheet,ParagraphStyle
from reportlab.lib import colors
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from xml.sax.saxutils import escape
root=Path(__file__).resolve().parents[1];out=root/'2026/tables/weight-age-revision-20261109'
for name,file in [('Arial','Arial.ttf'),('Arial-Bold','Arial Bold.ttf'),('Arial-Italic','Arial Italic.ttf'),('Arial-BoldItalic','Arial Bold Italic.ttf')]:
 pdfmetrics.registerFont(TTFont(name,'/System/Library/Fonts/Supplemental/'+file))
pdfmetrics.registerFontFamily('Arial',normal='Arial',bold='Arial-Bold',italic='Arial-Italic',boldItalic='Arial-BoldItalic')
styles=getSampleStyleSheet();styles.add(ParagraphStyle(name='Body',fontName='Arial',fontSize=10.2,leading=12.6,spaceAfter=7))
for key,size in [('H1',15),('H2',12.5),('H3',11)]:styles.add(ParagraphStyle(name=key,parent=styles['Body'],fontName='Arial-Bold',fontSize=size,leading=size+3,spaceBefore=8,spaceAfter=7,keepWithNext=True))
styles.add(ParagraphStyle(name='Cell',parent=styles['Body'],fontSize=9.2,leading=11,spaceAfter=0))
anchors={};headings=[]
class Doc(SimpleDocTemplate):
 def afterFlowable(self,flow):
  if hasattr(flow,'anchor_id'):anchors[flow.anchor_id]={'page':self.page,'y':self.canv._y if hasattr(self.canv,'_y') else 36}
  if hasattr(flow,'heading_text'):headings.append({'text':flow.heading_text,'page':self.page})
def inline(x):
 if isinstance(x,NavigableString):return escape(str(x).replace('\xad',''))
 if x.name in ['script','style']:return ''
 inner=''.join(inline(c) for c in x.children)
 if x.name in ['em','i']:return '<i>'+inner+'</i>'
 if x.name in ['strong','b']:return '<b>'+inner+'</b>'
 if x.name in ['sub','sup']:return '<'+x.name+'>'+inner+'</'+x.name+'>'
 if x.name=='br':return '<br/>'
 return inner
flows=[]
def walk(el):
 if isinstance(el,NavigableString):return
 if el.name in ['p','h1','h2','h3','h4']:
  text=inline(el).strip()
  if not text:return
  p=Paragraph(text,styles[{'h1':'H1','h2':'H2','h3':'H3','h4':'H3'}.get(el.name,'Body')]);p.anchor_id=el.get('data-anchor-id',el.get('id',''))
  if el.name.startswith('h'):p.heading_text=el.get_text(' ',strip=True)
  flows.append(p);return
 if el.name=='table':
  rows=[]
  for tr in el.find_all('tr'):
   rows.append([Paragraph(inline(td),styles['Cell']) for td in tr.find_all(['th','td'],recursive=False)])
  if rows:
   n=max(map(len,rows));widths=[100,190,250] if n==3 else [540/n]*n
   table=Table(rows,colWidths=widths,repeatRows=1,hAlign='LEFT');table.setStyle(TableStyle([('VALIGN',(0,0),(-1,-1),'TOP'),('BACKGROUND',(0,0),(-1,0),colors.HexColor('#eeeeee')),('LINEBELOW',(0,0),(-1,0),.5,colors.grey),('BOTTOMPADDING',(0,0),(-1,-1),5),('TOPPADDING',(0,0),(-1,-1),5)]));flows.extend([table,Spacer(1,7)])
  return
 for c in el.children:walk(c)
soup=BeautifulSoup((out/'pdf-text-fragment.html').read_text(),'html.parser')
for x in soup.children:walk(x)
doc=Doc(str(out/'restructured-text-pages.pdf'),pagesize=(612,792),leftMargin=36,rightMargin=36,topMargin=36,bottomMargin=50)
doc.build(flows)
(out/'text-page-anchors.json').write_text(json.dumps({'anchors':anchors,'headings':headings},indent=2));print(headings)
