"""Attach publication anchors and semantic captions to Quarto's native output.

No report text is sourced here: prose, captions, tables and figures all come
from EBSpollock.qmd and the CSV files consumed by tables.lua.
"""
from pathlib import Path
import re
import sys
from lxml import html


def caption_layout(caption):
    if caption.xpath('./span[@class="safe-caption-label"]'):
        return
    if len(caption)==1 and caption[0].tag=='p':
        paragraph=caption[0]
        caption.text=paragraph.text
        for child in list(paragraph):caption.append(child)
        caption.remove(paragraph)
    text=caption.text or ''
    match=re.match(r'\s*((?:Table|Figure)\s+[A-J]?\d+:?)\s+',text)
    if match:
        label=html.Element('span',{'class':'safe-caption-label'});label.text=match[1]
        body=html.Element('span',{'class':'safe-caption-text'});body.text=text[match.end():]
        for child in list(caption):body.append(child)
        caption.text=None;caption.append(label);caption.append(body)
        caption.set('class','safe-caption safe-caption-hanging')


def finish(path):
    document=html.parse(str(path),parser=html.HTMLParser(huge_tree=True))
    for figure in document.xpath('//main//div[@data-public-id]'):
        figure.set('id',figure.get('data-public-id'));figure.attrib.pop('data-public-id')
        figure.tag='figure'
        for caption in figure.xpath('./div[contains(@class,"figure-caption")]'):
            caption.tag='figcaption'
    for caption in document.xpath('//main//figcaption | //main//caption'):
        caption_layout(caption)
    # A hanging caption is a grid outside the table, so it cannot be mistaken
    # for a data cell by the browser's table layout algorithm.
    for number, table in enumerate(document.xpath('//main//table'),1):
        captions=table.xpath('./caption')
        if captions:
            caption=captions[0];caption.tag='figcaption'
            caption.set('id',f'table-caption-{number}')
            table.set('aria-labelledby',caption.get('id'))
            parent=table.getparent();position=parent.index(table)
            figure=html.Element('figure',{'class':'report-table-figure'})
            parent.insert(position,figure)
            figure.append(caption);figure.append(table)
    for heading in document.xpath('//section[@id="appendices"]/section/h2'):
        heading.set('class',(heading.get('class','')+' safe-appendix-heading').strip())
        labels=heading.xpath('./strong')
        if labels and re.fullmatch(r'Appendix\s+[A-J]',labels[0].text_content().strip()):
            labels[0].tail=None
            heading.insert(heading.index(labels[0])+1,html.Element('br'))
    path.write_bytes(html.tostring(document,encoding='utf-8',doctype='<!DOCTYPE html>'))

if __name__=='__main__':
    finish(Path(sys.argv[1]))
