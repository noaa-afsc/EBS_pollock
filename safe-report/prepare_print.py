"""Create a print view from the QMD's HTML; never read an existing PDF."""
from copy import deepcopy
from pathlib import Path
import re
from urllib.parse import urljoin

from lxml import etree, html
import pymupdf

ROOT = Path(__file__).resolve().parent
PUBLIC = "https://noaa-afsc.github.io/EBS_pollock/2026/"


def add_class(element, name):
    element.set("class", (element.get("class", "") + " " + name).strip())


def prepare(source):
    tree = html.parse(str(source), parser=html.HTMLParser(huge_tree=True))
    main = deepcopy(tree.xpath("//main")[0])
    for element in main.xpath(".//script | .//nav[@aria-label='Assessment links'] | .//div[contains(@class,'assessment-links')] | .//button"):
        element.getparent().remove(element)
    for anchor in main.xpath(".//a[@href]"):
        href = anchor.get("href")
        if not href.startswith("#"):
            anchor.set("href", urljoin(PUBLIC, href))
    cover = html.Element("div", {"class": "pdf-cover"})
    header = main.xpath("./header")[0]
    title = deepcopy(header.xpath(".//h1")[0])
    cover.append(title)
    authors = etree.SubElement(cover, "p", {"class": "pdf-authors"})
    authors.text = ", ".join(" ".join(e.text_content().split()) for e in header.xpath(".//p[@class='author']"))
    for affiliation in header.xpath(".//p[@class='affiliation']"):
        para = etree.SubElement(cover, "p", {"class": "pdf-affiliation"})
        para.text = " ".join(affiliation.text_content().split())
    main.remove(header)
    for callout in main.xpath("./div[contains(@class,'callout')]"):
        cover.append(callout)
    main.insert(0, cover)

    contents = html.Element("div", {"class": "pdf-contents"})
    etree.SubElement(contents, "h1").text = "Contents"
    listing = etree.SubElement(contents, "ul")
    for section in main.xpath("./section | ./section[@id='appendices']/section"):
        headings = section.xpath("./h1 | ./h2")
        if not headings:
            continue
        item = etree.SubElement(listing, "li")
        if section.getparent().get("id") == "appendices":
            add_class(item, "pdf-appendix-entry")
        anchor = etree.SubElement(item, "a", href="#" + section.get("id"))
        anchor.text = " ".join(headings[0].text_content().split())
    main.insert(1, contents)

    # Keep captions above tables, independent of the web stylesheet.
    for figure in main.xpath(".//figure[.//table]"):
        captions = figure.xpath("./figcaption")
        if captions:
            figure.insert(0, captions[0])
        add_class(figure, "pdf-table-figure")
    for image in main.xpath(".//img"):
        image.set("loading", "eager")
    # Use the published table renderer's minimum-column-width rule.
    font = pymupdf.Font(fontfile=str(ROOT / "fonts/cmunrm.otf"))
    table_layout = []
    for table in main.xpath(".//table"):
        widths = []
        for row in table.xpath(".//tr"):
            column = 0
            for cell in row.xpath("./th | ./td"):
                span = int(cell.get("colspan", 1))
                while len(widths) < column + span:
                    widths.append(0)
                tokens = re.split(r"[\s\-–—]+", cell.text_content())
                need = max((font.text_length(t, fontsize=9.5) for t in tokens), default=0) + 4
                if span == 1:
                    widths[column] = max(widths[column], need)
                column += span
        tables_section = next((p for p in table.iterancestors() if p.get("id") == "tables"), None)
        if tables_section is not None:
            root = table
            while root.getparent() is not tables_section:
                root = root.getparent()
            add_class(root, "pdf-main-table")
        else:
            root = next((p for p in table.iterancestors() if p.tag == "figure"), table)
        rows = len(table.xpath(".//tbody/tr"))
        if 40 <= rows <= 52:
            add_class(root, "pdf-compact-table")
        wide = sum(widths) > 550
        if wide:
            add_class(root, "pdf-wide")
        table_layout.append({"id": table.get("id") or root.get("id"),
                             "rows": rows, "landscape": wide})
    figures = main.xpath("./section[@id='figures']")[0]
    figure_blocks = [child for child in figures if isinstance(child.tag, str) and child.xpath(".//img")]
    for number, block in enumerate(figure_blocks):
        add_class(block, "pdf-main-figure")
        if number == 0:
            add_class(block, "pdf-first-figure")
    document = html.Element("html", lang="en")
    head = etree.SubElement(document, "head")
    etree.SubElement(head, "meta", charset="utf-8")
    etree.SubElement(head, "title").text = title.text_content()
    etree.SubElement(head, "style").text = (ROOT / "print.css").read_text()
    etree.SubElement(document, "body").append(main)
    destination = source.with_suffix(".print.html")
    destination.write_bytes(etree.tostring(document, method="html", encoding="utf-8", doctype="<!doctype html>"))
    return destination, table_layout
