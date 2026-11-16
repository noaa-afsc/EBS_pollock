"""Typeset the entire assessment from the QMD-generated HTML and figure assets."""
import argparse
from hashlib import sha256
import json
from pathlib import Path
from urllib.parse import urljoin

from playwright.sync_api import sync_playwright
from prepare_print import prepare

ROOT = Path(__file__).resolve().parent
PUBLIC = "https://noaa-afsc.github.io/EBS_pollock/2026/"


def render(source, destination):
    print_source, table_layout = prepare(source)
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch()
        page = browser.new_page()
        # Embedded HTML assets and local figure files make the build offline.
        page.route("http://**/*", lambda route: route.abort())
        page.route("https://**/*", lambda route: route.abort())
        page.goto(print_source.resolve().as_uri(), wait_until="load")
        page.emulate_media(media="print")
        page.evaluate("""() => {
            for (const node of [...document.body.childNodes]) {
                if (node.nodeType === Node.TEXT_NODE && node.textContent.trim() === 'html') {
                    node.remove();
                }
            }
        }""")
        page.evaluate("document.fonts.ready")
        page.evaluate("document.querySelectorAll('main img').forEach(image => image.loading = 'eager')")
        page.wait_for_function("[...document.querySelectorAll('main img')].every(image => image.complete && image.naturalWidth > 0)")
        images = page.evaluate("""() => {
            return [...document.querySelectorAll('main img')].map(image => ({
                src: image.getAttribute('src'), width: image.naturalWidth,
                height: image.naturalHeight
            }));
        }""")
        assert images and all(image["width"] > 0 for image in images), "Unloaded report image"
        # Keep fragment references inside the PDF. Resolve file downloads to
        # their published addresses so links work away from the build machine.
        for anchor in page.locator("main a[href]").all():
            href = anchor.get_attribute("href")
            if not href.startswith("#"):
                anchor.evaluate("(element, href) => element.href = href", urljoin(PUBLIC, href))
        destination.parent.mkdir(parents=True, exist_ok=True)
        temporary = destination.with_suffix(".tmp.pdf")
        try:
            page.pdf(path=str(temporary), prefer_css_page_size=True, print_background=True,
                     tagged=True, outline=True, display_header_footer=False)
            temporary.replace(destination)
        finally:
            temporary.unlink(missing_ok=True)
        record = {"renderer": "Chromium", "browser_version": browser.version,
                  "images": images, "source": source.name, "pdf": destination.name,
                  "table_layout": table_layout,
                  "template": "dedicated-print",
                  "historical_pdf_input": False}
        inputs = ["EBSpollock.qmd", "_quarto.yml", "tables.lua", "finish_html.py", "prepare_print.py", "print.css",
                  "report.css", "render_pdf.py", "pdf-requirements.txt", "render.sh", "post-render.sh"]
        inputs += [str(path.relative_to(ROOT)) for path in sorted((ROOT / "fonts").glob("*.otf"))]
        inputs += [str(path.relative_to(ROOT)) for path in sorted((ROOT / "tables").glob("*")) if path.is_file()]
        record["source_inputs"] = {name: sha256((ROOT / name).read_bytes()).hexdigest() for name in inputs}
        record["html_sha256"] = sha256(source.read_bytes()).hexdigest()
        record["pdf_sha256"] = sha256(destination.read_bytes()).hexdigest()
        destination.with_suffix(".render.json").write_text(json.dumps(record, indent=2) + "\n")
        browser.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("html", type=Path)
    parser.add_argument("pdf", type=Path)
    args = parser.parse_args()
    render(args.html, args.pdf)
