"""Compare the source rebuild with the published assessment. Exit 1 on a layout mismatch."""
import argparse
from hashlib import md5, sha256
import json
from pathlib import Path
import re
import unicodedata

from lxml import html
from PIL import Image
import pymupdf

SOURCE = Path(__file__).resolve().parent
RELEASE = SOURCE.parent / "2026"
BUILD = SOURCE / "_build/2026"


def normal(text):
    return re.sub(r"[^a-z0-9]", "", unicodedata.normalize("NFKD", text).casefold())


def link_value(link):
    return {key: str(value) for key, value in link.items() if key not in {"xref", "id"}}


def verify(require_exact):
    manifest = json.loads((RELEASE / "release.json").read_text())
    for name, expected in manifest["files"].items():
        assert sha256((RELEASE / name).read_bytes()).hexdigest() == expected, name
    report = {"reference_version": manifest["version"],
              "html_byte_identical": (BUILD / "assessment.html").read_bytes() ==
                                     (RELEASE / "assessment.html").read_bytes()}
    dom = html.parse(str(BUILD / "assessment.html"))
    with pymupdf.open(RELEASE / "assessment.pdf") as baseline, pymupdf.open(BUILD / "assessment.pdf") as candidate:
        report.update(published_pages=len(baseline), generated_pages=len(candidate))
        texts = []
        for sorted_text in (False, True):
            texts.append(normal("".join(page.get_text(
                clip=pymupdf.Rect(0, 0, page.rect.width, page.rect.height - 40), sort=sorted_text
            ) for page in candidate)))
        unlocated = []
        count = 0
        for element in dom.xpath("//main//p | //main//th | //main//td | //main//figcaption"):
            if element.xpath(".//math"):
                continue
            expected = normal(element.text_content())
            count += 1
            if expected and all(expected not in text for text in texts):
                unlocated.append(" ".join(element.text_content().split()))
        report["text_elements_checked"] = count
        report["text_elements_requiring_visual_review"] = unlocated
        report["math_review"] = "Not Tested"
        images = [info for page in candidate for info in page.get_image_info(hashes=True)]
        expected_images = dom.xpath("//main//img/@src")
        report["source_images"] = len(expected_images)
        report["pdf_images"] = len(images)
        assert len(images) == len(expected_images), "Missing PDF images"
        hashes = {info["digest"] for info in images}
        unmatched = []
        for name in expected_images:
            with Image.open(RELEASE / name) as source_image:
                rgb = source_image.convert("RGB")
                if md5(rgb.tobytes()).digest() not in hashes:
                    unmatched.append(name)
        report["image_pixel_checks_requiring_visual_review"] = unmatched
        names = candidate.resolve_names()
        links = [link for page in candidate for link in page.get_links()]
        for link in links:
            if link["kind"] in (pymupdf.LINK_GOTO, pymupdf.LINK_NAMED):
                assert 0 <= link.get("page", -1) < len(candidate), link
            if "nameddest" in link:
                assert link["nameddest"] in names, link
            assert not link.get("uri", "").startswith("file:"), link
        report["links"] = len(links)
        report["bookmarks"] = len(candidate.get_toc())
        report["off_page_text"] = [index + 1 for index, page in enumerate(candidate)
                                   if any(word[0] < -1 or word[2] > page.rect.width + 1
                                          for word in page.get_text("words"))]
        assert not report["off_page_text"], "Text extends outside page bounds"
        mismatch = []
        if len(baseline) == len(candidate):
            for number, (before, after) in enumerate(zip(baseline, candidate), start=1):
                if before.rect != after.rect or before.get_pixmap(dpi=144).digest != after.get_pixmap(dpi=144).digest:
                    mismatch.append(number)
                if [link_value(link) for link in before.get_links()] != [link_value(link) for link in after.get_links()]:
                    mismatch.append(number)
        report["bookmark_layout_identical"] = baseline.get_toc() == candidate.get_toc()
        report["exact_layout"] = (len(baseline) == len(candidate) and not mismatch
                                  and report["bookmark_layout_identical"])
        report["different_pages"] = sorted(set(mismatch))
        report["manual_accessibility"] = "Not Tested"
    (BUILD.parent / "release-comparison.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({key: report[key] for key in ("html_byte_identical", "published_pages", "generated_pages",
                                                  "pdf_images", "links", "bookmarks", "exact_layout")}, indent=2))
    if require_exact and not (report["exact_layout"] and report["html_byte_identical"]):
        raise SystemExit("Publication pending: the rebuilt PDF differs from the required published layout.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--inspect", action="store_true", help="Write the comparison without enforcing the release gate")
    args = parser.parse_args()
    verify(require_exact=not args.inspect)
