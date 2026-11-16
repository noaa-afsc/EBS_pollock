"""Verify source content, images and links; compare publication layout separately."""
import argparse
from hashlib import md5, sha256
import io
import json
from pathlib import Path
import re
import unicodedata

from lxml import html
from PIL import Image, ImageChops
import pymupdf

SOURCE = Path(__file__).resolve().parent
RELEASE = SOURCE.parent / "2026"
BUILD = SOURCE / "_build/2026"


def normal(text):
    return re.sub(r"[^a-z0-9]", "", unicodedata.normalize("NFKD", text).casefold())


def link_value(link):
    return {key: str(value) for key, value in link.items() if key not in {"xref", "id"}}


def verify(require_exact=False, require_html_match=False, inspect=False):
    build_record = json.loads((BUILD / "assessment.render.json").read_text())
    for name, digest in build_record["source_inputs"].items():
        assert sha256((SOURCE / name).read_bytes()).hexdigest() == digest, f"Rebuild after changing {name}"
    for name in ("html", "pdf"):
        assert sha256((BUILD / f"assessment.{name}").read_bytes()).hexdigest() == build_record[f"{name}_sha256"], "Build artifact changed"
    manifest = json.loads((RELEASE / "release.json").read_text())
    for name, expected in manifest["files"].items():
        assert sha256((RELEASE / name).read_bytes()).hexdigest() == expected, name
    report = {"reference_version": manifest["version"],
              "layout_requirement": "Close visual match; pagination may differ",
              "source_sha256": sha256((SOURCE / "assessment.qmd").read_bytes()).hexdigest(),
              "candidate_sha256": sha256((BUILD / "assessment.pdf").read_bytes()).hexdigest(),
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
        report["math_review"] = "Native MathML with embedded Latin Modern Math; visual review recorded separately"
        images = [info for page in candidate for info in page.get_image_info(hashes=True)]
        expected_images = dom.xpath("//main//img/@src")
        report["source_images"] = len(expected_images)
        report["pdf_images"] = len(images)
        assert len(images) == len(expected_images), "Missing PDF images"
        hashes = {info["digest"] for info in images}
        unmatched = []
        alpha_verified = []
        for name in expected_images:
            with Image.open(RELEASE / name) as source_image:
                rgb = source_image.convert("RGB")
                if md5(rgb.tobytes()).digest() not in hashes:
                    # Transparent PNGs can have different invisible RGB values.
                    # Check alpha and the visible pixels composited on paper.
                    matched = False
                    if "A" in source_image.getbands():
                        expected = source_image.convert("RGBA")
                        paper = Image.new("RGBA", expected.size, "white")
                        visible = Image.alpha_composite(paper, expected).convert("RGB")
                        for page in candidate:
                            for item in page.get_images(full=True):
                                if item[2:4] != expected.size or not item[1]:
                                    continue
                                pixmap = pymupdf.Pixmap(pymupdf.Pixmap(candidate, item[0]),
                                                       pymupdf.Pixmap(candidate, item[1]))
                                actual = Image.open(io.BytesIO(pixmap.tobytes("png"))).convert("RGBA")
                                alpha_same = not ImageChops.difference(expected.getchannel("A"), actual.getchannel("A")).getbbox()
                                actual_visible = Image.alpha_composite(paper, actual).convert("RGB")
                                if alpha_same and not ImageChops.difference(visible, actual_visible).getbbox():
                                    matched = True
                        if matched:
                            alpha_verified.append(name)
                    if not matched:
                        unmatched.append(name)
        report["image_pixel_checks_requiring_visual_review"] = unmatched
        report["transparent_images_verified_on_white"] = alpha_verified
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
                                          or word[1] < -1 or word[3] > page.rect.height + 1
                                          for word in page.get_text("words"))]
        assert not report["off_page_text"], "Text extends outside page bounds"
        mismatch = []
        if require_exact and len(baseline) == len(candidate):
            for number, (before, after) in enumerate(zip(baseline, candidate), start=1):
                if before.rect != after.rect or before.get_pixmap(dpi=144).digest != after.get_pixmap(dpi=144).digest:
                    mismatch.append(number)
                if [link_value(link) for link in before.get_links()] != [link_value(link) for link in after.get_links()]:
                    mismatch.append(number)
        report["bookmark_layout_identical"] = baseline.get_toc() == candidate.get_toc()
        report["exact_layout"] = (len(baseline) == len(candidate) and not mismatch
                                  and report["bookmark_layout_identical"]) if require_exact else None
        report["different_pages"] = sorted(set(mismatch)) if require_exact else None
        report["manual_accessibility"] = "Not Tested"
        report["source_content_checks_passed"] = not unlocated and not unmatched and not report["off_page_text"]
    (BUILD.parent / "release-comparison.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({key: report[key] for key in ("html_byte_identical", "published_pages", "generated_pages",
                                                  "pdf_images", "links", "bookmarks", "exact_layout")}, indent=2))
    if not inspect and not report["source_content_checks_passed"]:
        raise SystemExit("Source content checks failed; inspect release-comparison.json.")
    if require_html_match and not report["html_byte_identical"]:
        raise SystemExit("HTML differs from the published reference.")
    if require_exact and not report["exact_layout"]:
        raise SystemExit("PDF differs from the optional exact-layout reference.")
    return report


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--inspect", action="store_true", help="Report content differences for review")
    parser.add_argument("--require-html-match", action="store_true", help="Also require published HTML byte identity")
    parser.add_argument("--require-exact", action="store_true", help="Optional archival comparison of every page pixel and link")
    args = parser.parse_args()
    verify(require_exact=args.require_exact, require_html_match=args.require_html_match, inspect=args.inspect)
