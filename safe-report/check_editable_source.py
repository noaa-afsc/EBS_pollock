"""Check that narrative, table, and caption edits reach both output formats."""
import os
import csv
from pathlib import Path
import re
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory

import pymupdf

ROOT = Path(__file__).resolve().parent
with TemporaryDirectory(prefix="assessment-source-") as temporary:
    work = Path(temporary)
    source = work / "safe-report"
    source.mkdir()
    for path in ROOT.iterdir():
        if path.is_file() and path.suffix not in (".html", ".pdf"):
            shutil.copyfile(path, source / path.name)
    shutil.copytree(ROOT / "fonts", source / "fonts")
    shutil.copytree(ROOT / "tables", source / "tables")
    (work / "2026").mkdir()
    (work / "2026/assets").symlink_to(ROOT.parent / "2026/assets", target_is_directory=True)
    qmd = source / "EBSpollock.qmd"
    text = qmd.read_text()
    text = text.replace("Directed foreign fishing began in 1964.",
                        "Directed foreign fishing began in 1964. NARRATIVEEDIT123.", 1)
    text = text.replace('Nominal seasonal catch per tow-hour',
                        'CAPTIONEDIT123 Nominal seasonal catch per tow-hour', 1)
    table = source / "tables/001-advice-model260.csv"
    with table.open(newline="") as stream:
        rows = list(csv.reader(stream))
    rows[4][1] = "TABLEEDIT123"
    with table.open("w", newline="") as stream:
        csv.writer(stream, lineterminator="\n").writerows(rows)
    qmd.write_text(text)
    environment = os.environ.copy()
    environment["SAFE_PYTHON"] = sys.executable
    subprocess.run(["bash", "safe-report/render.sh"], cwd=work, env=environment, check=True)
    output = source / "_build/2026"
    with pymupdf.open(output / "assessment.pdf") as document:
        pdf_text = " ".join(page.get_text() for page in document)
    for marker in ("NARRATIVEEDIT123", "CAPTIONEDIT123", "TABLEEDIT123"):
        assert marker in pdf_text, marker
        assert marker in (output / "assessment.html").read_text(), marker
    assert not (work / ".git").exists()
    assert not (work / "2026/assessment.pdf").exists()
print("PASS: narrative, table and caption edits reach both formats without an input PDF or Git history")
