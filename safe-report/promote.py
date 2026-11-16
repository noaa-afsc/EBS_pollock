"""Promote a verified exact-layout source build to the website's release directory."""
from hashlib import sha256
import json
from pathlib import Path
import shutil
import subprocess
from tempfile import TemporaryDirectory

from verify_release import verify

ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "safe-report/_build/2026"
RELEASE = ROOT / "2026"

# Recheck immediately before the writes, including when invoked directly.
verify(require_exact=True)
manifest = json.loads((RELEASE / "release.json").read_text())
manifest["report_entrypoint"] = "safe-report/assessment.qmd"
manifest["source_revision"] = subprocess.check_output(
    ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
).strip()
manifest["source_sha256"] = sha256((ROOT / "safe-report/assessment.qmd").read_bytes()).hexdigest()
manifest["pages"] = json.loads((BUILD.parent / "release-comparison.json").read_text())["generated_pages"]
names = ("assessment.html", "assessment.pdf", "release.json")
with TemporaryDirectory(prefix="assessment-release-", dir=ROOT / "safe-report/_build") as work:
    staging = Path(work)
    for name in names:
        shutil.copyfile(RELEASE / name, staging / (name + ".before"))
    for name in names[:2]:
        shutil.copyfile(BUILD / name, staging / name)
        manifest["files"][name] = sha256((staging / name).read_bytes()).hexdigest()
    (staging / "release.json").write_text(json.dumps(manifest, indent=2) + "\n")
    try:
        for name in names:
            (staging / name).replace(RELEASE / name)
    except OSError:
        for name in names:
            (staging / (name + ".before")).replace(RELEASE / name)
        raise
