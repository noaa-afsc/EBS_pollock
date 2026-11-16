"""Promote a source build after verifying its content, images and links."""
from hashlib import sha256
from datetime import date
import json
from pathlib import Path
import re
import shutil
import subprocess
from tempfile import TemporaryDirectory

from verify_release import verify

ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "safe-report/_build/2026"
RELEASE = ROOT / "2026"

# Recheck immediately before the writes, including when invoked directly.
verification = verify()
manifest = json.loads((RELEASE / "release.json").read_text())
manifest["report_entrypoint"] = "safe-report/assessment.qmd"
manifest["source_base_revision"] = subprocess.check_output(
    ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
).strip()
manifest.pop("source_revision", None)
manifest["source_tree_dirty"] = bool(subprocess.check_output(
    ["git", "status", "--porcelain", "--", "safe-report"], cwd=ROOT, text=True
).strip())
manifest["build_inputs_sha256"] = json.loads((BUILD / "assessment.render.json").read_text())["source_inputs"]
manifest["source_sha256"] = sha256((ROOT / "safe-report/assessment.qmd").read_bytes()).hexdigest()
manifest["PDF_layout"] = "QMD print template; close visual match with pagination allowed to differ"
manifest["source_content_verification"] = verification["source_content_checks_passed"]
manifest["pages"] = json.loads((BUILD.parent / "release-comparison.json").read_text())["generated_pages"]
today = date.today()
build_key = sha256(json.dumps(manifest["build_inputs_sha256"], sort_keys=True).encode()).hexdigest()[:8]
manifest["version"] = f"{today.isoformat()}-qmd-{build_key}"
homepage = (ROOT / "index.qmd").read_text()
homepage, replacements = re.subn(r"\*\*Version:.*?\*\*",
    f"**Version: {today.strftime('%B')} {today.day}, {today.year} · Review draft · Built from assessment.qmd**",
    homepage, count=1)
assert replacements == 1, "Homepage version marker missing"
names = ("assessment.html", "assessment.pdf", "release.json")
targets = {name: RELEASE / name for name in names}
targets["index.qmd"] = ROOT / "index.qmd"
with TemporaryDirectory(prefix="assessment-release-", dir=ROOT / "safe-report/_build") as work:
    staging = Path(work)
    for name, target in targets.items():
        shutil.copyfile(target, staging / (name + ".before"))
    for name in names[:2]:
        shutil.copyfile(BUILD / name, staging / name)
        manifest["files"][name] = sha256((staging / name).read_bytes()).hexdigest()
    (staging / "release.json").write_text(json.dumps(manifest, indent=2) + "\n")
    (staging / "index.qmd").write_text(homepage)
    try:
        for name, target in targets.items():
            (staging / name).replace(target)
    except OSError:
        for name, target in targets.items():
            (staging / (name + ".before")).replace(target)
        raise
