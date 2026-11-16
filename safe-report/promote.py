"""Publish a verified QMD build, including its local web styles and scripts."""
from datetime import date
from hashlib import sha256
import json
from pathlib import Path
import re
import shutil
import subprocess
from tempfile import TemporaryDirectory
from verify_release import verify

ROOT=Path(__file__).resolve().parents[1]
BUILD=ROOT/'safe-report/_build/2026'
RELEASE=ROOT/'2026'
verification=verify()
manifest=json.loads((RELEASE/'release.json').read_text())
record=json.loads((BUILD/'assessment.render.json').read_text())
manifest.update(report_entrypoint='safe-report/EBSpollock.qmd',
    source_sha256=sha256((ROOT/'safe-report/EBSpollock.qmd').read_bytes()).hexdigest(),
    build_inputs_sha256=record['source_inputs'], pages=verification['generated_pages'],
    PDF_layout='QMD print template; close visual match with pagination allowed to differ',
    authoring_format='Native Quarto Markdown and editable CSV tables',
    layout_note='Both formats generated from EBSpollock.qmd; PDF uses a dedicated print stylesheet. Pagination may differ from earlier releases.',
    source_content_verification=verification['source_content_checks_passed'])
manifest['source_base_revision']=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
manifest.pop('source_revision',None)
manifest['source_tree_dirty']=bool(subprocess.check_output(['git','status','--porcelain','--','safe-report'],cwd=ROOT,text=True).strip())
today=date.today()
build_key=sha256(json.dumps(record['source_inputs'],sort_keys=True).encode()).hexdigest()[:8]
manifest['version']=f'{today.isoformat()}-qmd-{build_key}'
homepage=(ROOT/'index.qmd').read_text()
homepage,count=re.subn(r'\*\*Version:.*?\*\*',
    f'**Version: {today.strftime("%B")} {today.day}, {today.year} · Review draft · Built from EBSpollock.qmd**',homepage,count=1)
assert count==1,'Homepage version marker missing'
resources=['report.css']+[str(p.relative_to(BUILD)) for p in sorted((BUILD/'EBSpollock_files').rglob('*')) if p.is_file()]
build_files=['assessment.html','assessment.pdf']+resources
for name in build_files:
    manifest['files'][name]=sha256((BUILD/name).read_bytes()).hexdigest()
targets={name:RELEASE/name for name in build_files+['release.json']}
targets['index.qmd']=ROOT/'index.qmd'
with TemporaryDirectory(prefix='assessment-release-',dir=ROOT/'safe-report/_build') as temporary:
    staging=Path(temporary);written=[]
    for name,target in targets.items():
        backup=staging/'before'/name;backup.parent.mkdir(parents=True,exist_ok=True)
        if target.exists():shutil.copyfile(target,backup)
        output=staging/'after'/name;output.parent.mkdir(parents=True,exist_ok=True)
        if name in build_files:shutil.copyfile(BUILD/name,output)
        elif name=='release.json':output.write_text(json.dumps(manifest,indent=2)+'\n')
        else:output.write_text(homepage)
    try:
        for name,target in targets.items():
            target.parent.mkdir(parents=True,exist_ok=True)
            (staging/'after'/name).replace(target);written.append(name)
    except OSError:
        for name in reversed(written):
            backup=staging/'before'/name
            if backup.exists():backup.replace(targets[name])
            else:targets[name].unlink(missing_ok=True)
        raise
print('Promoted EBSpollock.qmd build:',manifest['version'])
