#!/usr/bin/env python
"""Run the pelican classifier over each site's TARGET 24h window -> Raven tables.

This is the "detector" step and lives OUTSIDE the R package. It calls
birdnet_analyzer.analyze() directly (not the `birdnet-analyze` CLI) to avoid the
import name-collision bug, and only processes the recordings that fall in each
site's chosen midnight-to-midnight day, so we don't analyse the whole
deployment. A ~70 min lead file is included so detections right after midnight
(which start in the previous hourly file) are covered; the R side filters back
to the exact calendar date.

Run with the BirdNET-Analyzer venv:
    /Users/z3484779/Documents/ecoacoustics/BirdNET-Analyzer/.venv/bin/python \
        phenology_explore/run_detector.py
"""
import os
import re
import glob
import shutil
import importlib
from datetime import datetime, timedelta

# Import the analyze() function via importlib rather than `import birdnet_analyzer`
# / `from birdnet_analyzer import analyze`: the Apple-Silicon hang workaround makes
# the `birdnet_analyzer.analyze` attribute resolve unreliably (sometimes the
# submodule, sometimes missing). import_module returns the core module directly.
analyze = importlib.import_module("birdnet_analyzer.analyze.core").analyze

CLASSIFIER = "/Users/z3484779/Library/CloudStorage/OneDrive-UNSW/call_library/recognizers/pelican0-15.tflite"
REC_BASE = "/Users/z3484779/Library/CloudStorage/OneDrive-UNSW/call_library/Smiths Lake Long Format Recordings"
OUT_BASE = "/Users/z3484779/Documents/birdnet_play/smiths_lake"
STAGE_BASE = os.path.join(OUT_BASE, "_staging")

# site -> the continuous 24h (midnight-midnight) window to analyse
TARGETS = {
    "Powerline Strip": "2026-02-03",
    "Powerline Field": "2026-02-04",
    "Mowed Field": "2026-02-03",
}

LEAD = timedelta(minutes=70)   # grab the hourly file straddling midnight
STAMP = re.compile(r"_(\d{8})_(\d{6})\.wav$", re.IGNORECASE)


def files_in_window(site, day):
    """wavs whose start time falls in [day 00:00 - LEAD, next day 00:00)."""
    start = datetime.strptime(day, "%Y-%m-%d")
    lo, hi = start - LEAD, start + timedelta(days=1)
    out = []
    for path in glob.glob(os.path.join(REC_BASE, site, "*.wav")):
        m = STAMP.search(os.path.basename(path))
        if not m:
            continue
        t = datetime.strptime(m.group(1) + m.group(2), "%Y%m%d%H%M%S")
        if lo <= t < hi:
            out.append(path)
    return sorted(out)


def main():
    for site, day in TARGETS.items():
        wavs = files_in_window(site, day)
        stage = os.path.join(STAGE_BASE, site)
        out_dir = os.path.join(OUT_BASE, site)
        shutil.rmtree(stage, ignore_errors=True)
        os.makedirs(stage, exist_ok=True)
        os.makedirs(out_dir, exist_ok=True)
        for w in wavs:
            link = os.path.join(stage, os.path.basename(w))
            if not os.path.lexists(link):
                os.symlink(w, link)
        print(f">>> {site} [{day}] - {len(wavs)} files", flush=True)
        analyze(
            stage,
            output=out_dir,
            classifier=CLASSIFIER,
            fmin=0,
            fmax=15000,
            overlap=0.0,
            audio_speed=1.0,
            merge_consecutive=1,
            min_conf=0.1,
            rtype="table",
            skip_existing_results=True,
            threads=6,
        )
    print(f">>> done; detections under {OUT_BASE}", flush=True)


# Guard required: analyze() uses multiprocessing, which on macOS/Py3.13 spawns
# workers by re-importing this module. Without the guard the staging loop reruns
# in every child process.
if __name__ == "__main__":
    import multiprocessing
    multiprocessing.freeze_support()
    main()
