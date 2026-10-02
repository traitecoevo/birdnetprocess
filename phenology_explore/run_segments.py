#!/usr/bin/env python
"""Extract the top-20 most-confident audio segments per species, per site.

Reads the detection tables produced by run_detector.py and slices the matching
recordings into per-species folders of example clips. Uses collection_mode
"confidence" so the 20 highest-confidence detections of each species are kept.

Driven via importlib (not the CLI) to avoid the birdnet_analyzer import
name-collision bug; guarded for macOS spawn-multiprocessing.

Run with the BirdNET-Analyzer venv:
    /Users/z3484779/Documents/ecoacoustics/BirdNET-Analyzer/.venv/bin/python \
        phenology_explore/run_segments.py
"""
import os
import importlib

segments = importlib.import_module("birdnet_analyzer.segments.core").segments

REC_BASE = "/Users/z3484779/Library/CloudStorage/OneDrive-UNSW/call_library/Smiths Lake Long Format Recordings"
DET_BASE = "/Users/z3484779/Documents/birdnet_play/smiths_lake"          # detection tables
SEG_BASE = "/Users/z3484779/Documents/birdnet_play/smiths_lake_segments"  # output clips

SITES = ["Powerline Strip", "Powerline Field", "Mowed Field"]


def main():
    for site in SITES:
        out_dir = os.path.join(SEG_BASE, site)
        os.makedirs(out_dir, exist_ok=True)
        print(f">>> {site}", flush=True)
        segments(
            os.path.join(REC_BASE, site),       # audio_input
            output=out_dir,
            results=os.path.join(DET_BASE, site),
            min_conf=0.7,
            max_segments=20,
            collection_mode="confidence",        # keep the most confident
            seg_length=3.0,
            audio_speed=1.0,
            threads=6,
        )
    print(f">>> done; segments under {SEG_BASE}", flush=True)


if __name__ == "__main__":
    import multiprocessing
    multiprocessing.freeze_support()
    main()
