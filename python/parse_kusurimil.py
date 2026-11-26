"""Parse the Kusurimil hand-search .txt files into the unified review CSV
schema and verify per-file review counts match the header.

Input:  クスリミルレビュー置き場_20260424/*.txt
Output: outputs/hand_reviews_kusurimil_20260424.csv

Each file looks like:
    <drug name>
    レビュー (N件)
    いいね＝＝＝＝＝＝＝＝＝＝＝＝＝
    <rating, e.g. 3.0>
    <title / short summary>
    <user handle, e.g. 名無しさん>
    <body line 1>
    <body line 2 ... blank line ends the review>
    <M いいね＝＝＝＝＝＝＝＝＝＝＝＝＝>   (next review's separator)
    ...
"""
from __future__ import annotations

import hashlib
import re
from pathlib import Path
from typing import List, Dict, Tuple

import pandas as pd

from config import normalize_text

SRC_DIR = Path("クスリミルレビュー置き場_20260424")
OUT_CSV = Path("outputs/hand_reviews_kusurimil_20260424.csv")
SNAPSHOT = "2026-04-24"
SOURCE_URL = "(Kusurimil hand-search 2026-04-24)"

# filename stem (lowercased) → canonical generic name
DRUG_NAME_MAP = {
    "eszopiclone": "eszopiclone",
    "lemborexant": "lemborexant",
    "ramelteon": "ramelteon",
    "suvorexant": "suvorexant",
    "trazodone": "trazodone",
    "triazoram": "triazolam",   # typo in filename
    "zolpidem": "zolpidem",
}

SEP_RE = re.compile(r"＝{3,}")                 # separator line
RATING_RE = re.compile(r"^\s*\d+(?:\.\d+)?\s*$")
COUNT_RE = re.compile(r"\((\d+)\s*件\)")


def parse_file(path: Path) -> Tuple[str, int, List[Dict]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    # Header
    drug_filename = path.stem.lower()
    drug = DRUG_NAME_MAP.get(drug_filename)
    if drug is None:
        raise ValueError(f"Unknown drug filename: {path}")

    # expected count
    expected = None
    for L in lines[:5]:
        m = COUNT_RE.search(L)
        if m:
            expected = int(m.group(1))
            break

    # Separator indices (each review is bounded by these)
    sep_idx = [i for i, L in enumerate(lines) if SEP_RE.search(L)]
    if not sep_idx:
        raise ValueError(f"No separator found: {path}")

    reviews = []
    for start, end in zip(sep_idx, sep_idx[1:] + [len(lines)]):
        block_raw = lines[start + 1 : end]
        # Drop trailing empty lines
        while block_raw and not block_raw[-1].strip():
            block_raw.pop()
        # Also drop leading empty lines
        while block_raw and not block_raw[0].strip():
            block_raw.pop(0)
        if not block_raw:
            continue

        # Parse positional
        idx = 0
        rating = None
        if idx < len(block_raw) and RATING_RE.match(block_raw[idx]):
            rating = block_raw[idx].strip()
            idx += 1
        title = block_raw[idx].strip() if idx < len(block_raw) else ""
        idx += 1
        user = block_raw[idx].strip() if idx < len(block_raw) else ""
        idx += 1
        body_lines = block_raw[idx:]
        body = "\n".join(body_lines).strip()
        commentary = (title + "\n" + body).strip() if body else title.strip()

        if not commentary:
            continue
        reviews.append({
            "drug": drug,
            "commentary": commentary,
            "rating": rating,
            "_user": user,
            "_title": title,
        })

    return drug, expected, reviews


def main() -> None:
    all_rows: List[Dict] = []
    print("=== Parse & count check ===")
    for path in sorted(SRC_DIR.glob("*.txt")):
        drug, expected, reviews = parse_file(path)
        status = "OK" if expected == len(reviews) else f"MISMATCH (expected {expected})"
        print(f"  {path.name:<20} drug={drug:<12} parsed={len(reviews):>4}  {status}")
        for r in reviews:
            all_rows.append({
                "drug": drug,
                "commentary": r["commentary"],
                "site": "kusurimil",
                "source_url": SOURCE_URL,
                "scraped_at": f"{SNAPSHOT}T00:00:00Z",
                "review_date": None,
                "duration": None,
                "condition": None,
                "age": None,
                "sex": None,
                "rating": r["rating"],
                "side_effects": None,
                "dose_quantity": None,
                "dose_strength": None,
                "dose_fdosage": None,
                "dose_frequency": None,
            })

    df = pd.DataFrame(all_rows)
    df["commentary_norm"] = df["commentary"].apply(lambda s: normalize_text(s, lang="ja"))
    df["commentary_hash"] = df["commentary_norm"].apply(
        lambda s: hashlib.sha1(s.encode("utf-8")).hexdigest() if s else ""
    )

    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(OUT_CSV, index=False, encoding="utf-8")
    print(f"\nWrote {len(df)} rows → {OUT_CSV}")
    print("\nBy drug:")
    print(df.groupby("drug").size().sort_values(ascending=False))


if __name__ == "__main__":
    main()
