from __future__ import annotations

import argparse
from pathlib import Path
import json
import sys
from typing import Dict, Any, List

import pandas as pd

from scrapers import SCRAPER_REGISTRY
from scrapers.base import BaseScraper, Review


def load_ledger(path: str | Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    if "start_url" not in df.columns and "review_list_url" in df.columns:
        df = df.rename(columns={"review_list_url": "start_url"})
    if "snapshot_date" not in df.columns:
        df["snapshot_date"] = ""
    if "enabled" not in df.columns:
        df["enabled"] = 1
    required = {"site", "drug", "start_url", "paging_rule", "enabled"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"ledger missing columns: {missing}")
    return df


def ensure_out_dirs(out_dir: Path):
    out_dir.mkdir(parents=True, exist_ok=True)


def write_log_line(log_path: Path, payload: Dict[str, Any]):
    with log_path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(payload, ensure_ascii=False) + "\n")


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ledger", default="url_ledger.csv")
    ap.add_argument("--out", default="outputs")
    ap.add_argument("--rate", type=float, default=1.5)
    ap.add_argument("--max-pages", type=int, default=200)
    args = ap.parse_args(argv)

    out_dir = Path(args.out)
    ensure_out_dirs(out_dir)
    log_path = out_dir / "scrape_log.jsonl"

    ledger = load_ledger(args.ledger)
    all_reviews: List[Review] = []

    for _, row in ledger.iterrows():
        if int(row.get("enabled", 1)) == 0:
            continue
        site = str(row["site"]).strip()
        drug = str(row["drug"]).strip()
        start_url = str(row["start_url"]).strip()
        paging_rule = str(row.get("paging_rule", "")).strip()
        if not start_url:
            continue
        scraper_cls = SCRAPER_REGISTRY.get(site)
        if not scraper_cls:
            write_log_line(log_path, {"site": site, "drug": drug, "error": "unknown site"})
            continue

        scraper = scraper_cls(
            drug=drug,
            start_url=start_url,
            paging_rule=paging_rule if paging_rule else None,
            out_dir=out_dir,
            rate_limit_s=args.rate,
            max_pages=args.max_pages,
        )
        result = scraper.scrape()
        all_reviews.extend(result.reviews)
        write_log_line(log_path, BaseScraper.result_to_log(result, site, drug, start_url))

    if all_reviews:
        df = pd.DataFrame([r.__dict__ for r in all_reviews])
        df.to_csv(out_dir / "raw_reviews.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
