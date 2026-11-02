"""Apply has_dream_nightmare / has_sleep_paralysis keyword flags to a review CSV.

Automatically detects per-row language (English vs. Japanese) from the commentary
text and applies the appropriate vocabulary. English vocabulary follows
OUTCOME_VOCAB (Natter 2021 style); Japanese vocabulary follows
OUTCOME_VOCAB_JA_NIGHTMARE / OUTCOME_VOCAB_JA_SP.

Rules (both languages, matching existing project conventions):
  - has_dream_nightmare = 1 if any of primary/secondary/sub matches,
    excluding rows that only match "exclude" terms.
  - has_sleep_paralysis = 1 if any SP category matches (any of direct /
    intruder / incubus / ube / immobile), after removing exclude matches
    (facial paralysis, Bell's palsy).
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from config import (
    VOCAB_RX,
    VOCAB_RX_JA_NIGHTMARE,
    VOCAB_RX_JA_SP,
    _has_japanese,
    normalize_text,
)

# English SP vocabulary (keyword list from CLAUDE.md / Natter-equivalent).
# We keep it here instead of config.py to avoid a larger refactor; the
# SP detection for English reviews uses these regexes.
import re as _re

EN_SP_PATTERNS = [_re.compile(p, _re.I) for p in [
    r"\bsleep paralysis\b",
    r"\bparaly[sz]ed\b", r"\bparaly[sz]is\b",
    r"\bparaly[sz]ing\b",
    r"\bparalyisis\b", r"\bparaly[sz]ied\b",
    r"\bcouldn'?t move\b", r"\bcan'?t move\b",
    r"\bcould not move\b", r"\bcannot move\b", r"\bunable to move\b",
    r"\bcouldn'?t get up\b",
    r"\bfrozen\b", r"\bfroze\b", r"\bimmobili[sz]ed?\b",
    r"\bhallucinations?\b",
    r"\bhallucinat", r"\bhalucinat", r"\bhullucinat", r"\ballusinat",
    r"\bhypnagogic\b", r"\bhypnopompic\b", r"\bhypnogogic\b",
    r"\bpresence\b", r"\bsomeone in (?:my )?room\b",
    r"\bshadow(?:s|y)?\b", r"\bintruder\b",
    r"\bdemons?\b", r"\bdemonic\b", r"\bevil\b",
    r"\bchest pressure\b", r"\bweight on (?:my )?chest\b",
    r"\bpressure on (?:my )?chest\b",
    r"\bcouldn'?t breathe?\b", r"\bcan'?t breathe?\b",
    r"\bshortness of breath\b", r"\bbreathless(?:ness)?\b",
    r"\bsuffocat", r"\bchok(?:ing|ed)\b",
    r"\bfloating\b", r"\bout of body\b", r"\bflying\b",
]]
EN_SP_EXCLUDE = [_re.compile(p, _re.I) for p in [
    r"\bfacial paralysis\b", r"\bbell'?s palsy\b",
    r"\banalysis paralysis\b",
    r"\bparaly[sz]ed\s+(?:with|by|from)\s+fear\b",
    r"\bno paralysis\b", r"\bwithout paralysis\b",
]]


def _collect_matches(text: str, patterns) -> list[str]:
    """Return unique literal matches (preserving order). Drops any match
    that is strictly a substring of another match, so "hallucinations"
    subsumes the prefix-only regex hit "hallucinat"."""
    out: list[str] = []
    for p in patterns:
        for m in p.finditer(text):
            hit = m.group(0).strip()
            if hit and hit not in out:
                out.append(hit)
    return [s for s in out if not any(len(o) > len(s) and s in o for o in out)]


def nightmare_matches(norm_text: str, is_ja: bool) -> list[str]:
    if not norm_text:
        return []
    rx = VOCAB_RX_JA_NIGHTMARE if is_ja else VOCAB_RX
    pats = []
    for cat in ("primary", "secondary", "sub"):
        pats.extend(rx.get(cat, []))
    return _collect_matches(norm_text, pats)


def sp_matches(norm_text: str, is_ja: bool) -> list[str]:
    if not norm_text:
        return []
    if is_ja:
        rx = VOCAB_RX_JA_SP
        for p in rx.get("exclude", []):
            if p.search(norm_text):
                return []
        pats = []
        for cat in ("direct", "intruder", "incubus", "ube", "immobile"):
            pats.extend(rx.get(cat, []))
        return _collect_matches(norm_text, pats)
    else:
        for p in EN_SP_EXCLUDE:
            if p.search(norm_text):
                return []
        return _collect_matches(norm_text, EN_SP_PATTERNS)


def has_nightmare(norm_text: str, is_ja: bool) -> int:
    return 1 if nightmare_matches(norm_text, is_ja) else 0


def has_sp(norm_text: str, is_ja: bool) -> int:
    return 1 if sp_matches(norm_text, is_ja) else 0


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("input_csv", help="Review CSV with commentary column")
    ap.add_argument("output_csv", nargs="?", help="Output path (default: overwrite)")
    args = ap.parse_args()

    in_path = Path(args.input_csv)
    out_path = Path(args.output_csv or args.input_csv)

    df = pd.read_csv(in_path, low_memory=False)
    has_nad = []
    has_spp = []
    mkw_nad = []
    mkw_sp = []
    for commentary in df["commentary"].fillna("").astype(str):
        is_ja = _has_japanese(commentary)
        norm = normalize_text(commentary, lang="ja" if is_ja else "en")
        n_hits = nightmare_matches(norm, is_ja)
        s_hits = sp_matches(norm, is_ja)
        has_nad.append(1 if n_hits else 0)
        has_spp.append(1 if s_hits else 0)
        mkw_nad.append(", ".join(n_hits))
        mkw_sp.append(", ".join(s_hits))
    df["has_dream_nightmare"] = has_nad
    df["has_sleep_paralysis"] = has_spp
    df["matched_keywords_nad"] = mkw_nad
    df["matched_keywords_sp"] = mkw_sp

    out_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out_path, index=False, encoding="utf-8")
    print(f"Input : {in_path} ({len(df)} rows)")
    print(f"Output: {out_path}")
    print(f"  has_dream_nightmare = 1: {sum(has_nad)}")
    print(f"  has_sleep_paralysis = 1: {sum(has_spp)}")


if __name__ == "__main__":
    main()
