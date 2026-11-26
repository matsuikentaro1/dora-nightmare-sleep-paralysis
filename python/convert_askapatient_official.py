"""Convert AskAPatient official xlsx → extended-schema CSV.

Input : AskAPatient_raw-data_kentaro-research-insomnia.xlsx (3,916 rows)
Output: outputs/askapatient_official_20260424.csv (extended schema + hash)
"""
from __future__ import annotations

import hashlib
from pathlib import Path

import pandas as pd

from config import normalize_text

INPUT_XLSX = Path("AskAPatient_raw-data_kentaro-research-insomnia.xlsx")
OUTPUT_CSV = Path("outputs/askapatient_official_20260424.csv")
SNAPSHOT_DATE = "2026-04-24"
SOURCE_URL = "(official dataset provided by AskAPatient 2026-04-24)"

INGREDIENT_TO_GENERIC = {
    "ZOLPIDEM TARTRATE": "zolpidem",
    "ESZOPICLONE": "eszopiclone",
    "TRAZODONE HYDROCHLORIDE": "trazodone",
    "DOXEPIN HYDROCHLORIDE": "doxepin",
    "RAMELTEON": "ramelteon",
    "TEMAZEPAM": "temazepam",
    "ZALEPLON": "zaleplon",
    "TRIAZOLAM": "triazolam",
    "LEMBOREXANT": "lemborexant",
    "SUVOREXANT": "suvorexant",
    "daridorexant": "daridorexant",
}

GENDER_MAP = {"F": "Female", "M": "Male"}


def main() -> None:
    df = pd.read_excel(INPUT_XLSX)
    print(f"Loaded {len(df)} rows from {INPUT_XLSX}")

    unknown = set(df["Ingredient"].unique()) - set(INGREDIENT_TO_GENERIC)
    if unknown:
        raise ValueError(f"Unknown Ingredient values: {unknown}")

    out = pd.DataFrame()
    out["drug"] = df["Ingredient"].map(INGREDIENT_TO_GENERIC)
    out["commentary"] = df["Comments"].fillna("").astype(str)
    out["site"] = "askapatient"
    out["source_url"] = SOURCE_URL
    out["scraped_at"] = f"{SNAPSHOT_DATE}T00:00:00Z"
    out["duration"] = df["Timelength"]
    out["condition"] = df["Reason"]
    out["age"] = df["Age"]
    out["sex"] = df["Gender"].map(GENDER_MAP).fillna(df["Gender"])
    out["rating"] = df["Satisfaction"]
    out["review_date"] = pd.to_datetime(df["DateEntered"]).dt.strftime("%Y-%m-%d")
    out["side_effects"] = df["Sideffects"]
    out["dose_quantity"] = df["Quantity"]
    out["dose_strength"] = df["Strength"]
    out["dose_fdosage"] = df["Fdosage"]
    out["dose_frequency"] = df["Frequency"]

    out["commentary_norm"] = out["commentary"].apply(normalize_text)
    out["commentary_hash"] = out["commentary_norm"].apply(
        lambda s: hashlib.sha1(s.encode("utf-8")).hexdigest() if s else ""
    )

    cols = [
        "drug", "commentary", "site", "source_url", "scraped_at",
        "review_date", "duration", "condition", "age", "sex", "rating",
        "side_effects", "dose_quantity", "dose_strength",
        "dose_fdosage", "dose_frequency",
        "commentary_norm", "commentary_hash",
    ]
    out = out[cols]

    OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")
    print(f"Wrote {len(out)} rows → {OUTPUT_CSV}")
    print("\nDrug breakdown:")
    print(out["drug"].value_counts())
    print(f"\nEmpty commentary rows: {(out['commentary'] == '').sum()}")


if __name__ == "__main__":
    main()
