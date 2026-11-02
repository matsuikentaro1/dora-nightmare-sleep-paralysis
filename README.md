# Nightmares/abnormal dreams and sleep paralysis associated with dual orexin receptor antagonists

Analysis code for:

**Matsui K, Tsuru A, Fuwa M, et al. Nightmares/abnormal dreams and sleep paralysis associated with dual orexin receptor antagonists: a pharmacovigilance analysis of patient drug reviews.**

## Overview

This repository contains the code used in a pharmacovigilance study comparing the reporting frequency of nightmares and abnormal dreams (NAD) and sleep paralysis (SP) across three dual orexin receptor antagonists (DORAs: suvorexant, lemborexant, and daridorexant) and eight comparator hypnotics (zolpidem, eszopiclone, zaleplon, temazepam, triazolam, ramelteon, doxepin, and trazodone), using free-text patient drug reviews.

## Data availability

The review data analyzed in this study are **not included** in this repository and cannot be made publicly available for ethical reasons, because they consist of free-text accounts of individuals' health experiences.

Reviews were obtained from three patient review platforms (snapshot date: 2026-04-24):

| Platform | Language | Acquisition |
|----------|----------|-------------|
| WebMD (reviews.webmd.com) | English | Web scraping (`python/run_scrape.py`) |
| AskAPatient (askapatient.com) | English | Research dataset provided by the site operator |
| Kusuri-miru (kusuri-miru.com) | Japanese | Manual collection under permission from the site operator |

## Repository structure

```
├── python/
│   ├── config.py                        # Drug definitions, text normalization, outcome keyword vocabulary (EN + JA)
│   ├── run_scrape.py                    # Scraping entry point (reads url_ledger.csv)
│   ├── url_ledger.csv                   # WebMD review pages for the 11 study drugs
│   ├── scrapers/
│   │   ├── __init__.py                  # Scraper registry
│   │   ├── base.py                      # Base scraper class (normalization, SHA-1 hashing)
│   │   └── webmd.py                     # webmd.com scraper
│   ├── convert_askapatient_official.py  # AskAPatient dataset -> common schema
│   ├── parse_kusurimil.py               # Manually collected kusuri-miru.com text -> common schema
│   ├── apply_keyword_flags.py           # Stage 1 keyword screening (language detected automatically)
│   ├── find_sp_variants.py              # Misspelling variants by Levenshtein distance
│   └── requirements.txt
└── R/
    ├── .here                            # Marks R/ as the project root for the here package
    └── scripts/
        ├── 00_run_all.R                 # Runs 01-05 in sequence
        ├── 01_load_data.R               # Data loading, drug-class assignment, withdrawal exclusion
        ├── 02_table1_descriptive.R      # Table 1
        ├── 03_main_prr_forest.R         # PRR with bootstrap CI, forest plots, class comparisons (Table 2)
        ├── 04_sensitivity.R             # Sensitivity analysis restricted to English-language platforms
        └── 05_dora_4group_rating.R      # User ratings among DORA reviews by outcome status
```

## Outcome definitions

- **NAD (nightmares and abnormal dreams)**: screened with English and Japanese keyword dictionaries (`python/config.py`) following Natter et al. (2021), then manually adjudicated for drug relatedness.
- **SP (sleep paralysis)**: screened with a vocabulary based on the three-factor model of Cheyne et al. (1999) (Intruder, Incubus, and Unusual Bodily Experiences) plus direct and immobility expressions, then manually adjudicated with reference to the ICSD-3-TR criteria for recurrent isolated sleep paralysis.
- **Withdrawal-related events**: NAD or SP attributable to withdrawal from a benzodiazepine or Z-drug; excluded from the primary analysis.

All screen-positive reviews were reviewed by two raters, and disagreements were resolved by consensus.

## Reproducing the statistical analysis

### Input data

The R scripts expect `R/data/merged_reviews.csv` with one row per review and at least these columns:

| Column | Description |
|--------|-------------|
| `drug` | Generic drug name (one of the 11 study drugs) |
| `site` | `webmd.com`, `askapatient`, or `kusurimil` |
| `rating` | Overall satisfaction rating (webmd.com: mean of three 1-5 subscales; askapatient.com: integer 1-5; kusuri-miru.com: 0.5-5 in 0.5-point steps) |
| `excluded` | 1 if excluded (condition other than insomnia/sleep disorder, no commentary text, or duplicate) |
| `drug_related_nightmare_final` | 1 if a drug-related NAD event, after manual adjudication |
| `drug_related_sp_final` | 1 if a drug-related SP event, after manual adjudication |
| `withdrawal_related_final` | 1 if the NAD/SP event was judged withdrawal-related |

### Running

Requires R 4.3+ with `tidyverse`, `boot`, `dunn.test`, `openxlsx`, and `here`.

```r
# With R/ as the working directory:
source("scripts/00_run_all.R")   # runs 01-05; tables and figures are written to output/
```

`03_main_prr_forest.R` uses 100,000 bootstrap replications (seed = 42) and takes several minutes.

### Data collection (Python)

Requires Python 3.10+ (`pip install -r python/requirements.txt`). Run the scripts from the `python/` directory.

## References

- Natter J, Yokoyama T, Michel B. Relative frequency of drug-induced sleep disorders for 32 antidepressants in a large set of Internet user reviews. *Sleep*. 2021;44(12):zsab174.
- Cheyne JA, Rueffer SD, Newby-Clark IR. Hypnagogic and hypnopompic hallucinations during sleep paralysis: neurological and cultural construction of the night-mare. *Conscious Cogn*. 1999;8(3):319-337.

## Contact

Kentaro Matsui (matsui.kentaro@ncnp.go.jp)
Department of Clinical Laboratory, National Center Hospital, National Center of Neurology and Psychiatry, Tokyo, Japan
