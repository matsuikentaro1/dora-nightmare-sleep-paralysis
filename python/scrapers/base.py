from __future__ import annotations

from dataclasses import dataclass, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable, Iterator, Optional, Dict, Any, List, Tuple
import json
import time

import requests
from bs4 import BeautifulSoup
from tenacity import retry, stop_after_attempt, wait_exponential, retry_if_exception_type


@dataclass
class Review:
    drug: str
    commentary: str
    site: str
    source_url: str
    scraped_at: str
    duration: Optional[str] = None
    condition: Optional[str] = None
    age: Optional[str] = None
    sex: Optional[str] = None
    commentary_norm: Optional[str] = None
    commentary_hash: Optional[str] = None
    rating: Optional[str] = None
    review_date: Optional[str] = None
    side_effects: Optional[str] = None
    dose_quantity: Optional[str] = None
    dose_strength: Optional[str] = None
    dose_fdosage: Optional[str] = None
    dose_frequency: Optional[str] = None


@dataclass
class ScrapeResult:
    reviews: List[Review]
    pages_fetched: int
    errors: List[Dict[str, Any]]


class BaseScraper:
    site: str = ""

    def __init__(
        self,
        drug: str,
        start_url: str,
        paging_rule: str | None = None,
        out_dir: str | Path = "outputs",
        rate_limit_s: float = 1.5,
        max_pages: int = 200,
        session: Optional[requests.Session] = None,
    ):
        self.drug = drug
        self.start_url = start_url
        self.paging_rule = paging_rule or ""
        self.out_dir = Path(out_dir)
        self.rate_limit_s = rate_limit_s
        self.max_pages = max_pages
        self.session = session or requests.Session()
        self.html_cache_dir = self.out_dir / "html_cache" / self.site / drug
        self.html_cache_dir.mkdir(parents=True, exist_ok=True)

    def _now_iso(self) -> str:
        return datetime.now(timezone.utc).isoformat()

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=1, max=8),
        retry=retry_if_exception_type(requests.RequestException),
        reraise=True,
    )
    def fetch(self, url: str) -> Tuple[str, int]:
        resp = self.session.get(url, timeout=20, headers={"User-Agent": "Mozilla/5.0"})
        status = resp.status_code
        resp.raise_for_status()
        return resp.text, status

    def soup_from_html(self, html: str) -> BeautifulSoup:
        return BeautifulSoup(html, "lxml")

    def cache_html(self, url: str, html: str, page_idx: int):
        safe = f"page_{page_idx:04d}.html"
        (self.html_cache_dir / safe).write_text(html, encoding="utf-8")

    def iter_page_urls(self) -> Iterator[str]:
        yield self.start_url
        for page in range(2, self.max_pages + 1):
            if not self.paging_rule:
                break
            yield self.paging_rule.format(page=page)

    def parse_reviews(self, soup: BeautifulSoup, page_url: str) -> List[Review]:
        raise NotImplementedError

    def scrape(self) -> ScrapeResult:
        reviews: List[Review] = []
        errors: List[Dict[str, Any]] = []
        pages = 0
        seen_hashes = set()

        for idx, url in enumerate(self.iter_page_urls(), start=1):
            try:
                html, status = self.fetch(url)
                pages += 1
                self.cache_html(url, html, idx)
                soup = self.soup_from_html(html)
                page_reviews = self.parse_reviews(soup, url)
                if not page_reviews:
                    break

                # Check for duplicate reviews (pagination loop detection)
                new_reviews = []
                duplicate_count = 0
                for review in page_reviews:
                    if review.commentary_hash and review.commentary_hash in seen_hashes:
                        duplicate_count += 1
                    else:
                        new_reviews.append(review)
                        if review.commentary_hash:
                            seen_hashes.add(review.commentary_hash)

                # If all reviews on this page are duplicates, we've looped back
                if duplicate_count == len(page_reviews):
                    break

                reviews.extend(new_reviews)
            except Exception as e:
                errors.append(
                    {
                        "site": self.site,
                        "drug": self.drug,
                        "page_idx": idx,
                        "url": url,
                        "error": repr(e),
                        "scraped_at": self._now_iso(),
                    }
                )
                break
            time.sleep(self.rate_limit_s)
        return ScrapeResult(reviews=reviews, pages_fetched=pages, errors=errors)

    @staticmethod
    def result_to_log(result: ScrapeResult, site: str, drug: str, start_url: str) -> Dict[str, Any]:
        return {
            "site": site,
            "drug": drug,
            "start_url": start_url,
            "pages_fetched": result.pages_fetched,
            "n_reviews": len(result.reviews),
            "errors": result.errors,
            "finished_at": datetime.now(timezone.utc).isoformat(),
        }

