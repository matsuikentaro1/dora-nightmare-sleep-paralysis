from __future__ import annotations

from typing import List, Any, Dict, Optional
from bs4 import BeautifulSoup
import json
import re
import hashlib

from .base import BaseScraper, Review
from config import normalize_text


class WebMDScraper(BaseScraper):
    site = "webmd.com"

    def _extract_jsonld_reviews(self, soup: BeautifulSoup) -> List[Dict[str, Any]]:
        scripts = soup.find_all("script", attrs={"type": "application/ld+json"})
        out: List[Dict[str, Any]] = []
        for sc in scripts:
            raw = sc.string or sc.get_text()
            if not raw:
                continue
            try:
                data = json.loads(raw)
            except Exception:
                continue

            def walk(node: Any):
                if isinstance(node, dict):
                    if node.get("@type") == "Review" and "reviewBody" in node:
                        out.append(node)
                    for v in node.values():
                        walk(v)
                elif isinstance(node, list):
                    for v in node:
                        walk(v)

            walk(data)
        return out

    def _extract_html_review_metadata(self, soup: BeautifulSoup) -> List[Dict[str, Optional[str]]]:
        """Extract age, sex, duration, condition, rating from HTML review holders."""
        metadata: List[Dict[str, Optional[str]]] = []
        review_items = soup.select(".review-details-holder")

        for item in review_items:
            age: Optional[str] = None
            sex: Optional[str] = None
            duration: Optional[str] = None
            condition: Optional[str] = None
            rating: Optional[str] = None

            # Extract overall rating
            rating_el = item.select_one(".overall-rating strong")
            if rating_el:
                rating_text = rating_el.get_text(strip=True)
                match = re.search(r"[\d.]+", rating_text)
                if match:
                    rating = match.group()

            # Extract condition
            condition_el = item.select_one(".condition")
            if condition_el:
                cond_text = condition_el.get_text(strip=True)
                # Remove "Condition:" prefix
                condition = re.sub(r"^Condition:\s*", "", cond_text, flags=re.IGNORECASE)

            # Extract age, sex, duration from details
            details_el = item.select_one(".details")
            if details_el:
                text = details_el.get_text(" | ", strip=True)
                parts = [p.strip() for p in text.split("|")]
                for p in parts:
                    p_clean = p.strip()
                    # Age pattern: 19-24, 65-74, 75+, etc.
                    if re.match(r"^\d+-\d+$|^\d+\+$", p_clean):
                        age = p_clean
                    # Sex pattern
                    elif p_clean.lower() in ["male", "female"]:
                        sex = p_clean.capitalize()
                    # Duration pattern
                    elif "medication for" in p_clean.lower() or "on medication" in p_clean.lower():
                        duration = p_clean

            metadata.append({
                "age": age,
                "sex": sex,
                "duration": duration,
                "condition": condition,
                "rating": rating,
            })

        return metadata

    def parse_reviews(self, soup: BeautifulSoup, page_url: str) -> List[Review]:
        # WebMD pages embed reviews in JSON-LD (reviewBody)
        jsonld_reviews = self._extract_jsonld_reviews(soup)
        # Extract metadata from HTML (age, sex, etc.)
        html_metadata = self._extract_html_review_metadata(soup)

        reviews: List[Review] = []
        for i, r in enumerate(jsonld_reviews):
            text = str(r.get("reviewBody", "")).strip()
            if not text:
                continue
            norm = normalize_text(text)
            h = hashlib.sha1(norm.encode("utf-8")).hexdigest()

            # Get metadata from HTML by index (same order)
            meta = html_metadata[i] if i < len(html_metadata) else {}

            # datePublished is ISO date (e.g. "2024-01-15"); keep the YYYY-MM-DD part
            raw_date = r.get("datePublished")
            review_date = str(raw_date)[:10] if raw_date else None

            reviews.append(
                Review(
                    drug=self.drug,
                    commentary=text,
                    site=self.site,
                    source_url=page_url,
                    scraped_at=self._now_iso(),
                    duration=meta.get("duration"),
                    condition=meta.get("condition"),
                    age=meta.get("age"),
                    sex=meta.get("sex"),
                    commentary_norm=norm,
                    commentary_hash=h,
                    rating=meta.get("rating"),
                    review_date=review_date,
                )
            )

        # Fallback: some layouts may render visible blocks
        if not reviews:
            items = soup.select(".review-text") or soup.select(".review-item p")
            for it in items:
                text = it.get_text(" ", strip=True)
                if not text:
                    continue
                norm = normalize_text(text)
                h = hashlib.sha1(norm.encode("utf-8")).hexdigest()
                reviews.append(
                    Review(
                        drug=self.drug,
                        commentary=text,
                        site=self.site,
                        source_url=page_url,
                        scraped_at=self._now_iso(),
                        commentary_norm=norm,
                        commentary_hash=h,
                    )
                )
        return reviews
