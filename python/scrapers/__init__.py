from .base import Review, ScrapeResult, BaseScraper
from .webmd import WebMDScraper


SCRAPER_REGISTRY = {
    "webmd.com": WebMDScraper,
    # aliases used in ledger
    "webmd": WebMDScraper,
}
