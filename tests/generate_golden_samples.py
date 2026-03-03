"""
Generate golden reference samples using the official lseg-data Python library.

Run this script from a project-local .venv that has lseg-data and pandas
installed, while a Refinitiv Workspace or Eikon terminal is running.

Usage:
    .venv/Scripts/python tests/generate_golden_samples.py

Output:
    tests/testthat/golden/truth_lseg_data.json

These samples serve as "ground truth" to cross-validate RefinitivR's JSON
output against the official library. They are NOT used by R code at runtime.
"""

import json
import sys
from pathlib import Path
from datetime import datetime

import pandas as pd

try:
    import lseg.data as ld
except ImportError:
    sys.exit(
        "lseg-data not found. Set up the venv first:\n"
        "  python -m venv .venv\n"
        "  .venv\\Scripts\\activate\n"
        "  pip install lseg-data pandas"
    )

# ── helpers ─────────────────────────────────────────────────────────────────

def df_to_records(df: pd.DataFrame) -> list[dict]:
    """Convert a DataFrame to JSON-serialisable list of dicts."""
    return json.loads(df.reset_index().to_json(orient="records", date_format="iso"))


def safe_call(label: str, fn, *args, **kwargs):
    """Run fn, returning records on success or an error dict on failure."""
    print(f"  [{label}] ... ", end="", flush=True)
    try:
        result = fn(*args, **kwargs)
        if isinstance(result, pd.DataFrame):
            out = df_to_records(result)
        else:
            out = result
        print(f"OK ({len(out) if isinstance(out, list) else '?'} rows)")
        return out
    except Exception as e:
        print(f"FAILED: {e}")
        return {"error": str(e)}


# ── main ────────────────────────────────────────────────────────────────────

def main():
    out_path = Path("tests/testthat/golden/truth_lseg_data.json")
    out_path.parent.mkdir(parents=True, exist_ok=True)

    print("Opening LSEG Data session ...")
    ld.open_session()
    print("Session opened.\n")

    samples: dict = {}
    meta: dict = {"generated_at": datetime.now().isoformat(),
                  "lseg_data_version": ld.__version__}

    # ── rd_GetData equivalents ──────────────────────────────────────────────

    print("=== get_data ===")

    samples["get_data_multi_ric"] = safe_call(
        "get_data_multi_ric",
        ld.get_data,
        universe=["AAPL.O", "GOOG.O", "III.L", "MMM"],
        fields=["TR.InstrumentType", "TR.ExchangeName",
                "TR.ExchangeMarketIdCode", "TR.InstrumentIsActive"],
    )

    samples["get_data_single_ric_pricing"] = safe_call(
        "get_data_single_ric_pricing",
        ld.get_data,
        universe=["AAPL.O"],
        fields=["TR.PriceClose.Currency", "CF_CURR", "CF_EXCHNG"],
    )

    samples["get_data_with_params"] = safe_call(
        "get_data_with_params",
        ld.get_data,
        universe=["AAPL.O"],
        fields=["TR.CAEffectiveDate", "TR.CAAdjustmentFactor",
                "TR.CAAdjustmentType"],
        parameters={"CAEventType": "SSP",
                     "SDate": "2020-10-27", "EDate": "2020-12-01"},
    )

    # ── rd_GetHistory (pricing) ─────────────────────────────────────────────

    print("\n=== get_history (pricing) ===")

    samples["get_history_default_fields"] = safe_call(
        "get_history_default_fields",
        ld.get_history,
        universe="AAPL.O",
        start="2020-01-02",
        end="2020-01-10",
        interval="1D",
    )

    samples["get_history_multi_ric"] = safe_call(
        "get_history_multi_ric",
        ld.get_history,
        universe=["GOOG.O", "MSFT.O"],
        start="2020-01-02",
        end="2020-01-10",
        interval="1D",
    )

    # ── rd_GetHistory (fundamental) ─────────────────────────────────────────

    print("\n=== get_data (fundamental / revenue) ===")

    samples["get_data_revenue"] = safe_call(
        "get_data_revenue",
        ld.get_data,
        universe=["GOOG.O", "MSFT.O"],
        fields=["TR.Revenue.date", "TR.Revenue", "TR.GrossProfit"],
        parameters={"SDate": 0, "EDate": -3, "FRQ": "FY",
                     "Curn": "USD"},
    )

    # ── EikonGetSymbology ───────────────────────────────────────────────────

    print("\n=== symbology ===")

    samples["symbology_ric_to_isin"] = safe_call(
        "symbology_ric_to_isin",
        ld.get_data,
        universe=["AAPL.O"],
        fields=["TR.ISINCode"],
    )

    # ── Search (RDP) ────────────────────────────────────────────────────────

    print("\n=== search ===")

    samples["search_basic"] = safe_call(
        "search_basic",
        ld.discovery.search,
        query="AAPL.O",
        top=5,
    )

    # ── Historical Pricing (interday) ───────────────────────────────────────

    print("\n=== historical pricing ===")

    samples["historical_pricing_interday"] = safe_call(
        "historical_pricing_interday",
        ld.get_history,
        universe="VOD.L",
        interval="1D",
        count=20,
    )

    samples["historical_pricing_multi_ric"] = safe_call(
        "historical_pricing_multi_ric",
        ld.get_history,
        universe=["VOD.L", "AAPL.O"],
        interval="1D",
        count=20,
    )

    # ── News ────────────────────────────────────────────────────────────────

    print("\n=== news headlines ===")

    samples["news_headlines_msft"] = safe_call(
        "news_headlines_msft",
        ld.news.get_headlines,
        query="R:MSFT.O",
        count=2,
    )

    # ── Done ────────────────────────────────────────────────────────────────

    ld.close_session()

    output = {"_meta": meta, "samples": samples}
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(output, f, default=str, indent=2)

    print(f"\nGolden samples written to {out_path}")
    n_ok = sum(1 for v in samples.values() if not isinstance(v, dict) or "error" not in v)
    n_fail = len(samples) - n_ok
    print(f"  {n_ok} succeeded, {n_fail} failed")


if __name__ == "__main__":
    main()
