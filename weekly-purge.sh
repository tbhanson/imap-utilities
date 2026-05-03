#!/bin/bash
# weekly-purge.sh — guided purge of older bulk mail
#
# Run this when you have time to supervise (~10-20 min).
# Walks year-by-year identifying high-volume unknown senders and
# prompting before deleting.
#
# Strategy:
#   - Older years: keep 1 sample per sender per year
#   - Recent years (within --recent-years): keep 1 sample per sender per month
#   - This year and previous year are completely untouched
#
# Safety:
#   - Only operates on senders NOT in known-contacts or derived-contacts
#     (people you've ever written to are protected automatically)
#   - You can abort at any time with Ctrl-C
#
# Usage:
#   ./weekly-purge.sh                       # interactive prompts
#   ./weekly-purge.sh --yes                 # auto-confirm (use after trust built)
#   ./weekly-purge.sh --min 200             # higher threshold (more conservative)
#   ./weekly-purge.sh --account gmx         # just one account
#   ./weekly-purge.sh --recent-years 5      # treat last 5 years as "recent" (monthly)
#   ./weekly-purge.sh --recent-years 0      # everything yearly (older behavior)
#   ./weekly-purge.sh --keep 2              # keep 2 samples per period

set -e

cd "$(dirname "$0")"

MIN=100
ACCOUNT=""
AUTO_YES=""
DRY_RUN=""
KEEP=1
RECENT_YEARS=3   # last N pre-cutoff years use --keep-per month

while [ $# -gt 0 ]; do
  case "$1" in
    --yes|-y) AUTO_YES="-y" ;;
    --dry-run|-n) DRY_RUN="1" ;;
    --min) MIN="$2"; shift ;;
    --account) ACCOUNT="$2"; shift ;;
    --keep) KEEP="$2"; shift ;;
    --recent-years) RECENT_YEARS="$2"; shift ;;
    -h|--help)
      sed -n '2,/^set -e/p' "$0" | sed 's/^# \?//;/^set -e/d'
      exit 0
      ;;
    *) echo "Unknown arg: $1"; exit 1 ;;
  esac
  shift
done

CURRENT_YEAR=$(date +%Y)
# Don't touch this year or the past 2 full years (most recent is fluid).
# So if it's currently 2026, we purge through 2023.
CUTOFF_YEAR=$((CURRENT_YEAR - 3))
RECENT_BOUNDARY=$((CUTOFF_YEAR - RECENT_YEARS + 1))
START_YEAR=2008

ACCOUNT_FLAG=""
if [ -n "$ACCOUNT" ]; then
  ACCOUNT_FLAG="--account $ACCOUNT"
fi

echo "=== Weekly purge ==="
echo "  Years $((CUTOFF_YEAR + 1)) through $CURRENT_YEAR untouched (recent — protected)"
echo "  Years $START_YEAR through $((RECENT_BOUNDARY - 1)): keep $KEEP per sender per year"
echo "  Years $RECENT_BOUNDARY through $CUTOFF_YEAR: keep $KEEP per sender per month"
echo "Min messages per sender to consider: $MIN"
[ -n "$ACCOUNT" ] && echo "Account filter: $ACCOUNT"
[ -n "$AUTO_YES" ] && echo "Auto-confirm: ON"
[ -n "$DRY_RUN" ] && echo "DRY RUN: showing plans only, no deletions"
echo

if [ -z "$AUTO_YES" ] && [ -z "$DRY_RUN" ]; then
  read -p "Proceed? [y/N] " ans
  case "$ans" in
    y|Y|yes|YES) ;;
    *) echo "Aborted."; exit 0 ;;
  esac
fi

for year in $(seq $START_YEAR $CUTOFF_YEAR); do
  next_year=$((year + 1))

  if [ "$year" -ge "$RECENT_BOUNDARY" ]; then
    KEEP_PER="month"
  else
    KEEP_PER="year"
  fi

  echo
  echo "==========================================================="
  if [ -n "$ACCOUNT" ]; then
    echo "  Year $year — account: $ACCOUNT — keep $KEEP per $KEEP_PER"
  else
    echo "  Year $year — all accounts — keep $KEEP per $KEEP_PER"
  fi
  echo "==========================================================="

  racket purge-candidates.rkt $ACCOUNT_FLAG --min $MIN \
    --after $year-01-01 --before $next_year-01-01 \
    --delete-all --keep $KEEP --keep-per $KEEP_PER \
    $AUTO_YES ${DRY_RUN:+--dry-run}
done

echo
if [ -n "$DRY_RUN" ]; then
  echo "=== Dry run complete (no deletions performed) ==="
else
  echo "=== Done. Running status... ==="
  racket status.rkt
fi
