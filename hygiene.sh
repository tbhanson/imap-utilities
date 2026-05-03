#!/bin/bash
# hygiene.sh — periodic mail hygiene check (non-destructive)
#
# What it does:
#   1. Fetches new mail since last run (incremental)
#   2. Patches RFC822.SIZE for any new messages
#   3. Refreshes derived-contacts from Sent folders
#   4. Prints a status dashboard
#
# Safe to run as often as you like. Does no deletions.
#
# Usage:
#   ./hygiene.sh                  # full run
#   ./hygiene.sh --quick          # skip size fetch (much faster)
#   ./hygiene.sh --no-fetch       # skip the IMAP fetch entirely
#
# Note on OAuth2 tokens:
#   If a Gmail refresh token has expired, fetch-all.rkt will open a
#   browser for re-authorization. This breaks unattended (cron) runs.
#   Consider switching your OAuth2 app from "Testing" to "Production"
#   in Google Cloud Console to keep refresh tokens indefinitely.

set -e

# Run from the script's own directory so relative paths work.
cd "$(dirname "$0")"

# Parse flags
QUICK=0
NO_FETCH=0
for arg in "$@"; do
  case "$arg" in
    --quick) QUICK=1 ;;
    --no-fetch) NO_FETCH=1 ;;
    -h|--help)
      sed -n '2,/^set -e/p' "$0" | sed 's/^# \?//;/^set -e/d'
      exit 0
      ;;
  esac
done

step() {
  echo
  echo "=== $1 ==="
}

if [ "$NO_FETCH" -eq 0 ]; then
  step "Fetching new mail (incremental)"
  racket fetch-all.rkt --update
fi

if [ "$QUICK" -eq 0 ] && [ "$NO_FETCH" -eq 0 ]; then
  step "Patching message sizes"
  racket fetch-sizes.rkt
fi

step "Refreshing derived contacts (people you've written to)"
racket derive-contacts.rkt --write

step "Status"
racket status.rkt
