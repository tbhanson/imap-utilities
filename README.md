# imap-utilities

Tools for exploring, analyzing, and cleaning up IMAP email accounts using Racket.

## Goals

- Connect to IMAP servers (password and OAuth2 authentication)
- Fetch and locally cache mail header digests for offline analysis
- Report statistics: message counts by sender, by year/month, etc.
- Identify important mail from friends and family vs. bulk/automated senders
- Eventually: rule-based identification of mail to keep or purge

## Project structure

```
src/                    Source modules
  imap-email-account-credentials.rkt   Account credential struct & I/O
  oauth2-details.rkt                   OAuth2 client credential struct & I/O
  gmail-oauth2.rkt                     Gmail OAuth2 auth flow, token storage, refresh
  connect-to-imap-account.rkt          SSL IMAP connection (password auth)
  main-mail-header-parts.rkt           Header field extraction
  parse-mail-dates.rkt                 Date parsing for varied header formats
  mail-digest.rkt                      Per-message digest (query interface)
  mailbox-digest.rkt                   Folder-level digest: fetch, save, load, analyze
  digest-report.rkt                    Statistics report from a mailbox digest
  known-contacts.rkt                   Category-aware contact management
  parse-date-time-string-statistics.rkt
  parse-from-address-statistics.rkt
  parse-to-address-statistics.rkt

test/                   Offline unit tests
test-data/              Anonymized test fixtures
all-tests.rkt           Test runner (offline tests only)

fetch.rkt               Fetch headers from one account and save locally
fetch-all.rkt           Fetch headers from all accounts in one run
report.rkt              Load a saved digest and print statistics
report-all.rkt          Combined report across all saved digests
suggest-contacts.rkt    Suggest known contacts from sent-mail analysis
find-unread.rkt         Find unread messages from known contacts
list-folders.rkt        List IMAP folders (with counts, gaps, fetch scripts)
inspect-digest.rkt      Sanity-check field population in saved digests
credentials-example.txt Example credentials file format
```

## Setup

### Prerequisites

```
raco pkg install gregor
```

### Credentials

Credentials are stored outside the codebase in `~/.imap_secrets/`:

```
~/.imap_secrets/
  credentials              # IMAP account list (Racket #prefab format)
  known-contacts           # Categorized contact list (one email per line)
  digests/                 # Locally cached mail header digests
  .oauth2_google           # Google OAuth2 client credentials
  .oauth2_tokens_<addr>    # OAuth2 tokens per account (auto-managed)
```

The credentials file contains a list of `imap-email-account-credentials` structs:

```racket
(
  ;; Gmail accounts (OAuth2) — password field is unused, xoauth2? is #t
  #s(imap-email-account-credentials "my-gmail" "imap.gmail.com" "me@gmail.com" "" #f #t)

  ;; Password-authenticated accounts — xoauth2? is #f
  #s(imap-email-account-credentials "my-other" "imap.example.com" "me@example.com" "password" #f #f)
)
```

The six fields are: account-name, hostname, email-address, password, try-tls?, xoauth2?

**Note:** If your password contains a backslash, double it (`\\`). Double-quotes
need escaping as `\"`.

Set permissions:
```bash
chmod 700 ~/.imap_secrets
chmod 600 ~/.imap_secrets/credentials
```

### Gmail OAuth2 setup

To access Gmail accounts, you need a Google Cloud project with OAuth2 credentials:

1. Go to [Google Cloud Console](https://console.cloud.google.com/) → APIs & Services → Credentials
2. Configure the OAuth consent screen (External, add your Gmail addresses as test users)
3. Create credentials → OAuth client ID → Desktop app
4. Save the client ID and secret:

```bash
cat > ~/.imap_secrets/.oauth2_google <<'EOF'
#s(oauth2-details "YOUR_CLIENT_ID" "YOUR_CLIENT_SECRET" "http://localhost:8080")
EOF
chmod 600 ~/.imap_secrets/.oauth2_google
```

One set of credentials works for all your Gmail accounts. The first time you
fetch from a Gmail account, your browser will open for authorization. After
that, saved tokens are used automatically.

**Tip:** Your Google Cloud project starts in "testing" mode where refresh
tokens expire after 7 days. Once everything works, publish the app in the
Cloud Console (OAuth consent screen → Publish App) to get permanent tokens.

### Known contacts

Create `~/.imap_secrets/known-contacts` with email addresses of people whose
mail you want to keep. Lines starting with `#` serve as category headers —
all addresses below a header belong to that category:

```
# family
mom@example.com
dad@example.com

# friends
alice@example.org
bob@example.com

# written to 10 or more times
carol@work.com
```

Matching is case-insensitive. Categories are used by `find-unread.rkt` to
filter results (e.g. `--category family`). See `suggest-contacts.rkt` below
for a way to bootstrap this file from your sent mail.

### Run tests

```bash
racket all-tests.rkt
```

Tests are fully offline and use anonymized test data.

## Usage

### List IMAP folders

```bash
racket list-folders.rkt "my-account"             # list folder names
racket list-folders.rkt "my-account" --counts     # include message counts (slower)
racket list-folders.rkt "my-account" --gaps       # non-empty folders with no local digest
racket list-folders.rkt "my-account" --fetch-gaps # generate fetch commands for gaps
racket list-folders.rkt                           # all accounts
racket list-folders.rkt --counts                  # all accounts with counts
racket list-folders.rkt --gaps                    # find undigested folders across all accounts
racket list-folders.rkt --fetch-gaps > fetch-gaps.sh  # generate a script to close all gaps
```

The `--fetch-gaps` option generates a bash script with one `racket fetch.rkt`
command per unfetched folder. Review and edit the script (removing folders like
All Mail, Spam, Trash, Drafts), then run it with `bash fetch-gaps.sh`.

Useful for finding the correct folder names, which vary by provider and
locale — e.g. Gmail in German uses `[Gmail]/Gesendet` instead of
`[Gmail]/Sent Mail`.

### Fetch headers from an account

```bash
racket fetch.rkt "my-account"                              # fetch all of INBOX
racket fetch.rkt "my-account" "INBOX" 500                  # fetch first 500
racket fetch.rkt "my-account" "[Gmail]/Sent Mail"          # different folder
racket fetch.rkt "my-account" "INBOX" --update             # incremental: only new messages
```

With `--update`, the tool finds the most recent saved digest for that
account+folder, fetches only newer messages, merges them, and saves a
combined digest. If no previous digest exists, it falls back to a full fetch.

Headers are fetched in batches of 200 with progress reporting.

### Fetch all accounts at once

```bash
racket fetch-all.rkt                    # full fetch of INBOX for all accounts
racket fetch-all.rkt --update           # incremental fetch of INBOX for all accounts
racket fetch-all.rkt "Sent" --update    # specify a different folder
racket fetch-all.rkt --all-digested     # incremental update of every account+folder
                                        #   that already has a saved digest
```

The `--all-digested` flag is the most convenient for routine use: it scans
your saved digests, finds every (account, folder) pair you've previously
fetched, and does an incremental update for each one.

Accounts that fail (e.g. expired tokens, wrong password) are skipped with
a warning — one bad account doesn't stop the rest.

### Generate a statistics report

```bash
racket report.rkt latest            # report on most recently saved digest
racket report.rkt list              # list all saved digests
racket report.rkt <path-to-.ser>    # report on a specific digest file
```

The report shows total messages, message counts by year, top senders
(with `*` markers for known contacts), and a known vs. unknown breakdown.

### Combined report across all accounts

```bash
racket report-all.rkt
```

Loads all saved digests (keeping only the latest per account+folder) and
produces a combined view: per-account summary, combined year counts, top
senders across all accounts (noting which ones appear in multiple accounts),
and known/unknown breakdown.

### Suggest known contacts from sent mail

First, fetch your sent-mail folders:

```bash
racket fetch.rkt "my-gmail" "[Gmail]/Sent Mail"
racket fetch.rkt "my-other" "Sent"
```

(Use `list-folders.rkt` to find the correct sent-mail folder name for each
account.)

Then analyze who you've been writing to:

```bash
racket suggest-contacts.rkt                 # people you've written to 2+ times
racket suggest-contacts.rkt --min 10        # only those you've written to 10+ times
racket suggest-contacts.rkt --save          # append new addresses to known-contacts file
racket suggest-contacts.rkt --min 5 --bare >> ~/.imap_secrets/known-contacts
```

Start with a high `--min` value to get your closest contacts first, review
the list, then lower it gradually. The `--save` flag appends to your
known-contacts file; `--bare` outputs just email addresses (one per line,
with a `#` category comment) suitable for piping directly into the file.
Each run only outputs addresses not already in your known-contacts, so you
can progressively lower the threshold without creating duplicates.

### Find unread messages from known contacts

```bash
racket find-unread.rkt                              # unread from known contacts
racket find-unread.rkt --all                        # unread from anyone
racket find-unread.rkt --from someone@example.com   # unread from one sender
racket find-unread.rkt --category family            # unread from a contact category
racket find-unread.rkt --account "my-gmail"         # only search one account
racket find-unread.rkt --category friends --account "my-gmail"  # combine filters
racket find-unread.rkt --categories                 # list available categories
```

Shows date, sender, category, and subject for each unread message.

**Note:** Unread status reflects the state at fetch time. For current status,
do a fresh full fetch first (not `--update`, which only grabs new messages
without re-checking flags on existing ones).

### Inspect digest quality

```bash
racket inspect-digest.rkt latest    # inspect most recent digest
racket inspect-digest.rkt each      # detailed report for every digest
racket inspect-digest.rkt all       # combined summary across all digests
```

Shows field population percentages (date, from, to, cc, bcc, subject, flags)
and flag distribution. Useful for verifying data quality after fetching.

## Status

- **Working:** Password and OAuth2 (Gmail) IMAP connections, batched header
  fetching with progress, incremental fetch, local digest save/load,
  single-account and cross-account statistics reports, category-aware
  known-contacts, sent-mail analysis for contact suggestion, unread message
  search with category and account filtering, IMAP folder listing with
  message counts, gap detection, and fetch script generation, digest
  quality inspection
- **Planned:** Purge candidate reports, batch deletion, more providers
  for OAuth2

## License

Dual-licensed under the Apache License 2.0 and the MIT License, at your option.
See [LICENSE](LICENSE) for details.
