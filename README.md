# imap-with-racket

Tools for exploring, analyzing, and cleaning up IMAP email accounts using Racket.

## Goals

- Connect to IMAP servers (password and OAuth2 authentication)
- Fetch and locally cache mail header digests for offline analysis
- Report statistics: message counts by sender, by year/month, etc.
- Eventually: rule-based identification of mail to keep or purge

## Project structure

```
src/                    Source modules
  imap-email-account-credentials.rkt   Account credential struct & I/O
  oauth2-details.rkt                   OAuth2 client credential struct & I/O
  connect-to-imap-account.rkt          SSL IMAP connection (password auth)
  main-mail-header-parts.rkt           Header field extraction
  parse-mail-dates.rkt                 Date parsing for varied header formats
  mail-digest.rkt                      Per-message digest (query interface)
  mailbox-digest.rkt                   Folder-level digest: fetch, save, load, analyze
  digest-report.rkt                    Statistics report from a mailbox digest
  known-contacts.rkt                   Whitelist of contacts to keep
  parse-date-time-string-statistics.rkt
  parse-from-address-statistics.rkt
  parse-to-address-statistics.rkt

test/                   Offline unit tests
test-data/              Anonymized test fixtures
all-tests.rkt           Test runner (offline tests only)
fetch.rkt               CLI: fetch headers from an account and save locally
report.rkt              CLI: load a saved digest and print statistics
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
  credentials           # IMAP account list (Racket #prefab format)
  digests/              # Locally cached mail header digests
  .oauth2_<address>     # OAuth2 tokens per account (owner-readable only)
```

The credentials file contains a list of `imap-email-account-credentials` structs:

```racket
(
  #s(imap-email-account-credentials "my-gmail" "imap.gmail.com" "me@gmail.com" "" #f #t)
  #s(imap-email-account-credentials "my-other" "imap.example.com" "me@example.com" "password" #f #f)
)
```

Set permissions:
```
chmod 700 ~/.imap_secrets
chmod 600 ~/.imap_secrets/credentials
```

### Known contacts (optional)

Create `~/.imap_secrets/known-contacts` with email addresses of people whose
mail you want to keep — one address per line, `#` for comments:

```
# Family
mom@example.com
dad@example.com
# Friends
alice@example.org
```

### Run tests

```
racket all-tests.rkt
```

These tests are fully offline and use anonymized test data.

## Usage

### Fetch headers from an account

```bash
racket fetch.rkt "my-account"                     # fetch all of INBOX
racket fetch.rkt "my-account" "INBOX" 500          # fetch first 500
racket fetch.rkt "my-account" "[Gmail]/All Mail"   # different folder
```

This saves a digest file to `~/.imap_secrets/digests/`.

### Generate a statistics report

```bash
racket report.rkt latest          # report on most recently saved digest
racket report.rkt list            # list all saved digests
racket report.rkt <path-to-.ser>  # report on a specific digest file
```

The report shows message counts by year, top senders, and (if you have a
known-contacts file) a breakdown of mail from known vs unknown senders.

## Status

- **Working:** Password-authenticated IMAP connections, header fetching,
  local digest save/load, date parsing, sender/recipient statistics
- **In progress:** OAuth2 (Gmail) authentication, statistics reporting CLI
- **Planned:** Rule-based mail classification, batch deletion, GUI
