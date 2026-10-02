# Stream sample archive: handoff

State as of 2026-10-02. Picks up when Amey (HBEF site manager) sends back
resolutions for the bottle checklist, or new archive files.

## Where things stand

- **Amey's W6 "duplicates" are resolved.** Her 25 W6 pairs (251 barcodes in
  all, across W1–W9 and RG11/22/23, in the 2022 portal data) were one bottle
  joined to a primary and a lab `Dup` chemistry analysis. Not archive errors.
  - Portal: `archive_merger.R` keeps one row per analysis, with a `duplicate`
    column (blank / Dup / Dup2). Live on the server.
  - EDI: `edi_upload_prep.R` now puts a bottle's barcode on all of its
    analyses (primary + Dup). Takes effect at the next EDI upload. No schema
    change: `duplicate` was already in the EDI file. Barcodes are no longer
    unique per row; consider a sentence in the EDI metadata for `barcode`.
- **Amey's new W6 file is live** (`w6 sample archive collection through
  2026.xlsx`: all 2745 previous W6 bottles, unchanged except 109 bins, plus
  460 new through 2026-05-24).
- **131 bottle records need someone to read the physical label.** Nothing has
  been changed in the archive or chemistry for these; we can't tell from data
  which side is wrong. Sent (or about to be sent) to Amey:
  - Report: https://claude.ai/code/artifact/d950cfca-fe23-4069-b4ec-c991a6c3d592
  - `output/bottle_checklist.csv` (sorted by bin; blank `label_barcode`,
    `label_date`, `label_time`, `notes` columns for her to fill)
  - `output/lab_duplicate_bottles.csv`
  - `output/` is gitignored; regenerate with `Rscript archive_discrepancy_audit.R`.
- 50 of the 131 come from Amey's W6 file; 81 come from our 2021 copy of the
  other watersheds (`HB physical archives stream samples.csv`), which Amey
  may since have fixed in her own files. No EDI version (2019–2025) shows any
  of the 131 resolved, so the earlier investigation didn't cover them.

### Checklist groups

| Group | n | Meaning |
| --- | --- | --- |
| A. barcode on two bottles | 4 | 42666 (W5 1995-08-14 and 1995-10-16), 46221 (W5 1987-09-27 and W9 1997-11-24). Sequence suggests 42656 and 42221, both unused. Both collisions are published in EDI. |
| B. date differs from chemistry | 46 | No chemistry on the bottle date; chemistry at the same site and identical time 1–7 days away. Several whole sampling days (1991-09-04, 2010-02-08, 1995-09-12, 2012-01-09, 2015-11-02). |
| C. time differs from chemistry | 30 | Includes all-watershed mismatches on 2015-01-12 and 2015-03-02; 51227/51228 have no time. |
| D. more bottles than chemistry | 17 | Incl. W6 1999-11-27 (2 bottles × primary/Dup), 1993-01-25, 1988-01-03 (11448 may be the 1988-01-10 sample). |
| E. chemistry has no time | 11 | Bottle time could fill the chemistry record. 2400 on 2007-10-11 (W1 36079, W6 28183). |
| F. no chemistry record | 21 | Nothing within 7 days at the same time. |
| G. note changed | 2 | "cloudy debris" moved from 10577 to 10576. |

### Open questions for Amey (in the report)

1. 109 W6 bins (2013-12-02 to 2017-05-30) are one lower than before. Looks
   like 250 ml bins repacked at 27 bottles instead of 24. Re-boxed, or old
   file wrong? Portal uses her new bins (Mike hasn't answered the doc comment
   asking whether to wait).
2. W101: 47 bottles 2013-06 to 2017-05, but W101 chemistry ends 2013-05-13.
3. W101 bin 1.05613 (34027–34044): probably 1.0613.
4. What 2400 means.
5. Weigh date/time for her 460 new bottles (her file has no such columns).

Not yet asked, found later: bin 2.0562 holds 18 W5 + 24 W7 bottles (42,
more than any bin); W7 44767 is in bin "647". Also: what is a bin,
physically? (Inferred: a container; median 18 × 500 ml or 24 × 250 ml.)

### Decisions made

- Archive dates/times/barcodes are only changed on evidence from the label or
  field sheets, never by inference from chemistry or neighboring watersheds.
- Corrections should be made in Amey's source files where possible, not as
  overrides in code. Exceptions so far are bottle-type normalizations in
  `archive_merger.R`: "nalgene 251" → Nalgene250 (56153), "Nalgene###NM" →
  narrow### (NM = narrow mouth).
- 24:00 times are kept as recorded until Amey answers.
- Precip archive bottles have not been audited.

## When Amey's resolutions arrive

1. **New/updated archive xlsx files** (one watershed each, same layout as the
   W6 file; must contain every bottle for that watershed):
   - Put them unmodified in `shiny/restricted_QAQC/data/archive_data/stream_updates/`,
     locally and on the server (gitignored; copy by hand, e.g. scp). Files
     apply in alphabetical order; a later file for the same watershed wins, so
     remove or rename superseded ones.
   - The merger stops if a file omits a previously archived barcode, has
     repeated barcodes, or has an unparseable time/date. Times may be HHMM
     integers, Excel clock times, or "09:15"; -9999 and n/a mean missing.
2. **A filled-in checklist**: for each row, decide which side the label
   supports.
   - Label agrees with chemistry → archive needs fixing: ask Amey to fix her
     file (preferred) or patch it in the source CSV/xlsx.
   - Label agrees with the archive → chemistry is wrong: fix the `current` /
     `historical` table in the hbef database on the server, by refNo/uniqueID.
     This changes published data, so note it for the next EDI version.
   - Same-day groups (B) may resolve together from one label or field sheet.
3. **Re-audit.** `archive_discrepancy_audit.R` currently reads only the W6
   xlsx by name (`data/w6 sample archive collection through 2026.xlsx`) and
   the 2021 CSV for everything else. To audit new files, first generalize it
   to read all of `stream_updates/` with `read_stream_archive_xlsx()` from
   `archive_merger.R`. It compares against
   `edi_upload/HubbardBrook_weekly_stream_chemistry.csv`, so results reflect
   the last EDI build.
4. **Update the portal** on the server (`mike@165.22.183.247`):
   `git pull` in `/home/mike/shiny`, then
   `Rscript /home/mike/shiny/scheduled_scripts/archive_merger.R`, then
   `sudo systemctl restart shiny-server`. No commit needed afterward: the
   data goes to gitignored `HTML/archive_explore/archive_data.js`.
5. **Next EDI upload**: follow `edi_upload/edi_upload_preR_steps.txt`. The
   merger now writes `archive_samples.csv` to `/home/mike/misc/edi_prep_files/`
   on the server, so `get *` brings the current one down. The local
   `edi_upload/archive_samples.csv` is the old 2022 file (with chemistry
   columns); it gets replaced by that download. Expected effect, tested
   2026-10-02: stream rows with barcodes 15,715 → 16,559; precip 4,915 → 5,011;
   no existing barcode changes. Barcodes after 2017-05 exist only for
   watersheds whose newer archive files Amey has sent (W6 so far).

## File map

| Path | What |
| --- | --- |
| `hbef_misc/archive_inventory/archive_discrepancy_audit.R` | Audit: archive vs EDI chemistry → `output/*.csv` |
| `hbef_misc/archive_inventory/run_merger_local.R` | Runs the merger locally against the local DB via the mysql CLI (no RMariaDB needed) |
| `hbef_misc/archive_inventory/data/` | Amey's files as received (xlsx gitignored) |
| `hbef_misc/archive_inventory/archive_and_field_duplicates_exploration.R` | 2022 investigation (duplicates only) |
| `hbef_misc/edi_upload/edi_upload_prep.R` | Builds EDI files; `get_unambiguous_barcodes()` = barcode rule |
| `shiny/scheduled_scripts/archive_merger.R` | Archive + DB chemistry → portal data + `archive_samples.csv` |
| `shiny/scheduled_scripts/README.txt` | Server run instructions |
| `shiny/HTML/archive_explore/archive_explore.html` | Portal template; loads `archive_data.js` |
| `shiny/restricted_QAQC/data/archive_data/` | Archive sources (gitignored; exist locally and on server, copied 2026-10-02) |

Local hbef MariaDB runs through 2025-02-24; the server's is current. The
merger's `old_date` column (pre-2021 date corrections) is dropped from the
portal; the audit uses it.
