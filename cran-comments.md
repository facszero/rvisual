## Resubmission (2nd)

Changes since previous submission:

* DESCRIPTION: Added single quotes around all non-standard English words
  in Description field: 'addin', 'tidyverse', 'LLM' (previously only
  some software names had quotes).

## R CMD check results

0 errors | 0 warnings | 2 notes

### NOTE 1: unable to verify current time
Local Windows environment issue, not present on CRAN servers.

### NOTE 2: assignment to .GlobalEnv
Intentional: needed so generated R code can reference the dataset by
name in the console. Only triggered by explicit user action. Consistent
with other RStudio addins (esquisse, DataEditR).

## Test environments

* Windows 10, R 4.5.3 (local)
* win-builder R-release 4.5.3 — 2 notes
* win-builder R-devel r89685 — 2 notes
* Ubuntu latest (GitHub Actions CI)
* Windows latest (GitHub Actions CI)

## Downstream dependencies

None (new package submission).
