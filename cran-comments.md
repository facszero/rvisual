## Resubmission

This is a resubmission. Changes since first submission:

* DESCRIPTION: Added single quotes around software names (RStudio, SPSS,
  OpenAI, Anthropic, Gemini, dplyr, tidyr) as required by CRAN policy.

## R CMD check results

0 errors | 0 warnings | 2 notes

### NOTE 1: unable to verify current time

Local environment issue (Windows clock verification), not related to
package code. Does not appear on CRAN build servers.

### NOTE 2: assignment to .GlobalEnv

Found in File 'rvisual/R/mod_dataset.R':
  assign(nombre, df, envir = .GlobalEnv)

This is intentional and necessary. RVisual is an RStudio addin that loads
user datasets into a Shiny interface. Datasets must be available in
.GlobalEnv so that generated R code (e.g. `my_dataset |> filter(...)`)
can be executed directly in the R console. This only occurs on explicit
user action, never on package load. Pattern is consistent with other
RStudio addins (e.g. esquisse, DataEditR).

## Test environments

* Windows 10, R 4.5.3 (local) — 0 errors | 0 warnings | 2 notes
* win-builder R-release 4.5.3 — 2 notes
* win-builder R-devel r89685 — 2 notes
* Ubuntu latest (GitHub Actions CI)
* Windows latest (GitHub Actions CI)

## Downstream dependencies

None (new package submission).
