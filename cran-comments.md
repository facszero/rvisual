## R CMD check results

0 errors | 0 warnings | 2 notes

### NOTE 1: unable to verify current time

This is a local environment issue (Windows clock verification) and is not
related to the package code. It does not appear on CRAN's build servers.

### NOTE 2: assignment to .GlobalEnv

```
Found the following assignments to the global environment:
File 'rvisual/R/mod_dataset.R':
  assign(nombre, df, envir = .GlobalEnv)
```

This is intentional and necessary for the core functionality of the package.
RVisual is an RStudio addin that loads user datasets into an interactive
Shiny interface. When a user loads a dataset, it must be made available in
`.GlobalEnv` so that:

1. Generated R code (e.g. `my_dataset |> filter(...)`) can be executed
   directly by the user in the R console
2. The AI assistant generates code that references the dataset by name
3. The workflow mirrors what users expect from interactive data analysis

This pattern is consistent with other RStudio addins that manage datasets
(e.g. `esquisse`, `DataEditR`). The assignment only occurs on explicit user
action (clicking the Load button), never automatically on package load.

## Test environments

* Windows 10, R 4.5.3 (local) — 0 errors | 0 warnings | 2 notes
* Ubuntu latest, R release (GitHub Actions CI)
* Windows latest, R release (GitHub Actions CI)

## Downstream dependencies

None (new package submission).
