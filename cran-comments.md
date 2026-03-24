## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: assignment to .GlobalEnv

```
Found the following assignments to the global environment:
File 'rvisual/R/mod_dataset.R':
  assign(nombre, df, envir = .GlobalEnv)
```

This is intentional and necessary for the core functionality of the package.
RVisual is an RStudio addin that loads user datasets into an interactive 
Shiny interface. When a user loads a dataset, it must be made available in 
`.GlobalEnv` so that:

1. Generated R code (e.g. `mi_dataset |> filter(...)`) can be executed 
   directly by the user in the R console
2. The AI assistant can generate code that references the dataset by name
3. The workflow mirrors what users expect from interactive data analysis

This pattern is consistent with other RStudio addins that manage datasets
(e.g. `esquisse`, `DataEditR`). The assignment only occurs on explicit user 
action (clicking "Cargar" / "Load"), never automatically on package load.

## Test environments

* Windows 10, R 4.5.3 (local)
* Ubuntu (GitHub Actions CI)
* Windows (GitHub Actions CI)

## Downstream dependencies

None (new package submission).
