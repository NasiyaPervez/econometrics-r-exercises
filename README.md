# Econometrics in R — applied exercises

**Short description:** Five hands-on applied-econometrics exercises in R —
multiple regression, panel fixed effects, difference-in-differences, IV/2SLS,
and regression discontinuity — plus an R/RStudio survival primer.

## What this is

Coursework econometrics lab in R (OVGU Magdeburg). Two scripts:

- **`Intro to R.R`** — a self-contained R primer: RStudio workflow, objects and
  data frames, `dplyr` wrangling, `ggplot2`, OLS with robust standard errors,
  and exporting tables/plots in publication format.
- **`Exercises.R`** — five worked exercises (with math setup + written answers
  as string blocks):

| Exercise | Method | Data |
|---|---|---|
| 1. Regression | Multiple regression (`dir ~ lvr + mcs`) | `Ecdat::Hmda` (mortgage denials) |
| 2. Fixed effects | Panel FE: police per capita → crime rate | `cornwell.csv` |
| 3. DID | DiD on `wooldridge` injury data | `wooldridge` |
| 4. IV / 2SLS | Quarter-of-birth instrument for education | `angristandkrueger1991.csv` |
| 5. RDD | Sharp/fuzzy regression discontinuity on tutoring | `tutoring_program.csv` |

## Repository layout

```
.
├── Intro to R.R       # R/RStudio primer
├── Exercises.R        # Exercises 1-5 with answers
├── cornwell.csv       # panel: NC counties, police & crime
├── tutoring_program.csv  # RDD tutoring data
└── LICENSE
```

## Requirements

```r
install.packages(c(
  "tidyverse","broom","modelsummary","sandwich","lmtest",
  "Ecdat","ivreg","fixest","margins","wooldridge","knitr",
  "rddensity","ggdag"
))
```

The `wooldridge` package is also the source of the `ak91` dataset used in
Exercise 4 — see below.

## Data & big-file policy

- `cornwell.csv` and `tutoring_program.csv` are small and committed.
- `angristandkrueger1991.csv` (Angrist & Krueger 1991 Quarterly Journal of
  Economics; ~367k rows, ~10 MB) is **not** committed. `Exercises.R` reads it
  if present, else transparently falls back to the identical copy shipped in
  the `wooldridge` package (`wooldridge::ak91`).
- If you want the local CSV: download it and place it in this folder, e.g. from
  the data repository of the course/Stock & Watson textbook companion sites.

## Running

Run `Intro to R.R` and then `Exercises.R` top-to-bottom in RStudio, or:

```bash
Rscript Intro\ to\ R.R
Rscript Exercises.R
```

Results (regression tables, plots) print to the console / RStudio plots pane.

## License

MIT — see [LICENSE](LICENSE). Datasets (`cornwell`, `tutoring_program`,
`angristandkrueger1991`) are cited in the script headers and retain their
original sources' licenses; they are redistributed for reproducibility only.