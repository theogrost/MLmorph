# MLmorph

MLmorph integrates **morphological modeling** with **machine learning** (random forests) to support structured decision-making. The package enumerates a **morphospace**—the set of feasible configurations across selected variables—and estimates class probabilities over that space. It includes:

- utilities for factorizing inputs into analysis-ready factors,
- a wrapper to train random forest classifiers and report validation performance,
- construction of morphospaces with predicted probabilities and a flag for purely simulated combinations,
- a Shiny app for interactive scenario exploration.

## Installation

```r
# install.packages("devtools")
devtools::install_github("theogrost/MLmorph")
```

## Quick start

```r
library(MLmorph)

set.seed(1)
n  <- 120
y  <- factor(sample(letters[1:3], n, TRUE))
x1 <- factorize_numeric_vector(runif(n, 10, 20), breaks_no = 3)
x2 <- factorize_numeric_vector(runif(n,  1,  2),  breaks_no = 4)
df <- data.frame(y, x1, x2)

# 1) Train a random forest and validate on a holdout
fit <- create_rf_model(df, dependent = "y", ntree = 100, train_validate_split = 0.75)
fit$model                       # randomForest object
head(fit$variables_importance)  # importance per predictor
fit$model_performance_on_test   # caret::confusionMatrix
```

## Morphospace: predicted probabilities over all configurations

```r
ms <- create_morphospace(df, fit$model)

str(ms$morphospace)
# Columns: predictors (x1, x2), '<dependent>' (class label), 'calculated' (probability),
#          'purely_simulated' (TRUE if configuration not observed in df)

# Example: top 10 configurations by predicted probability
head(ms$morphospace[order(ms$morphospace$calculated, decreasing = TRUE), ], 10)

# Probability mass per class (sums to 1 for each unique predictor combination)
aggregate(calculated ~ x1 + x2, data = ms$morphospace, sum)
```

## Factorization helpers

```r
# Numeric → ordered factor via equal-frequency bins (default)
z  <- runif(12)
fz <- factorize_numeric_vector(z, breaks_no = 3)

# Logical → factor with labeled levels
fb <- factorize_binary_vector(c(TRUE, FALSE, TRUE))

# Character → factor with stable, numbered labels
fc <- factorize_character_vector(c("alpha", "beta", "alpha"))

# Apply heuristics column-wise
df2 <- factorize_nicely_dataframe(data.frame(
  a = runif(20), b = c(TRUE, FALSE), c = c("x","y","x","z")
))
str(df2)
```

## Loading data from files

```r
# CSV
tmp_csv <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(a = 1:3, b = c("x","y","z")), tmp_csv, row.names = FALSE)
dat_csv <- load_data(tmp_csv)

# JSON
tmp_json <- tempfile(fileext = ".json")
jsonlite::write_json(list(a = 1:2, b = c("u","v")), tmp_json, auto_unbox = TRUE)
dat_json <- load_data(tmp_json)

# XLSX
tmp_xlsx <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(data.frame(a = 1:2, b = c("m","n")), tmp_xlsx)
dat_xlsx <- load_data(tmp_xlsx)
```

## Shiny app

```r
# Launch the interactive app from the installed package
# (opens in browser; not run during automated checks)
# MLmorph()
```

## Reproducibility and citation

To ensure reproducibility, record:
- the variable set and factorization (binning method and parameters),
- model hyperparameters (e.g., `ntree`),
- the random seed and train/validate split.

If you use MLmorph in research or consulting reports, please cite the package and your model specification.

## License

MIT © Oskar Kosch. See `LICENSE.md` for details.

## Issues

Please report bugs or feature requests at this repository's Issues page.
