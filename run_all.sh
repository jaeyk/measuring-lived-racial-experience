#!/usr/bin/env bash
set -euo pipefail

echo "=== 01: Data cleaning ==="
Rscript -e 'rmarkdown::render("src/01_data_cleaning.Rmd")'

echo "=== 02: Imputation ==="
Rscript -e 'rmarkdown::render("src/02_imputation.Rmd")'

echo "=== 03: Factor analysis ==="
Rscript -e 'rmarkdown::render("src/03_factor_analysis.Rmd")'

echo "=== 04: Regression analysis ==="
Rscript -e 'rmarkdown::render("src/04_reg_analysis.Rmd")'

echo "=== 05: Interaction analysis ==="
Rscript -e 'rmarkdown::render("src/05_interaction_analysis.Rmd")'

echo "=== All steps completed ==="
