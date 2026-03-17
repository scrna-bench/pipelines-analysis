#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "${ROOT_DIR}"

DOCS=(
  "analysis/fig1.Rmd"
  "analysis/fig2.Rmd"
  "analysis/supp1.Rmd"
  "analysis/supp2.Rmd"
  "analysis/supp3.Rmd"
  "analysis/supp4.Rmd"
)

for doc in "${DOCS[@]}"; do
  stem="${doc##*/}"; stem="${stem%.Rmd}"
  RMD_FIG_PATH="../out/figs/" \
    Rscript -e "source('rv/scripts/activate.R'); rmarkdown::render('${doc}', output_dir = 'out')"
done
