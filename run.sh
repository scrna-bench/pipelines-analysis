#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "${ROOT_DIR}"

DOCS=(
  "fig1.Rmd"
  "fig2.Rmd"
  "supp1.Rmd"
  "supp2.Rmd"
  "supp3.Rmd"
  "supp4.Rmd"
  "supp5.Rmd"
  "supp6.Rmd"
  "supp7.Rmd"
)

for doc in "${DOCS[@]}"; do
  stem="${doc##*/}"; stem="${stem%.Rmd}"
  RMD_FIG_PATH="../out/figs/" \
    Rscript -e "source('rv/scripts/activate.R'); rmarkdown::render('analysis/${doc}', output_dir = 'out')"
done
