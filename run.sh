#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "${ROOT_DIR}"

DOCS=(
  "fig-timings.Rmd"
  "fig-ari.Rmd"
  "supp-leiden-vs-louvain.Rmd"
  "supp-ari-vs-clusters.Rmd"
  "supp-rapids-version.Rmd"
  "supp-pca-correlation.Rmd"
  "supp-timings-breakdown.Rmd"
  "supp-timings-summary.Rmd"
  "supp-ari-vs-params.Rmd"
  "supp-linear-modelling.Rmd"
)

for doc in "${DOCS[@]}"; do
  stem="${doc##*/}"; stem="${stem%.Rmd}"
  RMD_FIG_PATH="../out/figs/" \
    Rscript -e "source('rv/scripts/activate.R'); rmarkdown::render('analysis/${doc}', output_dir = 'out')"
done
