#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "${ROOT_DIR}"

export RMD_FIG_PATH="../out/figs/"

Rscript -e 'source("rv/scripts/activate.R"); rmarkdown::render("analysis/fig2.Rmd", output_dir = "out")'
