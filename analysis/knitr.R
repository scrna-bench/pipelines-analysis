knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  dpi = 300, fig.retina = 1,
  dev = c("png", "pdf"), dev.args = list(png = list(type = "cairo")),
  fig.path = Sys.getenv("RMD_FIG_PATH", "figs/")
)
