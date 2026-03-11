library(ggplot2)
library(ggtext)
library(grid)
library(scales)

# colour used for different pipelines
met_cols <- c(
  seurat = "#abd878", scrapper = "#53b271", osca = "#043615",
  scanpy = "#fc8d59", rapids = "#d7301f"
)
# ordering for datasets used
dataset_levels <- c("sc-mix", "cb", "be1")

theme_paper <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(face = "bold", size = base_size + 1),
      plot.subtitle = element_markdown(size = base_size),
      axis.title.x = element_markdown(size = base_size, margin = margin(t = 4)),
      axis.title.y = element_markdown(
        size = base_size, angle = 90, margin = margin(r = 4)
      ),
      axis.title.y.right = element_markdown(
        size = base_size, angle = -90, margin = margin(l = 4)
      ),
      axis.text.x = element_markdown(size = base_size - 2),
      axis.text.y = element_markdown(size = base_size - 2),

      # Make axes feel intentional (minimal() hides these)
      axis.line = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks.length = unit(2.5, "pt"),

      # Grid: no grid
      panel.grid = element_blank(),

      # Spacing
      panel.spacing = unit(8, "pt"),
      plot.margin = margin(4, 4, 4, 4),

      # Facets
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = base_size),

      # Legend (works well both outside and embedded)
      legend.key = element_blank(),
      legend.title = element_markdown(size = base_size),
      legend.text = element_markdown(size = base_size - 1),
      legend.spacing = unit(0, "pt"),
      legend.box.background = element_blank()
    )
}

scale_color_method <- function(...) {
  scale_color_manual(values = met_cols, ...)
}

scale_fill_method <- function(...) {
  scale_fill_manual(values = met_cols, ...)
}
