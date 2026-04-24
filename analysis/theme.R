library(ggplot2)
library(ggtext)
library(grid)
library(scales)

# colour used for different pipelines
### Pauline: I find that these colors are easier to distinguish,

met_cols <- c(
  seurat = "#009988", scrapper = "#33BBEE", osca = "#0077BB",  
  scanpy = "#EE7733", rapids = "#CC3311"   
)

# colours for different number of PCA components
pca_cols <- c("#FFB000", "#FE6100", "#DC267F")
# colours for different number of neighbours in NN graph
n_neig_cols <- c("#F1E6E1FF", "#BC927BFF", "#7E5945FF", "#2B1917FF")
# colours for datasets used
dataset_cols <- c("sc-mix" = "#1E88E5", cb = "#FFC107", be1 = "#D81B60")

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
      panel.spacing = unit(4, "pt"),
      plot.margin = margin(2, 2, 2, 2),

      # Facets
      strip.background = element_blank(),
      strip.text = element_markdown(face = "bold", size = base_size - 2),

      # Legend (works well both outside and embedded)
      legend.key = element_blank(),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 2),
      legend.spacing = unit(0, "pt"),
      legend.box.background = element_blank()
    )
}

scale_color_method <- function(...) {
  scale_color_manual(..., values = met_cols)
}

scale_fill_method <- function(...) {
  scale_fill_manual(..., values = met_cols)
}

scale_color_pca <- function(...) {
  scale_color_manual(..., name = "PCs", values = pca_cols)
}

scale_color_n_neig <- function(...) {
  scale_color_manual(..., name = "Nghbrs", values = n_neig_cols)
}

scale_color_dataset <- function(...) {
  scale_color_manual(..., values = dataset_cols)
}

scale_fill_dataset <- function(...) {
  scale_fill_manual(..., values = dataset_cols)
}
