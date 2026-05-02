
cols <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#000000"  # black
)

light9 <- c(
  "#DCEAF7",  # light blue
  "#FBE5D6",  # light peach
  "#E2F0D9",  # light green
  "#FFF2CC",  # light yellow
  "#EADCF8",  # light lavender
  "#F4CCCC",  # light rose
  "#D9EAD3",  # pale sage
  "#D0E0E3",  # light teal
  "#FCE5CD"   # light apricot
)

read_aggregates <- function(f) {
  require(dplyr)
  require(rjson)
  lines <- readLines(f)
  samples <- lapply(lines[-1], fromJSON) %>%
    lapply(FUN = .subset, "aggregated") %>%
    lapply(FUN = data.frame) %>%
    bind_rows()
}

read_sandwiches <- function(f, type=1) {
  keepers <- c("pca","t_sne","umap", "louvain", "leiden")
  require(dplyr)
  require(rjson)
  if(type==1) # R
    READER <- function(u) readLines(u)
  else if (type==2) # Python
    READER <- function(u) readLines(u) %>% 
      paste0(collapse="")
  minmax_list <- READER(f) %>% fromJSON %>%
    .subset(keepers) 
  sapply(minmax_list, setNames, c("xmin","xmax")) %>% 
    t %>% as.data.frame
}

#ggplot() + geom_rect(data = 

plot_annotated_profiles <- function(u,v,w, mx=900, 
                                    suppress.right = FALSE, suppress.left = FALSE) {
  require(ggplot2)
  min_time <- min(u$aggregated.ts_ms/1000)
  mx_memory <- max(u$aggregated.mem_rss_kb / 1024)
  spacing <- mx / 10
  w$y <- seq(.9*mx, .9*mx-spacing*(nrow(w)-1), by=-spacing)
  w <- w %>% filter(!is.null(xmin))
  p <- ggplot(u, aes(aggregated.ts_ms/1000-min_time, 
                aggregated.cpu_usage)) +
    geom_rect(
      data = w,
      aes(xmin = xmin-min_time, xmax = xmax-min_time,
          ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = light9[seq_len(nrow(w))],
      alpha = 0.7
    ) +
    geom_point() +
    geom_line() +
    geom_line(aes(y = aggregated.mem_rss_kb/1024/mx_memory*mx), color = "#009E73") +
    scale_y_continuous(
      name = "cpu usage (%)",
      sec.axis = sec_axis(~ (.) * mx_memory / mx, name = "memory (MB)"),
      limits = c(0, mx)
    ) +
    #coord_cartesian(ylim = c(0, 900)) +
    geom_hline(yintercept = c(100,400,800), 
               colour=c("blue","orange","grey")) +
    geom_label(data = w, aes(x = xmin-min_time, y = y,
                             label = rownames(w)),
               inherit.aes = FALSE, hjust = 0, size = 2) +
    ggtitle(v) +
    # scale_y_sqrt() +
    xlab("time (s)") +
    theme_bw()
   if(suppress.right) {
     p <- p + theme(axis.text.y.right = element_text(color = "#009E73"),
                    axis.title.y.right = element_blank())
   } else {
     p <- p + theme(axis.title.y.right = element_text(color = "#009E73"))
   }
   if(suppress.left) {
     p <- p + theme(axis.text.y.left = element_blank(),
                    axis.title.y.left = element_blank())
   }
  return(p)
}
