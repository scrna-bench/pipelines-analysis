library(dplyr)
library(tidyr)
library(here)

data_dir <- here("data")

metrics <- readr::read_tsv(file.path(data_dir, "out", "report", "metrics.tsv"))
timings <- readr::read_tsv(file.path(data_dir, "out", "report", "timings.tsv"))
billato <- readr::read_csv(file.path(data_dir, "billato.csv"))

keys_m <- names(metrics)[names(metrics) %in% names(timings)]
keys_t <- names(timings)[names(timings) %in% names(metrics)]

stopifnot(identical(keys_m, keys_t))

stopifnot(identical(
  metrics %>% select(all_of(keys_m)),
  timings %>% select(all_of(keys_t)),
))

timing_value_cols <- setdiff(names(timings), keys_m)
timings <- timings %>%
  mutate(across(
    all_of(timing_value_cols), ~ readr::parse_number(as.character(.x))
  )) %>%
  rename_with(~ paste0("timings_", .x), all_of(timing_value_cols))

metrics_all <- bind_cols(metrics, select(timings, -all_of(keys_m)))

metrics_all <- metrics_all %>%
  mutate(
    d_cluster = as.integer(d_cluster),
    n_comp = as.integer(n_comp),
    n_neig = as.integer(n_neig),
    n_hvg = as.integer(n_hvg),
    dataset = factor(dataset, levels = names(dataset_cols)),
    method = factor(method, levels = names(met_cols)),
    filtering = as.factor(filtering),
    module = as.factor(module)
  )


metrics_long <- metrics_all %>%
  pivot_longer(
    cols = c(
      agree_ari_leiden, agree_ari_louvain,
      struc_silhouette_leiden, struc_silhouette_louvain,
      resolution_leiden, resolution_louvain,
      n_clusters_leiden, n_clusters_louvain
    ),
    names_to = c(".value", "algorithm"),
    names_pattern =
      "^(agree_ari|struc_silhouette|n_clusters|resolution)_(leiden|louvain)$"
  ) %>%
  rename(
    ari = agree_ari,
    silhouette = struc_silhouette
  ) %>%
  mutate(
    n_clusters = as.integer(n_clusters),
    resolution = as.numeric(resolution),
    algorithm = as.factor(algorithm)
  )

metrics_new <- metrics_all %>%
  filter(module %in% c("pipelines-r", "pipelines-py"))
metrics_long_new <- metrics_long %>%
  filter(module %in% c("pipelines-r", "pipelines-py"))
