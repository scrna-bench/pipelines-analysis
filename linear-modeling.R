

library(here)
library(dplyr)

data_dir <- here("data")

metrics <- readr::read_tsv(file.path(data_dir, "out", "report", "metrics.tsv"))
timings <- readr::read_tsv(file.path(data_dir, "out", "report", "timings.tsv"))

metrics <- metrics %>%
  mutate(d_cluster = as.factor(d_cluster),
         n_comp = as.factor(n_comp),
         n_neig = as.factor(n_neig),
         n_hvg = as.factor(n_hvg)) %>%
  mutate(d_cluster = relevel(d_cluster, ref = "0"))

f.cb <- lm(agree_ari_leiden ~ d_cluster + 
          filtering + n_comp + n_neig + n_hvg +
          d_cluster*n_comp + d_cluster*n_neig + d_cluster*n_hvg +
          n_comp*n_neig + n_comp*n_hvg + n_neig*n_hvg,
        data = metrics %>% filter(dataset=="cb"))
summary(f.cb)

f.be1 <- lm(agree_ari_leiden ~ d_cluster + 
          filtering + n_comp + n_neig + n_hvg +
          d_cluster*n_comp + d_cluster*n_neig + d_cluster*n_hvg +
            n_comp*n_neig + n_comp*n_hvg + n_neig*n_hvg,
          data = metrics %>% filter(dataset=="be1"))
summary(f.be1)

f.scmix <- lm(agree_ari_leiden ~ d_cluster + 
          filtering + n_comp + n_neig + n_hvg +
          d_cluster*n_comp + d_cluster*n_neig + d_cluster*n_hvg +
            n_comp*n_neig + n_comp*n_hvg + n_neig*n_hvg,
          data = metrics %>% filter(dataset=="sc-mix"))
summary(f.scmix)

coefs <- data.frame(cb=f.cb$coefficients,
                    be1=f.be1$coefficients,
                    scmix=f.scmix$coefficients)

pairs(coefs, lower.panel = NULL)


coefs <- data.frame(cb=f.cb$coefficients,
                    be1=f.be1$coefficients,
                    scmix=f.scmix$coefficients)
coefs$grp <- gsub("[0-9]","",names(f.cb$coefficients))
coefs$grp <- gsub("-","",coefs$grp)

library(reshape2)
library(ggplot2)

coefs_m <- melt(coefs)

ggplot(coefs_m, aes(grp, value, colour=variable, group=variable)) + 
  geom_point(size = 3, position=position_dodge(width = .4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


library(lme4)
# Fit model
model.cb <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                    filtering + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg), 
                  data=metrics %>% filter(dataset=="cb"))

model.be1 <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                      filtering + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg), 
                    data=metrics %>% filter(dataset=="be1"))

model.scmix <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                filtering + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg), 
              data=metrics %>% filter(dataset=="sc-mix"))

get_varcomps <- function(m, label = "xx") {
  var_comps <- as.data.frame(VarCorr(m)) %>%
    select(grp, vcov)
  var_comps$vcov <- var_comps$vcov / sum(var_comps$vcov)
  data.frame(dataset = label, var_comps)
}


vp <- mapply( function(u,v)  get_varcomps(u, v),
              list(model.cb, model.be1, model.scmix), 
              c("cb","be1","sc-mix"), SIMPLIFY = FALSE) %>% 
  bind_rows()

ggplot(vp, aes(grp, vcov, group=dataset, colour = dataset)) + 
  geom_point(size = 3, position=position_dodge(width = .4))

