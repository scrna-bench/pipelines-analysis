

library(here)
library(dplyr)
library(reshape2)
library(ggplot2)


data_dir <- here("data")

metrics <- readr::read_tsv(file.path(data_dir, "out", "report", "metrics.tsv")) %>%
  dplyr::filter(module != "rapids pipelines-py-old")

metrics <- metrics %>%
  mutate(d_cluster = as.factor(d_cluster),
         method = factor(method, levels = c("osca", "rapids", "scanpy", 
                                            "scrapper", "seurat")),
         n_comp = as.factor(n_comp),
         n_neig = as.factor(n_neig),
         n_hvg = as.factor(n_hvg)) %>%
  mutate(d_cluster = relevel(d_cluster, ref = "0"))

# +  

f.cb <- lm(agree_ari_leiden ~ method + d_cluster + 
            # d_cluster*method + n_comp*method + n_hvg*method + n_neig*method +
          filtering + n_comp + n_neig + n_hvg,
        data = metrics %>% filter(dataset=="cb"))
summary(f.cb)

f.be1 <- lm(agree_ari_leiden ~ method + d_cluster + 
          filtering + n_comp + n_neig + n_hvg,
          #d_cluster*n_comp + d_cluster*n_neig + d_cluster*n_hvg +
            #d_cluster*method + n_comp*method + n_hvg*method + n_neig*method +
            #n_comp*n_neig + n_comp*n_hvg + n_neig*n_hvg,
          data = metrics %>% filter(dataset=="be1"))
summary(f.be1)

f.scmix <- lm(agree_ari_leiden ~ method + d_cluster + 
          filtering + n_comp + n_neig + n_hvg,
          # +
          #d_cluster*n_comp + d_cluster*n_neig + d_cluster*n_hvg +
          #  d_cluster*method + n_comp*method + n_hvg*method + n_neig*method +
          #  n_comp*n_neig + n_comp*n_hvg + n_neig*n_hvg,
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
coefs$grp[grep("method",coefs$grp)] <- "method"
coefs$grp[grep("filtering",coefs$grp)] <- "filtering"

coefs_m <- melt(coefs)


ts <- data.frame(t.cb=summary(f.cb)$coefficients[,"t value"],
                 t.be1=summary(f.be1)$coefficients[,"t value"],
                 t.scmix=summary(f.scmix)$coefficients[,"t value"])
ts$grp <- gsub("[0-9]","",names(f.cb$coefficients))
ts$grp <- gsub("-","",coefs$grp)
ts$grp[grep("method",ts$grp)] <- "method"
ts$grp[grep("filtering",ts$grp)] <- "filtering"


# # https://stackoverflow.com/questions/37446064/i-need-ggplot-scale-x-log10-to-give-me-both-negative-and-positive-numbers-as-o
# weird <- scales::trans_new("signed_log",
#                            transform=function(x) sign(x)*sqrt(abs(x)),
#                            inverse=function(x) sign(x)*abs(x^2))

ts_m <- melt(ts)

coefs_m <- cbind(coefs_m, ts_m %>% 
                   mutate(t_value = value) %>% 
                   select(-grp, -variable, -value) )


#> qnorm(1-(.025)/57)
#[1] 3.327213
coefs_m$grp[coefs_m$grp=="(Intercept)"] <- "Intercept"
coefs_m$grp <- factor(coefs_m$grp,
                      levels = c("Intercept", "method", "d_cluster", 
                                 "filtering", "n_comp",
                                 "n_neig", "n_hvg"))

# , "d_cluster:n_comp",
#        "d_cluster:n_neig", "d_cluster:n_hvg",
#                                  "n_comp:n_neig", "n_comp:n_hvg",
#                                  "n_neig:n_hvg"))

coefs_m$Baseline <- "Relative"
coefs_m$Baseline[coefs_m$grp=="Intercept"] <- "Absolute"


p1 <- ggplot() +
  geom_point(aes(grp, value, colour=variable, group=variable),
             data = coefs_m %>% dplyr::filter(abs(t_value) < 2.5),
             size = 1.5, position=position_dodge(width = .4),
             shape = 21) +
  geom_point(aes(grp, value, colour=variable, group=variable),
             data = coefs_m %>% dplyr::filter(abs(t_value) > 2.5),
             size = 3, position=position_dodge(width = .4),
             shape = 19) +
  #geom_vline(xintercept = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 10),
        legend.position=c(.85,.85),
        legend.box.background = element_rect(colour = "black")) +
  xlab("Parameter group") +
  ylab("Coefficient") +
  guides(colour=guide_legend(title="dataset")) +
  facet_grid(~Baseline, scales = "free", space = "free") +
  geom_rect(data = subset(coefs_m, Baseline == "Absolute"), 
            fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
            ymin = -Inf, ymax = Inf)
p1
  


# ggplot(ts_m, 
#        aes(grp, value, colour=variable, group=variable)) + 
#   geom_point(size = 3, position=position_dodge(width = .4)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
#   scale_y_continuous(trans=weird) +
#   xlab("Parameter group") +
#   ylab("t-statistic")
 



# didn't use these below


library(lme4)
# Fit model
model.cb <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                    (1 | filtering) + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg) +
                   (1 | method), 
                  data=metrics %>% filter(dataset=="cb"))

model.be1 <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                      (1 | filtering) + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg) +
                    (1 | method), 
                    data=metrics %>% filter(dataset=="be1"))

model.scmix <- lmer(agree_ari_leiden ~ (1 | d_cluster) + 
                (1| filtering) + (1 | n_comp) + (1 | n_neig) + (1 | n_hvg) +
                  (1 | method), 
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

vp$grp <- factor(vp$grp,
                 levels = c("Intercept", "method", "d_cluster", 
                            "filtering", "n_comp", "n_neig", 
                            "n_hvg", "Residual"))


# ggplot(vp %>% filter(vcov > .000001), 
p2 <- ggplot(vp, aes(grp, vcov, group=dataset, colour = dataset)) + 
  geom_point(size = 3, position=position_dodge(width = .4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 10),
        legend.position="none") +
  xlab("Parameter group") +
  ylab("% variance explained")
p2


library(cowplot)

q <- plot_grid(p1, p2, labels = c('A', 'B'), nrow = 2,
          rel_heights = c(1.6,1))
q


ggsave("supp-lms.pdf", plot=q, width=8, height = 9)


# ggplot(vp, aes(grp, group=dataset, colour = dataset)) + 
#   geom_bar(aes(fill = vcov)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 25, hjust = 1))
