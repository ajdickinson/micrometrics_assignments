library(pacman)
p_load(fixest, mvtnorm, magrittr, tidyverse, broom)

n_obs = 1000
n_cluster = 20
rho = 1
iter = 1


# individual level 
Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
values_i <- rmvnorm(n = n, sigma = Sigma_i)
# cluster level
cluster_name <- rep(1:n_cluster, each = n / n_cluster)
Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
# error consists of individual- and cluster-level components
e1 <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
e2 <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
# now introduce treatment that varies across i within c and across c
treat_ish = values_i[,1] + rep(values_cl[ , 2], each = n_obs / n_cluster)
treat = ifelse(treat_ish > 1, 1, 0)
# data generating process
y <- 1 + treat + e1 + e2 
d_iter <- tibble(cluster = cluster_name, e1, e2, y, treat)
# regress y on treat
m = feols(data = d_iter, y ~ treat)
m1 = m %>% tidy()
m2 = m %>% tidy(cluster = c("cluster"))
# m1 = lm_robust(y ~ treat, data = d_iter, clusters = cluster) %>% tidy()
# m2 = lm_robust(y ~ treat, data = d_iter, clusters = cluster) %>% tidy()
# save model estimates
bind_rows(m1, m2) %>%
  select(1:5) %>% filter(term == "treat") %>%
  mutate(diff = estimate[1] - estimate[2],
         lower = estimate - 1.96*(std.error/sqrt(n_obs)),
         upper = estimate + 1.96*(std.error/sqrt(n_obs)),
         se_type = c("normal", "clustered"),
         i = iter,
         rho = rho)



a = gen_cluster(n_cluster = 10, n = 300, rho = 0)
m1 = feols(data = df, y ~ x1 + x2)
summary(m1, cluster=c("cluster"))
summary(m1)

tidy(m1)
m2 <- tidy(m1, cluster = c("cluster"))
