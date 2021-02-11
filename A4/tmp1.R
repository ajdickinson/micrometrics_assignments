library(pacman)
p_load(fixest, mvtnorm, magrittr, tidyverse, broom, estimatr)

n_obs = 1000
n_cluster = 20
rho = 0
iter = 1


## individual level
Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
values_i = rmvnorm(n = n_obs, sigma = Sigma_i)
## cluster level
cluster_name = rep(1:n_cluster, each = n_obs / n_cluster)
Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
values_cl = rmvnorm(n = n_cluster, sigma = Sigma_cl)
d_iter = tibble(
  cluster = cluster_name,
  ## error consists of individual- and cluster-level components
  e1 = values_i[ , 1] + rep(values_cl[ , 1], each = n_obs / n_cluster),
  e2 = values_i[ , 2] + rep(values_cl[ , 2], each = n_obs / n_cluster),
  ## now introduce treatment that varies across i
  treat_i = sample(c(0,1), n_obs, replace = TRUE),
  treat_c = rep(0:1, each = n_obs / n_cluster, len = n_obs),
  ## data generating process for both treatment levels i and c
  y_i = 1 + treat_i,
  y_c = 1 + treat_c
)
## run each model; treatment at i and c
# m_i1 = lm_robust(y_i ~ treat_i, data = d_iter) %>% tidy()
# m_i2 = lm_robust(y_i ~ treat_i, data = d_iter, clusters = cluster) %>% tidy()
# m_c1 = lm_robust(y_c ~ treat_c, data = d_iter) %>% tidy()
# m_c2 = lm_robust(y_c ~ treat_c, data = d_iter, clusters = cluster) %>% tidy()
m_i = feols(data = d_iter, y_i ~ treat_i)
m_c = feols(data = d_iter, y_c ~ treat_c)
## package into single data frame that will output into a list
m_i1 = m_i %>% tidy()
m_i2 = m_i %>% tidy(cluster = c("cluster"))
m_c1 = m_c %>% tidy()
m_c2 = m_c %>% tidy(cluster = c("cluster"))
bind_rows(m_i1, m_i2, m_c1, m_c2) %>%
  select(1:5) %>% filter(term %in% c("treat_i", "treat_c")) %>%
  mutate(diff_i = estimate[1] - estimate[2],
         diff_c = estimate[3] - estimate[4],
         lower = -1.96*(std.error/sqrt(n_obs)),
         upper = 1.96*(std.error/sqrt(n_obs)),
         se_type = c("normal", "clustered","normal", "clustered"),
         i = iter,
         rho = rho)



a = gen_cluster(n_cluster = 10, n = 300, rho = 0)
m1 = feols(data = df, y ~ x1 + x2)
summary(m1, cluster=c("cluster"))
summary(m1)

tidy(m1)
m2 <- tidy(m1, cluster = c("cluster"))
